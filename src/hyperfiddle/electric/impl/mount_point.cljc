(ns hyperfiddle.electric.impl.mount-point "
A mount-point instance maintains :
* a hash map storing items indexed by tag. An item is a mutable object tracking the lifecycle of each entry in the
  resulting incseq.
* a set of active readers. Each mutation of the store sends an invalidation event for the item being touched to the
  readers currently active.

A reader process maintains :
* a call tree isomorphic to the subset of the application's call tree restricted to the ancestors of active items.
  Leaves are items, nodes are either blocks or calls, block children are items or calls, call children are blocks, the
  root is a call.
* a hash map storing blocks indexed by frame. A block is a mutable object tracking each known frame. A frame is known
  when either it's an ancestor of an active item, or it's currently being mounted by a call that is in the common
  ancestry of an active item.
* a mailbox for step events on calls and another one for invalidation events on items. Both mailboxes are consumed
  during reader process transfer, call events take priority over the item events but ordering of events within a single
  mailbox is irrelevant. The processing of each event mutates the call tree and generates a diff, then the concatenation
  of successive diffs is returned. When the reader is spawned, an invalidation event is posted for each active item.

On item invalidation event :
* If the item is inactive :
  * If it was attached in the call tree, it is detached from the tree and a shrink is generated if the item was mounted.
  * If it was detached from the call tree, nothing happens.
* If the item is active :
  * If it was attached in the call tree, a change is generated if the item was mounted.
  * If it was detached from the call tree, it is attached to the tree and a grow is generated if the item was mounted.

On call step event :
1. Apply permutation. The call permutation must be expanded to take into account the offset and length of the call
   segment in the current sequence state.
2. Apply changes. For each item change that is not a grow, the block associated to previous frame is unmounted. The new
   frame is then associated to its block and mounted.
3. Apply shrinks. The blocks associated with removed frames are unmounted.

Unmounting a block generates a shrink for each active item having this block's frame as an ancestor.
Mounting a block generates a grow for each active item having this block's frame as an ancestor.
" (:require [hyperfiddle.kvs :refer [KVS]]
            [hyperfiddle.incseq.arrays-impl :as a]
            [hyperfiddle.incseq.fixed-impl :as f]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.electric.impl.runtime3 :as r])
  #?(:clj (:import (clojure.lang IFn IDeref)
                   (java.util.concurrent.locks ReentrantLock))))

;; TODO
;; do not spawn the call until it has two children at least.
;; maintain a weight tree on each call to prevent buffer traversal when computing local block index
;; use mutable hash maps to decrease GC pressure (item store + block store)
;; support concurrent readers
;; deprecate kvs/update! and expose diffs as a stateless incseq
;; get rid of the lock

(def slot-lock 0)
(def slot-peer 1)
(def slot-items 2)
(def slot-reader 3)
(def slot-pending 4)
(def slots 5)

(def reader-slot-state 0)
(def reader-slot-step 1)
(def reader-slot-done 2)
(def reader-slot-call-queue 3)
(def reader-slot-item-queue 4)
(def reader-slot-root 5)
(def reader-slot-alive 6)
(def reader-slot-pending 7)
(def reader-slot-blocks 8)  ;; a map associating frames to blocks
(def reader-slots 9)

(def item-slot-parent 0)   ;; parent block, nil if not active
(def item-slot-queue 1)    ;; next item, nil if last item
(def item-slot-tag 2)      ;; static id
(def item-slot-state 3)    ;; any value, this after remove
(def item-slots 4)

(def call-slot-reader 0)    ;; the reader instance, immutable
(def call-slot-parent 1)    ;; parent block
(def call-slot-queue 2)     ;; next call, nil if last item
(def call-slot-buffer 3)    ;; current state of incremental sequence
(def call-slot-children 4)  ;; head of the doubly-linked list of child blocks
(def call-slot-weight 5)    ;; total count of entries in all mounted child frames
(def call-slot-process 6)   ;; the flow process instance
(def call-slots 7)

(def block-slot-parent 0)   ;; the parent call
(def block-slot-index 1)    ;; position of frame in parent call, nil if unmounted
(def block-slot-frame 2)    ;; static frame
(def block-slot-children 3) ;; static array of child calls
(def block-slot-weights 4)  ;; static int array representing a complete binary tree of child weights
(def block-slot-prev 5)     ;; previous sibling
(def block-slot-next 6)     ;; next sibling
(def block-slots 7)

(defn error [^String msg]
  (new #?(:clj Error :cljs js/Error) msg))

(defn enter [^objects state]
  #?(:clj (let [^ReentrantLock lock (aget state slot-lock)
                held (.isHeldByCurrentThread lock)]
            (.lock lock) held)
     :cljs (let [held (aget state slot-lock)]
             (aset state slot-lock true) held)))

(defn unlock [^objects state held]
  #?(:clj  (.unlock ^ReentrantLock (aget state slot-lock))
     :cljs (aset state slot-lock held)))

(defn exit [^objects state held]
  (if held
    (unlock state held)
    (let [pending (aget state slot-pending)]
      (aset state slot-pending nil)
      (unlock state held)
      (loop [^objects reader pending]
        (when-not (nil? reader)
          (let [pending (aget reader reader-slot-pending)]
            (aset reader reader-slot-pending nil)
            ((if (zero? (aget reader reader-slot-alive))
               (aget reader reader-slot-done)
               (aget reader reader-slot-step)))
            (recur pending)))))))

(defn ensure-capacity [^objects buffer cap]
  (let [n (alength buffer)]
    (if (< n cap)
      (let [b (object-array
                (loop [n n]
                  (let [n (bit-shift-left n 1)]
                    (if (< n cap) (recur n) n))))]
        (a/acopy buffer 0 b 0 n) b) buffer)))

(defn call-slot [^objects call]
  (r/frame-slot (aget ^objects (aget call call-slot-children) block-slot-frame)))

(defn call-weight [^objects call]
  (aget call call-slot-weight))

(defn block-weight [^objects block]
  (let [^ints weights (aget block block-slot-weights)]
    (aget weights 1)))

(defn local-block-offset [^objects call index]
  (let [^objects buffer (aget call call-slot-buffer)]
    (loop [index index
           offset 0]
      (if (zero? index)
        offset
        (let [index (dec index)]
          (recur index
            (if-some [block (aget buffer index)]
              (unchecked-add-int offset
                (block-weight block))
              offset)))))))

(defn local-call-index [^objects block index]
  (let [^ints weights (aget block block-slot-weights)]
    (loop [o 0, i (unchecked-add (bit-shift-right (alength weights) 1) index)]
      (case i
        1 o
        (recur (if (even? i)
                 o (unchecked-add o
                     (aget weights (unchecked-dec i))))
          (bit-shift-right i 1))))))

(defn call-index [^objects block id]
  (loop [^objects block block
         id id
         offset 0]
    (when-some [index (aget block block-slot-index)]
      (let [^objects call (aget block block-slot-parent)
            ^objects reader (aget call call-slot-reader)
            offset (unchecked-add-int
                     (unchecked-add-int offset
                       (local-call-index block id))
                     (local-block-offset call index))]
        (if (identical? call (aget reader reader-slot-root))
          offset (recur (aget call call-slot-parent)
                   (r/slot-id (call-slot call)) offset))))))

(defn swap-indices [^objects call i j]
  (let [^objects buffer (aget call call-slot-buffer)
        ^objects bi (aget buffer i)
        ^objects bj (aget buffer j)]
    (aset buffer i bj)
    (aset buffer j bi)
    (when-not (nil? bi)
      (aset bi block-slot-index j))
    (when-not (nil? bj)
      (aset bj block-slot-index i))))

(defn block-index [^objects call id]
  (let [^objects reader (aget call call-slot-reader)
        offset (local-block-offset call id)]
    (if (identical? call (aget reader reader-slot-root))
      offset (when-some [to (call-index (aget call call-slot-parent)
                              (r/slot-id (call-slot call)))]
               (unchecked-add-int offset to)))))

(defn current-size [^objects reader]
  (if-some [^objects call (aget reader reader-slot-root)]
    (aget call call-slot-weight) 0))

(defn reader-pending [^objects reader]
  (let [^objects state (aget reader reader-slot-state)]
    (aset reader reader-slot-pending (aget state slot-pending))
    (aset state slot-pending reader)))

(defn terminate [^objects reader]
  (when (zero? (aset reader reader-slot-alive
                 (dec (aget reader reader-slot-alive))))
    (reader-pending reader)))

(defn reader-event [^objects reader]
  (when (identical? reader (aget reader reader-slot-pending))
    (reader-pending reader)))

(defn enqueue-call [^objects reader ^objects call]
  (aset call call-slot-queue (aget reader reader-slot-call-queue))
  (aset reader reader-slot-call-queue call)
  (reader-event reader))

(defn enqueue-item [^objects reader ^objects item]
  (aset item item-slot-queue (aget reader reader-slot-item-queue))
  (aset reader reader-slot-item-queue item)
  (reader-event reader))

(defn update-local-weights [^ints weights id delta]
  (loop [i (unchecked-add (bit-shift-right (alength weights) 1) id)]
    (aset weights i (unchecked-add-int (aget weights i) delta))
    (when (< 1 i) (recur (bit-shift-right i 1)))))

(defn update-weights [^objects block id delta]
  (loop [^objects block block
         id id]
    (update-local-weights (aget block block-slot-weights) id delta)
    (when-not (nil? (aget block block-slot-index))
      (let [^objects call (aget block block-slot-parent)
            ^objects reader (aget call call-slot-reader)]
        (aset call call-slot-weight (+ delta (aget call call-slot-weight)))
        (when-not (identical? call (aget reader reader-slot-root))
          (recur (aget call call-slot-parent) (r/slot-id (call-slot call))))))))

(defn call-update-weights [^objects call delta]
  (let [^objects reader (aget call call-slot-reader)]
    (aset call call-slot-weight (+ (aget call call-slot-weight) delta))
    (when-not (identical? call (aget reader reader-slot-root))
      (update-weights (aget call call-slot-parent)
        (r/slot-id (call-slot call)) delta))))

(defn change [^objects item diff]
  (let [^objects block (aget item item-slot-parent)
        ^objects call (aget block block-slot-parent)]
    (if-some [index (call-index block (r/tag-index (aget item item-slot-tag)))]
      (d/combine diff
        {:grow        0
         :degree      (current-size (aget call call-slot-reader))
         :shrink      0
         :permutation {}
         :change      {index (aget item item-slot-state)}
         :freeze      #{}}) diff)))

(defn get-block [^objects reader frame]
  (get (aget reader reader-slot-blocks) frame))

(defn block-release [^objects block]
  (let [^objects call (aget block block-slot-parent)
        ^objects reader (aget call call-slot-reader)]
    (aset reader reader-slot-blocks
      (dissoc (aget reader reader-slot-blocks)
        (aget block block-slot-frame)))))

(defn call-release [^objects call]
  (let [^objects buffer (aget call call-slot-buffer)]
    (loop [i 0]
      (when (< i (alength buffer))
        (when-some [^objects block (aget buffer i)]
          (aset block block-slot-index nil)
          (aset buffer i nil)
          (block-release block)
          (recur (inc i)))))
    (aset call call-slot-parent nil)
    (aset call call-slot-children nil)
    ((aget call call-slot-process))))

(defn make-block [^objects reader frame]
  (let [size (r/frame-call-count frame)
        block (object-array block-slots)]
    (aset reader reader-slot-blocks
      (assoc (aget reader reader-slot-blocks) frame block))
    (aset block block-slot-frame frame)
    (aset block block-slot-children (object-array size))
    (aset block block-slot-weights (a/weight-tree size))
    block))

(defn make-call [^objects reader ^objects child]
  (let [call (object-array call-slots)]
    (aset call call-slot-reader reader)
    (aset call call-slot-buffer (object-array 1))
    (aset call call-slot-weight (identity 0))
    (aset call call-slot-children child)
    (aset child block-slot-prev child)
    (aset child block-slot-next child)
    (aset child block-slot-parent call)
    call))

(defn call-discard [^objects call]
  (try @(aget call call-slot-process)
       (catch #?(:clj Throwable :cljs :default) _)))

(defn call-init-frame [^objects call i f]
  (let [^objects buffer (aget call call-slot-buffer)
        ^objects reader (aget call call-slot-reader)]
    (if-some [block (get-block reader f)]
      (do (aset block block-slot-index i)
          (aset buffer i block)
          (call-update-weights call (block-weight block)))
      (let [^objects block (make-block reader f)]
        (aset block block-slot-parent call)
        (aset block block-slot-index i)
        (aset buffer i block))) call))

(defn call-spawn [^objects call]
  (let [^objects reader (aget call call-slot-reader)
        ^objects state (aget reader reader-slot-state)]
    (aset reader reader-slot-alive
      (inc (aget reader reader-slot-alive)))
    (let [ps ((if-some [slot (call-slot call)]
                (r/incseq (r/peer-root (aget state slot-peer)) slot)
                (f/flow (r/invariant (aget ^objects (aget call call-slot-children) block-slot-frame))))
              #(let [^objects reader (aget call call-slot-reader)
                     ^objects state (aget reader reader-slot-state)
                     held (enter state)]
                 (if (nil? (aget call call-slot-process))
                   (aset call call-slot-process call)
                   (if (nil? (aget call call-slot-children))
                     (call-discard call)
                     (enqueue-call reader call)))
                 (exit state held))
              #(let [^objects reader (aget call call-slot-reader)
                     ^objects state (aget reader reader-slot-state)
                     held (enter state)]
                 (terminate reader)
                 (exit state held)))
          init (identical? call (aget call call-slot-process))]
      (aset call call-slot-process ps)
      (if init
        (let [{:keys [degree change]} @ps]
          (aset call call-slot-buffer (ensure-capacity (aget call call-slot-buffer) degree))
          (reduce-kv call-init-frame call change))
        (throw (error "Uninitialized."))))))

(defn block-child [^objects block id]
  (let [^objects children (aget block block-slot-children)]
    (aget children id)))

(defn block-set-child [^objects block id child]
  (let [^objects children (aget block block-slot-children)]
    (aset children id child)))

(defn call-attach-to-block [^objects call ^objects block]
  (aset call call-slot-parent block)
  (block-set-child block (r/slot-id (call-slot call)) call))

(defn block-single-child [^objects block]
  (let [frame (aget block block-slot-frame)
        ^objects children (aget block block-slot-children)]
    (loop [r nil
           i 0]
      (if (< i (alength children))
        (if (nil? (r/frame-call frame i))
          (recur r (unchecked-inc-int i))
          (if-some [c (aget children i)]
            (when (nil? r)
              (recur c (unchecked-inc-int i)))
            (recur r (unchecked-inc-int i)))) r))))

(defn call-make-ancestors [call]
  (let [^objects reader (aget call call-slot-reader)]
    (loop [^objects call call]
      (when-some [slot (call-slot call)]
        (let [block (make-block reader (r/slot-frame slot))]
          (call-attach-to-block call block)
          (recur (make-call reader block)))))))

(defn block-attach-to-call [^objects block]
  (let [^objects call (aget block block-slot-parent)
        ^objects prev (aget call call-slot-children)
        ^objects next (aget prev block-slot-next)]
    (aset block block-slot-prev prev)
    (aset block block-slot-next next)
    (aset prev block-slot-next block)
    (aset next block-slot-prev block)))

(defn root-up [^objects block]
  (when (nil? (aget block block-slot-prev))
    (block-attach-to-call block))
  (let [^objects call (aget block block-slot-parent)
        ^objects reader (aget call call-slot-reader)]
    (when (nil? (aget call call-slot-process))
      (loop [^objects root (aget reader reader-slot-root)]
        (let [^objects block (aget root call-slot-parent)
              ^objects parent (aget block block-slot-parent)]
          (update-local-weights (aget block block-slot-weights)
            (r/slot-id (call-slot root)) (call-weight root))
          (aset reader reader-slot-root parent)
          (call-spawn parent)
          (when (nil? (aget call call-slot-process))
            (recur parent)))))))

(defn root-down [^objects call]
  (let [^objects reader (aget call call-slot-reader)]
    (when (identical? call (aget reader reader-slot-root))
      (aset reader reader-slot-root
        (loop [^objects call call]
          (let [^objects block (aget call call-slot-children)]
            (if (identical? block (aget block block-slot-prev))
              (if-some [^objects child (block-single-child block)]
                (let [^objects parent (aget call call-slot-parent)]
                  (call-attach-to-block (make-call reader block) parent)
                  (call-release call)
                  (recur child)) call) call)))))))

(defn block-attach-to-tree [^objects block ^objects reader]
  (loop [^objects block block]
    (let [slot (r/frame-slot (aget block block-slot-frame))
          frame (r/slot-frame slot)]
      (if-some [^objects parent (get-block reader frame)]
        (do (if-some [call (block-child parent (r/slot-id slot))]
              (do (aset block block-slot-parent call)
                  (block-attach-to-call block))
              (let [call (make-call reader block)]
                (call-attach-to-block call parent)
                (call-spawn call)))
            (root-up parent))
        (let [parent (make-block reader frame)
              call (make-call reader block)]
          (call-attach-to-block call parent)
          (call-spawn call)
          (recur parent))))))

(defn item-attach-to-block [^objects item ^objects block id diff]
  (let [^objects call (aget block block-slot-parent)
        ^objects reader (aget call call-slot-reader)
        size-before (current-size reader)]
    (block-set-child block id item)
    (aset item item-slot-parent block)
    (update-weights block id 1)
    (if-some [index (call-index block id)]
      (d/combine diff
        {:grow        1
         :degree      (inc size-before)
         :shrink      0
         :permutation (p/rotation size-before index)
         :change      {index (aget item item-slot-state)}
         :freeze      #{}}) diff)))

(defn item-attach-to-tree [^objects item ^objects reader diff]
  (let [tag (aget item item-slot-tag)
        frame (r/tag-frame tag)
        id (r/tag-index tag)]
    (if-some [^objects root (aget reader reader-slot-root)]
      (let [weight (call-weight root)
            block (if-some [block (get-block reader frame)]
                    (doto block root-up)
                    (doto (make-block reader frame)
                      (block-attach-to-tree reader)))]
        (item-attach-to-block item block id
          (if (= weight (call-weight (aget reader reader-slot-root)))
            diff (d/combine diff
                   {:grow        0
                    :degree      weight
                    :shrink      weight
                    :permutation {}
                    :change      {}
                    :freeze      #{}}))))
      (let [block (make-block reader frame)
            call (make-call reader block)]
        (aset reader reader-slot-root call)
        (call-make-ancestors call)
        (call-spawn call)
        (item-attach-to-block item block id diff)))))

(defn block-empty? [^objects block]
  (let [^objects children (aget block block-slot-children)]
    (loop [i 0]
      (if (< i (alength children))
        (if (nil? (aget children i))
          (recur (unchecked-inc-int i)) false) true))))

(defn detach-root [^objects block id]
  (let [^objects call (aget block block-slot-parent)
        ^objects parent (aget call call-slot-parent)]
    (block-set-child block id nil)
    (aset call call-slot-parent nil)
    (aset call call-slot-children nil)
    (aset block block-slot-prev nil)
    (aset block block-slot-next nil)
    (block-release block)
    (when-not (nil? parent)
      (recur parent (r/slot-id (r/frame-slot (aget block block-slot-frame)))))))

(defn item-detach-from-tree [^objects item diff]
  (let [^objects block (aget item item-slot-parent)
        ^objects call (aget block block-slot-parent)
        id (r/tag-index (aget item item-slot-tag))
        size-before (current-size (aget call call-slot-reader))
        diff (if-some [index (call-index block id)]
               (d/combine diff
                 {:grow        0
                  :degree      size-before
                  :shrink      1
                  :permutation (p/rotation index (dec size-before))
                  :change      {}
                  :freeze      #{}}) diff)]
    (update-weights block id -1)
    (aset item item-slot-parent nil)
    (aset ^objects (aget block block-slot-children) id nil)
    (loop [^objects block block]
      (when (block-empty? block)
        (when (nil? (aget block block-slot-index))
          (block-release block))
        (let [^objects prev (aget block block-slot-prev)
              ^objects next (aget block block-slot-next)
              ^objects call (aget block block-slot-parent)]
          (if (identical? block prev)
            (let [^objects reader (aget call call-slot-reader)
                  ^objects parent (aget call call-slot-parent)
                  slot (r/frame-slot (aget block block-slot-frame))]
              (aset block block-slot-prev nil)
              (aset block block-slot-next nil)
              (call-release call)
              (if (identical? call (aget reader reader-slot-root))
                (do (aset reader reader-slot-root nil)
                    (when-not (nil? parent)
                      (detach-root parent (r/slot-id slot))))
                (do (block-set-child parent (r/slot-id slot) nil)
                    (recur parent))))
            (do (aset call call-slot-children prev)
                (aset prev block-slot-next next)
                (aset next block-slot-prev prev)
                (root-down call))))))
    diff))

(defn apply-permutation [^objects call permutation diff]
  (let [^objects buffer (aget call call-slot-buffer)
        ^objects reader (aget call call-slot-reader)
        degree (current-size reader)
        permutation (loop [p permutation
                           q {}]
                      (case p
                        {} q
                        (let [[i j] (first p)
                              k1 (min i j)
                              k2 (max i j)
                              i1 (block-index call k1)
                              i2 (block-index call k2)
                              w1 (if-some [b (aget buffer k1)]
                                   (block-weight b) 0)
                              w2 (if-some [b (aget buffer k2)]
                                   (block-weight b) 0)]
                          (swap-indices call i j)
                          (recur (p/compose p (p/transposition i j))
                            (if (nil? i1)
                              q (if (nil? i2)
                                  q (p/compose (p/split-long-swap i1 w1 (- i2 i1 w1) w2)
                                      q)))))))]
    (d/combine diff
      {:grow        0
       :degree      degree
       :shrink      0
       :permutation permutation
       :change      {}
       :freeze      #{}})))

(defn block-unmount [^objects block diff]
  (let [^objects call (aget block block-slot-parent)
        ^objects reader (aget call call-slot-reader)
        ^objects buffer (aget call call-slot-buffer)
        shrink (block-weight block)
        i (aget block block-slot-index)]
    (aset block block-slot-index nil)
    (aset buffer i nil)
    (if (nil? (aget block block-slot-prev))
      (block-release block)
      (call-update-weights call (- shrink)))
    (if-some [index (block-index call i)]
      (let [size-after (current-size reader)]
        (d/combine diff
          {:grow        0
           :degree      (unchecked-add-int size-after shrink)
           :shrink      shrink
           :permutation (p/split-swap index shrink
                          (unchecked-subtract-int size-after index))
           :change      {}
           :freeze      #{}}))
      diff)))

(defn block-mount [^objects block offset diff]
  (let [frame (aget block block-slot-frame)]
    (loop [i 0
           o offset
           d diff]
      (if (< i (r/frame-call-count frame))
        (if (nil? (r/frame-call frame i))
          (if-some [^objects item (block-child block i)]
            (recur (inc i) (inc o)
              (let [size-before (- (:degree d) (:shrink d))]
                (d/combine d
                  {:grow        1
                   :degree      (inc size-before)
                   :shrink      0
                   :permutation (p/rotation size-before o)
                   :change      {o (aget item item-slot-state)}
                   :freeze      #{}})))
            (recur (inc i) o d))
          (if-some [^objects call (block-child block i)]
            (let [^objects buffer (aget call call-slot-buffer)]
              (recur (inc i) (+ o (aget call call-slot-weight))
                (loop [i 0
                       o o
                       d d]
                  (if (< i (alength buffer))
                    (if-some [^objects block (aget buffer i)]
                      (let [w (block-weight block)]
                        (recur (inc i) (+ o w)
                          (if (pos? w) (block-mount block o d) d)))
                      d) d))))
            (recur (inc i) o d))) d))))

(defn apply-change [^objects call change diff]
  (let [^objects buffer (aget call call-slot-buffer)
        ^objects reader (aget call call-slot-reader)]
    (reduce-kv
      (fn [diff i f]
        (let [diff (if-some [block (aget buffer i)]
                     (block-unmount block diff) diff)]
          (if-some [block (get-block reader f)]
            (do (aset block block-slot-index i)
                (aset buffer i block)
                (call-update-weights call (block-weight block))
                (if-some [index (block-index call i)]
                  (block-mount block index diff) diff))
            (let [^objects block (make-block reader f)]
              (aset block block-slot-parent call)
              (aset block block-slot-index i)
              (aset buffer i block) diff))))
      diff change)))

(defn apply-shrink [^objects call degree shrink diff]
  (let [^objects buffer (aget call call-slot-buffer)]
    (loop [d diff
           i 0]
      (if (< i shrink)
        (recur
          (block-unmount
            (aget buffer
              (unchecked-subtract-int degree
                (unchecked-inc-int i))) d)
          (inc i)) d))))

(defn call-transfer [^objects call diff]
  (let [{:keys [degree shrink permutation change]} @(aget call call-slot-process)]
    (aset call call-slot-buffer (ensure-capacity (aget call call-slot-buffer) degree))
    (->> diff
      (apply-permutation call permutation)
      (apply-change call change)
      (apply-shrink call degree shrink))))

(defn reader-transfer [^objects reader]
  (let [^objects state (aget reader reader-slot-state)
        held (enter state)]
    (if (identical? reader (aget state slot-reader))
      (loop [diff (d/empty-diff (current-size reader))]
        (if-some [^objects call (aget reader reader-slot-call-queue)]
          (do (aset reader reader-slot-call-queue (aget call call-slot-queue))
              (aset call call-slot-queue call)
              (recur (if (nil? (aget call call-slot-children))
                       (do (call-discard call) diff)
                       (call-transfer call diff))))
          (if-some [^objects item (aget reader reader-slot-item-queue)]
            (do (aset reader reader-slot-item-queue (aget item item-slot-queue))
                (aset item item-slot-queue item)
                (recur (if (identical? item (aget item item-slot-state))
                         (if (nil? (aget item item-slot-parent))
                           diff (item-detach-from-tree item diff))
                         (if (nil? (aget item item-slot-parent))
                           (item-attach-to-tree item reader diff)
                           (change item diff)))))
            (do (aset reader reader-slot-pending reader)
                (exit state held) diff))))
      (do (aset reader reader-slot-pending reader)
          (terminate reader) (exit state held)
          (throw (missionary.Cancelled.))))))

(defn item-cancel [^objects item]
  (aset item item-slot-parent nil))

(defn call-cancel [^objects call]
  (let [children (aget call call-slot-children)]
    (aset call call-slot-children nil)
    (aset call call-slot-parent nil)
    (loop [^objects block children]
      (let [f (aget block block-slot-frame)
            n (aget block block-slot-next)]
        (aset block block-slot-parent nil)
        (aset block block-slot-prev nil)
        (aset block block-slot-next nil)
        (loop [i 0]
          (when (< i (r/frame-call-count f))
            (when-some [child (block-child block i)]
              ((if (nil? (r/frame-call f i))
                 item-cancel call-cancel) child))
            (recur (inc i))))
        (when-not (identical? n children)
          (recur n))))
    ((aget call call-slot-process))))

(defn reader-cancel [^objects reader]
  (let [^objects state (aget reader reader-slot-state)
        held (enter state)]
    (when (identical? reader (aget state slot-reader))
      (aset state slot-reader nil)
      (when-some [root (aget reader reader-slot-root)]
        (aset reader reader-slot-root nil)
        (call-cancel root))
      (loop []
        (when-some [^objects item (aget reader reader-slot-item-queue)]
          (aset reader reader-slot-item-queue (aget item item-slot-queue))
          (aset item item-slot-queue item)
          (recur)))
      (loop []
        (when-some [^objects call (aget reader reader-slot-call-queue)]
          (aset reader reader-slot-call-queue (aget call call-slot-queue))
          (aset call call-slot-queue call)
          (call-discard call)
          (recur)))
      (reader-event reader))
    (exit state held)))

(deftype Reader [state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (reader-cancel state))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (reader-transfer state)))

(defn reader-spawn [^objects state step done]
  (let [held (enter state)
        reader (object-array reader-slots)]
    (aset reader reader-slot-state state)
    (aset reader reader-slot-step step)
    (aset reader reader-slot-done done)
    (aset reader reader-slot-alive (identity 1))
    (when (nil? (aget state slot-reader))
      (aset state slot-reader reader)
      (aset reader reader-slot-item-queue
        (reduce (fn [queue ^objects item]
                  (aset item item-slot-queue queue) item)
          nil (vals (aget state slot-items)))))
    (reader-pending reader)
    (exit state held)
    (->Reader reader)))

(deftype MountPoint [^objects state]
  KVS
  (insert! [_ tag init]
    (assert (identical? (r/frame-peer (r/tag-frame tag)) (aget state slot-peer)))
    (let [held (enter state)
          items (aget state slot-items)]
      (if (contains? items tag)
        (do (exit state held)
            (throw (error "Can't insert - tag already present.")))
        (let [item (object-array item-slots)]
          (aset state slot-items (assoc items tag item))
          (aset item item-slot-tag tag)
          (aset item item-slot-state init)
          (if-some [reader (aget state slot-reader)]
            (enqueue-item reader item)
            (aset item item-slot-queue item))
          (exit state held)))))
  (update! [_ tag f]
    (assert (identical? (r/frame-peer (r/tag-frame tag)) (aget state slot-peer)))
    (let [held (enter state)
          items (aget state slot-items)]
      (if-some [^objects item (get items tag)]
        (let [prev (aget item item-slot-state)]
          (when-not (= prev (aset item item-slot-state (f prev)))
            (when-some [reader (aget state slot-reader)]
              (when (identical? item (aget item item-slot-queue))
                (enqueue-item reader item))))
          (exit state held))
        (do (exit state held)
            (throw (error "Can't update - tag is absent."))))))
  (remove! [_ tag]
    (assert (identical? (r/frame-peer (r/tag-frame tag)) (aget state slot-peer)))
    (let [held (enter state)
          items (aget state slot-items)]
      (if-some [^objects item (get items tag)]
        (do (aset state slot-items (dissoc items tag))
            (aset item item-slot-state item)
            (when-some [reader (aget state slot-reader)]
              (when (identical? item (aget item item-slot-queue))
                (enqueue-item reader item)))
            (exit state held))
        (do (exit state held)
            (throw (error "Can't remove - tag is absent."))))))
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (reader-spawn state step done)))

(defn create [peer]
  (->MountPoint
    (doto (object-array slots)
      (aset slot-lock #?(:clj (ReentrantLock.) :cljs false))
      (aset slot-peer peer)
      (aset slot-items {}))))