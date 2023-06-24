(ns hyperfiddle.stage
  (:require [datascript.core :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m]
            [wip.demo-datomic-helpers :refer [slow-transact!]]))


; FIXME there is no ::dirty state; and we listen to tx-report to know when our txn lands

; Edits happen one at a time
; Backpressure is relieved, which means edits can be merged/batched
; If sampling rate is low, edits are so batched
; If a popover branches, edits can be batched (merged into single txn)

; Edits start dirty, which renders the field as dirty
; Dirty edits are transacted by the entrypoint
; Before transacting, dirty edits are promoted to pending
; Pending edits become either ::completed or ::failed per transaction result

; Dirty edits are locally optimistic (field-local)
; Pending edits are desired to be globally optimistic, but list-local in practice

; Complex edits
; edits may span multiple records, this is hard to apply optimistically?



; Acceptance criteria

(tests
  "a field's edit state can be monitored"
  (ui5/Field.
    :record (get ?record :task/status)
    :Control ui5/Checkbox
    :parse {:done true, :active false}
    :unparse {true :done, false :active}
    :optimistic (fn [v] {:db/id (:db/id ?record) :task/status x})
    :txn (fn [x] [{:db/id (:db/id ?record) :task/status x}]))
  := [{:status ::dirty
       :x {100 {:task/status :done}} ; indexed by lookup ref
       ; no db/id in x record because it's field level view
       :dx [[:db/add 100 :task/status :done]]}]) ; not indexed

(tests
  "edits can be batched, by merging. This is common – see hf/branch"
  (batch-edits
    [{:status ::dirty
      :xs {100 {:task/status :done}}
      ; {:target 100 :optimistic {}} ?
      :dx [[:db/add 100 :task/status :done]]}
     {:status ::dirty
      :x {100 {:task/description "feed baby"}}
      :dx [[:db/add 100 :task/description "feed baby"]]}])
  := [{:status ::dirty
       :x {100 {:task/status :done
                :task/description "feed baby"}}
       :dx [[:db/add 100 :task/status :done]
            [:db/add 100 :task/description "feed baby"]]}])

(tests
  (CreateForm. ...)
  
  ; they need IDs to update the status on the right txn
  monitor := [{:status ::dirty
               :x {-1 {:task/status :active
                       :task/description "buy milk"}}
               :dx [[:db/add -1 :task/status :active]
                    [:db/add -1 :task/description "buy milk"]]}]

  ; dirty -1 sampled by entrypoint
  monitor := [{:status ::pending
               :x {-1 {:task/status :active
                       :task/description "buy milk"}}
               :dx [[:db/add -1 :task/status :active]
                    [:db/add -1 :task/description "buy milk"]]}]

  ; rapid entry - subsequent genesis
  (CreateForm. ...)
  monitor := [{:status ::dirty
               :x {-2 {:task/status :active
                       :task/description "exercise"}}
               :dx [[:db/add -2 :task/status :active]
                    [:db/add -2 :task/description "exercise"]]}
              {:status ::pending
               :x {-1 {:task/status :active
                       :task/description "buy milk"}}
               :dx [[:db/add -1 :task/status :active]
                    [:db/add -1 :task/description "buy milk"]]}]

  ; dirty -2 sampled by entrypoint
  monitor := [{:status ::pending
               :x {-2 {:task/status :active
                       :task/description "exercise"}}
               :dx [[:db/add -2 :task/status :active]
                    [:db/add -2 :task/description "exercise"]]}
              {:status ::pending
               :x {-1 {:task/status :active
                       :task/description "buy milk"}}
               :dx [[:db/add -1 :task/status :active]
                    [:db/add -1 :task/description "buy milk"]]}]

  ; -1 completes
  monitor := [{:status ::pending
               :x {-2 {:task/status :active
                       :task/description "exercise"}}
               :dx [[:db/add -2 :task/status :active]
                    [:db/add -2 :task/description "exercise"]]}
              {:status ::completed
               :x {101 {:task/status :active
                        :task/description "buy milk"}}
               :dx [[:db/add -1 :task/status :active]
                    [:db/add -1 :task/description "buy milk"]]
               :tempids {-1 100}}]

  ; completed record fed back into field which clears it
  monitor := [{:status ::pending
               :x {-2 {:task/status :active
                       :task/description "exercise"}}
               :dx [[:db/add -2 :task/status :active]
                    [:db/add -2 :task/description "exercise"]]}]

  ; -2 completes
  monitor := [{:status ::completed
               :x {102 {:task/status :active
                        :task/description "exercise"}}
               :dx [[:db/add -2 :task/status :active]
                    [:db/add -2 :task/description "exercise"]]
               :tempids {-2 102}}])

; QUERIES
; list of dirty edits, pending, completed, failed -> return edit-ids
; resolve edit-id to x/dx
; which edits are targetting domain entity 123?
; what is current state of domain entity 123?
;   db state + pending state + dirty state
;   there can be multiple edits targetting one entity (hopefully uncommon)

; OPERATORS
; promote-edits

; DIRTY HACKS
; simply use txns only, skip optimistic update. Lots of problems go away
; Can we operate on transactions (kept separate), each txn associates a target-state?
;   give each txn a unique id, this lets us track state
;      the tx-monitor-db associates txns with their state

{:db/id 1000
 :monitor/txn ; cache join by hash if needed
 [[:db/add -2 :task/status :active]
  [:db/add -2 :task/description "exercise"]]
 :monitor/txn-state ::dirty
 :monitor/target-state ; cache join - opaque value, this is component local state
 {:task/status :active
  :task/description "exercise"}}

; does the target-state even need to be in the db? If it is applied locally?



; IDEAS
; "monitor" – show x+dx+status together at the top to see transitions
; "cheap and dirty" - send dx up, status is implied, x is local




(defn query-edits [edit-db status]
  (d/q '[:find ?e :in $ ?status :where
         [?e :edit/status ?status]]
    edit-db status))

(tests 
  (query-edits test-db ::dirty) := [2]
  (query-edits test-db ::pending) := [1]
  (query-edits test-db ::completed) := [3]
  (query-edits test-db ::failed) := [4])

(tests 
  (def edit-db [{:status ::dirty
                 :x {-2 {:task/status :active
                         :task/description "exercise"}}
                 :dx [[:db/add -2 :task/status :active]
                      [:db/add -2 :task/description "exercise"]]}])
  (d/entity edit-db 2) 
  := {:db/id 2 :edit/status ::dirty
      :edit/target [-2 {:task/status :active
                        :task/description "exercise"}]
      :edit/dx [[:db/add -2 :task/status :active]
                [:db/add -2 :task/description "exercise"]]}
  )




; ---

(def ds (d/create-conn {}))

(def edits
  (d/with @ds
    [{:db/id 1
      :status ::dirty 
      :x {:db/id "tempid" :task/status :done}
      :dx [[:db/add "tempid" :task/status :done]]}
     
     {:db/id 2
      :status ::dirty
      :x {:db/id 1 :task/description "feed baby"}
      :dx [[:db/add 1 :task/description "feed baby"]]}
     
     {:db/id 3
      :status ::pending
      :x {}
      :dx [[:db/add]]}
     
     {:db/id 4
      :status ::completed
      :x {:db/id 2 :task/status :done} ; never seen
      :dx [[:db/add "tempid" :task/status :done]] ; never seen
      :tempids {"tempid" 2}}
     
     {:db/id 5
      :status ::failed :error "rejected"
      :x {:db/id "tempid" :task/status :done}
      :dx [[:db/add "tempid" :task/status :done]]}]))

#_
(tests
  (transpose_ edits)
  := [[::hf/dirty ::hf/dirty ::hf/pending ::hf/synced ::hf/failed]
      [#:task{:status :done} #:task{:description "feed baby"} {} {} {}]
      [[[:db/add '. :task/status :done]]
       [[:db/add '. :task/description "feed baby"]]
       [[:db/add]] nil [[:db/add]]]
      [nil nil nil nil "rejected"]])

(defn squash-edits [edits]
  (let [[statuses xs dxs errs] (transpose edits)]
    (assert (every? #(= (first statuses) %) statuses)  (str "can't squash-edits: " edits))
    [(apply merge xs)
     (vec (apply concat dxs))
     (seq (remove nil? errs))]))

(tests
  (squash-edits edits)
  := [#:task{:status :done, :description "feed baby"}
      [[:db/add '. :task/status :done]
       [:db/add '. :task/description "feed baby"]
       [:db/add] [:db/add]]
      ["rejected"]])

(tests
  (squash-edits
    [[::hf/dirty #:task{:status :done} [[:db/add '. :task/status :done]]]
     [::hf/dirty #:task{:description "feed baby"} [[:db/add '. :task/description "feed baby"]]]])
  := [#:task{:status :done, 
             :description "feed baby"}
      [[:db/add '. :task/status :done] 
       [:db/add '. :task/description "feed baby"]] nil])

(tests
  (group-by first edits)
  := {::hf/dirty [[::hf/dirty #:task{:status :done} [[:db/add '. :task/status :done]]]
                  [::hf/dirty #:task{:description "feed baby"} [[:db/add '. :task/description "feed baby"]]]],
      ::hf/pending [[::hf/pending {} [[:db/add]]]],
      ::hf/synced [[::hf/synced {} nil]],
      ::hf/failed [[::hf/failed {} [[:db/add]] "rejected"]]})

(defn aggregate-edits [edits]
  (d/q '[:find ?e :where
         [?e :status ::dirty]]
    edits)
  
  (-> (group-by first edits)
    (update-vals squash-edits)))

(tests
  (aggregate-edits edits)
  := {::hf/dirty [{?x [{:db/id "tempid" :task/status :done}
                       [[:db/add "tempid" :task/status :done]]]
                   }
                  
                  {:db/id 1 :task/description "feed baby"}
                  [[:db/add 1 :task/description "feed baby"]]
                  nil],
      ::hf/pending [{} [[:db/add]] nil],
      ::hf/synced [{} [] nil],
      ::hf/failed [{} [[:db/add]] ["rejected"]]})

(defn promote [edits changing status']
  (update-in edits [status'] concat changing)

  (merge-deep
    (remove__ edits changing)
    {status' changing}))

(tests )

; In Google Sheets mode, transactions are isolated and race.
; In CRUD mode, popovers are introduced to manage batching.

; are they isolated or should we batch them here?
; the popover merges them; this is parallel edits.
; By the time they are seen here they are already batched
; which means there is only ever one edit in this list
; if there are two it means the system is lagging or the user
; is a robot and can type in multiple widgets simultaneously, or
; faster than the system's sampling rate

(defn transact!_ [old-tx-report <txs] ; Flow (List Tx)
  (m/ap
    (let [tx (m/?> ##Inf (m/seed (m/?< <txs)))] ; race them
      (try
        (let [tx-report (m/? (slow-transact! conn dirty))]
          (assoc tx-report ::accepted tx))
        (catch Exception e
          (assoc old-tx-report ::rejected tx))))))
