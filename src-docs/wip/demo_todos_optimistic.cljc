(ns wip.demo-todos-optimistic
  (:import [hyperfiddle.electric Pending])
  (:require [contrib.identity :refer [tempid?]]
            #?(:clj [datomic.api :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.electric-ui5 :as ui5]
            [contrib.data]
            [contrib.debug :as dbg]
            [hyperfiddle.api :as hf]))

;; showcases how to render an item optimistically and without e/for-event
;; missing:
;; - ordering
;; - idempotent entity creation
;; - integration of a local and remote seq in e/for-by

#?(:clj
   (def schema
     [{:db/ident :task/status,      :db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one}
      {:db/ident :task/description, :db/valueType :db.type/string,  :db/cardinality :db.cardinality/one}
      {:db/ident :hf/stable-id,     :db/valueType :db.type/uuid,    :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}]))

#?(:clj (defn init-conn []
          (let [uri "datomic:mem://db"]
            (d/delete-database uri)
            (d/create-database uri)
            (let [conn (d/connect uri)]
              (d/transact conn schema)
              conn))))

(defonce !conn #?(:clj (init-conn) :cljs nil)) ; database on server
#?(:clj (comment (alter-var-root #'!conn (fn [_] (init-conn)))))
(e/def db) ; injected database ref; Electric defs are always dynamic
(e/def tx-report) ; for promoted tempids

(defonce !db #?(:clj (atom nil) :cljs nil))
;; singleton database queue polling
;; in the future this can be done with `m/signal`
(defonce !taker #?(:clj (future
                          (reset! !db (d/db !conn))
                          (let [q (d/tx-report-queue !conn)]
                            (loop []
                              (reset! !db (:db-after (.take ^java.util.concurrent.LinkedBlockingQueue q)))
                              (recur))))
                   :cljs nil))

;; user configurable latency and tx fail rate
#?(:clj (def !latency (atom 200)))
(e/def latency (e/server (e/watch !latency)))

#?(:clj (def !fail-rate (atom 1)))
(e/def fail-rate (e/server (e/watch !fail-rate)))

#?(:clj (defn tx! "tx with configured latency and fail rate" [tx]
          (m/sp
            (m/? (m/sleep @!latency))
            (if (< (rand-int 10) @!fail-rate)
              (throw (ex-info "tx failed" {:tx tx}))
              @(d/transact !conn (dbg/dbg :tx tx))))))

(e/def Tx!)

(e/defn Latency [min max]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (dom/span (dom/text "Latency: " latency "ms"))
    (ui/range latency (e/fn [v] (e/server (reset! !latency v)))
      (dom/props {:min min, :max max, :style {:width "200px"}}))))

(e/defn FailRate [min max]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (dom/span (dom/text "Fail Rate: " fail-rate " out of " max))
    (ui/range fail-rate (e/fn [v] (e/server (reset! !fail-rate v)))
      (dom/props {:min min, :max max, :style {:width "200px"}}))))

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :where [?e :task/status :active]] db))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id
                                       :task/description
                                       :task/status
                                       :hf/stable-id
                                       #_:task/order]) ...]
                      :where [?e :task/status]] db)
            #_(sort-by :task/order #(compare %2 %1)))))

; Typing into fields should emit txns right away (with green dots) – locally
; the commit event is on the popover, which emits the txn up to the caller

(e/defn TodoItem [{:keys [db/id] :as ?record} submit!] ; pre-pulled, todo entity api
  (e/client
    (dom/div (dom/style {:display "flex", :align-items "center"})
      ; who transacts these fields and monitors progress?
      ; They auto-save, use hf/branch to add commit/discard semantics.
      ; "Save" here can mean transact, or really "sync to server so data is safe and not lost"

      (e/server
        (hf/into-tx ; can the tx-monoid be made implicit via side channel?
          ; ui5/fForm - bind the db/id in scope? and branch here for atomicity?
          ; v-request-server
          (ui5/Field.
            :record ?record ; should be lazy loaded - entity api. This is over-fetched
            :a :task/status
            :Control ui5/Checkbox
            :parse {:done true, :active false}
            :unparse {true :done, false :active}
            :txn (fn [x] [{:db/id (:db/id ?record) :task/status x}]))

          (ui5/Field.
            :record ?record
            :a :task/description
            :Control ui5/Input
            :parse identity
            :unparse identity
            :txn (fn [tx] [{:db/id (:db/id ?record) :task/description v}])))))))

(e/defn TodoItemCreate [submit!] ; submit is the stage commit? this is a branch?
  (ui5/Field.
    :record .
    :a :task/description
    :Control ui5/InputSubmit ; how can Submit be wired directly to staging area in DT?
    :parse identity
    :unparse identity
    :txn (fn [tx] [{:db/id (:db/id ?record) :task/description v}])
    (dom/props {:placeholder "Buy milk"}))
  
  (ui5/input "" ; esc to revert. This input has no colors actually due to being a glorified popover submit button?
    ; its a colored input but perhaps we suppress the styles as the popover is inlined?
    (dom/props {:placeholder "Buy milk"})
    (when-some [v (ui5/On-input-submit. dom/node)] ; discrete, wire this to staging area submit.
      (submit! {:task/description b
                :task/status :active
                #_#_:task/order (e/server (swap! !order-id inc))}))
    nil))

; Do all controls have submit? No, all controls emit value signals.
; Staging area controls submit.
; Inputs have special event callbacks to submit, then. (Are they missionary flows?)

(e/defn CreateController
  "maintains a local index of created entities by watching the Datomic tx-report"
  [kf Body]
  (let [!local-index (atom {}), local-index (e/watch !local-index) ; perhaps global to the client, is it a datascript db?
        local-tempids (vals local-index)
        promoted-tempids (vals (:ids->tempids >tx-report)) ; includes those from other sessions
        local-promotions (clojure.set/intersection promoted-tempids local-tempids)]

    (when (seq local-promotions) ; these genesis records have been promoted 
      ; and now appear in the masterlist query, so stop tracking them locally.
      (swap! !local-index #(apply dissoc % local-promotions))) ; "birth"

    (let [_ (e/client (Popover. "open" ; todo PopoverBody - auto-open, no anchor
                        (e/fn []
                          (e/server
                            (Body. (fn submit! [local-record] ; todo: commit/discard wired up to the widget events
                                     (->> local-record
                                       (merge {:db/id (contrib.identity/genesis-tempid! db)})
                                           ; commit and close the popover
                                       (as-> x (swap! !local-index assoc (kf x) x)))))))))] ; discrete
      local-index))) ; return also the local records, for use in optimistic queries

(defn merge-unordered [kf local-records ?records]
  (vals (reduce (fn [acc x] (assoc acc (kf x) x))
          (group-by kf local-records)
          ?records)))

(e/defn MasterList
  "encapsulates both rendering the table and also adding elements to it, in 
order to stabilize identity"
  [query-records] ; specifically not entities due to the optimism strategy
  (e/client 
    (let [!ids (atom {}) ; #tempid and/or reified id -> process-unique identity
          stable-kf (partial contrib.identity/entity-id-locally-stabilzied! !ids tx-report) ; todo
          local-index (CreateController. stable-kf TodoItemCreate {:task/status :active})

          ; does this operate on records or entity ids?
          ; note, datomic entity api is has broken equality anyway
          records (merge-unordered stable-kf
                    (vals local-index) ; must have matching pull shape
                    (try (e/server (query-records)) ; matching pull shape
                      (catch Pending _))) ; todo rethrow pending for load timers above
          
      ; where is the transaction? Where is the 4-colored result on the create?
      ; it MUST be in todo-item
      ; TodoItem-create does not transact as it only submits at the last instant
      ; local-index is a create REQUEST?
          
      ; todo move query diffing to server
          edit-txns (e/for-by stable-kf [record records] ; must include genesised records, for stability
        ; Ensure local entities here, they've been submitted
        ; What if the local-records end up in two places? That's a race, both will ensure
        ; existance, ok, one will win (so long as tempids are not reused and remain valid)
                      (TodoItem. record))]
      
      )))

(e/defn AdvancedTodoList []
  (e/server
    (binding [db (e/watch !db)
              tx-report ...
              Tx! (e/fn [tx] (new (e/task->cp (tx! tx))) nil)]
      ;; (d/transact !conn schema)
      (e/client
        (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (Latency. 0 2000)
        (FailRate. 0 10)
        (dom/div (dom/props {:class "todo-list"})
          ;(dom/div {:class "todo-items"})
          (MasterList. (fn [] (todo-records db)))
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
            (dom/text " items left")))))))
