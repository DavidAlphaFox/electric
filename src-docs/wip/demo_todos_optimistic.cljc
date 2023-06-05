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
            [contrib.debug :as dbg]))

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

(e/defn TodoItem [{:keys [db/id] :as record}] ; pre-pulled, todo entity api
  (e/client
    (dom/div (dom/style {:display "flex", :align-items "center"})
      (ui5/entity record
        (ui5/checkbox (= :done (:task/status record))
          (e/fn [checked?]
            (e/server (new Tx! [[:db/add (:db/id record) :task/status (if checked? :done :active)]]))))
        (ui5/input (:task/description record)
          (e/fn [v] (e/server (new Tx! [[:db/add (:db/id record) :task/description v]]))))))))

(e/defn TodoItem-create "maintains a local index of created entities"
  [kf record]
  (let [!local-index (atom {}), local-index (e/watch !local-index) ; perhaps global to the client, is it a datascript db?
        local-tempids (vals local-index)
        promoted-tempids (vals (:ids->tempids db)) ; includes those from other sessions
        local-promotions (clojure.set/intersection promoted-tempids local-tempids)]

    (when (seq local-promotions) ; these genesis records have been promoted 
      ; and now appear in the masterlist query, so stop tracking them locally.
      (swap! !local-index #(apply dissoc % local-promotions)))

    (ui5/input (:task/description record)
      (dom/props {:placeholder "Buy milk"})
      (when-some [v (ui5/On-input-submit. dom/node)] ; commit?
        (let [local-record {:db/id (contrib.identity/genesis-tempid! db)
                            :task/description b
                            :task/status :active
                            #_#_:task/order (e/server (swap! !order-id inc))}]
          (swap! !local-index assoc (kf local-record) local-record))
        nil))
    local-index)) ; return also the local records, for use in optimistic queries

(defn merge-unordered [kf local-records ?records]
  (vals (reduce (fn [acc x] (assoc acc (kf x) x))
          (group-by kf local-records)
          ?records)))

(e/defn MasterList
  "encapsulates both rendering the table and also adding elements to it, in 
order to stabilize identity"
  [query-records] ; specifically not entities due to the optimism strategy
  (e/client 
    (let [!ids (atom {}) ; tempid or reified id -> process-unique identity
          stable-kf (partial contrib.identity/entity-id-locally-stabilzied! db)
          local-index (TodoItem-create. stable-kf {:task/status :active}) 
          
          ; does this operate on records or entity ids?
          ; note, datomic entity api is has broken equality anyway
          records (merge-unordered stable-kf
                    (vals local-index)
                    (try (e/server (query-records))
                      (catch Pending _)))]
      
      ; where is the transaction? Where is the 4-colored result on the create?
      ; it MUST be in todo-item
      ; TodoItem-create does not transact as it only submits at the last instant
      ; local-index is a create REQUEST ?
      
      ; todo move query diffing to server
      (e/for-by stable-kf [record records] ; must include genesised records, for stability
        ; Ensure local entities here, they've been submitted
        ; What if the local-records end up in two places? That's a race, both will ensure
        ; existance, ok, one will win (so long as tempids are not reused and remain valid)
        (TodoItem. record)))))

(e/defn TodoListOptimistic []
  (e/server
    (binding [db (e/watch !db), Tx! (e/fn [tx] (new (e/task->cp (tx! tx))) nil)]
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
