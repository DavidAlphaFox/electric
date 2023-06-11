(ns wip.demo-todos-optimistic
  (:import [hyperfiddle.electric Pending])
  (:require [contrib.identity :refer [tempid?]]
            #?(:clj [datomic.api :as d]) ; database on server
            [hyperfiddle.api :as hf]
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
      (e/server
        ; strategy = commit asap, no reason to delay.
        ; this means create-new records are imediately synced, which is correct
        ; this means edits are isolated, which is correct
        ; if edits should be batched, that's what hf/branch provides, so this is correct. 
        
        (ui5/Field. ; returns nil or
          :record ?record ; should be lazy loaded - entity api. This is over-fetched. Thereby ensure right here?
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
          :txn (fn [tx] [{:db/id (:db/id ?record) :task/description v}]))))

(e/defn TodoItemCreate "just another form, the caller will branch and deal with genesis 
on submit"
  []
  ; its a colored input but perhaps we suppress the styles as the popover is inlined?
  ; This input has no colors actually due to being a glorified popover submit button?  
  (e/server
    (ui5/Field. ; local optimistic updates via side channel on client
      :record (e/server {})
      :a :task/description
      :Control ui5/Input ; todo esc to revert
; how can Submit be wired directly to commit-stage in DT? 
      :parse identity
      :unparse identity
      :txn (fn [v]
             {#_#_:txn [[:db/add ?x :task/description v] ; no ID yet! Cannot transact, have local view repr only
                        [:db/add ?x :task/status :active]
                        [:db/add ?x :task/order (e/server (swap! !order-id inc))]]
              :optimistic {:task/description v
                           :task/status :active
                           :task/order (e/server (swap! !order-id inc))}})
      (dom/props {:placeholder "Buy milk"})))
  #_(e/client v'-client) ; return optimistic client value as local-index for the masterlist
  
  ; todo return both :txn and :optimitic, & sync state (contains :optimistic)
  ; does it also return the view document once stabilized (i.e. optimistic and server views converge?)
  ; is "view-document" the local-index? 
  )

(e/defn CreateController
  "maintains a local index of created entities by watching the Datomic tx-report"
  [kf Body #_&args]
  (let [!local-index (atom {}), local-index (e/watch !local-index) ; perhaps global to the client, is it a datascript db?
        local-tempids (vals local-index)
        promoted-tempids (vals (:ids->tempids >tx-report)) ; includes those from other sessions
        local-promotions (clojure.set/intersection promoted-tempids local-tempids)]

    (when (seq local-promotions) ; these genesis records have been promoted 
      ; and now appear in the masterlist query, so stop tracking them locally.
      (swap! !local-index #(apply dissoc % local-promotions))) ; "birth"

    (let [{:keys [optimistic]} ; blinks on popover close, this is the hf/stage from inside the branch
          (e/client (Popover. "open" ; todo PopoverBody - auto-open, no anchor
                      (e/fn []
                        (e/server
                          (let [{:keys [txn optimistic] :as xdx} (Body.)] ; on commit returns the stage here, todo fix
                            ; no point in updating dbval here, popover is closing. Due to the blink!
                            {:optimistic (merge optimistic {:db/id (contrib.identity/genesis-tempid! hf/db)})})))))]
      (swap! !local-index assoc (kf optimistic) optimistic) ; due to the blink?
      ; note we ignore hf/stage, it was damaged by swap!
      #_hf/stage local-index))) ; return also the local records, for use in optimistic queries

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
          local-index (CreateController. stable-kf TodoItemCreate #_{:task/status :active})
          
          ; the local-index is the branch
          
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
      
      ; has both txn and :optimistic, for comparison
      edit-txns))) ; use this to complete circuit, hf/stage has been damaged by swap!

(e/defn Page []
  (e/client
    (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
    (dom/p (dom/text "it's multiplayer, try two tabs"))
    (dom/div (dom/props {:class "todo-list"})
      #_(dom/div {:class "todo-items"})
      (e/server 
        (let [tx (MasterList. (fn [] (todo-records hf/db)))]
          tx))
      (dom/p (dom/props {:class "counter"})
        (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count hf/db))))
        (dom/text " items left")))))

;; user configurable latency and tx fail rate
#?(:clj (def !latency (atom 200)))
(e/def latency (e/server (e/watch !latency)))

#?(:clj (def !fail-rate (atom 1)))
(e/def fail-rate (e/server (e/watch !fail-rate)))

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

#?(:clj (defn transact! "tx with configured latency and fail rate" [db tx]
          (m/sp
            (m/? (m/sleep @!latency))
            (if (< (rand-int 10) @!fail-rate)
              (throw (ex-info "tx failed" {:tx tx}))
              (d/with db (dbg/dbg :tx tx))
              #_@(d/transact !conn (dbg/dbg :tx tx))))))

(e/defn AdvancedTodoList []
  (e/server
    (binding [hf/db (e/watch !db)
              hf/into-tx' hf/into-tx
              hf/with (fn [db tx] ; inject datomic dependency 
                        (try
                          (let [{:keys [db-after tx-report]} (new (e/task->cp (transact! db tx)))]
                            db-after)
                          (catch Exception e 
                            (println "...failure, e: " e)
                            db)))]
      (e/client
        (Latency. 300 2000)
        (FailRate. 3 10))
      (let [undamaged-tx
            (hf/branch
              [(Page.) ; undamaged tx (responsive)
               hf/stage] ; for comparison (damaged by swap!)
              #_(e/client
                  (dom/hr)
                  (dom/element "style" (str "." (css-slugify `staged) " { display: block; width: 100%; height: 10em; }"))
                  (ui/edn (e/server hf/stage) nil (dom/props {::dom/disabled true ::dom/class (css-slugify `staged)}))))]
        
        (e/client
          (dom/hr)
          (dom/element "style" (str "." (css-slugify `staged) " { display: block; width: 100%; height: 10em; }"))
          ; optimistic 
          (ui/edn (e/server undamaged-tx) nil (dom/props {::dom/disabled true ::dom/class (css-slugify `staged)}))
          (ui/edn (e/server hf/stage) nil (dom/props {::dom/disabled true ::dom/class (css-slugify `staged)})))
        
        ))))
