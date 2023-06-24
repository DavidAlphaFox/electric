(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            [clojure.set :refer [intersection]]
            [contrib.data :refer [transpose index-by]]
            [contrib.debug :as dbg]
            [contrib.identity :refer [tempid?]]
            [contrib.missionary-contrib :as mx]
            contrib.str
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [hyperfiddle.electric-data :refer [Triple First Second Third]]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [hyperfiddle.stage :refer [aggregate-edits]]
            [missionary.core :as m]))

(def !tx-report)
(e/def >tx-report)

(defn progress-color [state]
  (case state ::e/init "gray" ::e/ok "green" ::e/pending "yellow" ::e/failed "red"))

(e/defn Field ; client bias, bias is v. delicate
  "returns a signal of [viewstate tx]"
  [{:keys [v Control parse unparse txn] #_[status]
    :or {txn identity}}]
  (e/client
    (let [!status (atom ::synced) status (e/watch !status)
          !x (atom nil) x (e/watch !x)
          !dx (e/server (atom nil)) dx (try (e/server (e/watch !dx)) (catch Pending _)) ; prevent delays
          !err (atom nil) err (e/watch !err)]

      (let [v (unparse (Control. (parse v) status))]

        (reset! !status ::dirty)
        (reset! !x (optimistic v))
        (try
          (case (e/server
                  (let [dx (txn v)]
                    (reset! !dx dx)
                    ; where do we branch the tx-report binding? Popover?
                    #_(!tx-report (new (e/task->cp (transact! conn dx)))))) ; optional
            (reset! !status ::synced)) ; caller can shadow based on transact! configuration
          (catch Pending _
            (reset! !status ::pending)) ; caller can shadow
          (catch :default err
            (reset! !status ::failed)
            (reset! !err err)))

        #_!status ; hook to mark completed? is it needed? or is it branch by branch? does that imply a swap?
        #_[status x dx err]; monitor
        [x dx]))))

(e/defn DomInputController [node event-type v-server status ref!]
  (let [v (new (->> (m/observe (fn [!] (dom-listener node event-type ! opts)))
                 (m/relieve {}) ; input value signal, never backpressure the user
                 (m/eduction (map (fn event->value [e] (parse (ref! (.-target e))))))
                 (m/reductions {} v-server)))]
    (cond
      (dom/Focused?. node) v'
      (= ::pending status) v'
        ;(= ::failed status) ?
        ;(= ::init status) v-server
      (= ::ok status) (do (ref! v-server) v-server)))) ; concurrent edit, otherwise work-skipped

(e/defn Checkbox [v status]
  (dom/input (dom/props {:type "checkbox"})
    (let [v' (DOMInputController. dom/node "change" v status
               (partial dom/-node-checked! dom/node))]
      (dom/style {:outline (str "2px solid " (progress-color status))})
      (assert (not= ::failed status) "transaction failures are impossible here (otherwise would need to filter them here)")
      v')))

(e/defn Input [v status]
  (dom/input
    (let [v' (DOMInputController. dom/node "input" v status
               (partial dom/-node-value! dom/node))]
      (dom/style {:outline (str "2px solid " (progress-color status))})
      (assert (not= ::failed status) "transaction failures are impossible here (otherwise would need to filter them here)")
      v')))

(e/defn Button [label rf acc0]
  (dom/button
    (dom/text label)
    (->> (m/observe (fn [!] (dom-listener dom/node "click" !)))
      (m/reductions rf acc0))))

#_(case status ::e/failed (.warn js/console v) nil) ; debug, note cannot fail as not a transaction

(defn with-genesis [db [?x ?dx]]
  (let [id (contrib.identity/genesis-tempid! db)]
    [(?x id) (?dx id)])) ; see TodoItemCreate

(defn promoted-tempids [tx-report] (vals (:ids->tempids tx-report)))

(defn local-births [genesis tx-report]
  ; "Birthed" entities are no longer managed by their mother.
  (intersection (promoted-tempids tx-report) genesis))

(defn reconcile-tempids! [>x >tx-report]
  (m/ap
    (let [!genesis (atom #{}) >genesis (m/stream (m/watch !genesis))
          [op :as diff] (m/amb (let [x (m/?< >x)]
                                 [:assoc (stable-kf x) x])
                          (m/seed
                            (for [e (seq (local-births (m/?< >genesis) (m/?< >tx-report)))]
                              [:dissoc (stable-kf :dispose e)])))]
      (case op
        :assoc (swap! !genesis conj e) ; genesis
        :dissoc (swap! !genesis disj e) ; birth
        :update nil
        :move nil))))

(e/defn CreateController
  "maintains an index of locally created entities by watching the Datomic tx-report"
  [stable-kf Form #_&args]
  
  ; are we watching for tempids, or really for tx completion?
  ; Note, txns are concurrent and can overlap tempids!!
  ; don't track births, track txn completion by hash of txn.
  ; todo: enhance tx-report with the hash of txns that completed
  ;    so that they can be moved from the for? -- possible issue
  
  ; tx backpressure has been relieved, it's unlike to change
  (let [[x batched-dx] (e/snapshot ; todo handle popover reopen (unwind effects?)
                         (with-genesis hf/db
                           (Popover. "open" Form))) ; pending until commit
        >par-diffs (new (reconcile-tempids! (e/fn [] x) >tx-report))]
    [>par-diffs batched-dx]))

(e/defn MasterList
  "coordinates the insertion of new entities into a database-backed editable list
so as to stabilize the entity's associated Electric frame as the entity progresses 
through its edit lifecycle (dirty -> pending -> completed)."
  [stable-kf EditForm CreateForm]
  (e/server
    (e/fn [records] ; e/fn transfer
      (e/client
        (let [[>pending-view-diffs dx] (CreateController. stable-kf CreateForm)]
          (vector ; isolated txs that race in parallel unless a popover batches them
            dx #_x ; x is local now?
            (map second ; just the dxs for now, throw away the view state
              (maintain-vec
                (e/par EditForm
                  (mx/mix >pending-view-diffs
                    (e/server (e/reconcile stable-kf (e/fn [] records))))))))))))) ; illegal flow transfer
            
; send transaction up (in any state? dirty/pending is there a difference?)
; wire pending state into for-by-streaming, diffed
; just assume the dirty edit is about to upgrade to pending
; diff target-state into the for-by-streaming for optimistic view

; Note: when the dirty record becomes pending, and we edit further,           
; it's the same edit structure
