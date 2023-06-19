(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
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

(e/defn On-input-submit [node]
  ; (assert node is type input)
  (new (m/reductions {} nil
         (m/observe (fn [!] (e/dom-listener node "keydown"
                              #(some-> (ui4/?read-line! node %) !)))))))

(defn progress-color [state]
  (case state ::e/init "gray" ::e/ok "green" ::e/pending "yellow" ::e/failed "red"))

(e/defn Field ; client bias, bias is v. delicate
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
        [status x dx err])))) ; monitor

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

(defn with-genesis [db {[x dx] ::hf/dirty :as edits}]
  (let [id (contrib.identity/genesis-tempid! db)]
    ; do we have a txn now?
    ; how does this edit flow through the masterlist to the top?
    (update-in edits [::hf/dirty] (fn [[x dx]] [(x id) (dx id)])))) ; see TodoItemCreate

(e/defn CreateController
  "maintains an index of locally created entities by watching the Datomic tx-report"
  [kf Form #_&args]
  (let [>edit (Popover. "open" Form) ; pending until commit
        !genesis-edits-index (m/mbx {})
        >genesis-edits-index (m/stream (mx/poll-task !genesis-edits-index))]
    
    (new ; monitor genesis
      (m/ap
        (let [{[x] ::hf/dirty :as edit} (with-genesis hf/db (m/?< >edit))]
          (!genesis-edits-index (assoc genesis-edits-index (kf x) edit)))))
    
    (new ; monitor birth
      (m/ap
        (let [genesis-edits-index (m/?< >genesis-edits-index)
              genesis-tempids (keys genesis-edits-index)
              birthed-tempids (vals (:ids->tempids (m/?> >tx-report)))] ; non-preemptive
          
          (when-some [local-births (seq (clojure.set/intersection birthed-tempids
                                          genesis-tempids))]
            ; "Birthed" entities are no longer managed by their mother.
            (!genesis-edits-index (apply dissoc genesis-edits-index local-births))))))
    
    (m/zip vals >genesis-edits-index)))

(e/defn MasterList
  "coordinates the insertion of new entities into a database-backed editable list
so as to stabilize the entity's associated Electric frame as the entity progresses 
through its edit lifecycle (dirty -> pending -> completed)."
  [stable-kf EditForm CreateForm]
  (e/server
    (e/fn [records] ; e/fn transfer
      (e/client
        (let [>genesis-edits (CreateController. stable-kf CreateForm)
              >query-diffs (e/server (e/reconcile stable-kf (e/fn [] records))) ; check transfer
              >local-pending-diffs (m/ap (edit->diff (::hf/pending (m/?> >genesis-edits))))
              >local-dirty-edits (m/ap (::hf/dirty (m/?> >genesis-edits)))]
          
          ; Note: when the dirty record becomes pending, and we edit further, 
          ; it's the same edit structure
          (apply aggregate-edits (new >local-dirty-edits)
            (e/For-by-streaming. stable-kf EditForm >query-diffs >local-pending-diffs)))))))
