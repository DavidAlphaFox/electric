(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            [contrib.data :refer [transpose index-by]]
            [contrib.debug :as dbg]
            [contrib.identity :refer [tempid?]]
            contrib.str
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [hyperfiddle.electric-data :refer [Triple First Second Third]]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [hyperfiddle.stage :refer [aggregate-edits]]
            [missionary.core :as m]))

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
                    (reset! hf/!tx-report (new (e/task->cp (transact! conn dx)))))) ; optional
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

#_(case status ::e/failed (.warn js/console v) nil) ; debug, note cannot fail as not a transaction

(defn with-genesis [db {:keys [optimistic txn] :as xdx}]
  (let [id (contrib.identity/genesis-tempid! db)]
    {#_#_:txn ...
     :optimistic (merge optimistic {:db/id id})}))

(e/defn CreateController
  "maintains a local index of created entities by watching the Datomic tx-report"
  [kf Body #_&args]
  (let [!pending-index (atom {}), pending-index (e/watch !pending-index) ; perhaps global to the client, is it a datascript db?
        local-tempids (vals pending-index)
        promoted-tempids (vals (:ids->tempids >tx-report)) ; includes those from other sessions
        local-promotions (clojure.set/intersection promoted-tempids local-tempids)]

    (when (seq local-promotions) ; these genesis records have now been promoted
      ; and now appear in the masterlist query, so stop tracking them locally.
      (swap! !pending-index #(apply dissoc % local-promotions))) ; "birth" - independent entity is no longer managed by the mother

    (let [vdv (e/client (Popover. "open" ; todo PopoverBody - auto-open, no anchor
                          (e/fn Body' []
                            ; no point in updating the popover-local dbval here, as the popover is closing.
                            (e/server (with-genesis (e/snapshot hf/db) ; !!! todo make discrete
                                        (Body.))))))]
      (swap! !pending-index assoc (kf (:optimistic vdv)) vdv) ; discrete!
      pending-index))) ; the local-index is the branch

(e/defn For-by-streaming [stable-kf pending-index server-records Branch]
  (e/client
    ; operate on records because datomic-entity api has broken equality
    ; note, the optimistic view is also in record-shape not entity shape (though we could index that)
    (let [records (merge-unordered stable-kf ; todo rethrow pending for load timers above
                    (-> (vals pending-index) (map :optimistic)) ; must have matching pull shape
                    server-records)]
      (e/for-by kf [record records] ; must include genesised records, for stability
        (Branch. record)
        ...client-cont
        ...server-cont)))) ; Ensure local entities here, they've been submitted

(e/defn MasterList
  "encapsulates both rendering the table and also adding elements to it, in
order to stabilize identity"
  [stable-kf server-records EditForm CreateForm] ; specifically not entities due to the optimism strategy
  (e/client
    (let [stage (CreateController. stable-kf CreateForm)] ; todo discrete result
      (aggregate-edits
        (e/server ; must not accidentally transfer local-index
          (For-by-streaming. stable-kf server-records (e/client stage) ; matching pull shape
            (EditForm. record)))))))
