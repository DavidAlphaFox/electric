(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            [contrib.identity :refer [tempid?]]
            contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom :refer [-target-value -node-checked! -node-value!]]
            [hyperfiddle.electric-ui4 :as ui4]
            [missionary.core :as m]
            [contrib.debug :as dbg]
            [hyperfiddle.client.edn-view :as ev]
            [hyperfiddle.api :as hf]))

(e/def local?)
(def tempid? (some-fn nil? string?))

(e/defn On-input-submit [node]
  ; (assert node is type input)
  (new (m/reductions {} nil
         (m/observe (fn [!] (e/dom-listener node "keydown"
                              #(some-> (ui4/?read-line! node %) !)))))))

(defn progress-color [state]
  (case state ::e/init "gray" ::e/ok "green" ::e/pending "yellow" ::e/failed "red"))

(e/defn Field ; operate at AV level of asbtraction; optimistic views look like {a v} + any extra stuff -- todo datascript
  [{:keys [record ; record on server except create-new
           a Control parse unparse txn]}]
  (e/client
    (reset! hf/!x (optimistic (unparse (Control. (e/server (parse (get record a))) status)))) ; check transfers
    (reset! !status (if-not txn ; create-new does not have one
                      ::ok
                      (try (e/server (reset !hf/dx (when (txn v)))) ::ok
                        (catch Pending _ ::pending)))))
  (e/server (hf/Transact!. hf/dx))) ; optional

(e/defn DomInputController ; client bias
  "[server bias] signal of what the user types as [status v']"
  [node event-type v-server ref!] #_[server-v] ; side channel
  (e/client
    (let [>v (->> (m/observe (cc/fn [!] (dom-listener node event-type ! opts))) ; user input events
               (m/relieve {}) ; input value signal, never backpressure the user 
               (m/eduction (map (fn event->value [e] (parse (ref! (.-target e))))))
               (m/reductions {} v-server))

          [status v :as state] (SyncController. (new >v))]
; todo this is hoisted out, but what about focused logic? OOps?
      (cond
        (dom/Focused?. node) state
        (= ::pending status) state
        ;(= ::failed status) ?
        ;(= ::init status) v-server
        (= ::ok status) (do (ref! v-server) [::ok v-server]))))) ; concurrent edit, otherwise work-skipped

(e/defn Checkbox
  "[server bias]"
  [checked-server status]
  (e/client
    (dom/input (dom/props {:type "checkbox"})
      (let [[status v] (DOMInputController. dom/node "change" checked-server (partial -node-checked! dom/node))]
        (dom/style {:outline (str "2px solid " (progress-color status))})
        (assert (not= ::failed status) "transaction failures are impossible here (otherwise would need to filter them here)")
        v))))

(e/defn Input [v-server status]
  (e/client
    (dom/input
      (let [[status v] (DOMInputController. dom/node "input" v-server (partial -node-value! dom/node))]
        (dom/style {:outline (str "2px solid " (progress-color status))})
        (assert (not= ::failed status) "transaction failures are impossible here (otherwise would need to filter them here)")
        v))))

#_
(e/defn InputSubmit [v-server #_ body]
  (e/client
    (dom/input
      (let [[status v] (DOMInputController. "input" v-server :keep-fn (partial ui4/?read-line! dom/node)
                         (e/client (partial -node-value! dom/node)))]
        (dom/style {:outline (str "2px solid " (progress-color status))})
        v))))

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
  [stable-kf server-records CreateForm EditForm] ; specifically not entities due to the optimism strategy
  (e/client
    (let [local-index (CreateController. stable-kf CreateForm)] ; todo discrete result
      (e/server ; must not accidentally transfer local-index
        (hf/branch
          (For-by-streaming. stable-kf server-records (e/client local-index) ; matching pull shape
            (e/fn [record]
              (hf/branch
                (EditForm. record)
                ... hf/dx ...
                ))))))))

; does it need to branch each body and then collect and delegate to parent branch?

; client - collect hf/x
; server - collect hf/dx
; theres a 1:N due to the e/for

; hf/branch is as common as reutrning values, then. it's just a call convention
; hf/branchN ?
; how do you do it in CT? mount/unmount?