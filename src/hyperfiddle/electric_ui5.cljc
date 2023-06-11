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

(e/defn Field
  "[server bias] orchestration helper for declarative config. Optimistic local
updates are returned by side channel, see `Control`."
  [{:keys [record ; record on server except create-new
           a Control parse unparse txn]}]
  (e/server
    (binding [!v'-server (atom nil)] ; setup side channel
      (let [v-domain (get record a)
            v-physical (parse v-domain)
            _v'-physical (Control. v-physical) ; optimistic, can it rollback? ; monitor progress here? 
            v'-physical (e/watch !v'-server) ; weird trick - the InputController owns the sync
            ; otherwise, progress is monitored right here, which is the wrong place. (or is it? - yes due
            ; to stabilizing concurrent edits inside the control.)
            v'-domain (unparse v'-physical)
            {:keys [txn optimistic] :as xdx} (txn v'-domain)]
        
        (if-not txn ; in create-new case, we don't have a txn (due to no ID until popover submit)
          xdx ; but we have the perfect optimistic record already!
          (let [{:keys [db tx-report]} (hf/Transact!. field-tx)] ; impacts this branch only; even needed?
            xdx))
        
        ; still send both v and dv up to top? why?
        ; because at higher levels we may concat in more txn at form level e.g. create-new-todo
        ; they will have branched in this case, causing the above to be local.
        xdx))))

#(:cljs (def ^:dynamic !v'-client #_(atom nil))) ; out of band return
#(:clj (def ^:dynamic !v'-server #_(atom nil))) ; out of band return

; can we use currying to fix this concern?
#(:cljs (def !sync (atom nil))) ; out of band return
(e/def sync (e/client (e/watch !sync))) ; todo cannot be global


;   1. status (green dot), client-side (includes optimistic value)
;   2. v', server-side (so that it's safe, this is what the green dot is monitoring)
;   3. v'-client ??? 
(e/defn SyncController
  "locally return [status v'], + remotely return `v-server` by side channel"
  [v'-local]
  (e/client
    ;(reset! !sync) ; side channel
    (try
      ; todo don't roundtrip initial value?
      [::ok (e/server (reset! !v'-server v'-local))] ; make it "safe" immediately, + side channel
      ; ::ok is never actually seen, overridden in DomController2
      (catch Pending _ [::pending v'-local]) ; optimistic, must not lag
      (catch :default ex [::failed ex])))) ; impossible

(comment 
  (e/server
    (binding [!server-v (atom .)] ; return channel
      (e/client
        (DDomInputController ...)))))

(e/defn DomInputController
  "[server bias] signal of what the user types as [status v']"
  [node event-type v-server ref!] #_[server-v] ; side channel
  (e/client
    (let [>v (->> (m/observe (cc/fn [!] (dom-listener node event-type ! opts))) ; user input events
                (m/relieve {}) ; input value signal, never backpressure the user 
                (m/eduction (map (fn event->value [e] (parse (ref! (.-target e))))))
                (m/reductions {} [::ok v-server])) ; try to accidental round trip of initial value
          [status v :as state] (SyncController. (new >v))] ; also returns server value in side channel

      (cond
        (dom/Focused?. node) state
        (= ::pending status) state
        ;(= ::failed status) ?
        ;(= ::init status) v-server
        (= ::ok status) (do (ref! v-server) [::ok v-server]))))) ; concurrent edit, otherwise work-skipped

(e/defn Checkbox
  "[server bias]"
  [checked-server] #_[*status*] ; dynamic status from Field?
  (e/client
    (dom/input (dom/props {:type "checkbox"})
      (let [[status v] (e/client (DOMInputController. dom/node "change" checked-server (e/client (partial -node-checked! dom/node))))]
        (dom/style {:outline (str "2px solid " (progress-color status))})
        (assert (not= ::failed status) "transaction failures are impossible here (otherwise would need to filter them here)")
        v))))

(e/defn Input [v-server]
  (e/client
    (dom/input
      (let [[status v] (DOMInputController. dom/node "input" v-server (e/client (partial -node-value! dom/node)))]
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

(defn with-genesis [db {:keys [optimistic] :as xdx}]
  {:optimistic (merge optimistic
                 {:db/id (contrib.identity/genesis-tempid! db)})})

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
    
    ; on commit latches and returns the xdx, fix as it currently returns the stage
    (let [{:keys [optimistic]} (e/client (Popover. "open" ; todo PopoverBody - auto-open, no anchor
                                           ; no point in updating the popover-local dbval here, as the popover is closing.
                                           (e/fn [] (e/server (with-genesis hf/db (Body.))))))]

      (swap! !local-index assoc (kf optimistic) optimistic) ; due to the blink?
      ; note we ignore hf/stage, it was damaged by swap!
      #_hf/stage local-index))) ; return also the local records, for use in optimistic queries

(e/defn MasterList
  "encapsulates both rendering the table and also adding elements to it, in 
order to stabilize identity"
  [stable-kf CreateForm EditForm query-records] ; specifically not entities due to the optimism strategy
  (e/client
    (let [!ids (atom {}) ; #tempid and/or reified id -> process-unique identity
          local-index (CreateController. stable-kf CreateForm) ; the local-index is the branch

          ; operate on records because datomic-entity api has broken equality
          ; note, the optimistic view is also in record-shape not entity shape (though we could index that) 
          records (merge-unordered stable-kf
                    (vals local-index) ; must have matching pull shape
                    (try (e/server (query-records)) ; matching pull shape
                      (catch Pending _))) ; todo rethrow pending for load timers above

          edit-txns (e/for-by stable-kf [record records] ; must include genesised records, for stability
                      ; What if the local-records end up in two places? That's a race, both will 
                      ; ensure existance, one will win (so long as tempids are not reused and remain valid)
                      (EditForm. record))] ; Ensure local entities here, they've been submitted

      ; has both txn and :optimistic, for comparison
      edit-txns))) ; use this to complete circuit, hf/stage has been damaged by swap!