(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            [contrib.identity :refer [tempid?]]
            contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
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

#_#_#_
(e/defn ReadEntity [{:keys [db/id] :as record}]
  ; we should ask the process-local store for any optimistic updates for this record
  (try
    (e/server [::e/init record ; assumes pull API was already used, todo use entity api below
               #_(d/entity db e)])
    #_(catch Pending _ [::e/pending record]) ; never happens
    (catch :default e [::e/failed e])))

(e/defn CreateEntity [{:keys [db/id] :as record}]
  (try ; create is never ::e/init -- what is this?
    (case (d/transact !conn [record]) ; todo ensure, not raw transact, don't fail if someone else beat us
      ; (when-not (d/entity db id)))
      #_[::e/ok (into {} (d/touch (d/entity db (ids->tempids id))))])  ; no need to query, a branch above switches
    (catch Pending _ [::e/pending record]) ; optimistic
    (catch :default e [::e/failed e])))

(e/defn EnsureEntity [{:keys [db/id] :as record}] ; optimistic record, pre-pulled
  (if-not (tempid? id)
    (ReadEntity. record)
    (CreateEntity. record))) ; todo must be idempotent

(defn progress-color [state]
  (case state ::e/init "gray" ::e/ok "green" ::e/pending "yellow" ::e/failed "red"))

; What is the staging area anyway?
; Why is it tied to Datomic?
; Under what circumstances is something a control accepted, rejected by the server?
; -- duplicate email address
; ... accepted and "safe", and also invalid

; what is red status?
; it failed
; at which level did it fail?
; did the value fail to sync over network?
; is the value invalid in the context of the view of the field? the form?
; did the transaction at form level fail?

; What does user care about?
;  - is my edit safe (acknowledged) - don't let me lose data
;  - control over the submit action
;  - is the submit safe
;  - did the submit succeed

; Do we ensure at field, form, ... each level?
; why not?
; once edit is ultimately accepted by a higher level, the tempid is promoted in that layer
; thus indexing responsibility propagates upwards?

; Progress is necessary at each level of interaction:
;  - optimistic value edits   - V
;  - txn at field level       - d/with: EAV + field-txn   
;  - d/with at form level     - d/with: N * (EAV + field-txn) + form-txn
;  - d/transact at top level  - final rebase

; optimism
; optimistic queries, create-new

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


#?(:cljs (defn -target-value [^js e] (.-target.value e))) ; workaround inference warnings
#?(:cljs (defn -node-value!
           ([node] (.-value node))
           ([node v] (set! (.-value node) v) #_v)))
#?(:cljs (defn -node-checked!
           ([node] (.-checked node))
           ([node checked] (set! (.-checked node) checked)))) ; js coerce

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

;; (e/defn ServerInc [x]
;;   (inc x))

;; (e/server (ServerInc. 42))