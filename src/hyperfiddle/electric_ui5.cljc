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
            [hyperfiddle.client.edn-view :as ev]))

(e/def local?)
(def tempid? (some-fn nil? string?))

; todo, don't use for-event-pending-switch
(defmacro control [event-type parse unparse v V! setter & body]
  `(let [[state# v#] (e/for-event-pending-switch [e# (e/listen> dom/node ~event-type)]
                       (some->> (~parse e#) (new ~V!)))]
     #_(dom/style {:background-color (when (= ::e/pending state#) "yellow")}) ; collision
     ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
     (when-some [v# (when (and (not (new dom/Focused? dom/node)) (#{::e/init ::e/ok} state#)) ~v)]
       (~setter dom/node (~unparse v#))) ; js coerce
     ~@body
     [state# v#]))

(defmacro input [v V! & body] ; todo remove V!
  `(dom/input
     (let [[state# v#] (control "input" #(-> % .-target .-value) identity ~v ~V! dom/set-val)
           color# (if local? "blue" (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red"))]
       (dom/style {:border-width "2px", :border-style "solid", :border-color color#, :border-radius "0.2em"})
       #_(when local? (dom/props {:disabled true})) ; why? Not sure
       (case state# ::e/failed (.error js/console v#) nil)
       ~@body)))

(e/defn On-input-submit [node]
  ; (assert node is type input)
  (new (m/reductions {} nil
         (m/observe (fn [!] (e/dom-listener node "keydown"
                              #(some-> (ui4/?read-line! node %) !)))))))

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


; WRONG: To auto-sync and monitor sync progress, we must be at the EAV abstraction 
; level which is the minimum unit of persistence to Datomic


#(:clj (def !v'-server (atom nil))) ; out of band return
(e/def v'-server (e/server (e/watch !v'-server)))

#(:cljs (def !state-client (atom nil))) ; out of band return
(e/def state-client (e/client (e/watch !state-client)))

(e/defn Field
  "[server bias] orchestration helper for declarative config"
  [{:keys [record a Control parse unparse txn]}]
  ; domain -> control transformation -- parse, unparse -- :done to true
  ; control -> physical/DOM transformation -- getter, setter -- target-checked to true, true to target-checked
  ; DomInputController must own the server io.
  ; what if we send the physical value repr to the server
  ; and run txn on the server? 
  (e/server
    (let [v-domain (get record a)
          v-physical (parse v-domain)
          v'-physical (Control. v-physical) ; optimistic, can it rollback? ; monitor progress here? 
          v'-domain (unparse v'-physical)
          field-tx (txn v'-domain)]
      field-tx)))

(e/defn DOMInputController
  "[server bias, returns state-client and v'-server]"
  ;"read and write a single control's DOM with optimistic state and rollback"
          ; here we setup side channel return values for multi-colored return values, 
        ; in order to avoid an undesired transfer.
        ;   1. status (green dot), client-side (includes optimistic value)
        ;   2. v', server-side (so that it's safe, this is what the green dot is monitoring)
        ;   3. v'-client ??? 
  ; v' may not be transacted yet, even if this control is green/safe
  [node event-type v-server ref!]
  (e/client
    (when-some [e (e/listen> node event-type)] ; CT event signal is mostly nil
      (let [v'-local (parse (ref! (.-target e)))
            
            ; split middle SyncController from DomInputontroller
            [[status v'-local :as state-client]
             (try
               [::ok (e/server (reset! v'-server v'-local) nil)] ; make it "safe" immediately, + side channel
               (catch Pending _ [::pending v'-local]) ; optimistic local value
               (catch :default ex [::failed ex]))]] ; impossible
        (reset! !state-client state-client) ; side channel
        
        ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
        (when-some [v-server (when (and (not (dom/Focused?. dom/node)) ; prefer local when focused
                                     (#{::e/init ::e/ok} status)) ; keep local until safe
                               v-server)] ; overwrite local value with newer server value
          (ref! v-server))))))

#?(:cljs (defn -target-value [^js e] (.-target.value e))) ; workaround inference warnings
#?(:cljs (defn -node-checked!
           ([node] (.-checked node))
           ([node checked] (set! (.-checked node) checked)))) ; js coerce

(e/defn Checkbox 
  "[server bias]"
  [checked-server]
  (e/client
    (dom/input (dom/props {:type "checkbox"})
      (e/server
        (let [v' (DOMInputController. "change" checked (e/client (partial -node-checked! dom/node)))] ; fix color
          (e/client
            (let [[status v'] state-client]
              (dom/style {:outline (str "2px solid " (progress-color status))})))
          v')))))


#_(case status ::e/failed (.warn js/console v) nil) ; debug, note cannot fail as not a transaction

;; (e/defn ServerInc [x]
;;   (inc x))

;; (e/server (ServerInc. 42))