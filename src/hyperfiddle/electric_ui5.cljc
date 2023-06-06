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
     (when-some [v# (when (and (not (new dom/Focused?)) (#{::e/init ::e/ok} state#)) ~v)]
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

(e/defn Field
  "To auto-sync and monitor sync progress, we must be at the EAV abstraction level
which is the minimum unit of persistence to Datomic"
  [{:keys [record #_e a Control parse unparse txn]}]
  (let [v (get ?record a)

        ; domain -> control transformation -- parse, unparse -- :done to true
        ; control -> physical/DOM transformation -- getter, setter -- target-checked to true, true to target-checked
        
        tx (when-some [v' (unparse (Control. (parse v)))] ; domain->control transformation
             (txn v'))
        
        ; DomInputController must own the server io.
        ; Can the Control be separated from the Controller? YES!
        ; the control is a rendering of the status.

         ; local txn unit, likely in a branch but not always.
        status (Ensure. tx)] ; (V!.) -- this goes into the control i think

    (case status ::e/failed (.warn js/console v) nil) ; debug
    tx)) ; may not be transacted yet, even if this control is green/safe

(e/defn DOMInputController
  ;"read and write a single control's DOM with optimistic state and rollback"
  [event-type v-server ref!]
  (when-some [e (e/listen> dom/node ~event-type)] ; CT event signal is mostly nil
    (let [v-local (parse (ref! (.-target e)))]
      ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
      (when-some [v-request (when (and
                                    (not (dom/Focused?.))
                                    (#{::e/init ::e/ok} status)) ; !!!!!!!!!!!
                            v-local)]
        (ref! v-request)) ; js coerce 
      v-request #_[state v-request])))

#?(:cljs (defn -target-value [^js e] (.-target.value e))) ; workaround inference warnings
#?(:cljs (defn -node-checked!
           ([node] (.-checked node))
           ([node checked] (set! (.-checked node) checked))))

(e/defn Checkbox [checked-server]
  (dom/input (dom/props {:type "checkbox"})
    (let [checked-request (DOMInputController. "change" checked (partial -node-checked! dom/node))]
      (dom/style {:outline (str "2px solid " (progress-color state))})
      checked-request)))

#_
(defmacro checkbox [v V! & body]
  `(dom/input (dom/props {:type "checkbox"})
     (let [[state# v#] (control "change" -node-checked! identity
                         ~v ~V! #(set! (.-checked %) %2) ~@body)]
       (dom/style {:outline (str "2px solid " (progress-color state#))}))))

;; (defmacro checkbox [record V V! EnsureEntity & body]
;;   ; (reset! util/*!side-channel* 42)
;;   `(dom/input (dom/props {:type "checkbox"})
;;      (let [[state# v#] (control "change" checked identity ~record ~V! #(set! (.-checked %) %2)
;;                          ~@body)]

;;        (let [[state# e#] (new ~EnsureEntity (:db/id ~record) ~record)
;;              v# (new ~V e#)
;;              color# (case state# 
;;                       ::e/init "gray"
;;                       ::e/ok "green"
;;                       ::e/pending "yellow"
;;                       ::e/failed "red")] ; tooltip
;;          (dom/style {:outline (str "2px solid " color#)})

;;          #_(case state#
;;              ::e/pending (dom/text "⌛ " v#)
;;              ::e/failed
;;              (do (dom/text "💀 " v#)
;;                (ui/button (e/fn [] #_(reset! !err nil)) ; retry
;;                  (dom/text "⟳")) (dom/text " (" (ex-message v#) ")"))
;;              (::e/ok ::e/init) .)))))
