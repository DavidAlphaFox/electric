(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [contrib.debug :as dbg]))

(e/def local?)
(def tempid? (some-fn nil? string?))

(defmacro control [event-type parse unparse v V! setter & body]
  `(let [[state# v#] (e/for-event-pending-switch [e# (e/listen> dom/node ~event-type)]
                       (some->> (~parse e#) (new ~V!)))]
     #_(dom/style {:background-color (when (= ::e/pending state#) "yellow")}) ; collision
     ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
     (when-some [v# (when (and (not (new dom/Focused?)) (#{::e/init ::e/ok} state#)) ~v)]
       (~setter dom/node (~unparse v#))) ; js coerce
     ~@body
     [state# v#]))

(defmacro input [v V! & body]
  `(dom/input
     (let [[state# v#] (control "input" #(-> % .-target .-value) identity ~v ~V! dom/set-val)
           color# (if local? "blue" (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red"))]
       (dom/style {:border-width "2px", :border-style "solid", :border-color color#, :border-radius "0.2em"})
       #_(when local? (dom/props {:disabled true})) ; why? Not sure
       (case state# ::e/failed (.error js/console v#) nil)
       ~@body)))

(defmacro entity [record EnsureEntity & body]
  `(dom/div (dom/text "entity" (:hf/stable-id ~record))
     (let [[state# e#] (new ~EnsureEntity (:db/id ~record) ~record)
           color# (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red")]  ; tooltip
       (dom/style {:border (str "2px solid " color#)})
       (case state# ::e/failed (.error js/console e#) nil) 
       (binding [local? (tempid? (:db/id ~record))] ;; mark entity local for downstream code
         ~@body))))

#?(:cljs (defn value [^js e] (.-target.value e))) ; workaround inference warnings, todo rename
#?(:cljs (defn checked [^js e] (.-target.checked e)))

;; (defmacro checkbox [v V! & body]
;;   `(dom/input (dom/props {:type "checkbox"})
;;      (let [[state# v#] (control "change" #(-> % .-target .-checked) identity ~v ~V! #(set! (.-checked %) %2))
;;            color# (if local? "blue" (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red"))]
;;        (dom/style {:outline-width "2px", :outline-style "solid", :outline-color color#})
;;        (when local? (dom/props {:disabled true}))
;;        (case state# ::e/failed (.error js/console v#) nil)
;;        ~@body)))

(defmacro checkbox [v V! & body]
  `(dom/input (dom/props {:type "checkbox"})
     (let [[state# v#] (control "change" checked identity
                         ~v ~V! #(set! (.-checked %) %2) ~@body)]
       (dom/style {:outline (str "2px solid " (case state#
                                                ::e/init "gray"
                                                ::e/ok "green"
                                                ::e/pending "yellow"
                                                ::e/failed "red"))}))))

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
