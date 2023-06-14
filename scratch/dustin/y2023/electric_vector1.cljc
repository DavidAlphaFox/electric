(ns dustin.y2023.electric-vector1
  (:require [hyperfiddle.electric :as e]
            [missionary.core :as m]))


; Definition : `xΔx` is a logical tuple with a client component and a server component.
; How to return a (client) optimistic view + a (server) transaction request from an e/fn?
; How to do the same for an e/for which collects N of these

(e/def !x) (e/def x)
(e/def !dx) (e/def dx)


(defmacro Branch [Body]
  (e/client
    (binding [!x (atom nil) x (e/watch !x)]
      (e/server
        (binding [!dx (atom nil) dx (e/watch !dx)]
          (let [_xdx (Body.)]
            nil))))))

(defmacro branch [& body] (e/fn [] ~@body))

(e/defn CheckboxField [{:keys [a v txn optimistic]}]
  (e/client
    (let [x'-client (new (m/reductions (m/relieve {} (m/observe (fn [!] ...)))))]
      (e/server (reset! !x (txn x'-client)))
      (e/client (reset! !dx (optimistic x'-client))))))

(e/defn Page []
  (e/client
    (let [xdxs (e/for-by [record records] ; streaming actually
                 (CheckboxField.
                   {:a :task/status
                    :v (query-from-server db)
                    :txn (fn [x'] [[:db/add e :task/status x']])
                    :optimistic (fn [x'] {:task/status x'})})
                 (Vector.
                   (e/client x)
                   (e/server dx)))]
      (e/server (transact! dxs) #_(e/for [dx dxs] (dom/pre (dom/text (pprint-str dx)))))
      (e/client (dom/pre (dom/text (pprint-str xs))) #_(e/for [x xs] (dom/pre (dom/text (pprint-str x)))))
      )))

(e/defn App []
  (e/server
    (let [xdx (branch
                (do (Page. args)
                  (e/server (transact dx))
                  (e/client (dom/pre (dom/text (pprint-str x)))))

                #_(let [[x dx] (Page. args)]
                    (e/server (transact dx))
                    (e/client (dom/pre (dom/text (pprint-str x))))))])))

; 