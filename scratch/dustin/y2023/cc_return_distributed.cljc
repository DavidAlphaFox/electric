(ns dustin.y2023.cc-return-distributed
  (:require [hyperfiddle.electric :as e]
            [missionary.core :as m]))


; Definition : `xΔx` is a logical tuple with a client component and a server component.
; How to return a (client) optimistic view + a (server) transaction request from an e/fn?
; How to do the same for an e/for which collects N of these

(e/def !x) (e/def x (e/watch !x))
(e/def !dx) (e/def dx (e/watch !dx))


(defmacro Branch [Body]
  (e/client 
    (binding [!x (atom nil)]
      (e/server 
        (binding [!dx (atom nil)]
          (let [xdx (Body.)]))))))

(defmacro branch [& body] (e/fn [] ~@body))

(e/defn CheckboxField [{:keys [a v txn]}]
  (e/client
    (let [x'-client (new (m/reductions (m/relieve {} (m/observe (fn [!] ...)))))]
     (vector
       (e/server (txn x'-client))
       (e/client x')))))

(e/defn Page []
  (e/client 
    (CheckboxField. 
      {:a :task/status
       :v (query-from-server db)
       :txn (fn [x'] [[:db/add e :task/status x']]) ; dx
       :optimistic (fn [x'] {:task/status x'})}) ; x
    
    #_(CheckboxField.
        {:a :task/status
         :v (query-from-server db)
         :txn (fn [x'] [[:db/add e :task/status x']])
         :optimistic (fn [x'] {:task/status x'})})
    
    ))

(e/defn App []
  (e/server
    (let [xdx (branch
                (let [[x dx] (Page.)]
                  (e/server (transact dx))
                  (e/client (dom/pre (dom/text (pprint-str x))))))])))