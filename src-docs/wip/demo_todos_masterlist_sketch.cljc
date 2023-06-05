(ns wip.demo-todos-masterlist-sketch
  "masterlist sketch - not in working order"
  (:import [hyperfiddle.electric Pending])
  (:require #?(:clj [datomic.api :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.electric-ui5 :as ui5]
            [contrib.debug :as dbg]
            [hyperfiddle.electric.impl.runtime :as r]))

(defonce !order-id #?(:clj (atom 0) :cljs nil)) ; stabilize order for optimistic create new

(e/defn TodoItemOld [record]
  (e/client
    (dom/div
      (ui5/checkbox
        record
        (e/fn [v] (= :done (:task/status v)))
        (e/fn [checked?]
          (e/server (new Tx! [[:db/add (:db/id record)
                               :task/status (if checked? :done :active)]])))
        EnsureEntity
        (dom/props {:id e}))
      (dom/label (dom/props {:for e}) (dom/text (e/server (:task/description e)))))))

(e/defn MasterList [TodoItem]
  (e/server
    ; create new (modal semantics, user submits via "stage" or "enter", can be rapid) 
    (let [_ (ui5/modal (TodoItem. {:task/status :active}))
          optimistic-records .
          txns hf/stage]
      
      (e/for-by :hf/stable-id [record (todo-records db)] optimistic-records
        (TodoItem. record)))))

(e/defn TodoItem [record]
  (e/client
    (dom/div (dom/pre (dom/text record))
      #_(ui5/entity record EnsureEntity) 
      (ui5/checkbox)
      (ui5/input))))

(e/defn AdvancedTodoList []
  (e/server
    (binding [db (e/watch !db), Tx! (e/fn [tx] (new (e/task->cp (tx! tx))) nil)]
      (d/transact !conn schema)
      (prn :db db)
      (e/client
        (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (Latency. 0 2000)
        (FailRate. 0 10)
        (dom/div (dom/props {:class "todo-list"})
          ;(dom/div {:class "todo-items"}) 
          
          (MasterList. TodoItem)
          
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
            (dom/text " items left")))))))

;; gotchas
;; - in failed state moving a range picker floods more errors in the console
;; - checkbox failure doesn't revert, is that correct/good?
