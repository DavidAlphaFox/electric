(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            [clojure.set :refer [intersection]]
            [contrib.data :refer [transpose index-by]]
            [contrib.debug :as dbg]
            [contrib.identity :refer [tempid?]]
            [contrib.missionary-contrib :as mx]
            contrib.str
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [hyperfiddle.electric-data :refer [Triple First Second Third]]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [hyperfiddle.stage :refer [aggregate-edits]]
            [missionary.core :as m]))

(def !tx-report)
(e/def >tx-report)

(defn progress-color [state]
  (case state ::e/init "gray" ::e/ok "green" ::e/pending "yellow" ::e/failed "red"))

(e/defn Field ; client bias, bias is v. delicate
  "returns a signal of [viewstate tx]; is it pure?
where is error/pending? on the tx-report for this tx?"
  [{:keys [v Control parse unparse txn] #_[status]
    :or {txn identity}}]
  (e/client
    (let [!status (atom ::accepted) status (e/watch !status) ; failed, pending
          #_#_#_#_!err (atom nil) err (e/watch !err)]

      ; Any new v triggers Pending transition
      ; construct the txn, send up
      ; Await a tx-report that marks our txn as Accepted or Rejected
      
      (let [v (unparse (Control. (parse v) status))
            _ (reset! !status ::dirty) ; pre blur
            
            ; Todo commit/discard logic here, a popover level
            ; produce xdx on commit
            
            _ (reset! !status ::pending) ; on blur, txn is sent up
            
            >xdx (e/fn [] [(optimistic v) (txn v)])] 
        
                ; send up txn in CT
        ; Know if this exact txn is reflected in the tx report
        ; -- do we need to perform deep datom inspection?
        ; -- we will get the exact tx-report back in a world without popovers
        ; -- with popovers, we get a batched txn back, we need to know if our edit was included
        ; -- -- i can solve this later
        
        ; for now: just check for exact match of txn in the report
        ; m/relieve causes txn to change. How do we track successive versions
        ; of the same logical txn that is pending?
        
        ; How do we avoid smashing the transactor in spreadsheet mode
        ; when we type fast? We need to allocate another popover boundary for each
        ; field to get tab/enter/esc -- commit/discard
        
        (try (if-some [tx (new (monitor-txn! >xdx >tx-report))]
               tx (reset! !status ::accepted))
          (catch Exception e (reset! !status ::rejected)))))))

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

(e/defn Button [label rf acc0]
  (dom/button
    (dom/text label)
    (->> (m/observe (fn [!] (dom-listener dom/node "click" !)))
      (m/reductions rf acc0))))

#_(case status ::e/failed (.warn js/console v) nil) ; debug, note cannot fail as not a transaction

(defn with-genesis [db [?x ?dx]]
  (let [id (contrib.identity/genesis-tempid! db)]
    [(?x id) (?dx id)])) ; see TodoItemCreate

(defn monitor-txn!
  "Fields use this to return the transaction until it is marked completed, then 
switch to nil. controls need to know if a pending txn they submitted has been 
completed or rejected to draw the green dot (and not glitch under concurrent 
modification)."
  [>xdx >tx-report]
  (m/ap
    (let [!pending-tx (atom nil)
          [x batched-dx] (m/?< >xdx)] ; intent is one at a time, this is an explicit submit event
      (reset! !pending-tx batched-dx) ; but only track latest 
      (when (contains? (::accepted (m/?< >tx-report)) batched-dx) ; assume unaltered, fixme
        (reset! !pending-tx nil)) 
      (m/?< (m/watch !pending-tx)))))

(defn genesis-entity-lifecycle!
  "Master-list uses this to coordinate entity creation between the create-controller
and the e/for-by-streaming. Unlike monitor-txn!, this will manage multiple entities 
created concurrently which progress in isolation."
  [>xdx >tx-report]
  (m/ap
    (let [[x dx] (m/?> ##Inf >xdx)]
      (try (if-some [tx (monitor-txn! >xdx >tx-report)]
             [:assoc (stable-kf x) x]
             [:dissoc (stable-kf :dispose x)])
        (catch Exception e #_(reset! !status ::rejected)))))) ; todo retry

(e/defn CreateController
  "maintains an index of locally created entities by watching the Datomic tx-report"
  [stable-kf Form #_&args]  
  ; tx backpressure has been relieved, it's unlikely to change
  (let [[x batched-dx :as xdx] (e/snapshot ; todo handle popover reopen (unwind effects?)
                                 (with-genesis hf/db
                                   (Popover. "open" Form))) ; pending until commit
        >par-diffs (new (genesis-entity-lifecycle! (e/fn [] xdx) >tx-report))]
    [>par-diffs batched-dx]))

(e/defn MasterList
  "coordinates the insertion of new entities into a database-backed editable list
so as to stabilize the entity's associated Electric frame as the entity progresses 
through its edit lifecycle (dirty -> pending -> completed)."
  [stable-kf EditForm CreateForm]
  (e/server
    (e/fn [records] ; e/fn transfer
      (e/client
        (let [[>pending-view-diffs dx] (CreateController. stable-kf CreateForm)]
          (vector ; isolated txs that race in parallel unless a popover batches them
            dx #_x ; x is local now?
            (map second ; just the dxs for now, throw away the view state
              (maintain-vec
                (e/par EditForm
                  (mx/mix >pending-view-diffs
                    (e/server (e/reconcile stable-kf (e/fn [] records))))))))))))) ; illegal flow transfer
