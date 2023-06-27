(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:import [hyperfiddle.electric Failure Pending])
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
            [hyperfiddle.stage :refer [cons-edit]]
            [missionary.core :as m]))

(def !tx-report)
(e/def >tx-report)

(defn progress-color [state]
  (case state ::dirty "blue" ::pending "yellow" ::accepted "green" ::rejected "red"))

(e/defn Field
  "returns a signal of pending edits that clears to nil once acknowledged in the 
tx-report."
  [{:keys [v Control parse unparse edit]
    :or {parse identity, unparse identity, edit identity}}]
  (e/client
    (let [!status (atom ::accepted) status (e/watch !status)]
      (when-some [v! (Popover "todo iframe" ; simulated esc/tab/enter
                       (e/fn [commit!discard!]
                         (let [v (unparse (Control. (parse v) status))]
                           (when (e/Edge. v) (reset! !status ::dirty)) ; yet to commit
                           v)))]
        (reset! !status ::pending) ; committed
        (let [[x! tx!] (edit v!)] ; what about when tx! is curried? can we genesis here?
          [x! (e/amb tx! ; todo check latency and races
                (e/server
                  (try (let [?tx! (new (monitor-txn! (e/fn [] tx!) >tx-report))]
                         (when (nil? ?tx!) (reset! !status ::accepted))
                         ?tx!)
                    (catch Exception e (reset! !status ::rejected) tx!))))])))))

(e/defn DomInputController [node event-type v0 status ref!]
  (let [v (new (->> (m/observe (fn [!] (dom-listener node event-type ! opts)))
                 (m/relieve {}) ; never backpressure the user
                 (m/eduction (map (fn event->value [e] (parse (ref! (.-target e))))))
                 (m/reductions {} v0)))]
    (cond
      (dom/Focused?. node) v ; ignore concurrent modifications while typing
      (#{::dirty ::pending ::rejected} status) v ; ignore concurrent modifications until accepted
      (= ::accepted status) (do (ref! v0) v0)))) ; concurrent modification, otherwise work-skipped

(e/defn Checkbox [v status]
  (dom/input (dom/props {:type "checkbox"})
    (let [v (DOMInputController. dom/node "change" v status
              (partial dom/-node-checked! dom/node))]
      (dom/style {:outline (str "2px solid " (progress-color status))})
      (assert (not= ::failed status) "transaction failures are impossible here (otherwise would need to filter them here)")
      v)))

(e/defn Input [v status]
  (dom/input
    (let [v (DOMInputController. dom/node "input" v status
              (partial dom/-node-value! dom/node))]
      (dom/style {:outline (str "2px solid " (progress-color status))})
      (assert (not= ::failed status) "transaction failures are impossible here (otherwise would need to filter them here)")
      v)))

(e/defn Button [label rf acc0]
  (dom/button
    (dom/text label)
    (->> (m/observe (fn [!] (dom-listener dom/node "click" !)))
      (m/reductions rf acc0))))

#_(case status ::e/failed (.warn js/console v) nil) ; debug, note cannot fail as not a transaction

(defn with-genesis [db [?x! ?dx!]]
  (let [id (contrib.identity/genesis-tempid! db)]
    [(?x! id) (?dx! id)])) ; see TodoItemCreate

(defn monitor-txn! ; Await a tx-report that marks our txn as Accepted or Rejected
  "Fields use this to return the transaction until it is marked completed, then 
switch to nil. controls need to know if a pending txn they submitted has been 
completed or rejected to draw the green dot (and not glitch under concurrent 
modification)."
  [>tx >tx-report] ; server
  (m/ap
    (let [!pending-tx (atom nil)
          {:keys [::accepted ::rejected ::rejection]} (m/?< >tx-report)
          batched-dx (m/?< >tx)] ; intent is one at a time, this is an explicit submit event
      (reset! !pending-tx batched-dx) ; but only track latest 
      (when (contains? accepted batched-dx) (reset! !pending-tx nil)) ; assume unaltered, fixme
      (when (contains? rejected batched-dx) (Failure. rejection))
      (m/?< (m/watch !pending-tx)))))

(defn genesis-entity-lifecycle!
  "Master-list uses this to coordinate entity creation between the create-controller
and the e/for-by-streaming. Unlike monitor-txn!, this will manage multiple entities 
created concurrently which progress in isolation."
  [>x >dx >tx-report]
  (m/ap
    (let [[x dx] (m/?> ##Inf (m/zip >x >dx))]
      (try (if-some [tx (monitor-txn! >dx >tx-report)]
             [:assoc (stable-kf x) x]
             [:dissoc (stable-kf :dispose x)])
        (catch Exception e #_(reset! !status ::rejected)))))) ; todo retry

(e/defn CreateController
  "maintains an index of locally created entities by watching the Datomic tx-report"
  [stable-kf Form #_&args]  
  ; tx backpressure has been relieved, it's unlikely to change
  (let [[x! batched-dx!] (e/snapshot ; todo handle popover reopen (unwind effects?)
                           (with-genesis hf/db
                             (Popover. "open" Form))) ; pending until commit
        >par-diffs (new (genesis-entity-lifecycle! (e/fn [] x!) (e/fn [] dx!) >tx-report))]
    [>par-diffs x! batched-dx!]))

(e/defn MasterList
  "coordinates the insertion of new entities into a database-backed editable list
so as to stabilize the entity's associated Electric frame as the entity progresses 
through its edit lifecycle (dirty -> pending -> completed)."
  [stable-kf EditForm CreateForm]
  (e/server
    (e/fn [records] ; e/fn transfer
      (e/client
        ; Instead, can we re-parent the Electric frame into the list?
        (let [[>pending-view-diffs x! dx!] (CreateController. stable-kf CreateForm)]
          ; emit isolated txs that race unless a popover batches them
          (->> (e/server (e/reconcile stable-kf (e/fn [] records))) ; illegal flow transfer
            (mx/mix >pending-view-diffs)
            (e/par EditForm) ; list of fields
            maintain-vec ; list of forms is a list-list-fields
            (mapcat identity) ; flattened list of fields
            #_(remove nil?) ; most fields are not firing concurrently
            (cons-edit [x! dx!]) ; send up create tx
            ))))))
