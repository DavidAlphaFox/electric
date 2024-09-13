(ns contrib.triple-store-test
  (:require [contrib.triple-store :as ts]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

(tests
  (-> (ts/->ts) (ts/add {:db/id 1, :foo 2}) (ts/->node 1) :foo) := 2
  (-> (ts/->ts) (ts/add {:db/id 1, :foo 1}) (ts/add {:db/id 2, :foo 1}) :ave :foo (get 1)) := #{1 2}
  ;; (-> (ts/->ts) (ts/add {:db/id 1, :foo 2, :bar 2}) :vea (get 2) (get 1)) := #{:foo :bar}
  (-> (ts/->ts) (ts/add {:db/id 1, :foo 2, :bar 2}) (ts/->node 1) (select-keys [:foo :bar :baz])) := {:foo 2, :bar 2}

  (-> (ts/->ts) (ts/add {:db/id '_}) (ts/upd '_ :x (fnil inc 0)) (ts/upd '_ :x (fnil inc 0)) (ts/->node '_) :x) := 2

  (-> (ts/->ts) (ts/add {:db/id 1}) (ts/asc 1 :x 2) (ts/asc 1 :x 2) :ave :x (get 2)) := #{1}
  (-> (ts/->ts) (ts/add {:db/id 1}) (ts/asc 1 :x 2 :y 3) :eav (get 1)) := {:db/id 1, :x 2, :y 3}

  (-> (ts/->ts) (ts/add {:db/id 1, :foo 1, :bar 1}) (ts/add {:db/id 2, :foo 1, :bar 1}) (ts/find :foo 1 :bar 1)) := #{1 2}

  (-> (ts/->ts) (ts/add {:db/id 1, :foo 1}) (ts/asc 1 :foo 2) :ave :foo) := {2 #{1}}

  (let [ts (-> (ts/->ts) (ts/add {:db/id 1, :foo 1}) (ts/add {:db/id 2, :foo 2}))]
    (count (->> ts :ave :foo vals (reduce into))) := 2
    (let [ts (ts/del ts 2)]
      (ts/->node ts 2) := nil
      (count (->> ts :ave :foo vals (reduce into))) := 1
      )))
