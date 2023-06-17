(ns hyperfiddle.stage
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests tap % with]]))

(tests
  (def edits [[::hf/dirty {:task/status :done} [[:db/add '. :task/status :done]]]
              [::hf/dirty {:task/description "feed baby"} [[:db/add '. :task/description "feed baby"]]]
              [::hf/pending {} [[:db/add]]]
              [::hf/synced {} nil]
              [::hf/failed {} [[:db/add]] "rejected"]])
  (transpose edits)
  := [[::hf/dirty ::hf/dirty ::hf/pending ::hf/synced ::hf/failed]
      [#:task{:status :done} #:task{:description "feed baby"} {} {} {}]
      [[[:db/add '. :task/status :done]]
       [[:db/add '. :task/description "feed baby"]]
       [[:db/add]] nil [[:db/add]]]
      [nil nil nil nil "rejected"]])

(defn squash-edits [edits]
  (let [[statuses xs dxs errs] (transpose edits)]
    (assert (every? #(= (first statuses) %) statuses)  (str "can't squash-edits: " edits))
    [(apply merge xs)
     (vec (apply concat dxs))
     (seq (remove nil? errs))]))

(tests
  (squash-edits edits)
  := [#:task{:status :done, :description "feed baby"}
      [[:db/add '. :task/status :done]
       [:db/add '. :task/description "feed baby"]
       [:db/add] [:db/add]]
      ["rejected"]])

(tests
  (squash-edits
    [[::hf/dirty #:task{:status :done} [[:db/add '. :task/status :done]]]
     [::hf/dirty #:task{:description "feed baby"} [[:db/add '. :task/description "feed baby"]]]])
  := [#:task{:status :done, :description "feed baby"}
      [[:db/add '. :task/status :done] [:db/add '. :task/description "feed baby"]] nil])

(tests
  (group-by first edits)
  := {::hf/dirty [[::hf/dirty #:task{:status :done} [[:db/add '. :task/status :done]]]
                  [::hf/dirty #:task{:description "feed baby"} [[:db/add '. :task/description "feed baby"]]]],
      ::hf/pending [[::hf/pending {} [[:db/add]]]],
      ::hf/synced [[::hf/synced {} nil]],
      ::hf/failed [[::hf/failed {} [[:db/add]] "rejected"]]})

(defn aggregate-edits [edits]
  (-> (group-by first edits)
    (update-vals squash-edits)))

(tests
  (aggregate-edits edits)
  := {::hf/dirty [#:task{:status :done, :description "feed baby"}
                  [[:db/add '. :task/status :done]
                   [:db/add '. :task/description "feed baby"]]
                  nil],
      ::hf/pending [{} [[:db/add]] nil],
      ::hf/synced [{} [] nil],
      ::hf/failed [{} [[:db/add]] ["rejected"]]})
