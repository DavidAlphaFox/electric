(ns hyperfiddle.stage
  (:require [hyperfiddle.rcf :refer [tests tap % with]]))

(tests
  (def edits [[::dirty {:task/status :done} [[:db/add '. :task/status :done]]]
              [::dirty {:task/description "feed baby"} [[:db/add '. :task/description "feed baby"]]]
              [::pending {} [[:db/add]]]
              [::synced {} nil]
              [::failed {} [[:db/add]] "rejected"]])
  (transpose edits)
  := [[::dirty ::dirty ::pending ::synced ::failed]
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
    [[::dirty #:task{:status :done} [[:db/add '. :task/status :done]]]
     [::dirty #:task{:description "feed baby"} [[:db/add '. :task/description "feed baby"]]]])
  := [#:task{:status :done, :description "feed baby"}
      [[:db/add '. :task/status :done] [:db/add '. :task/description "feed baby"]] nil])

(tests
  (group-by first edits)
  := {::dirty [[::dirty #:task{:status :done} [[:db/add '. :task/status :done]]]
               [::dirty #:task{:description "feed baby"} [[:db/add '. :task/description "feed baby"]]]],
      ::pending [[::pending {} [[:db/add]]]],
      ::synced [[::synced {} nil]],
      ::failed [[::failed {} [[:db/add]] "rejected"]]})

(defn aggregate-edits [edits]
  (-> (group-by first edits)
    (update-vals squash-edits)))

(tests
  (aggregate-edits edits)
  := {::dirty [#:task{:status :done, :description "feed baby"}
               [[:db/add '. :task/status :done]
                [:db/add '. :task/description "feed baby"]]
               nil],
      ::pending [{} [[:db/add]] nil],
      ::synced [{} [] nil],
      ::failed [{} [[:db/add]] ["rejected"]]})
