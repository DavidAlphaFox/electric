(ns dustin.y2023.electric-tuple2
  (:require [contrib.trace :refer [trace]]
            [contrib.trace.datascript-tracer :refer [DatascriptTraceView]]
            [contrib.missionary-contrib :as mx]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests tap with %]]
            [missionary.core :as m]))

(e/defn Cons$ [x y]
  (e/fn [Getter] (Getter. x y)))

(e/defn Car [Data]
  (Data. (e/fn [x y] (trace :car/x x))))

(e/defn Cdr [Data]
  (Data. (e/fn [x y] (trace :car/y y))))

; ---

#?(:cljs (def !a (m/dfv)))

(e/defn Example []
  (e/client
    (dom/h1 (dom/text "Electric Tuple2"))
    (contrib.trace.datascript-tracer/with-defaults
      (let [a' (trace :a' (new (e/task->cp !a))) ; dom input
            z (trace :z (Cons$.
                          (trace :txn (e/server {:txn (new (e/task->cp (mx/slow 500 #(do a'))))}))
                          (trace :opt (e/client {:opt a'}))))]
        (dom/div (dom/text (pr-str #_(trace :a2') a')))
        (dom/div (dom/text (pr-str #_(trace :Car) (Car. z))))
        (dom/div (dom/text (pr-str #_(trace :Cdr) (Cdr. z)))))
      (DatascriptTraceView.))))

(comment
  (!a 1)
  )

(tests
  (with
    (e/run
      (tap ::control)
      (let [z (Cons$.
                (tap {:txn (new (e/task->cp (m/sleep 10 42)))})
                (tap {:opt 42}))]
        (tap (Cdr. z))
        (tap (Car. z))
        ))
    % := ::control
    % := {:opt 42}
    % := {:opt 42}
    % := {:txn 42}
    % := {:txn 42}
    ))