(ns dustin.y2023.electric-tuple3
  #?(:cljs (:require-macros dustin.y2023.electric-tuple3))
  (:require [contrib.trace :refer [trace]]
            [contrib.trace.datascript-tracer :refer [DatascriptTraceView]]
            [contrib.missionary-contrib :as mx]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric.impl.compiler :as c]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests tap with %]]
            [missionary.core :as m]))

(defmacro cons$1 [l r]
  `(binding [c/%2 ~l c/%3 ~r]
     (e/fn [Getter#] 
       (binding [c/%1 ~l c/%2 ~r]
         (new Getter#)))))

(defmacro cons$ [l r]
  `(e/fn [Getter#] 
     (binding [c/%1 ~l c/%2 ~r]
       (new Getter#))))

(e/defn Car [Data]
  (Data. (e/fn [] (trace :car/x c/%1))))

(e/defn Cdr [Data]
  (Data. (e/fn [] (trace :car/y c/%2))))

; ---

#?(:cljs (def !a (m/dfv)))

(e/defn Example []
  (e/client
    (dom/h1 (dom/text "Electric Tuple2"))
    (contrib.trace.datascript-tracer/with-defaults
      (let [a' (trace :a' (new (e/task->cp !a))) ; dom input
            z (trace :z (cons$
                          (trace :txn (e/server {:txn (new (e/task->cp (mx/slow 500 #(do a'))))}))
                          (trace :opt (e/client {:opt a'}))))]
        (dom/div (dom/text (pr-str (trace :a2' a'))))
        (dom/div (dom/text (pr-str (trace :Car (Car. z)))))
        (dom/div (dom/text (pr-str (trace :Cdr (Cdr. z))))))
      (DatascriptTraceView.))))

(comment
  (!a 1)
  )

(tests
  (with
    (e/run
      (tap ::control)
      (let [z (cons$
                (tap {:txn (new (e/task->cp (m/sleep 10 42)))})
                (tap {:opt 42}))]
        (tap (Cdr. z))
        (tap (Car. z))))
    % := ::control
    % := {:opt 42}
    % := {:opt 42}
    % := {:txn 42}
    % := {:txn 42}))