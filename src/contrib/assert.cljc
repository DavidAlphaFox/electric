(ns contrib.assert
  #?(:cljs (:require-macros contrib.assert))
  (:require [hyperfiddle.rcf :refer [tests]]))

(defmacro check
  ([v] `(check some? ~v))
  ([pred v] `(check ~pred ~v {}))
  ([pred v ex-data]
   `(let [pred# ~pred, v# ~v]
      (when-not (pred# v#) (throw (ex-info (str "check failed: (" (pr-str '~pred) " " (pr-str '~v) ") for " (pr-str v#)) ~ex-data)))
      v#)))

(tests
  (check nil) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check odd? 2) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check odd? 1) := 1)
