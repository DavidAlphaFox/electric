(ns contrib.debug
  #?(:cljs (:require-macros contrib.debug))
  (:require [clojure.string :as str])
  (:import #?(:clj [clojure.lang IFn IDeref])
           [hyperfiddle.electric Failure]))

(def ^:dynamic *dbg* true)

(defmacro dbg
  ([form] `(dbg '~form ~form))
  ([label form]
   (let [[label form] (if (keyword? form) [form label] [label form])]
     `(if *dbg*
        (let [[st# v#] (try [:ok ~form] (catch ~(if (:js-globals &env) :default 'Throwable) ex# [:ex ex#]))]
          (prn ~label '~'==> v#)
          (if (= st# :ok) v# (throw v#)))
        ~form))))

(defmacro dbg-when [form & body] `(binding [*dbg* ~form] ~@body))

(defmacro dbgv [form]
  `(if *dbg*
     (let [args# [~@form], v# ~form] (prn '~form '~'==> (cons '~(first form) (rest args#))  '~'==> v#) v#)
     ~form))

(defmacro dbgc [[op & args :as form]]
  `(let [op# ~op, args# ~args, ret# (apply op# args#)]
     (prn '~form)
     (doseq [[form# arg#] (map vector '~args args#)]
       (prn '~'_ form# '~'==> arg#))
     (prn '~'==> ret#)
     ret#))

(defmacro do-traced [& body] `(do ~@(for [form body] `(dbg ~form))))

(defn ->nprn [n]
  (let [prns (long-array [0])]
    (fn [& args]
      (when (< (aget prns (int 0)) n)
        (aset prns 0 (unchecked-inc (aget prns (int 0))))
        (apply prn args)))))

(def !id (atom 0))

(defn instrument* [nm flow]
  (fn [n t]
    (let [id (swap! !id inc)
          it (flow #(do (prn nm id :notified) (n)) #(do (prn nm id :terminated) (t)))]
      (reify
        IFn (#?(:clj invoke :cljs -invoke) [_] (prn nm id :cancelled) (it))
        IDeref (#?(:clj deref :cljs -deref) [_]
                 (let [v (try @it (catch #?(:clj Throwable :cljs :default) e [::ex e]))]
                   (prn nm id :transferred
                     (if (instance? Failure v)
                       (let [e (.-error v)]
                         [(type e) (ex-message e)])
                       v))
                   (if (and (vector? v) (= ::ex (first v)))
                     (throw (second v))
                     v)))))))
(defmacro instrument [nm & body] `(new (instrument* ~nm (hyperfiddle.electric/fn [] ~@body))))

(defmacro js-measure [nm & body]
  (if (:js-globals &env)
    (let [st (str nm "-start"), fn (str nm "-end")]
      `(let [_# (js/performance.mark ~st)
             ret# (do ~@body)]
         (js/performance.mark ~fn)
         (js/performance.measure ~nm ~st ~fn)
         ret#))
    `(do ~@body)))
