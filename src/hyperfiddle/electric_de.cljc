(ns hyperfiddle.electric-de
  (:refer-clojure :exclude [fn defn apply letfn])
  (:require [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.incseq :as i]
            [hyperfiddle.electric.impl.mount-point :as mp]
            [clojure.core :as cc]
            [clojure.string :as str]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            #?(:clj [contrib.triple-store :as ts])
            #?(:clj [fipp.edn])
            [missionary.core :as m])
  #?(:cljs (:require-macros hyperfiddle.electric-de)))

(def web-config {::lang/peers {:client :cljs, :server :clj}})

#?(:clj (cc/defn dget [v] `(::lang/lookup ~v)))
#?(:clj (cc/defn ->pos-args [n] (eduction (take n) (map dget) (range))))

(defmacro check-electric [fn form]
  (if (::lang/electric &env)
    form
    (throw (ex-info (str "Electric code (" fn ") inside a Clojure function") (into {:electric-fn fn} (meta &form))))))

(defmacro ctor [expr] `(::lang/ctor ~expr))
(defmacro sited-ctor [expr] `(::lang/ctor (::lang/site ~(::lang/current &env) ~expr)))
(defmacro $ [F & args] `(check-electric $ (lang/$ ~F ~@args)))

(defmacro frame []
  `(::lang/pure (::lang/frame)))

(defmacro pure "
Syntax :
```clojure
(pure table)
```
Returns the incremental sequence describing `table`.
" [expr] `((::lang/static-vars r/incseq) (frame) (::lang/pure ~expr)))

(defmacro join "
Syntax :
```clojure
(join incseq)
```
Returns the successive states of items described by `incseq`.
" [flow] `(::lang/join ~flow))

#?(:clj (cc/defn- varargs? [args] (boolean (and (seq args) (= '& (-> args pop peek))))))

#?(:clj (cc/defn- throw-arity-conflict! [?name group]
          (throw (ex-info (str "Conflicting arity definitions" (when ?name (str " in " ?name)) ": "
                            (str/join " and " group))
                   {:name ?name}))))

#?(:clj (cc/defn- check-only-one-vararg! [?name varargs]
          (when (> (count varargs) 1)
            (throw-arity-conflict! ?name varargs))))

#?(:clj (cc/defn- check-arity-conflicts! [?name positionals vararg]
          (let [grouped (group-by count positionals)]
            (doseq [[_ group] grouped]
              (when (> (count group) 1)
                (throw-arity-conflict! ?name group)))
            (when-some [same (get grouped (-> vararg count dec))]
              (throw-arity-conflict! ?name (conj same vararg))))))

(defmacro -fn [& args]
  (let [[?name args2] (if (symbol? (first args)) [(first args) (rest args)] [nil args])
        arities (cond-> args2 (vector? (first args2)) list)
        {positionals false, varargs true} (group-by (comp varargs? first) arities)
        _ (check-only-one-vararg! ?name (mapv first varargs))
        _ (check-arity-conflicts! ?name (mapv first positionals) (ffirst varargs))]
    (into (if-some [[args & body] (first varargs)]
            (let [fixed (-> args pop pop)]
              {-1 [(count fixed)
                   (map? (peek args))
                   `(::lang/ctor
                      (let [~@(interleave fixed (map dget (range)))
                            ~(peek args) ~(dget (count fixed))] ~@body))]}) {})
      (map (cc/fn [[args & body]]
             [(count args)
              `(::lang/ctor
                (let [~@(interleave args (map dget (range)))]
                  ~@body))])) positionals)))

(defmacro fn [& args]
  (let [?nm (first args)]
    `(check-electric fn
       ~(if (symbol? ?nm) `(::lang/mklocal ~?nm (::lang/bindlocal ~?nm (-fn ~@args) ~?nm)) `(-fn ~@args)))))

(cc/defn ns-qualify [sym] (if (namespace sym) sym (symbol (str *ns*) (str sym))))

#?(:clj (tests
          (ns-qualify 'foo) := `foo
          (ns-qualify 'a/b) := 'a/b))

(defmacro defn [nm & fdecl]
  (let [[_defn sym] (macroexpand `(cc/defn ~nm ~@fdecl))
        env (merge (meta nm) (lang/normalize-env &env) web-config {::lang/def nm})
        nm2 (vary-meta nm merge (meta sym) {::lang/node true})
        source (lang/->source env (-> nm ns-qualify keyword)
                      `(-fn ~nm2 ~@(cond-> fdecl (string? (first fdecl)) next)))]
    (when-not (::lang/has-edef? (meta *ns*)) (alter-meta! *ns* assoc ::lang/has-edef? true))
    `(def ~nm2 ~source)))

(defmacro amb "
Syntax :
```clojure
(amb table1 table2 ,,, tableN)
```
Returns the concatenation of `table1 table2 ,,, tableN`.
" [& exprs] `(::lang/call (join (i/fixed ~@(map #(list `r/invariant (list `sited-ctor %)) exprs)))))

(defmacro input "
Syntax :
```clojure
(input cf)
```
Returns the current state of current continuous flow `cf`.
" [& flows] `(join (r/fixed-signals ~@flows)))

(defmacro watch "
Syntax :
```clojure
(watch !ref)
```
Returns the current state of current reference `!ref`.
" [ref] `(check-electric watch (input (m/watch ~ref))))

(defmacro diff-by "
Syntax :
```clojure
(diff-by kf xs)
```
Stabilizes successive states of collection `xs` with function `kf`. Returns each item as a table.
" [f xs] `(join (i/diff-by ~f (join (i/items (pure ~xs))))))

(defmacro drain "
Syntax :
```clojure
(drain expr)
```
Samples and discards `expr` synchronously with changes. Returns nothing.
" [expr] `(join (r/drain (pure ~expr))))

(defmacro client [& body] `(check-electric client (::lang/site :client ~@body)))
(defmacro server [& body] `(check-electric server (::lang/site :server ~@body)))

(defmacro cursor "
Syntax :
```clojure
(cursor [sym1 table1
         sym2 table2
         ,,,  ,,,
         symN tableN]
  & body)
```
For each tuple in the cartesian product of `table1 table2 ,,, tableN`, calls body in an implicit `do` with symbols
`sym1 sym2 ,,, symN` bound to the singleton tables for this tuple. Returns the concatenation of all body results.
" [bindings & body]
  (case bindings
    [] `(do ~@body)
    (let [[args exprs] (cc/apply map vector (partition-all 2 bindings))]
      `(::lang/call
        (r/bind-args (ctor (let [~@(interleave args (->pos-args (count args)))] ~@body))
          ~@(map (clojure.core/fn [expr]
                   `(r/effect (r/fixed-signals (join (i/items (pure ~expr))))))
              exprs))))))

(defmacro as-vec "
Syntax :
```clojure
(as-vec table)
```
Returns a single vector containing elements of `table`.
" [expr] `(input (m/reductions i/patch-vec (pure ~expr))))

(defmacro for-by "
Syntax :
```clojure
(for-by kf [sym1 coll1
            sym2 coll2
            ,,,  ,,,
            symN collN]
  & body)
```
Stabilizes successives states of `coll1 coll2 ,,, collN` with function `kf`. For each tuple in the cartesian product of
resulting tables, calls body in an implicit `do` with symbols `sym1 sym2 ,,, symN` bound to the singleton tables for
this tuple. Returns the concatenation of all body results as a single vector.
" [kf bindings & body]
  `(as-vec
     ~((clojure.core/fn rec [bindings]
         (if-some [[sym expr & bindings] bindings]
           `(cursor [~sym (diff-by ~kf ~expr)]
              ~(rec bindings)) `(do ~@body)))
       (seq bindings))))

(defmacro letfn [bs & body]
  (let [sb (reverse bs)]
    (reduce (cc/fn [ac [nm]] `(::lang/mklocal ~nm ~ac))
      (reduce (cc/fn [ac [nm & fargs]] `(::lang/bindlocal ~nm (hyperfiddle.electric-de/fn ~@fargs) ~ac)) (cons 'do body) sb)
      sb)))

(defmacro tag [] `(::lang/tag))
(def mount-point mp/create)

(hyperfiddle.electric-de/defn Dispatch [F static args]
  (let [offset (count static)
        arity (+ offset (count args))]
    (if-some [ctor (F arity)]
      (loop [args args
             static static]
        (if (< (count static) arity)
          (recur (next args) (conj static (::lang/pure (first args))))
          (cc/apply r/bind-args (r/bind-self ctor) static)))
      (let [[fixed map? ctor] (r/get-variadic "apply" F arity)]
        (if (< fixed offset)
          (loop [args args
                 static static]
            (let [args (cons (::lang/join (r/incseq (frame) (peek static))) args)
                  static (pop static)]
              (if (< fixed (count static))
                (recur args static)
                (cc/apply r/bind-args (r/bind (r/bind-self ctor) fixed (::lang/pure (cc/apply (r/varargs map?) args))) static))))
          (loop [args args
                 static static]
            (if (< (count static) fixed)
              (recur (next args) (conj static (::lang/pure (first args))))
              (cc/apply r/bind-args (r/bind (r/bind-self ctor) fixed (::lang/pure (cc/apply (r/varargs map?) args))) static))))))))

(hyperfiddle.electric-de/defn Apply
  ([F a]
   (::lang/call ($ Dispatch F [] a)))
  ([F a b]
   (::lang/call ($ Dispatch F [(::lang/pure a)] b)))
  ([F a b c]
   (::lang/call ($ Dispatch F [(::lang/pure a) (::lang/pure b)] c)))
  ([F a b c d]
   (::lang/call ($ Dispatch F [(::lang/pure a) (::lang/pure b) (::lang/pure c)] d)))
  ([F a b c d & es]
   (::lang/call ($ Dispatch F [(::lang/pure a) (::lang/pure b) (::lang/pure c) (::lang/pure d)] (concat (butlast es) (last es))))))

(cc/defn on-unmount* [f] (m/observe (cc/fn [!] (! nil) f)))

(defmacro on-unmount "Run clojure(script) thunk `f` during unmount.

  Standard electric code runs on mount, therefore there is no `on-mount`."
  [f] `(input (on-unmount* ~f))) ; experimental

(defmacro boot-server [opts Main & args]
  (let [env (merge (lang/normalize-env &env) web-config opts)
        source (lang/->source env ::Main `(e/fn [] ($ ~Main ~@args)))]
    `(cc/fn [events#]
       (m/stream (r/peer events# :server (r/->defs {::Main ~source}) ::Main)))))

(defmacro boot-client [opts Main & args]
  (let [env (merge (lang/normalize-env &env) web-config opts)
        source (lang/->source env ::Main `(e/fn [] ($ ~Main ~@args)))]
    `(hyperfiddle.electric-client-de/reload-when-stale
       (hyperfiddle.electric-client-de/boot-with-retry
         (cc/fn [events#] (m/stream (r/peer events# :client (r/->defs {::Main ~source}) ::Main)))
         hyperfiddle.electric-client-de/connector))))
