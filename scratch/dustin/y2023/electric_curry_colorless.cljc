(ns dustin.y2023.electric-curry-colorless)

(e/defn Foo [a b] ...)

(e/defn Bar [a-client] #_[b-server] ; curried
  (e/server
    (e/fn [b-server]
      (Foo. a-client b-server))))

(e/server
  (let [F (e/client (Bar. 1))] ; colorless e/fn
    (F. 2)))
