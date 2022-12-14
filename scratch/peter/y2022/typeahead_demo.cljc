(ns peter.y2022.typeahead-demo
  (:require
   [clojure.string :as str]
   [peter.y2022.typeahead :as tphd]
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui2]
   [missionary.core :as m])
  #?(:cljs (:require-macros peter.y2022.typeahead-demo)))

(p/defn ValueLog [v] (let [!log (atom []), log (p/watch !log)] (swap! !log conj v) (dom/br) (dom/div (str log))))

#?(:clj (def -data {:alice "Alice B", :bob "Bob C", :charlie "Charlie D", :derek "Derek B"}))
#?(:clj (defn q [search]
          (into [] (comp (filter #(or (empty? search) (str/includes? (second %) search))) (map first)) -data)))

#?(:clj (defn random-person [prev] (m/sp (m/? (m/sleep 500)) (rand-nth (-> -data keys set (disj prev) vec)))))

(p/defn App []
  (p/client
    (dom/h1 "Typeahead demo")
    (dom/element "style" "
.hf-typeahead-picklist-item > *:hover {
    background-color: gray;
}
")
    (dom/div {:style {:display "flex" :flex-flow "row wrap" :justify-content "center" :width "200px" :gap "20px"}}
      (dom/div
        (do
          (ValueLog.
            (tphd/typeahead :alice
              (p/fn [search]
                (p/server
                  (p/for [e (q search)]
                    (p/client
                      (tphd/typeahead-item e (dom/div (dom/text (p/server (get -data e)))))))))
              (p/fn [e] (p/server (get -data e)))
              (dom/props {:placeholder "Pick a person!"})))
          #_(p/with-cycle [v :alice]
            (tphd/typeahead v
              (p/fn [search]
                (prn :seach search)
                (p/server
                  (p/for [e (q search)]
                    (p/client
                      (tphd/typeahead-item e (dom/div (dom/text (p/server (get -data e)))))))))
              (p/fn [e] (p/server (get -data e)))
              (dom/props {:placeholder "Pick a person!"}))
            (p/server (new (p/task->cp (random-person v)))))
          #_(p/with-cycle [s "hi"]
            (ui2/input s)
            (new (p/task->cp (m/sleep 2000 (str s ".")))))
          #_(ValueLog.
            (tphd/typeahead (p/fn [search]
                              (p/server
                                (p/for [e (q search)]
                                  (p/client
                                    (tphd/typeahead-item e (dom/div (dom/text (p/server (get -data e)))))))))
              (p/fn [e] (p/server (get -data e)))
              (dom/props {:placeholder "Pick a person!"})))
          nil)))
    ))
