(ns user.demo-color
  #?(:cljs (:require-macros user.demo-color))
  (:require [contrib.data :refer [assoc-vec]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.router :as router]
            [contrib.color :as c]))

;; Goal is to show:
;; - fine grained reactivity on CSS properties
;; - Non trivial DOM api usage (canvas)

(def CANVAS-WIDTH 360)                  ; px
(def CANVAS-HEIGHT 100)                 ; px

(defn format-rgb [[r g b]] (str "rgb("r","g","b")"))

#?(:cljs
   (defn draw! [^js canvas colorf]
     (let [ctx (.getContext canvas "2d")]
       (loop [angle 0]
         (set! (.-strokeStyle ctx) (colorf angle))
         (.beginPath ctx)
         (.moveTo ctx angle 0)
         (.lineTo ctx angle CANVAS-HEIGHT)
         (.closePath ctx)
         (.stroke ctx)
         (when (< angle 360)
           (recur (inc angle)))))))

#?(:cljs
   (defn draw-gradient! [canvas hue colorf]
     (draw! canvas (fn [angle] (format-rgb (if (= angle hue) [255 255 255] (colorf angle)))))))

(defn saturation->chroma [saturation] (* 0.158 (/ saturation 100)))

(p/defn Tile [color]
  (let [rgb                          (format-rgb color)
        [static-class dynamic-class] (dom/css "{ background-color: $(rgb);}")]
    (dom/div (dom/props {:class [static-class dynamic-class "tile"]
                         :style {:display         :flex
                                 :align-items     :center
                                 :justify-content :center
                                 :color           :white
                                 :width           "100px"
                                 :height          "100%"
                                 }})
      (dom/text "Contrast"))))

(p/defn App []
  (p/client
    (let [[self h s l] router/route
          h (or h 180)
          s (or s 80)
          l (or l 70)
          swap-route! router/swap-route!]
      (dom/styled dom/div
        "{ display: grid; max-width: 600px }
         .tile { border: 5px gray solid; }"
        (dom/props {#_#_:class ["cls2"] ; FIXME would overwrite the class from `css/styled`
                    :style {:grid-template-columns "auto 1fr auto"
                            :gap                   "0 1rem"
                            :align-items           :center
                            :justify-items         :stretch}})
        (dom/p (dom/text "Lightness"))
        (dom/input (dom/props {:type  :range
                               :min   0
                               :max   100
                               :step  1
                               :value l})
          (dom/event "input" (fn [^js e] (swap-route! assoc-vec 3 (js/parseInt (.. e -target -value))))))
        (dom/p (dom/text l "%"))

        (dom/p (dom/text "Saturation"))
        (dom/input (dom/props {:type  :range
                               :min   0
                               :max   100
                               :step  1
                               :value s})
          (dom/event "input" (fn [^js e] (swap-route! assoc-vec 2 (js/parseInt (.. e -target -value))))))
        (dom/p (dom/text s "%"))


        (dom/p (dom/text "Hue"))
        (dom/input (dom/props {:type  :range
                               :min   0
                               :max   360
                               :step  1
                               :value h})
          (dom/event "input" (fn [^js e] (swap-route! assoc-vec 1 (js/parseInt (.. e -target -value))))))
        (dom/p (dom/text h "°"))


        (dom/p (dom/text "HSL"))
        (dom/canvas (dom/props {:width  360
                                :height 100})
          (draw-gradient! dom/node h (fn [h] (c/hsl->rgb [h s l])))
          )
        (Tile. (c/hsl->rgb [h s l]))

        (dom/p (dom/text "OKLCH"))
        (dom/canvas (dom/props {:width  360
                                :height 100})
          (draw-gradient! dom/node h (fn [h] (c/oklch->rgb [l (saturation->chroma s) h]))))
        (Tile. (c/oklch->rgb [l (saturation->chroma s) h]))

        (dom/p (dom/text "HSLuv"))
        (dom/canvas (dom/props {:width  360
                                :height 100})
          (draw-gradient! dom/node h (fn [h] (c/hsluv->rgb [h s l]))))
        (Tile. (c/hsluv->rgb [h s l]))))))
