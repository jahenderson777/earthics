(ns earthics.util)

(defn hsl
  ([hue sat lightness]
   (hsl hue sat lightness 1))
  ([hue sat lightness alpha]
   (str "hsla(" hue ", " sat "%, " lightness "%, " alpha ")")))

(defn clamp [x min max]
  (if (< x min)
    min
    (if (> x max)
      max
      x)))
