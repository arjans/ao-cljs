(ns ao-cljs.bounds
  (:require))

;; This generally seems hacky.
;; Re-write with maps?

(defn set-bounds [f [x y z] [x' y' z']]
  (let [z (or z -Infinity)
        z' (or z' +Infinity)]
    (fn [& args]
      (if (and (= 1 (count args))
               (= (nth args 0) :bounds))
        [[x y z] [x' y' z']]
        (apply f args)))))

(defn set-default-bounds [f]
  (set-bounds f
              [-Infinity -Infinity -Infinity]
              [+Infinity +Infinity +Infinity]))

;; Not sure this works for all f.
(defn bounds [f]
  (try
    (f :bounds)
    (catch js/Object e false)))

(defn bounds-union [bs]
  (let [lower (map first bs)
        upper (map second bs)
        xmin (apply min (map first lower))
        ymin (apply min (map second lower))
        zmin (apply min (map last lower))
        xmax (apply max (map first upper))
        ymax (apply max (map second upper))
        zmax (apply max (map last upper))]
    [[xmin ymin zmin] [xmax ymax zmax]]))

(defn bounds-intersection [bs]
  (let [lower (map first bs)
        upper (map second bs)
        xmin (apply max (map first lower))
        ymin (apply max (map second lower))
        zmin (apply max (map last lower))
        xmax (apply min (map first upper))
        ymax (apply min (map second upper))
        zmax (apply min (map last upper))]
    [[xmin ymin zmin] [xmax ymax zmax]]))
