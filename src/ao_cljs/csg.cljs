(ns ao-cljs.csg
  (:require
   [ao-cljs.bounds :as b]))

(defn union
  ([] "Too few args.") ;; Throw error.
  ([& shapes]
   (let [bs (map b/bounds shapes)
         f (fn [x y z] (apply min (map #(% x y z) shapes)))]
     (if (every? identity bs)
       (let [[l u] (b/bounds-union bs)]
         (b/set-bounds f l u))
       f))))

(defn intersection
  ([] "Too few args.") ;; Throw error.
  ([& shapes]
   (let [bs (map b/bounds shapes)
         f (fn [x y z] (apply max (map #(% x y z) shapes)))]
     (if (every? identity bs)
       (let [[l u] (b/bounds-intersection bs)]
         (b/set-bounds f l u))
       f))))

(defn inverse [f]
  (let [f' (fn [x y z] (- (f x y z)))]
    (if (b/bounds f)
      (b/set-default-bounds f')
      f')))

(defn difference [f & fs]
  (intersection f (inverse (apply union fs))))

(defn offset [f x0]
  (let [f' (fn [x y z] (- (f x y z) x0))]
    (if (b/bounds f)
      (b/set-default-bounds f')
      f')))

(defn clearance [f g x]
  (difference f (offset g x)))

(defn blend [f g x0]
  (let [joint (union f g)
        fillet (fn [x y z]
                 (- (+ (Math/sqrt (Math/abs (f x y z)))
                       (Math/sqrt (Math/abs (g x y z))))
                    x0))]
    (union joint fillet)))

(defn morph [f g x0]
  (fn [x y z]
    (+ (* (f x y z) (- 1 x0))
       (* (g x y z) x0))))
