(ns ao-cljs.shapes
  (:require))

;; Depends on: set-bounds, union, atan, sqrt
;; intersection, edge, taper-xy-z

(def pi 3.1415926)

(defn square [x] (* x x))

(defn average [xs] (/ (apply + xs) (count xs)))

(defn distance [xs] (Math/sqrt (reduce + (map square xs))))

(defn circle [[x y] r]
  (move
    (set-bounds
      (fn [x y z]
        (- (distance [x y]) r)
      [(- r) (- r)]
      [r r])
    [x y]))

(defn rectangle [[xa ya] [xb yb]]
  (let [xmin (min xa xb)
        xmax (max xa xb)
        ymin (min ya yb)
        ymax (max ya yb)]
    (set-bounds
      (fn [x y z]
        (max (- xmin x) (- x xmax) (- ymin y) (- y ymax)))
      [xmin ymin]
      [xmax ymax])))

(defn rounded-rectangle [[xa ya] [xb yb] r]
  (let [xmin (min xa xb)
        xmax (max xa xb)
        ymin (min ya yb)
        ymax (max ya yb)
        r (* r (min (- xmax xmin) (- ymax ymin)) 0.5)]
    (union
      (rectangle [xmin (+ ymin r)] [xmax (- ymax r)])
      (rectangle [(+ xmin r) ymin] [(- xmax r) ymax])
      (circle [(+ xmin r) (+ ymin r)] r)
      (circle [(+ xmin r) (- ymax r)] r)
      (circle [(- xmax r) (+ ymin r)] r)
      (circle [(- xmax r) (- ymax r)] r))))

(defn triangle [[xa ya :as a] [xb yb :as b] [xc yc :as c]]
  (defn edge [x' y' dx dy]
    (fn [x y z]
      (- (* dy (- x x')) (* dx (- y y')))))
  (let [xm (average [xa xb xc])
        ym (average [ya yb yc])
        angle (fn [[x y]] (Math/atan (- x xm) (- y ym)))
        [ta tb tc] (map angle [a b c])
        sorted (cond
                (and (< tb ta) (< tb tc)) [tb tc ta]
                (and (< tc ta) (< tc tb)) [tc ta tb]
                :else                     [ta tb tc])
        clockwise (if (> (nth sorted 2) (nth sorted 1))
                      [a c b]
                      [a b c])
        [[xa ya] [xb yb] [xc yc]] clockwise]
    (set-bounds
      (intersection
        (edge xc yc (- xa xc) (- ya yc))
        (edge xb yb (- xc xb) (- yc yb))
        (edge xa ya (- xb xa) (- yb ya)))
      [(min xa xb xc) (min ya yb yc)]
      [(max xa xb xc) (max xa xb xc)])))

;; 2D -> 3D functions
(defn extrude-z [shape z z']
  (let [zmin (min z z')
        zmax (max z z')
        bs (bounds shape)
        f (fn [x y z] (max (shape x y z) (- zmin z) (- z zmax)))]
    (if bs
      (let [[lower upper] bs]
        (set-bounds
          f
          (conj lower zmin)
          (conj upper zmax))
        f))))

;; 3D shapes
(defn sphere [[x y z :as c] r]
  (move
    (set-bounds
      (fn [x y z]
        (- (distance [x y z])
           r))
      [(- r) (- r) (- r)]
      [r r r])
    c))

(defn cube [x y z :as a] [x' y' z' :as b]
  (extrude-z (rectangle a b) x x'))

(defn cylinder-z [[x y z :as c] r h]
  (extrude-z (circle c r) z (+ z h)))

(defn cone-z [[x y z :as c] r h]
  (taper-xy-z
    (cylinder-z c r h)
    c
    z (+ z h) 1 0))

(defn pyramid-z [[x y :as a] [x' y' :as b] z h]
  (taper-xy-z
    (extrude-z (rectangle a b) z (+ z h))
    [(average x x') (average y y')]
    z (+ z h) 1 0))

(defn torus-z [[x y z :as c] R r]
  (move
    (set-bounds
      (fn [x y z]
        (- (distance (- R (distance x y) z) r)))
      [(- (+ R r)) (- (+ R r)) (- r)]
      [(+ R r) (+ R r) r])
    c))

(defn array-2d [shape i j dx dy]
  (let [xs (map #(* % dx) (range i))
        ys (map #(* % dy) (range j))
        row (apply union (map #(move shape [% 0 0]) xs))]
    (apply union (map #(move row [0 % 0]) ys))))

(defn array-3d [shape i j k dx dy dz]
  (let [zs (map #(* % dz) (range k))
        plane (array-2d shape i j dx dy)]
    (apply union (map #(move plane [0 0 %]) zs))))

(defn array-polar
  ([shape n
    (array-polar shape n [0 0])])
  ([shape n [x y :as c]
    (let [xs (map #(* 2 pi (/ % n)) (range n))]
      (apply union (map #(rotate-z shape % c) xs)))]))
