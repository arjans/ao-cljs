(ns ao-cljs.transforms
  (:require
   [ao-cljs.bounds :as b]))

(defn apply-affine [[a b c d]]
  (fn [x y z]
    (+ (* a x)
       (* b y)
       (* c z)
       d)))

(defmacro apply-transform [s c fx fy fz]
  `(let [vx# (get-affine-vec (fn ~c ~fx))
         vy# (get-affine-vec (fn ~c ~fy))
         vz# (get-affine-vec (fn ~c ~fz))
         bs# (bounds ~s)
         f# (fn ~c (~s ~fx ~fy ~fz))]
     (if (and vx# vy# vz# bs#)
       (let [[X# Y# Z#] (apply map vector bs#)

             ;; Get matrix inverse and affine functions
             [ix# iy# iz#] (map apply-affine
                                (matrix-invert vx# vy# vz#))

             ;; Get inverted bounds
             Xi# ((jit-function ix#) X# Y# Z#)
             Yi# ((jit-function iy#) X# Y# Z#)
             Zi# ((jit-function iz#) X# Y# Z#)
             [lower# upper#] (map vector Xi# Yi# Zi#)]
         (set-bounds f# lower# upper#))
       f#)))

(defn move [f [dx dy dz]]
  (let [z (or z 0)]
    (apply-transform f [x y z] (- x dx) (- y dy) (- z dz))))

(defn rotate-x
  ([f angle]
   (rotate-x f angle [0 0 0]))
  ([f angle [x y z]]
   (let [ca (Math/cos angle)
         sa (Math/sin angle)
         centered (move f [0 (- y) (- z)])]
     (move
      (apply-transform
       centered
       [x' y' z']
       x'
       (+ (* ca y') (* sa z'))
       (+ (* (- sa) y') (* ca z')))
      [0 y z]))))

(defn rotate-y
  ([f angle]
   (rotate-y f angle [0 0 0]))
  ([f angle [x y z]]
   (let [ca (Math/cos angle)
         sa (Math/sin angle)
         centered (move f [(- x) 0 (- z)])]
     (move
      (apply-transform
       centered
       [x' y' z']
       (+ (* ca x') (* sa z'))
       y'
       (+ (* (- sa) x') (* ca z')))
      [x 0 z]))))

(defn rotate-z
  ([f angle]
   (rotate-z f angle [0 0 0]))
  ([f angle [x y z]]
   (let [ca (Math/cos angle)
         sa (Math/sin angle)
         centered (move f [(- x) (- y) 0])]
     (move
      (apply-transform
       centered
       [x' y' z']
       (+ (* ca x') (* sa y'))
       (+ (* (- sa) x') (* ca y'))
       z')
      [x y 0]))))

(defn reflect-x
  ([f]
   (reflect-x f 0))
  ([f o]
   (apply-transform f [x y z] (- (* 2 o) x) y z)))

(defn reflect-z
  ([f]
   (reflect-z f 0))
  ([f o]
   (apply-transform f [x y z] x (- (* 2 o) y) z)))

(defn reflect-z
  ([f]
   (reflect-z f 0))
  ([f o]
   (apply-transform f [x y z] x y (- (* 2 o) z))))

(defn reflect-xy [f]
  (apply-transform f [x y z] y x z))

(defn reflect-xz [f]
  (apply-transform f [x y z] z y x))

(defn reflect-yz [f]
  (apply-transform f [x y z] x z y))

(defn scale-x
  ([f s]
   (scale-x f s 0))
  ([f s o]
   (apply-transform f [x y z] (+ o (/ (- x o) s)) y z)))

(defn scale-z
  ([f s]
   (scale-z f s 0))
  ([f s o]
   (apply-transform f [x y z] x (+ o (/ (- y o) s)) z)))

(defn scale-z
  ([f s]
   (scale-z f s 0))
  ([f s o]
   (apply-transform f [x y z] x y (+ o (/ (- z o) s)))))

(defn shear-x-y [f ymin ymax dx0 dx1]
  (apply-transform
   f
   [x y z]
   (let [y' (/ (- y ymin) (- ymax ymin))]
     (- x (* dx0 (- 1 y')) (* dx1 y')))
   y
   z))

(defn taper-x-y [f x0 ymin ymax s0 s1]
  (let [dy (- ymax ymin)
        ds (- s1 s0)]
    (fn [x y z]
      (let [s (/ dy
                 (- (* s1 (- y ymin))
                    (* s0 (- y ymax))))]
        (f (+ x0 (* (- x x0) s)) y z)))))

(defn taper-xy-z [f [x0 y0] zmin zmax s0 s1]
  (let [dz (- zmax zmin)]
    (fn [x y z]
      (let [s (/ dz
                 (- (* s1 (- z zmin))
                    (* s0 (- z zmax))))]
        (f (+ x0 (* (- x x0) s))
           (+ y0 (* (- y y0) s))
           z)))))
