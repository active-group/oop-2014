(ns oop-2014.functional-images
  (:use seesaw.core
        seesaw.color))

(defn image-value
  [img x y]
  (img [x y]))

(defn c
  [v]
  (int (* v 255.0)))

(defn draw-image!
  [img
   gr width height
   x-min x-max y-min y-max]
  (let [xinc (/ (- x-max x-min) (double width))
        yinc (/ (- y-max y-min) (double width))]
    (doseq [x (range width)
            y (range height)]
      (let [v (image-value img (+ x-min (* xinc x)) (+ y-min (* yinc y)))
            col (cond
                 (true? v) java.awt.Color/BLACK
                 (false? v) java.awt.Color/WHITE
                 (number? v) (let [b (int (* v 255.0))] (color b b b))
                 (vector? v) (let [[b g r a] v] (color (c r) (c g) (c b) (c a))))]
        (.setColor gr col)
        (.fillRect gr x y 1 1)))))

(defn check-image!
  [img
   width height
   x-min x-max y-min y-max]
  (let [xinc (/ (- x-max x-min) (double width))
        yinc (/ (- y-max y-min) (double width))]
    (doseq [x (range width)
            y (range height)]
      (let [v (image-value img (+ x-min (* xinc x)) (+ y-min (* yinc y)))
            col (cond
                 (true? v) java.awt.Color/BLACK
                 (false? v) java.awt.Color/WHITE
                 (number? v) (let [b (c v)] (color b b b))
                 (vector? v) (let [[b g r a] v] (color (c r) (c g) (c b) (c a))))]
        (println "point: " x y (+ x-min (* xinc x)) (+ y-min (* yinc y)) v col)))))

(defn display-image!
  [img width height x-min x-max y-min y-max]
  (let [c (canvas :paint 
                  (fn [c g]
                    (draw-image! img g width height x-min x-max y-min y-max)))
        f (frame :width width :height height
                 :title "Functional Image"
                 :content c)]
    (show! f)))

(defn write-image!
  [filename img width height x-min x-max y-min y-max]
  (let [bi (java.awt.image.BufferedImage. width height java.awt.image.BufferedImage/TYPE_INT_ARGB)]
    (draw-image! img (.getGraphics bi) width height x-min x-max y-min y-max)
    (javax.imageio.ImageIO/write bi "png" (java.io.File. filename))))

(defn vstrip
  [[x y]]
  (<= (Math/abs x) 0.5))

(defn checker
  [[x y]]
  (even? (+ (int (Math/floor x)) (int (Math/floor y)))))

(defn distance-from-origin
  [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn alt-rings
  [p]
  (even? (int (Math/floor (distance-from-origin p)))))

(defn from-polar
  [[r theta]]
  [(* r (Math/cos theta))
   (* r (Math/sin theta))])

(defn to-polar
  [[x y]]
  [(distance-from-origin [x y])
   (Math/atan2 x y)])

(defn polar-checker
  [n]
  (let [sc (fn [[r theta]]
             [r (* theta (/ (double n) Math/PI))])]
    (comp checker sc to-polar)))

(defn wavy-distance
  [p]
  (/ (+ 1.0 (Math/cos (* Math/PI (distance-from-origin p)))) 2.0))

(def invisible [0.0 0.0 0.0 0.0])
(def black [0.0 0.0 0.0 1.0])
(def white [1.0 1.0 1.0 1.0])
(def red [0.0 0.0 1.0 1.0])
(def green [0.0 1.0 0.0 1.0])
(def blue [1.0 0.0 0.0 1.0])
(def yellow [0.0 1.0 1.0 1.0])

(defn lerp-colors
  [w [b1 g1 r1 a1] [b2 g2 r2 a2]]
  (let [h (fn [x1 x2]
            (+ (* w x1) (* (- 1 w) x2)))]
    [(h b1 b2) (h g1 g2) (h r1 r2) (h a1 a2)]))

(defn color-overlay
  [[b1 g1 r1 a1] [b2 g2 r2 a2]]
  (let [h (fn [x1 x2]
            (+ x1 (* (- 1 a1) x2)))]
    [(h b1 b2) (h g1 g2) (h r1 r2) (h a1 a2)]))

(defn over
  [top bot]
  (fn [p]
    (color-overlay (top p) (bot p))))

(defn lift
  [f]
  (fn [& imgs]
    (fn [p]
      (apply f (map (fn [img] (img p)) imgs)))))

(defn lift1
  [f]
  (fn [img]
    (fn [p]
      (f (img p)))))

(defn lift2
  [f]
  (fn [img1 img2]
    (fn [p]
      (f (img1 p) (img2 p)))))

(def cond-image
  (lift (fn [a b c] (if a b c))))

(def lerp-images
  (lift lerp-colors))

(def empty (constantly invisible))
(def white-image (constantly white))
(def black-image (constantly black))
(def blue-image (constantly blue))
(def yellow-image (constantly yellow))

(def yellow-blue-rings
  (lerp-images wavy-distance blue-image yellow-image))

; Translations

(defn translate-point
  [dx dy [x y]]
  [(+ x dx) (+ y dy)])

(defn scale-point
  [sx sy [x y]]
  [(* sx x) (* sy y)])

(defn uscale-point
  [s p]
  (scale-point s s p))

(defn rotate-point
  [theta [x y]]
  [(- (* x (Math/cos theta))
      (* y (Math/sin theta)))
   (+ (* y (Math/cos theta))
      (* x (Math/sin theta)))])

(defn udisk
  [p]
  (< (distance-from-origin p) 1.0))

(defn translate
  [dx dy img]
  (comp img (partial translate-point (- dx) (- dy))))

(defn scale
  [sx sy img]
  (comp img (partial scale-point (/ 1.0 sx) (/ 1.0 sy))))

(defn uscale
  [s img]
  (comp img (partial uscale-point (/ 1.0 s))))

(defn rotate
  [theta img]
  (comp img (partial rotate-point (- theta))))

(defn swirl-point
  [r p]
  (rotate-point (* (distance-from-origin p) (/ (* 2 Math/PI) r)) p))

(defn swirl
  [r img]
  (comp img (partial swirl-point (- r))))

; Regions

(def universe-region (constantly true))

(def empty-region (constantly false))

(def complement-region (lift1 not))

(defn intersect-regions-simple
  [img1 img2]
  (fn [p]
    (and (img1 p) (img2 p))))

(def intersect-regions (lift2 (fn [a b] (and a b))))

(defn union-regions-simple
  [img1 img2]
  (fn [p]
    (or (img1 p) (img2 p))))

(def union-regions (lift2 (fn [a b] (or a b))))

(defn xor-regions-simple
  [img1 img2]
  (fn [p]
    (not= (img1 p) (img2 p))))

(def xor-regions (lift2 not=))

(defn difference-regions
  [r1 r2]
  (intersect-regions r1
                     (complement-region r2)))


(defn annulus
  [inner]
  (difference-regions udisk
                      (uscale inner udisk)))

(defn rad-reg
  [n]
  (let [test (fn [[_ theta]]
               (even? (int (Math/floor (* theta (/ (double n) Math/PI))))))]
    (comp test to-polar)))

(defn wedge-annulus
  [inner n]
  (intersect-regions (annulus inner)
                     (rad-reg n)))

(defn shift-xor
  [r img]
  (let [tr (fn [d]
           (translate d 0 img))]
    (xor-regions (tr r)
                 (tr (- r)))))


(defn from-polar-transformation
  [xf]
  (comp from-polar xf to-polar))

(def rad-invert-polar
  (from-polar-transformation (fn [[r theta]] 
                               [(if (zero? r)
                                  0.0
                                  (/ 1.0 r))
                                theta])))

(defn rad-invert
  [img]
  (comp img rad-invert-polar))

(defn polar-checker2
  [n]
  (xor-regions alt-rings (rad-reg n)))

  