(ns oop-2014.functional-images
  (:use seesaw.core
        seesaw.color))

(defrecord Point [x y])

(defn image-value
  [img x y]
  (img (Point. x y)))

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

(comment 
(defmacro defimg
  [?name [?x ?y] & ?body]
  `(defn ~?name
     [p#]
     (let [~?x (:x p#)
           ~?y (:y p#)]
       ~@?body)))
)

(defmacro fnimg
  [[?x ?y] & ?body]
  `(fn [p#]
     (let [~?x (:x p#)
           ~?y (:y p#)]
       ~@?body)))

(defmacro defimg
  [?name [?x ?y] & ?body]
  `(def ~?name (fnimg [~?x ~?y] ~@?body)))

(defimg vstrip
  [x y]
  (<= (Math/abs x) 0.5))

(defimg checker
  [x y]
  (even? (+ (int (Math/floor x)) (int (Math/floor y)))))

(defn distance-from-origin
  [x y]
  (Math/sqrt (+ (* x x) (* y y))))

(defimg alt-rings
  [x y]
  (even? (int (Math/floor (distance-from-origin x y)))))

(defimg to-polar
  [x y]
  (Point. (distance-from-origin x y)
          (Math/atan2 x y)))

(defn polar-checker
  [n]
  (let [sc (fnimg [r theta]
             (Point. r (* theta (/ (double n) Math/PI))))]
    (comp checker sc to-polar)))
