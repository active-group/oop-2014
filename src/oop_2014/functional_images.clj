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
        yinc (/ (- y-max y-min) (double height))]
    (doseq [x (range width)
            y (range height)]
      (let [v (image-value img (+ x-min (* xinc x)) (+ y-min (* yinc y)))
            col (if v
                  java.awt.Color/BLACK
                  java.awt.Color/WHITE)]
        (.setColor gr col)
        (.fillRect gr x y 1 1)))))

(defn img->bitmap 
  [img width height x-min x-max y-min y-max]
  (let [bi (java.awt.image.BufferedImage. width height java.awt.image.BufferedImage/TYPE_INT_ARGB)]
    (draw-image! img (.getGraphics bi) width height x-min x-max y-min y-max)
    bi))

(defn display-image!
  [img width height x-min x-max y-min y-max]
  (let [bi (img->bitmap img width height x-min x-max y-min y-max)
        c (canvas :paint 
                  (fn [c g]
                    (.drawImage g bi 0 0 java.awt.Color/WHITE nil)))
        f (frame :width width :height height
                 :title "Functional Image"
                 :content c)]
    (show! f)))

(defn write-image!
  [filename img width height x-min x-max y-min y-max]
  (let [bi (img->bitmap img width height x-min x-max y-min y-max)]
    (javax.imageio.ImageIO/write bi "png" (java.io.File. filename))))

(defn check-image!
  [img
   width height
   x-min x-max y-min y-max]
  (let [xinc (/ (- x-max x-min) (double width))
        yinc (/ (- y-max y-min) (double height))]
    (doseq [x (range width)
            y (range height)]
      (let [v (image-value img (+ x-min (* xinc x)) (+ y-min (* yinc y)))]
        (println "point: " x y (+ x-min (* xinc x)) (+ y-min (* yinc y)) v)))))

(comment 
(defmacro defimg
  [?name [?x ?y] ?body]
  `(defn ~?name
     [p#]
     (let [~?x (:x p#)
           ~?y (:y p#)]
       ~?body)))
)

(defmacro fnimg
  [[?x ?y] ?body]
  `(fn ^Boolean [^Point p#]
     (let [~?x (:x p#)
           ~?y (:y p#)]
       ~?body)))

(defmacro defimg
  [?name [?x ?y] ?body]
  `(def ~?name (fnimg [~?x ~?y] ~?body)))

(defn vstrip
  [p]
  (let [x (:x p)
        y (:y p)]
    (<= (Math/abs x) 0.5)))

(defn vstripes
  [p]
  (let [x (:x p)
        y (:y p)]
    (even? (int (Math/floor x)))))

(defn hstripes
  [p]
  (let [x (:x p)
        y (:y p)]
    (even? (int (Math/floor y)))))

(defn checker
  [p]
  (let [x (:x p)
        y (:y p)]
    (even? (+ (int (Math/floor x)) (int (Math/floor y))))))

(defimg vstrip
  [x y]
  (<= (Math/abs x) 0.5))

(defimg vstripes
  [x y]
  (even? (int (Math/floor x))))

(defimg hstripes
  [x y]
  (even? (int (Math/floor y))))

(defimg checker
  [x y]
  (even? (+ (int (Math/floor x)) (int (Math/floor y)))))

(defn distance-from-origin
  [x y]
  (Math/sqrt (+ (* x x) (* y y))))

(defimg alt-rings
  [x y]
  (even? (int (Math/floor (distance-from-origin x y)))))

; Combinators

(defn img-xor
  [img1 img2]
  (fn [p]
    (not= (img1 p) (img2 p))))

(def checker
  (img-xor hstripes vstripes))

(defn img-and
  [img1 img2]
  (fn [p]
    (and (img1 p) (img2 p))))

(defn lift2
  [f]
  (fn [img1 img2]
    (fn [p]
      (f (img1 p) (img2 p)))))

(def img-xor (lift2 not=))

(def img-and (lift2 (fn [a b] (and a b))))

(defn lift1
  [f]
  (fn [img]
    (fn [p]
      (f (img p)))))

(def img-not (lift1 not))

(defn img-subtract
  [r1 r2]
  (img-and r1 (img-not r2)))


; Transformations

(defn to-polar
  [p]
  (let [x (:x p)
        y (:y p)]
    (Point. (distance-from-origin x y)
            (Math/atan2 x y))))

(defmacro fntrafo
  [[?x ?y] ?body]
  `(fn ^Point [^Point p#]
     (let [~?x (:x p#)
           ~?y (:y p#)]
       ~?body)))

(def to-polar
  (fntrafo [x y]
    (Point. (distance-from-origin x y)
            (Math/atan2 x y))))

(defn apply-trafo
  [trafo img]
  (fn [p]
    (img (trafo p))))

(defn apply-trafo
  [trafo img]
  (fnimg [x y]
    (img (trafo (Point. x y)))))

(defn comp-trafos
  [trafo1 trafo2]
  (fn [p]
    (trafo1 (trafo2 p))))

(defn comp-trafos
  [trafo1 trafo2]
  (fntrafo [x y]
    (trafo1 (trafo2 (Point. x y)))))

(defn polar-checker
  [n]
  (let [sc (fntrafo [r theta]
             (Point. r (* theta (/ (double n) Math/PI))))]
    (apply-trafo (comp-trafos sc to-polar) checker)))

(defn translate-point
  [dx dy]
  (fntrafo [x y]
    (Point. (+ x dx) (+ y dy))))

(defn scale-point
  [sx sy]
  (fntrafo [x y]
    (Point. (* sx x) (* sy y))))

(defn uscale-point
  [s]
  (scale-point s s))

(defn rotate-point
  [theta]
  (fntrafo [x y]
    (Point. (- (* x (Math/cos theta))
               (* y (Math/sin theta)))
            (+ (* y (Math/cos theta))
               (* x (Math/sin theta))))))

(defn translate
  [dx dy]
  (translate-point (- dx) (- dy)))

(defn scale
  [sx sy]
  (scale-point (/ 1.0 sx) (/ 1.0 sy)))

(defn uscale
  [s]
  (uscale-point (/ 1.0 s)))

(defn rotate
  [theta]
  (rotate-point (- theta)))

(defn swirl-point
  [r]
  (fntrafo [x y]
    ((rotate-point (* (distance-from-origin x y) (/ (* 2 Math/PI) r)))
     (Point. x y))))

(defn swirl
  [r]
  (swirl-point (- r)))

; Rest

(def from-polar
  (fntrafo [r theta]
    (Point. (* r (Math/cos theta))
            (* r (Math/sin theta)))))

(defn from-polar-transformation
  [xf]
  (comp-trafos from-polar (comp-trafos xf to-polar)))

(def rad-invert-polar
  (from-polar-transformation (fntrafo [r theta]
                               (Point. (if (zero? r)
                                         0.0
                                         (/ 1.0 r))
                                       theta))))

(defn rad-invert
  [img]
  (apply-trafo rad-invert-polar img))


