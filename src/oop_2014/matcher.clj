(ns oop-2014.matcher)

(defrecord Matcher [predicate sexpr])

(defmethod print-method Matcher [m ^java.io.Writer w]
  (print-method (list 'matcher (:sexpr m)) w))

(defmethod print-dup Matcher [m ^java.io.Writer w]
  (print-dup (list 'matcher (:sexpr m)) w))

(defn matches?
  [matcher val]
  ((:predicate matcher) val))

(defn is
  ([p? val] (Matcher. (fn [x] (p? x val)) (list 'is p? val)))
  ([p] (cond
        (fn? p) (Matcher. p  (list 'is p))
        :else
        (Matcher. (fn [x] (= x p)) (list 'is p)))))

(defn anything
  []
  (Matcher. (constantly true)
            'anything))

(defn opposite
  [m]
  (Matcher. (fn [x]
              (not (matches? m x)))
            (list 'not (:sexpr m))))

(defn is-true
  []
  (Matcher. (fn [x] x)
            'is-true))

(defn is-positive
  []
  (Matcher. (fn [x] (> x 0))
            'is-positive))

(defn is-false
  []
  (Matcher. (fn [x] (not x))
           'is-false))

(defn all-of
  [& ms]
  (Matcher. (fn [x]
              (every? (fn [m]
                        (matches? m x))
                      ms))
            (cons 'is-all-of (map :sexpr ms))))

(defn seq-of
  [m]
  (Matcher. (fn [x]
              (and (seq? x)
                   (every? (fn [e]
                             (matches? m e))
                           x)))
            (list 'seq-of (:sexpr m))))

