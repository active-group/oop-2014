(ns oop-2014.autocheck
  (:use oop-2014.random)
  (:use oop-2014.matcher)
  (:use clojure.math.numeric-tower))

(defrecord
    ^{:doc "Generator monad for random values."}
    Generator
    ;; random-generator -> val
    [func])

(defn unit
  "Monadic return for generators."
  [val]
  (Generator. (fn [rgen] val)))

(defn bind
  "Monadic bind for generators."
  [m1 k]
  (let [func1 (:func m1)]
    (Generator.
     (fn [rgen]
       (let [[rgen1 rgen2] (random-generator-split rgen)]
         (let [gen (k (func1 rgen1))]
           ((:func gen) rgen2)))))))

; list-of (generator x) -> generator (list-of x) 
(defn seqm
  [ms]
  (if (not (seq ms))
    (unit '())
    (bind (first ms)
          (fn [x]
            (bind (seqm (rest ms))
                  (fn [xs]
                    (unit (conj xs x))))))))

(defn lift->generator
  "Lift a function on values to generators."
  [func gen]
  (bind gen
        (fn [v]
          (unit (func v)))))

; random-gen (generator a) -> a
(defn generate
  "Extract a value from a generator, using random generator rgen."
  [rgen gen]
  ((:func gen) rgen))

(defn choose-integer
  "Generator for integers within a range, bounds are inclusive."
  [lower upper]
  (Generator.
   (fn [rgen]
     (let [[n _] (random-integer rgen lower upper)]
       n))))

(defn choose-float
  "Generator for floats within a range, bounds are inclusive."
  [lower upper]
  (Generator.
   (fn [rgen]
     (let [[n _] (random-float rgen lower upper)]
       n))))

(defn choose-ascii-letter
  "Generator for ASCII alphabetic letters."
  []
  (lift->generator #(get "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" %)
                   (choose-integer 0 51)))

; (list a) -> (generator a)
(defn choose-one-of
  "Make a generator that yields one of a list of values."
  [lis]
  (lift->generator #(nth lis %)
                   (choose-integer 0 (- (count lis) 1))))

(defn choose-boolean
  []
  (choose-one-of '(true false)))

; vector from the paper
; (generator a) int -> (generator (list a))
(defn choose-list
  "Generator for a list of values with size n."
  [el-gen n]
  (letfn [(recurse [n]
            (if (zero? n)
              (unit '())
              (bind
               el-gen
               (fn [val]
                 (bind (recurse (- n 1))
                       (fn [rest]
                         (unit (conj rest val))))))))]
    (recurse n)))

; (list (promise (generator a))) -> (generator a)
(defn choose-mixed
  "Generator that chooses from a sequence of generators."
  [gens]
  (bind (choose-one-of gens) force))

(defrecord ^{:doc "QuickCheck property"}
    Property
    [;; args -> value
     func
     ;; matches the value returned by func
     matcher
     arg-names
     ;; (seq generator)
     arg-generators])

(defrecord Failure [property args])

(defn property->result-generator
  [prop]
  (bind (seqm (:arg-generators prop))
        (fn [arg-vals]
          (let [res (apply (:func prop) arg-vals)]
            (unit
             (if (matches? (:matcher prop) res)
               true
               (Failure. prop
                         (zipmap (:arg-names prop) arg-vals))))))))

(def max-test 100)

(defn tests
  "Run a series of test runs.

- gen a result generator
- rgen is a random-number generator
- ntest is the number of samples we want run 

returns two values:
- ntest
- true for success, false for exhausted, result for failure"
  [gen rgen ntest nfail]
  (loop [rgen rgen
         ntest ntest]
    (cond
     (= ntest max-test) (list ntest true)
     :else
     (let [[rgen1 rgen2] (random-generator-split rgen)
           result (generate rgen2 gen)]
       (if (true? result)
         (recur rgen1 (+ 1 ntest))
         (list ntest result))))))

(defn check
  [prop]
  (tests (property->result-generator prop) (make-random-generator 0) 0 0))

(def integer-positive
  (Property.
   (fn [a]
     a)
   (is-positive)
   (list 'a)
   (list
    (choose-integer -1000 1000))))

(def +-associative
  (Property.
   (fn [a b c]
     (= (+ a (+ b c)) (+ (+ a b) c)))
   (is-true)
   (list 'a 'b 'c)
   (list
    (choose-integer -1000 1000)
    (choose-integer -1000 1000)
    (choose-integer -1000 1000))))

(defmacro property
  [?matcher ?clauses & ?body]
  (let [?pairs (partition 2 ?clauses)
        ?ids (map first ?pairs)
        ?rhss (map second ?pairs)]
    `(Property. (fn [~@?ids] ~@?body)
                ~?matcher
                '~?ids
                (list ~@?rhss))))

(def integer-positive2
  (property (is-positive)
            [a (choose-integer -1000 1000)]
            a))

(def +-associative2
  (property (is-true)
            [a (choose-integer -1000 1000)
             b (choose-integer -1000 1000)
             c (choose-integer -1000 1000)]
            (= (+ a (+ b c)) (+ (+ a b) c))))
