(ns cljs.numbers
  (:require [goog.math.Integer :as gmathint])
  (:refer-clojure :exclude [+ - * / < > <= >= = quot rem]))

;; Seriously what are we doing here? I mean.
;; Well.

(defprotocol Number
  (-plus [this other])
  (-times [this other])
  (-negate [this])
  (-invert [this])
  (-quotient [this other])
  (-remainder [this other])
  (-lessThan [this other]))

;; TODO multimethod?
(defn bigint
  [x]
  (cond (string? x)
        (gmathint/fromString x)
        (number? x)
        (gmathint/fromNumber x)))

(declare ratio)

;; TODO: will have to make these work with any
;; numeric types, I guess by casting things
;; around. Actually we probably need a whole
;; theory of how the types combine.

(extend-type goog.math.Integer
  Number
  (-plus [this other]
    (.add this other))
  (-times [this other]
    (.multiply this other))
  (-negate [this] (.negate this))
  (-quotient [this other]
    (.divide this other))
  (-remainder [this other]
    (.modulo this other))
  (-invert [this] (ratio 1 this))
  (-lessThan [this other]
    (.lessThan this other)))

(defn +
  ([] (bigint "0"))
  ([x & xs]
     (reduce -plus x xs)))

(defn *
  ([] (bigint "1"))
  ([x & xs]
     (reduce -times x xs)))

(defn -
  ([x] (-negate x))
  ([x y & ys]
     (+ x (- (apply + y ys)))))

;; TODO: Check if there is already an ordered
;; protocol?
(defn <
  ([x] true)
  ([x y] (-lessThan x y))
  ([x y z & zs]
     (every? (partial apply <)
             (partition 2 1 (list* x y z zs)))))

(defn >
  [x & ys]
  (apply < (reverse (list* x ys))))

(defn =
  ([x] true)
  ([x y] (and (not (> x y))
              (not (< x y))))
  ([x y & ys]
     (every? (partial = x) (cons y ys))))

(defn quot
  [x y]
  (-quotient x y))

(defn rem
  [x y]
  (-remainder x y))

(defn /
  ([x] (-invert x))
  ([x y & ys] (* x (/ (apply * y ys)))))

(defn bigint?
  [x]
  (= goog.math.Integer (type x)))

(defn gcd
  [a b]
  {:pre [(bigint? a) (bigint? b)]}
  (if (< a b)
    (recur b a)
    (let [c (rem a b)]
      (if (= (bigint 0) c)
          b
          (recur b c)))))

(deftype Ratio [a b] ;; both fields should be bigints
  IPrintable
  (-pr-seq [this]
    (list (str a "/" b)))
  Number
  (-plus [this other]
    (let [a' (. other -a)
          b' (. other -b)]
      (new Ratio
           (+ (* a b') (* b a'))
           (* b b')))))

(defn ratio
  [a b]
  (let [a (bigint a),
        b (bigint b),
        c (gcd a b)]
    (new Ratio (/ a c) (/ b c))))
