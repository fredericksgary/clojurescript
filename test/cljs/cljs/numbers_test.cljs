(ns cljs.numbers-test
    (:refer-clojure :exclude [+ - * / < > <= >= = quot rem])        
    (:require [cljs.numbers :as nums]))

(defn test-numbers
  []
  (assert true)
  (assert false)
  :ok)
