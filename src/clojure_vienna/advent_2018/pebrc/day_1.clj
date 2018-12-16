(ns clojure-vienna.advent-2018.pebrc.day-1
  (:require [clojure-vienna.advent-2018 :as main]))

(defn day-1-1 [inputs]
  (reduce + 0 inputs))

(defn day-1-2 [inputs]
  (->> inputs
       cycle
       (reductions +)
       (reductions
        (fn [acc x]
          (if-let [dup (acc x)]
            x
            (assoc acc x 1))) {})
       (filter #(number? %) )
       first))

(main/register-solution!
 "pebrc" 1 (juxt day-1-1 day-1-2)
 :line-parser #(Long/parseLong %)
 :result-1 484
 :result-2 367)


(comment
  (main/run-solution "pebrc" 1))
