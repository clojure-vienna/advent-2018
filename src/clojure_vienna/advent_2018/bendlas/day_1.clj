(ns clojure-vienna.advent-2018.bendlas.day-1
  (:require [clojure-vienna.advent-2018 :as main]))

(defn calc-1 [inputs]
  (reduce + 0 inputs))

(defn calc-2 [inputs]
  (-> inputs cycle
      (->> (reductions + 0)
           (map-indexed vector)
           (reduce (fn [memory [index value]]
                     (if (contains? memory value)
                       (reduced [index value])
                       (conj memory value)))
                   #{}))
      (as-> [iterations value]
          (do (println "Result 2 found after" iterations "iterations")
              value))))

(main/register-solution!
 "bendlas" 1 (juxt calc-1 calc-2)
 :line-parser #(Long/parseLong %)
 :result-1 470
 :result-2 790)

(comment
  
  (main/run-solution "bendlas" 1)
  (main/read-input "bendlas" 1)
  
  )
