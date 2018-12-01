(ns clojure-vienna.advent-2018.bendlas.day-1
  (:require [clojure.java.io :as io]))

(def inputs
  (into [] (map #(Long/parseLong %))
        (line-seq (io/reader (io/resource "clojure-vienna/advent-2018/bendlas/day-1")))))

(def result-1
  (reduce + 0 inputs))

(println "Result 1:" result-1)

(def result-2
  (->> inputs
       cycle
       (reductions + 0)
       (map-indexed vector)
       (reduce (fn [memory [index value]]
                 (if (contains? memory value)
                   (reduced [index value])
                   (conj memory value)))
               #{})))

(println "Result 2:" (second result-2)
         "found after" (first result-2) "iterations")
