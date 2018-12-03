(ns clojure-vienna.advent-2018.{{user}}.day-{{x}}
  (:require [clojure-vienna.advent-2018 :as main]))

(defn calc-1 [input]
  (take 3 input))
(defn calc-2 [input])

(defn parser [line]
  line)

(main/register-solution!
 "{{user}}" {{x}} (juxt calc-1 calc-2)
 :line-parser parser)
