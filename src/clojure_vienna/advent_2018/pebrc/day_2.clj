(ns clojure-vienna.advent-2018.pebrc.day-2
  (:require [clojure-vienna.advent-2018 :as main]))

(defn day-2-1 [inputs]
  (->> inputs
       (map frequencies)
       (map  (fn [f] ((juxt (partial filter (fn [[k v]] (= v 2))) (partial filter (fn [[k v]] (= v 3))))f )))
       (map (fn [t] (map #(min (count %) 1) t)))
       (reduce (fn [[l1 r1] [l2 r2]] [(+ l1 l2) (+ r1 r2)]))
       (apply *)))

(defn hamming [a b]
  (count (filter true? (map (partial reduce not=) (map vector a b)))))

(defn day-2-2 [inputs]
  (let [input (vec inputs)]
    (->> (for [x input y input]
           [(hamming x y) x y])
         (filter #(= (first %) 1))
         first ;we are doing redundant computation :-(
         ((fn [[_ a b]] (map (partial reduce (fn [x y] (if (= x y) x "")))(map vector a b))))
         (apply str))))

(main/register-solution!
 "pebrc" 2 (juxt day-2-1 day-2-2)
 :result-1 7808
 :result-2 "efmyhuckqldtwjyvisipargno")
