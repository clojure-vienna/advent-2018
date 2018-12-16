(ns clojure-vienna.advent-2018.pebrc.day-3
   (:require [clojure-vienna.advent-2018 :as main]))


(defn day-3-1 [input]
  (->> (reduce (fn [m {:keys [left top width height]}]
                 (->> (for [x (range left (+ left  width)) y (range top (+ top height))]
                        [x y])
                      (reduce  (fn [acc [x y]] (update-in acc [x y] (fnil inc 0))) m )))
               {}
               input)
       (map (fn [[k v]] {k (into {} (filter (fn [[k v]] (not= 1 v)) v))}) )
       (into {})
       (reduce (fn [acc [k v]] (+ acc (count v))) 0)))

(defn day-3-2 [input]
  (let [expected-sizes (into {} (map (fn [{:keys [id width height]}] [id (* width height)]) input))]
    (->> (reduce (fn [m {:keys [id left top width height]}]
                   (->> (for [x (range left (+ left  width)) y (range top (+ top height))]
                          [x y])
                        (reduce  (fn [acc [x y]] (update-in acc [x y] #(if (nil? %) [1 [id]] [(inc (first %)) (conj  (second %) id)]))) m )))
                 {}
                 input)
         (map (fn [[k v]] {k (into {} (filter (fn [[k [v ids]]] (= 1 v)) v))}) )
         (into {})
         (mapcat (fn [[k v]] (map (comp first second) (vals v))))
         (frequencies)
         (filter (fn [[k v]] (= v (get expected-sizes k))))
         ffirst)))


(defn parse-line [line]
  (let [[_ id left top width height] (re-find #"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)" line)]
    (assoc {}
           :id (Integer. id)
           :left (Integer. left)
           :top (Integer.  top)
           :width (Integer. width)
           :height (Integer. height))))

(main/register-solution!
 "pebrc" 3 (juxt day-3-1 day-3-2)
 :line-parser parse-line
 :result-1 113716
 :result-2 742)
