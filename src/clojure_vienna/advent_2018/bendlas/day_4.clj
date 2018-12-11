(ns clojure-vienna.advent-2018.bendlas.day-4
  (:require [clojure-vienna.advent-2018.bendlas.util :as util]
            [clojure-vienna.advent-2018 :as main]))

(def guard-keeper
  (fn [xf]
    (completing
     (let [current-guard (volatile! nil)]
       (fn [s m]
         (if-let [guard-n (vswap! current-guard
                                  #(if (contains? m :guard-n)
                                     (:guard-n m)
                                     %))]
           (xf s (assoc m :guard-n guard-n))
           s)))
     xf)))

(def sleep-keeper
  (fn [xf]
    (completing
     (let [sleep-start (volatile! nil)]
       (fn [s {:keys [minute type guard-n]}]
         (case type
           :begin (do (vreset! sleep-start nil)
                      s)
           :wake (if-let [start @sleep-start]
                   (do (vreset! sleep-start nil)
                       (xf s (eduction
                              (map #(list guard-n %))
                              (range start minute))))
                   s)
           :sleep (do (when (nil? @sleep-start)
                        (vreset! sleep-start minute))
                      s))))
     xf)))

(defn calc [input]
  (let [sleepers (->> input
                      (sort-by :date)
                      (eduction guard-keeper sleep-keeper cat))
        [guard _] (->> sleepers
                       (map first)
                       frequencies
                       (apply max-key val))
        [[_ minute] _] (->> sleepers
                            (filter (comp #{guard} first))
                            frequencies
                            (apply max-key val))
        [[guard-2 minute-2] _] (->> sleepers
                                    frequencies
                                    (apply max-key val))]
    [(* guard minute)
     (* guard-2 minute-2)]))

(defn parser [line]
  (let [[_ date minute message]
        (re-matches #"\[(\d{4}-\d\d-\d\d \d\d:(\d\d))] (.*)" line)
        [begin guard-n] (re-matches #"Guard #(\d+) begins shift" message)]
    (-> {:date date
         :minute (Long/parseLong minute)
         :type (case message
                 "wakes up" :wake
                 "falls asleep" :sleep
                 (do (assert begin)
                     :begin))}
        (cond-> begin (assoc :guard-n (Long/parseLong guard-n))))))

(main/register-solution!
 "bendlas" 4 calc
 :line-parser parser
 :result-1 109659
 :result-2 36371)
