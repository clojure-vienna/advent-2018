(ns clojure-vienna.advent-2018.bendlas.day-5
  (:require [clojure-vienna.advent-2018.bendlas.util :as util]
            [clojure-vienna.advent-2018 :as main]))

(defn polar-opposite? [c1 c2]
  (and
   (some? c1) (some? c2)
   (= (Character/toLowerCase c1)
      (Character/toLowerCase c2))
   (not= (Character/isUpperCase c1)
         (Character/isUpperCase c2))))

(defn react [input]
  (-> input
      (->> (reduce (fn [stack ch]
                     (if (polar-opposite? ch (peek stack))
                       (pop stack)
                       (conj stack ch)))
                   ()))
      count))

(defn calc [input]
  [(react input)
   (->> input
        (reduce (fn [m ch]
                  (let [c (Character/toLowerCase ch)]
                    (if (contains? m c)
                      m
                      (assoc m c (react (eduction
                                         (remove #{c (Character/toUpperCase c)})
                                         input))))))
                {})
        (apply min-key val)
        val)])

(main/register-solution!
 "bendlas" 5 calc
 :input-reader (fn [r]
                 (sequence
                  (comp (take-while (complement neg?))
                        (map char))
                  (repeatedly #(.read r))))
 :result-1 11540
 :result-2 6918)
