(ns clojure-vienna.advent-2018.bendlas.day-2
  (:require [clojure-vienna.advent-2018 :as main]
            [clojure-vienna.runner :as runner]
            [clojure.set :as set]))

(defn calc-1 [inputs]
  (as-> (transduce
         (comp (map frequencies)
               (map (comp set vals)))
         (completing
          (fn [vs freqs]
            (cond-> vs
              (contains? freqs 2)
              (update :v2 inc)
              (contains? freqs 3)
              (update :v3 inc))))
         {:v2 0 :v3 0} inputs)
      {:keys [v2 v3]}
      (* v2 v3)))

;; calc-2

;; self join

(defn wrapped-reducer [rf]
  (completing
   #(let [s (rf %1 %2)]
      (if (reduced? s)
        (reduced s)
        s))
   rf))

(defn self-join [coll]
  (eduction
   (take-while some?)
   (fn [xf]
     (completing
      (fn [s [h & nxt]]
        (reduce (wrapped-reducer
                 #(xf %1 [h %2]))
                s nxt))
      xf))
   (iterate next coll)))

;; calc-2 logic

(defn compare-inputs [[i1 i2]]
  (assert (= (count i1)
             (count i2)))
  [(apply - (count i1)
          (map #(if (= %1 %2) 1 0)
               i1 i2))
   i1 i2])

(defn calc-2 [inputs]
  (let [[[_ s1 s2] :as r]
        (into []
              (comp (map compare-inputs)
                    (filter #(= 1 (nth % 0))))
              (self-join inputs))]
    (assert (= 1 (count r)))
    (->> (map (fn [c1 c2]
                (if (= c1 c2)
                  c1 nil))
              s1 s2)
         (filter some?)
         (apply str))))

(main/register-solution!
 "bendlas" 2 (juxt calc-1 calc-2)
 :result-1 7221
 :result-2 "mkcdflathzwsvjxrevymbdpoq")

(comment
  
  (main/run-solution "bendlas" 2)
  (main/read-input "bendlas" 2)
  
  )
