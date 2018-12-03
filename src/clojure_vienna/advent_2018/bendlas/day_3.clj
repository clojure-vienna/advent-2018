(ns clojure-vienna.advent-2018.bendlas.day-3
  (:require [clojure-vienna.advent-2018 :as main]
            [clojure-vienna.advent-2018.bendlas.util :refer [self-join]]
            [clojure-vienna.runner :as runner]))

(defrecord Coord [x y])

(defn unwrap-reduced [o]
  (if (reduced? o)
    @o o))

(defn y-xf [xform1 rf1 s1
            xform2 rf2 s2]
  (fn [xf]
    (let [xf1 (xform1 rf1)
          xf2 (xform2 rf2)
          sv1 (volatile! s1)
          sv2 (volatile! s2)]
      (fn
        ([s] (-> s
                 (xf (xf1 (unwrap-reduced @sv1)))
                 (xf (xf2 (unwrap-reduced @sv2)))
                 xf))
        ([s v]
         (when-not (reduced? @sv1)
           (vswap! sv1 xf1 v))
         (when-not (reduced? @sv2)
           (vswap! sv2 xf2 v))
         (if (and (reduced? @sv1)
                  (reduced? @sv2))
           (reduced s)
           s))))))

(comment

  (into [] (comp (map inc)
                 (y-xf
                  (map str)
                  conj ()
                  (map inc)
                  conj []))
        [1 2 3])
  )

(defn overlapping-coordinates [[{l1 :left t1 :top w1 :width h1 :height}
                                {l2 :left t2 :top w2 :width h2 :height}]]
  (let [l3 (max l1 l2)
        t3 (max t1 t2)
        r3 (min (+ l1 w1)
                (+ l2 w2))
        b3 (min (+ t1 h1)
                (+ t2 h2))]
    (when (and (< l3 r3)
               (< t3 b3))
      (eduction
       (runner/seq-bind (range l3 r3))
       (runner/seq-bind (range t3 b3))
       [(runner/curry 2 ->Coord)]))))

(defn calc [input]
  (count
   (into #{}
         (mapcat overlapping-coordinates)
         (self-join input)))
  (into []
        (comp (map (juxt identity overlapping-coordinates))
              (y-xf
               (comp (map second)
                     cat)
               (completing conj
                           count)
               #{}
               ;; Y
               (comp (remove (comp empty? second))
                     (mapcat first)
                     (map :id))
               (completing disj
                           #(do (assert (= 1 (count %)))
                                (first %)))
               (into #{}
                     (map :id)
                     input)))
        (self-join input)))

(defn parser [line]
  (->> (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)
       next
       (map #(Long/parseLong %))
       (zipmap
        [:id :left :top :width :height])))

(main/register-solution!
 "bendlas" 3 calc
 :line-parser parser
 :result-1 98005
 :result-2 331)
