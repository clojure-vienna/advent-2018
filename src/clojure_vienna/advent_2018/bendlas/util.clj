(ns clojure-vienna.advent-2018.bendlas.util)

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
   (mapcat (fn [[h & nxt]]
             (eduction (map #(vector h %))
                       nxt)))
   (iterate next coll)))
