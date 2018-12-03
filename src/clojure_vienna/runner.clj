(ns clojure-vienna.runner
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.stacktrace :as st]
            [clojure.tools.logging :as log]))

(def ^:dynamic *completed*)

;; generation

(defn st-set*
  ([s] s)
  ([s v] v))

(defn st-dup [xf]
  (fn
    ([s] (xf s))
    ([s f] (xf s (f s)))))

(def st-dup*
  (st-dup st-set*))

(defn seq-bind [coll]
  (fn [xf]
    (fn
      ([s] (xf s))
      ([s f] (reduce
              (fn [s v]
                (let [res (xf s (f v))]
                  (if (reduced? res)
                    (reduced res)
                    res)))
              s coll)))))

(defn curry* [n f]
  (case n
    (0 1) f
    2 (fn [& vs] #(apply f % vs))
    (recur (dec n) (curry* 2 f))))

(defn curry
  ([n f & pargs]
   (curry* n (fn [& args]
               (apply f (concat pargs (reverse args))))))
  ([n f]
   (curry* n (fn [& args]
               (apply f (reverse args))))))

(comment
  (into []
        (comp (seq-bind (range 3))
              (seq-bind (range 10 13))
              (seq-bind (range 20 23)))
        [(curry 3 list :yo)])
  )

;; calculation

(defn calculate-solution [user day input {:keys [solution result-1 result-2] :as sol}]
  (let [[tag calc out :as res] (try
                                 (let [sw (java.io.StringWriter.)
                                       res (with-open [ow (io/writer sw)]
                                             (binding [*out* ow]
                                               (solution input)))]
                                   [::result res (str sw)])
                                 (catch Exception e
                                   [::calculation-exception e user day input sol]))]
    (if (= ::result tag)
      (let [[calc-1 calc-2] calc]
        (if (and (= calc-1 result-1)
                 (= calc-2 result-2))
          [::result-match {:result-1 result-1 :result-2 result-2} out]
          [::result-mismatch {:result-1 result-1 :result-2 result-2
                              :calc-1 calc-1 :calc-2 calc-2}
           out]))
      res)))

(defn read-input-stream [user day]
  (let [input-path (str "clojure-vienna/advent-2018/" user "/day-" day)
        res (io/resource input-path)]
    (if res
      [::input (io/reader res)]
      [::no-input user day input-path])))

(defn read-input [all-solutions user day]
  (let [parser (get-in all-solutions [user day :input-parser])
        [tag rdr :as result] (read-input-stream user day)]
    (case tag
      ::input (try [::input (parser rdr)]
                   (catch Exception e
                     [::parser-error e user day parser result]))
      result)))

(defn complete-solution* [all-solutions user day]
  (let [{:keys [ns-sym ns-result solution v] :as sol} (get-in all-solutions [user day])
        input (read-input all-solutions user day)
        submitted-solution (if (some? solution)
                             [::submitted sol]
                             [::nothing-submitted ns-sym])]
    (update-in all-solutions [user day] assoc
               :v (inc (or v 0))
               :result (-> (if (and (= ::ok (first ns-result))
                                    (= ::submitted (first submitted-solution))
                                    (= ::input (first input)))
                             (calculate-solution user day
                                                 (second input)
                                                 (second submitted-solution))
                             [::calculation-error])
                           (with-meta
                             (assoc sol
                                    :input input
                                    :submitted-solution submitted-solution))))))

(defn load-solution* [all-solutions user day]
  (let [sym (symbol (str "clojure-vienna.advent-2018." user ".day-" day))
        ns-result (try
                    (binding [*completed* false]
                      (let [output (with-out-str (async-require sym :reload-all))]
                        [::ok output *completed*]))
                    (catch Exception e
                      [(if (.isFile (io/file "src/clojure_vienna/advent_2018"
                                             user
                                             (str "day_" day ".clj")))
                         ::load-error
                         ::no-file)
                       e user day]))
        completed (and (= ::ok (first ns-result))
                       (nth ns-result 2))]
    (-> all-solutions
        (update-in [user day]
                   #(-> %
                        (cond-> (not= ::ok (first ns-result))
                          (update :v (fnil inc 0)))
                        (dissoc :solution :result)
                        (assoc :user user :day day
                               :ns-sym sym
                               :ns-result ns-result)))
        (cond-> (not completed)
          ;; complete manually if register-solution! hasn't been called
          (complete-solution* user day)))))

(defn solution-generator* [all-solutions users days]
  (transduce (comp
              (seq-bind days)
              (seq-bind users)
              st-dup)
             st-set* all-solutions
             [(curry* 3 load-solution*)]))

(comment
  (into []
        (comp
         (seq-bind [1 2 3])
         (seq-bind "abc")
         st-dup)
        [(curry* 3 vector)])
  )

;; printer

(defn line [marker user day & content]
  (conj (into [marker " [" user "] day-" day ": "]
              content)
        \newline))

(defn replacer [orig replacement]
  (map #(str/replace (str %) orig replacement)))

(defn indenter [indent-str]
  (replacer "\n" (str "\n" indent-str)))

(defn reindent [indent-str strs]
  (let [strs (map str strs)]
   (when-not (every? str/blank? strs)
     (-> [\newline indent-str]
         (into
          (comp cat (indenter "    "))
          [(butlast strs)
           (let [lst (last strs)]
             (if (str/ends-with? lst "\n")
               (subs lst 0 (dec (count lst)))
               lst))])
         (conj \newline)))))

(defn print-error [tag e user day]
  (concat (line tag user day (.getMessage e))
          (reindent
           "    "
           (letfn [(print-frame [e]
                     (concat
                      [(.getName (.getClass e)) ": " (.getMessage e) \newline]
                      (if (::s/problems (ex-data e))
                        (reindent "    "
                                  [(with-out-str
                                     (s/explain-out (ex-data e)))
                                   \newline])
                        (reindent "    "
                                  [(with-out-str
                                     (when-let [d (ex-data e)] (pprint d)))
                                   \newline]))
                      (when-not (or (::s/problems (ex-data e))
                                    (ex-data e))
                        (into []
                              (comp
                               (take 6)
                               (map #(list "    "
                                           (with-out-str (st/print-trace-element %))
                                           \newline))
                               cat)
                              (.getStackTrace e)))
                      [\newline]
                      (when-let [c (.getCause e)]
                        (print-frame c))))]
             (print-frame e)))))

(defn print-solution [[tag {:keys [result-1 result-2 calc-1 calc-2] :as tag-v} out :as tagged]]
  (let [{:keys [user day]
         [submitted ns-sym] :submitted-solution
         [loaded & [e luser lday :as load-error]] :ns-result
         [input & [iuser iday ipath :as input-error]] :input}
        (meta tagged)]
    (concat
     (case tag
       ::calculation-exception (print-error "CALCULATION ERROR" tag-v user day)
       ::calculation-error []
       (concat
        (case tag
          ::result-match (line "SUCCESS" user day (pr-str [result-1 result-2]))
          ::result-mismatch (line "MISMATCH" user day
                                  "want " (pr-str [result-1 result-2])
                                  ", got " (pr-str [calc-1 calc-2])))
        (reindent "    " [out \newline])))
     (case loaded
       ::ok []
       ::no-file (when (= ::input input)
                   (line "ERROR" user day "Please implement solution in " ns-sym))
       ::load-error (print-error "LOAD ERROR" e user day))
     (case submitted
       ::submitted []
       (when (= ::ok loaded)
         (line "ERROR" user day "Please call clojure-vienna.advent-2018/register-solution! during loading of " ns-sym)))

     (when (= ::ok loaded)
       (case input
         ::parser-error (apply line "PARSER ERROR" user day
                               (eduction (interpose " ")
                                         input-error))
         ::no-input (line "ERROR" user day "Please put input at resources/" ipath)
         ::input [])))))

(defonce out System/out)
(defonce err System/err)

(defn prst [s] (reduce
                #(.print out %2)
                nil s))
