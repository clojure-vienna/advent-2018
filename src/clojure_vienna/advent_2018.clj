(ns clojure-vienna.advent-2018
  (:require [clojure-vienna.runner :as runner]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.data :as data]))

(def users (into [] (.list (io/file "src/clojure_vienna/advent_2018"))))
(def days (range 1 25))
(defonce all-solutions (agent nil
                              :error-handler (fn [a e]
                                               (log/error e "During registration"))))
;; Parsers for daily inputs

(defn read-input [user day]
  (let [[tag & nxt :as tagged] (runner/read-input @all-solutions user day)]
    (if (= ::runner/input tag)
      nxt tagged)))

;; API

(defn register-solution! [user day solution & {:keys [input-reader line-parser result-1 result-2]
                                               :or {line-parser identity}}]
  (when (contains? (get @all-solutions user) day)
    (log/debug "Overwriting registered solution for day" day "of user" user))
  (send all-solutions update-in [user day]
        (fn [sol]
          (-> sol
              (assoc
               :user user :day day :solution solution
               :result-1 result-1 :result-2 result-2
               :ns-sym (get sol :ns-sym (ns-name *ns*))
               :ns-result (get sol :ns-result [::runner/ok ""])
               :input-parser (or input-reader
                                 #(eduction (map line-parser)
                                            (line-seq %)))))))
  (send all-solutions runner/complete-solution* user day)
  (when (bound? #'runner/*completed*)
    (set! runner/*completed* true)))

(defn run-solution [user day]
  (-> @all-solutions
      (runner/complete-solution* user day)
      (get-in [user day :result])))

;; logging and running

(defn solutions-watcher [_ a old new]
  (let [[_ changes _] (data/diff old new)]
    (doseq [[user days] changes
            [day {:as sol :keys [v]}] days]
      (when-let [full (and v (get-in new [user day :result]))]
        (runner/prst (runner/print-solution full))))))

(defonce solutions-watch
  (add-watch all-solutions ::watch #'solutions-watcher))


(defn main []
  (println "INFO Running with users" users)
  (send all-solutions runner/solution-generator* users days))

(comment
  (main)
  )

(defn -main []
  (println "INFO Running with users" users)
  (send all-solutions
        (fn [s]
          (let [s* (runner/solution-generator* s users days)]
            (send *agent*
                  (fn [final]
                    (shutdown-agents)
                    (.flush *out*)
                    (.flush *err*)
                    (.flush System/out)
                    (.flush System/err)
                    (System/exit 0)))
            s*))))
