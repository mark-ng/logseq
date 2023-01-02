(ns frontend.extensions.sci
  "Provides a consistent approach to sci evaluation. Used in at least the following places:
- For :view evaluation
- For :result-transform evaluation
- For cljs evaluation in Src blocks
- For evaluating {{function }} under query tables"
  (:require [sci.core :as sci]
            [frontend.util :as util]
            [goog.dom]
            [goog.object]
            [goog.string]))

;; Helper fns for eval-string
;; ==========================
(def ^:private sum (partial apply +))

(defn- average [coll]
  (/ (reduce + coll) (count coll)))

(defn week-number
  "Week number according to the ISO-8601 standard, weeks starting on
  Monday. The first week of the year is the week that contains that
  year's first Thursday (='First 4-day week'). The highest week number
  in a year is either 52 or 53."
  []
  (let [year (.getFullYear (js/Date.))
        month (.getMonth (js/Date.))
        date (.getDate (js/Date.))
        day (.getDay (js/Date.))
        thursday (js/Date. year month (- (+ date 4) (if (= 0 day) 7 day)))
        year-start (js/Date. year 0 1)]
    (Math/ceil (/ (+ (/ (- (.getTime thursday)
                           (.getTime year-start))
                        (* 1000 60 60 24))
                     1)
                  7))))

(defn week-year
  []
  (let [date (js/Date.)]
  (.setDate date (- (+ (.getDate date) 3)
                   (mod (+ 6 (.getDay date)) 7)))
  (.getFullYear date)))

(defn- call-api
  "Given a fn name from logseq.api, invokes it with the given arguments"
  [fn-name & args]
  (when-not (aget js/window.logseq "api" fn-name)
    (throw (ex-info "Api function does not exist" {:fn fn-name})))
  (apply js-invoke (aget js/window.logseq "api") fn-name args))

;; Public fns
;; ==========
(defn eval-string
  "Second arg is a map of options for sci/eval-string"
  ([s]
   (eval-string s {}))
  ([s options]
   (try
     (sci/eval-string s (merge-with merge
                                    {:bindings {'sum sum
                                                'average average
                                                'week-number week-number
                                                'week-year week-year
                                                'parseFloat js/parseFloat
                                                'isNaN js/isNaN
                                                'log js/console.log
                                                'pprint util/pp-str
                                                ;; Provide to all evals as it useful in most contexts
                                                'call-api call-api}}
                                    options))
     (catch :default e
       (println "Query: sci eval failed:")
       (js/console.error e)))))

(defn call-fn
  [f & args]
  (apply f args))

(defn eval-result
  "Evaluate code with sci in a block context"
  [code block]
  [:div
   [:code "Results:"]
   [:div.results.mt-1
    (let [result (eval-string code {:bindings {'block block}})]
      (if (and (vector? result) (:hiccup (meta result)))
        result
        [:pre.code (str result)]))]])
