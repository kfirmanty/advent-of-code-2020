(ns advent-of-code-2020.day1
  (:require [clojure.string :as string]))

(def input
  (->>
   (-> (slurp "resources/day1a.in")
       (string/split #"\n"))
   (map #(Integer/valueOf %))
   sort))

;;494475
(defn solve-1 []
  (loop [[h & r] input]
    (if-let [result (->> r
                         (filter #(when (= 2020 (+ h %)) %))
                         first)]
      (* h result)
      (recur r))))

(defn solve-1b []
  (loop [[h & r] input]
    (if-let [result (->> r
                         (map #(let [complimenting (- 2020 (+ h %))
                                        in-coll? (->> r
                                                      (filter #{complimenting})
                                                      first)]
                                 (when in-coll? (* h % complimenting))))
                         (filter some?)
                         first)]
      result
      (recur r))))
