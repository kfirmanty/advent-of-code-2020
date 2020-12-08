(ns advent-of-code-2020.day6
  (:require [clojure.string :as string]))

(defn solve-1 [input]
  (let [groups (string/split input #"\n\n")]
    (->> groups
         (map #(reduce conj #{} (string/replace % "\n" "")))
         (map count)
         (reduce +))))

(defn solve-group [group]
  (->> (string/split group #"\n")
       (map #(reduce conj #{} %))
       (apply clojure.set/intersection)))

(defn solve-2 [input]
  (->> (string/split input #"\n\n")
       (map solve-group)
       (map count)
       (reduce +)))
