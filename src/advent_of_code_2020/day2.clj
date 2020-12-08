(ns advent-of-code-2020.day2
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (let [[policy letter pass] (string/split line #" ")
        [min max] (->> (string/split policy #"-")
                       (map #(Integer/valueOf %)))]
    {:min min
     :max max
     :pass pass
     :letter #{(-> letter
                   (string/replace ":" "")
                   first)}}))

(defn valid? [{:keys [min max pass letter]}]
  (<= min (->> pass
               (filter letter)
               count)
      max))

(def parsed-input
  (map parse-line (-> (slurp "resources/day2.in")
                      (string/split #"\n"))))
;;500
(defn solve-1 [input]
  (->> input
       (filter valid?)
       count))

(defn valid-b? [{:keys [min max pass letter]}]
  (not= (= (first letter) (nth pass (dec min)))
        (= (first letter) (nth pass (dec max)))))

;;313
(defn solve-1b [input]
  (->> input
       (filter valid-b?)
       count))


(def dynamo-state (let [ids (into #{} (:only-in-stripe diff))]
                    (->> rs (filter #(ids (:originalReceiptId %))))))
