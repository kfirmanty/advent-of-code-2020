(ns advent-of-code-2020.day8
  (:require [clojure.string :as string]))

(defn parse-input [input]
  (->>
   (string/split input #"\n")
   (map #(string/split % #"\s+"))
   (map (fn [[op v]]
          [(keyword op) (Integer/parseInt v)]))))

(defn execute [state [op val]]
  (condp = op
    :nop (update state :loc inc)
    :acc (-> state
             (update :acc + val)
             (update :loc inc))
    :jmp (update state :loc + val)))

(defn solve-1 [input]
  (let [state {:acc 0
               :loc 0
               :ops input}]
    (loop [state state visited #{}]
      (cond (visited (:loc state)) [:inifite-loop (:acc state)]
            (= (:loc state) (count (:ops state))) [:finished (:acc state)]
        :else (recur (execute state (nth (:ops state) (:loc state))) (conj visited (:loc state)))))))

(defn brute-2 [input]
  (let [input (into [] input)
        permutations (into #{}
                           (concat [input]
                                   (for [iteration (range (count input))]
                                     (let [[op val] (nth input iteration)]
                                       (assoc input iteration [(condp = op
                                                                 :nop :jmp
                                                                 :jmp :nop
                                                                 :acc)
                                                               val])))))]
    (println "all possible programs" (count permutations))
    (loop [[permutation & permutations] (into [] permutations)]
      (let [[result acc] (solve-1 permutation)]
        (if (= result :finished)
          acc
          (recur permutations))))))
