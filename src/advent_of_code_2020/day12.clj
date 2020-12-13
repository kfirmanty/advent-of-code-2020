(ns advent-of-code-2020.day12
  (:require [clojure.string :as string]))

(defn add-to [state field v]
  (assoc state field
         (map + (field state) v)))

(defn add-to-pos [state v]
  (add-to state :position v))

(defn l-rot [facing]
  ({:N :W
    :W :S
    :S :E
    :E :N} facing))
(defn r-rot [facing]
  ({:N :E
    :E :S
    :S :W
    :W :N} facing))
(defn rotate [state dir degrees]
  (let [times (int (/ degrees 90))]
    (loop [it times facing (:facing state)]
      (if (= it 0)
        (assoc state :facing facing)
        (recur (dec it) (if (= dir :R)
                          (r-rot facing)
                          (l-rot facing)))))))

(defn facing->vec [facing]
  (get {:N [0 -1]
        :S [0 1]
        :E [1 0]
        :W [-1 0]} facing))

(defn move [state [dir val]]
  ;;(println state dir val)
  (condp = dir
    :F (add-to-pos state (map #(* val %) (facing->vec (:facing state))))
    :N (add-to-pos state [0 (* -1 val)])
    :S (add-to-pos state [0 val])
    :E (add-to-pos state [val 0])
    :W (add-to-pos state [(* -1 val) 0])
    :L (rotate state :L val)
    :R (rotate state :R val)))

(defn parse-input [input]
  (->> (string/split input #"\n")
       (map #(vector (-> % first str keyword) (->> % rest (apply str) Integer/valueOf)))))

(defn solve [input move-fn]
  (let [commands (parse-input input)]
    (loop [[command & commands] commands state {:facing :E :position [0 0] :waypoint [10 -1]}]
      (if (nil? command)
        (->> state
             :position
             (map #(Math/abs %))
             (apply +))
        (recur commands (move-fn state command))))))

(defn solve-1 [input]
  (solve input move))

(defn transf [x-fn y-fn]
  (fn [[x y]]
    [(y-fn y) (x-fn x)]))

(defn transform-vec [v dir]
  (if (= dir :R)
    ((transf identity -) v)
    ((transf - identity) v)))

(defn rotate-waypoint [state dir degrees]
  (let [times (mod (int (/ degrees 90)) 4)]
    (loop [it times waypoint (:waypoint state)]
      (if (= it 0)
        (assoc state :waypoint waypoint)
        (recur (dec it)
               (transform-vec waypoint dir))))))

(defn move-2 [state [dir val]]
  (condp = dir
    :F (add-to-pos state (map #(* val %) (:waypoint state)))
    :N (add-to state :waypoint [0 (* -1 val)])
    :S (add-to state :waypoint [0 val])
    :E (add-to state :waypoint [val 0])
    :W (add-to state :waypoint [(* -1 val) 0])
    :L (rotate-waypoint state :L val)
    :R (rotate-waypoint state :R val)))

(defn solve-2 [input]
  (solve input move-2))
