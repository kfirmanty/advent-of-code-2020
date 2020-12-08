(ns advent-of-code-2020.day3)

(defn parse-grid [raw]
  (let [rows (clojure.string/split raw #"\n")
        columns (count (first rows))
        arr (make-array Integer/TYPE columns (count rows))]
    (doseq [x (range columns) y (range (count rows))]
      (aset arr x y (int (if (= \. (nth (nth rows y) x))
                           0
                           1))))
    arr))

(defn move-fn [right down]
  (fn [x y arr visited-count]
    (let [nx (mod (+ x right) (count arr))
          ny (+ y down)
          nvisited (+ visited-count (aget arr nx ny))]
      [nx ny arr nvisited])))

(defn solve-1 [move-fn]
  (let [state [0 0 (parse-grid (slurp "resources/day3.in")) 0]]
    (loop [[x y arr visited-count] state]
      (if (= y (dec (count (nth arr 0))))
        visited-count
        (recur (move-fn x y arr visited-count))))))

(defn solve-2 []
  (apply * (map solve-1 [(move-fn 1 1) (move-fn 3 1)
                         (move-fn 5 1) (move-fn 7 1)
                         (move-fn 1 2)])))
