(ns advent-of-code-2020.day4
  (:require [clojure.string :as string]))

(defn ->map [entry]
  (->> (string/split entry #"[\n\s]")
         (map #(string/split % #":"))
         (into {})))

(defn validate [entry]
  (= (->> entry
          (filter #(#{"byr" "iyr"
                      "eyr" "hgt"
                      "hcl" "ecl"
                      "pid"} (first %)))
          count) 7))

;;byr (Birth Year) - four digits; at least 1920 and at most 2002.
;;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;;hgt (Height) - a number followed by either cm or in:
;;If cm, the number must be at least 150 and at most 193.
;;If in, the number must be at least 59 and at most 76.
;;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;;pid (Passport ID) - a nine-digit number, including leading zeroes.

(defn validate-field [[field value]]
  (condp = field
    "cid" true
    "byr" (<= 1920 (Integer/parseInt value) 2002)
    "iyr" (<= 2010 (Integer/parseInt value) 2020)
    "eyr" (<= 2020 (Integer/parseInt value) 2030)
    "hgt" (let [matches (re-matches #"\d+(cm|in)" value)
                num (string/replace value #"(cm|in)" "")]
            (when matches
              (if (= (second matches) "in")
                (<= 59 (Integer/parseInt num) 76)
                (<= 150 (Integer/parseInt num) 193))))
    "hcl" (re-matches #"#[\da-f]{6}" value)
    "ecl" (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} value)
    "pid" (re-matches #"\d{9}" value)))

(defn validate-2 [entry]
  (let [entry (assoc entry "cid" :whatever)]
    (and (= (->> entry
                 (filter #(#{"byr" "iyr"
                             "eyr" "hgt"
                             "hcl" "ecl"
                             "pid"} (first %)))
                 count) 7)
         (= (->> entry
                 (filter validate-field)
                 count)
            8))))

(defn solve-1 [input]
  (->> (string/split input #"\n\n")
       (map ->map)
       (filter validate)
       count))

(defn solve-2 [input]
  (->> (string/split input #"\n\n")
       (map ->map)
       (filter validate-2)
       count))
