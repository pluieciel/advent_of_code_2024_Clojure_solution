;advent-of-code-2023.day02
(ns day02
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines))

(defn onerow [row]
  (let [[g l] (str/split row #": ")
        gn (read-string (second (str/split g #" ")))
        d (->> l
               (#(str/split % #"; "))
               (map (fn [l]
                      (->> (str/split l #", ")
                           (map #(let [[n c] (str/split % #" ")]
                                   {c (read-string n)}))
                           (#(conj % {"red" 0 "blue" 0 "green" 0}))
                           (apply merge)
                           (sort-by first)
                           (map second)
                           (map - [14 13 12])
                           (every? #(>= % 0)))))
               (every? true?))]
    (if d
      gn
      0)))
;part 1
(->> (parse "./2023/in02")
     (map onerow)
     (reduce +))

(defn onerow1 [row]
  (let [[g l] (str/split row #": ")
        gn (read-string (second (str/split g #" ")))
        d (->> l
               (#(str/split % #"; "))
               (map (fn [l]
                      (->> (str/split l #", ")
                           (map #(let [[n c] (str/split % #" ")]
                                   {c (read-string n)}))
                           (#(conj % {"red" 0 "blue" 0 "green" 0}))
                           (apply merge))))
               (apply merge-with max)
               vals
               (apply *))]
    d))
;part 2
(->> (parse "./2023/in02")
     (map onerow1)
     (reduce +))