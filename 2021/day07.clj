;advent-of-code-2021.day07
(ns day07
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #",")
       (map read-string)))

(def data (sort (parse "2021/in07")))
(def idx-mid (/ (count data) 2))
(def target (nth data idx-mid))

;part 1
;; median (or near it) is the target, so the sum of abs diff is the answer
(defn tran [n] (/ (* n (inc n)) 2))
(->> data
     (map #(Math/abs (- target %)))
     (reduce +))

;part 2
;; no easy math solution derived from the median. brute force is fast enough
(->> (range (inc (first data)) (last data))
     (map (fn [newtar] (reduce + (map #(tran (Math/abs (- newtar %))) data))))
     (apply min))