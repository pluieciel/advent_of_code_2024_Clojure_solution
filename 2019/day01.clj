;advent-of-code-2019.day01
(ns day01
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "2019/in01")
       str/split-lines
       (map read-string)))

(defn step [mass]
  (- (int (/ mass 3)) 2))

;; part 1
(->> input
     (map step)
     (reduce +))

;; part 2
(defn cal-one [mass]
  (->> (iterate step mass)
       (take-while pos?)
       rest
       (reduce +)))

(->> input
     (map cal-one)
     (reduce +))