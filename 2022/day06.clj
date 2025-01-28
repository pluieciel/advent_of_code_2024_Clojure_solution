;advent-of-code-2022.day06
(ns day06
  (:require [clojure.string :as str]))

(defn parse [i] (slurp i))
;part 1
(let [data (parse "./2022/in06")]
  (->> data
       (partition 4 1)
       (map-indexed (fn [i v] [v i]))
       (filter #(= 4 (count (distinct (first %)))))
       first
       second
       (#(+ 4 %))))
;part 2
(let [data (parse "./2022/in06")]
  (->> data
       (partition 14 1)
       (map-indexed (fn [i v] [v i]))
       (filter #(= 14 (count (distinct (first %)))))
       first
       second
       (#(+ 14 %))))