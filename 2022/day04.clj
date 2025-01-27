;advent-of-code-2022.day03
(ns day03
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines))
;part 1
(let [data (parse "./2022/in04")]
  (->> data
       (map (fn [line]
              (let [[a b c d] (map read-string (re-seq #"\d+" line))]
                (if (or (<= a c d b) (<= c a b d)) 1 0))))
       (reduce +)))
;part 2
(let [data (parse "./2022/in04")]
  (->> data
       (map (fn [line]
              (let [[a b c d] (map read-string (re-seq #"\d+" line))]
                (if (or (< b c) (< d a)) 0 1))))
       (reduce +)))