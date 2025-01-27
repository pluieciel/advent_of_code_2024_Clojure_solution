;advent-of-code-2022.day03
(ns day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines))
;part 1
(let [data (parse "./2022/in03")]
  (->> data
       (map (fn [line]
              (let [length (count line)
                    n (quot (inc length) 2)
                    a (set (take n line))
                    b (set (drop n line))
                    final (first (set/intersection a b))]
                (->> final
                     int
                     (#(if (>= % 97) (- % 96) (- % 38)))))))
       (reduce +)))
;part 2
(let [data (parse "./2022/in03")]
  (->> data
       (partition 3)
       (map (fn [lines]
              (->> lines
                   (map set)
                   (apply set/intersection)
                   first
                   int
                   (#(if (>= % 97) (- % 96) (- % 38))))))
       (reduce +)))