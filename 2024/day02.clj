;advent-of-code-2024.day-02
(ns day02
  (:require [clojure.string :as str]))

;;part 1
(->> (slurp "2024/in02")
     str/split-lines
     (map (fn [line]
            (->> (re-seq #"\d+" line)
                 (map read-string)
                 (partition 2 1)
                 (map #(apply - %)))))
     (filter #(or (every? #{1 2 3} %) (every? #{-1 -2 -3} %)))
     count)

;;part 2
(->> (slurp "2024/in02")
     str/split-lines
     (map (fn [line]
            (->> (re-seq #"\d+" line)
                 (map read-string)
                 ((fn [line]
                    (for [i (range (count line))]
                      (->> (concat (take i line) (drop (inc i) line))
                           (partition 2 1)
                           (map #(apply - %))))))
                 (some #(or (every? #{1 2 3} %) (every? #{-1 -2 -3} %))))))
     (keep identity)
     count)