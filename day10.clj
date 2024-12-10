;advent-of-code-2024.day-10
(ns day10
  (:require [clojure.string :as str]))

(let [Map (->> (slurp "input")
               str/split-lines
               (mapv #(vec (map (fn [c] (Character/digit c 10)) %))))
      starts (for [y (range (count Map))
                   x (range (count (first Map)))
                   :when (zero? (get-in Map [y x]))]
               [[y x]])
      step (fn [pos]
             (->> [[-1 0] [1 0] [0 -1] [0 1]]
                  (map #(mapv + pos %))
                  (filter #(= (inc (get-in Map pos)) (get-in Map %)))))
      poss-step (fn [poss]
                  (->> poss
                       (mapcat step)))
      start->dest (fn [start]
                    (loop [n 0 poss start]
                      (if (= n 9)
                        (count poss)
                        (recur (inc n) (set (poss-step poss))))))
      start->dest2 (fn [start]
                    (loop [n 0 poss start]
                      (if (= n 9)
                        (count poss)
                        (recur (inc n) (poss-step poss)))))]
  ;part 1
  (->> starts
       (map start->dest)
       (reduce +)
       println)
  ;part 2
  (->> starts
       (map start->dest2)
       (reduce +)
       println))