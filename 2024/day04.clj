;advent-of-code-2024.day-04
(ns day04
  (:require [clojure.string :as str]))
;part 1
(let [target '(\X \M \A \S)
      match? #(or (= target %) (= (reverse target) %))
      find-xmas #(->> % (partition 4 1) (filter match?) count)]
  (->> (slurp "input")
    str/split-lines
    (#(let [table %]
      (concat [table (apply map list table)]
              (for [l4 (partition 4 1 table)]
                (apply map list (map (fn [n l] (drop n l)) (range 4) l4)))
              (for [l4 (partition 4 1 (map reverse table))]
                (apply map list (map (fn [n l] (drop n l)) (range 4) l4))))))
    (map #(map find-xmas %))
    (map #(reduce + %))
    (reduce +)
    println))

;part 2
(let [target '(\M \M \S \S)
      target-set (->> (cycle target) (partition 4 1) (take 4) set)
      match? (fn [[[a _ b][_ c _][d _ e]]]
               (and (= c \A)
                    (target-set (list a b e d))))]
  (->> (slurp "input")
    str/split-lines
    ((fn [table]
      (for [l3 (partition 3 1 table)]
        (->> l3
          (map #(partition 3 1 %))
          (apply map list)
          (filter match?)
          count))))
    (reduce +)
    println))
