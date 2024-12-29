;advent-of-code-2024.day-07
(ns day07
  (:require [clojure.string :as str]))

(defn ok? [[target & lst] ops]
  (cond
    (= 1 (count lst)) (= target (first lst))
    (> (first lst) target) false
    :else (let [[fi se & re] lst]
            (some #(ok? (cons target %) ops) (map #(cons (% fi se) re) ops)))))

(let [ops [* +]
      ops2 [* + (fn [a b] (read-string (str a b)))]
      parsed (->> (slurp "input")
                  str/split-lines
                  (map #(map read-string (str/split % #": | "))))]
  ;part 1
  (->> parsed
       (filter #(ok? % ops))
       (map first)
       (reduce +)
       println)
  ;part 2
  (->> parsed
       (filter #(ok? % ops2))
       (map first)
       (reduce +)
       println))
