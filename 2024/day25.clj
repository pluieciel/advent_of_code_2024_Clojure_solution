;advent-of-code-2024.day-25
(ns day25
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #"\n\n"))
       (map (fn [one]
              (let [[f & r] (str/split-lines one)
                    lock? (= f "#####")]
                [lock?
                 (apply mapv
                        (fn [& l]
                          (let [num (count (filter #(= % \#) l))]
                            (if lock? (- 5 num) num)))
                        (butlast r))])))
       (group-by first)
       (reduce-kv #(assoc %1 %2 (map second %3)) {})))

(let [{locks true keys false} (parse "input")]
  (->> (for [l locks] (count (filter #(every? true? (map >= l %)) keys)))
       (reduce +)))