;advent-of-code-2024.day-25
(ns day25
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #"\n\n"))
       (map (fn [m] (->> m
                         str/split-lines
                         (#(let [[f & r] %
                                 lock? (= f "#####")]
                             (list lock?
                                   (apply mapv (fn [& l]
                                                 (let [num (count (filter (fn [x] (= x \#)) l))]
                                                   (if lock?
                                                     (- 5 num)
                                                     num)))
                                          (butlast r))))))))
       (group-by first)))

(let [{locks true keys false} (parse "input")
      locks (map second locks)
      keys (map second keys)]
  (->> (for [l locks ] (count (filter #(every? true? (map >= l %)) keys)))
       (reduce +)))