;advent-of-code-2024.day-11
(ns day11
  (:require [clojure.string :as str]))

(def length
  (memoize
   (fn [num step]
     (cond
       (zero? step) 1
       (zero? num) (length 1 (dec step))
       (even? (count (str num))) (let [s (str num)
                                       half (/ (count s) 2)]
                                   (reduce + (map #(length % (dec step))
                                                  (map #(Integer/parseInt %)
                                                       [(subs s 0 half) (subs s half)]))))
       :else (length (* num 2024) (dec step))))))

;part 1
(->> (slurp "input")
     (#(str/split % #" "))
     (map read-string)
     (reduce #(+ %1 (length %2 25)) 0)
     println)
;part 2
(->> (slurp "input")
     (#(str/split % #" "))
     (map read-string)
     (reduce #(+ %1 (length %2 75)) 0)
     println)