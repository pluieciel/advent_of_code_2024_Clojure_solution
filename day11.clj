;advent-of-code-2024.day-11
(ns day11
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #" "))
       (map read-string)))

(def calc
  (memoize
   (fn [num step]
     (cond
       (zero? step) 1
       (zero? num) (calc 1 (dec step))
       (even? (count (str num))) (let [s (str num)
                                       half (/ (count s) 2)]
                                   (reduce #(+ %1 (calc (Integer/parseInt %2) (dec step)))
                                           0 [(subs s 0 half) (subs s half)]))
       :else (calc (* num 2024) (dec step))))))

(defn calc-all [step lst]
  (reduce #(+ %1 (calc %2 step)) 0 lst))

;part 1
(->> (parse "input")
     ((partial calc-all 25))
     println)
;part 2
(->> (parse "input")
     ((partial calc-all 75))
     println)