;advent-of-code-2024.day-22
(ns day22
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input) str/split-lines (map read-string)))

(defn prune [x] (bit-and 16777215 x));same as mod 16777216
(defn mix [v s] (bit-xor v s))
(defn op1 [s] (bit-shift-left s 6))
(defn op2 [s] (bit-shift-right s 5))
(defn op3 [s] (bit-shift-left s 11))

(defn step [s]
  (->> s
       (#(mix (op1 %) %)) prune
       (#(mix (op2 %) %)) prune
       (#(mix (op3 %) %)) prune))
;part 1
(->> (parse "input")
     (transduce (map #(nth (iterate step %) 2000)) +)
     println)
;part 2, a bit slow
(->> (parse "input")
     (map #(->> % (iterate step) (take 2000)))
     (map (fn [lst]
            (->> lst
                 (map #(mod % 10))
                 (partition 5 1)
                 reverse
                 (map #(let [[_ _ _ _ p :as g] %]
                         {(map - (rest g) g) p}))
                 (apply merge))))
     (apply merge-with +)
     (apply max-key val)
     println)