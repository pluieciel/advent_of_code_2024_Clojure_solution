;advent-of-code-2024.day-13
(ns day13
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #"\n\n")) 
       (map #(map read-string (re-seq #"\d+" %)))))

(defn solve [[a1 b1 a2 b2 a b]]
  (let [ratio (/ a1 b1)
        c2 (* b2 ratio)
        c (* b ratio)
        y (/ (- a c) (- a2 c2))
        x (/ (- a (* y a2)) a1)]
    (when (every? integer? [x y]) [x y])))

;part 1
(->> (parse "input")
     (keep solve)
     (map #(let [[x y] %] (+ (* 3 x) y)))
     (reduce +)
     println)

;part 2
(->> (parse "input")
     (map #(let [[a b c d e f] %]
             [a b c d (+ 10000000000000 e) (+ 10000000000000 f)]))
     (keep solve)
     (map #(let [[x y] %] (+ (* 3 x) y)))
     (reduce +)
     println)