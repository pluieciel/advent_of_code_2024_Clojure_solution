;advent-of-code-2020.day24
(ns day24
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(re-seq #"se|sw|ne|nw|e|w" %))))

(def lines (parse "2020/in24"))

(defn cal-one [line]
  (reduce (fn [[x y] dir]
            (case dir
              "se" [(inc x) (dec y)]
              "sw" [(dec x) (dec y)]
              "ne" [(inc x) (inc y)]
              "nw" [(dec x) (inc y)]
              "e" [(+ x 2) y]
              "w" [(- x 2) y]))
          [0 0] line))

(def blacks
  (reduce (fn [blacks line]
            (let [tar (cal-one line)]
              (if (blacks tar)
                (disj blacks tar)
                (conj blacks tar))))
          #{} lines))

;;part 1
(count blacks)

;;part 2
(def neighbors [[-1 -1] [1 1] [-1 1] [1 -1] [-2 0] [2 0]])

(defn step [blacks]
  (let [xs (map first blacks)
        ys (map second blacks)]
    (->> (for [x (range (dec (apply min xs)) (+ 2 (apply max xs)))
               y (range (dec (apply min ys)) (+ 2 (apply max ys)))
               :when (and (even? (+ x y))
                          (->> (map #(mapv + [x y] %) neighbors)
                               (filter blacks)
                               (#(if (blacks [x y])
                                   (<= 1 (count %) 2)
                                   (= 2 (count %))))))]
           [x y])
         set)))

(->> (iterate step blacks)
     (#(nth % 100))
     count)