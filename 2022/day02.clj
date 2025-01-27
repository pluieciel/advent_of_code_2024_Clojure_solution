;advent-of-code-2022.day02
(ns day02
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines))
;part 1
(def dict1 {"A X" 3 "A Y" 6 "A Z" 0
           "B X" 0 "B Y" 3 "B Z" 6
           "C X" 6 "C Y" 0 "C Z" 3})
(def dict2 {"X" 1 "Y" 2 "Z" 3})

(let [data (parse "./2022/in02")]
  (->> data
       (map (fn [line]
              (let [[_ b] (re-seq #"\w" line)]
                (+ (dict1 line) (dict2 b)))))
       (reduce +)))
;part 2
(def dict3 {"A X" 3 "A Y" 1 "A Z" 2
           "B X" 1 "B Y" 2 "B Z" 3
           "C X" 2 "C Y" 3 "C Z" 1})
(def dict4 {"X" 0 "Y" 3 "Z" 6})

(let [data (parse "./2022/in02")]
  (->> data
       (map (fn [line]
              (let [[_ b] (re-seq #"\w" line)]
                (+ (dict3 line) (dict4 b)))))
       (reduce +)))