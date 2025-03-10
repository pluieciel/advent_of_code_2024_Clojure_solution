;advent-of-code-2021.day06
(ns day06
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #",")
       (map read-string)))

(def data (parse "2021/in06"))
(def default (into {} (map #(vector % 0) (range 9))))
(def init (->> data frequencies (merge default)))

(defn cal [day]
  (loop [cnt day dict init]
    (if (zero? cnt)
      (->> dict vals (reduce +))
      (recur
       (dec cnt)
       (let [{zero 0 one 1 two 2 three 3 four 4
              five 5 six 6 seven 7 eight 8} dict]
         {0 one 1 two 2 three 3 four 4 five
          5 six 6 (+ seven zero) 7 eight 8 zero})))))
;part 1
(cal 80)

;part 2
(cal 256)