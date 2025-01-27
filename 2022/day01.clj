;advent-of-code-2022.day01
(ns day01
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (map #(->> (str/split-lines %)
                  (map read-string)
                  (reduce +)))))

(let [data (parse "./2022/in01")]
  ;part 1
  (apply max data)
  ;part 2
  (->> (sort > data)
       (take 3)
       (reduce +)))