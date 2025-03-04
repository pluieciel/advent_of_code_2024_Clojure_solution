;advent-of-code-2021.day01
(ns day01
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map read-string)))

(def data (parse "2021/in01"))

;part 1
(->> data
     (partition 2 1)
     (filter (fn [[a b]] (< a b)))
     count)

;part 2
(->> data
     (partition 3 1)
     (map #(reduce + %))
     (partition 2 1)
     (filter (fn [[a b]] (< a b)))
     count)