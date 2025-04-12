;advent-of-code-2020.day06
(ns day06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (map str/split-lines)))

(def data (parse "2020/in06"))

;part 1
(->> data
     (map #(count (set (apply str %))))
     (reduce +))

;part 2
(->> data
     (map #(->> (map set %)
                (apply set/intersection)
                count))
     (reduce +))