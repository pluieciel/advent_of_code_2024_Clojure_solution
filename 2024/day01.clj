;advent-of-code-2024.day-01
(ns day01
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (map #(str/split % #"   "))
       (map #(map read-string %))))

;part 1
(->> (parse "input")
     (apply map list)
     (map sort) 
     (apply map #(->> (- %1 %2) Math/abs))
     (reduce +)
     println)

;part 2
(->> (parse "input")
     (apply map list)
     (#(let [[l1 l2] % freq (frequencies l2)]
         (map (fn [n] (* n (get freq n 0))) l1)))
     (reduce +)
     println)