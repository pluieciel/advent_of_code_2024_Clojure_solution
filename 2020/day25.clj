;advent-of-code-2020.day24
(ns day25
  (:require [clojure.string :as str]))

(let [[card door]
      (->> (slurp "2020/in25")
           str/split-lines
           (map read-string))]
  (def card card)
  (def door door))

(def sub-num 7)

(defn step [sub-num value]
  (-> (* sub-num value)
      (mod 20201227)))

(->> (iterate (partial step sub-num) 1)
     (map vector (range))
     (drop-while #(not= door (second %)))
     ffirst
     ((fn [door-loop-size]
        (->> (iterate (partial step card) 1)
             (#(nth % door-loop-size))))))