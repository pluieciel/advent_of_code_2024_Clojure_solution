;advent-of-code-2023.day12
(ns day12
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map (fn [line]
              (let [[a b] (str/split line #" ")]
                [(vec a) (map read-string (re-seq #"\d+" b))])))))

(defn calc [lst curr nums]
  ;(println lst curr nums)
  (cond
    (and (every? #{\. \?} lst) (empty? nums) (zero? curr)) 1
    (and (empty? lst) (= 1 (count nums)) (= curr (first nums))) 1
    (or (empty? lst) (empty? nums)) 0
    (and (zero? curr) (= \. (first lst))) (calc (rest lst) 0 nums)
    (and (zero? curr) (= \? (first lst))) (+ (calc (rest lst) 0 nums) (calc (rest lst) 1 nums))
    (and (or (= curr (first nums)) (and (zero? curr) (empty? nums))) (#{\. \?} (first lst))) (calc (rest lst) 0 (rest nums))
    (and (< curr (first nums)) (#{\# \?} (first lst))) (calc (rest lst) (inc curr) nums)
    :else 0))

(->> (parse "./2023/in12") 
     (map (fn [[lst nums]]
       (calc lst 0 nums)))
     (reduce +))
