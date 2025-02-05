;advent-of-code-2022.day13
(ns day13
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (map (fn [pair] (->> (str/split-lines pair)
                            (map read-string))))))

(defn cal [[la lb]]
  (cond
    (= la lb) nil
    (every? nil? [la lb]) nil
    (nil? la) true
    (nil? lb) false
    (every? number? [la lb]) (cond (< la lb) true (> la lb) false :else nil)
    (and (number? la) (not (number? lb))) (cal [[la] lb])
    (and (number? lb) (not (number? la))) (cal [la [lb]])
    :else
    (let [a (first la) b (first lb)
          res1 (cal [a b])]
      (cond
        (nil? res1) (cal [(rest la) (rest lb)])
        :else res1))))

(let [data (parse "./2022/in13")]
  (->> data
       (map cal)
       (map-indexed (fn [i x] (if x (inc i) 0)))
       (reduce +)))

(let [data (parse "./2022/in13")]
  (->> data
       (#(conj % [[[2]] [[6]]]))
       (apply concat)
       (sort-by identity #(cal [%1 %2]))
       (map-indexed (fn [i x] [(inc i) x]))
       (filter #(#{[[2]] [[6]]} (second %)))
       (map first)
       (reduce *)))