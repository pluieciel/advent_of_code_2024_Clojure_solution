;advent-of-code-2020.day08
(ns day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv #(let [[op arg] (str/split % #" ")]
                [op (read-string arg)]))))

(def data (parse "2020/in08"))

;part 1
(loop [idx 0 acc 0 seen #{}]
  (if (seen idx)
    acc
    (let [[op arg] (get data idx)]
      (case op
        "acc" (recur (inc idx) (+ acc arg) (conj seen idx))
        "jmp" (recur (+ idx arg) acc (conj seen idx))
        "nop" (recur (inc idx) acc (conj seen idx))))))

;part 2
(defn check [data]
  (loop [idx 0 acc 0 seen #{}]
    (cond
      (seen idx) false
      (= idx (count data)) acc
      :else
      (let [[op arg] (get data idx)]
        (case op
          "acc" (recur (inc idx) (+ acc arg) (conj seen idx))
          "jmp" (recur (+ idx arg) acc (conj seen idx))
          "nop" (recur (inc idx) acc (conj seen idx)))))))

(let [tochange (->> data
                    (map-indexed vector)
                    (remove #(= "acc" (first (second %))))
                    (map first))]
  (for [i tochange
        :let [newdata (update-in data [i 0] #(case % "jmp" "nop" "nop" "jmp"))
              res (check newdata)]
        :when res]
    res))