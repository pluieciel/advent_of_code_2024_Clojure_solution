;advent-of-code-2022.day25
(ns day25
  (:require [clojure.string :as str]))

(def dict {\1 1 \2 2 \0 0 \- -1 \= -2})

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(->> (map dict %) reverse))))

(defn myconvert [s]
  (loop [s s res '() mem 0]
    (if (empty? s)
      res
      (let [n (Character/digit (last s) 10)
            n (+ n mem)]
        (case n
          0 (recur (butlast s) (conj res \0) 0)
          1 (recur (butlast s) (conj res \1) 0)
          2 (recur (butlast s) (conj res \2) 0)
          3 (recur (butlast s) (conj res \=) 1)
          4 (recur (butlast s) (conj res \-) 1)
          5 (recur (butlast s) (conj res \0) 1))))))

(let [data (parse "2022/in25")
      base (iterate #(* 5 %) 1)]
  (->> data
       (map #(reduce + (map * % base)))
       (reduce +)
       (#(Long/toString % 5))
       myconvert
       (apply str)))