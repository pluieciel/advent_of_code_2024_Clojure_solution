;advent-of-code-2020.day10
(ns day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map read-string)))

(def raw (parse "2020/in10"))
(def M (apply max raw))
(def dest (+ M 3))
(def data (conj raw 0 dest))

;part 1
(->> data
     (sort >)
     (partition 2 1)
     (map #(apply - %))
     frequencies
     vals
     (apply *))

;part 2
(def dataset (set data))

(def cal
  (memoize
   (fn [n]
     (cond
       (zero? n) 1
       (not (dataset n)) 0
       :else (transduce (map #(cal (- n %))) + [1 2 3])))))

(cal dest)