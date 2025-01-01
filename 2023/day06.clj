;advent-of-code-2023.day06
(ns day06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (map #(map read-string (re-seq #"\d+" %)))
       (apply map vector)))

(defn cal [[n tar]]
  (let [mid (quot n 2)
        cannot (loop [cur 0]
                 (let [c (inc cur)]
                   (if (<= (* c (- n c)) tar)
                     (recur (inc cur))
                     cur)))]
    (- (dec n) (* 2 cannot))))
;part 1
(->> (parse "./2023/in06")
     (map cal)
     (reduce *))

(defn parse2 [input]
  (->> (slurp input)
       str/split-lines
       (map #(map read-string (re-seq #"\d+" (str/replace % #" " ""))))
       (apply map vector)))
;part 2
(let [[pair] (parse2 "./2023/in06")]
  (cal pair))