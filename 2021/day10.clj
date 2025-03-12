;advent-of-code-2021.day10
(ns day10
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines))

(def data (parse "2021/in10"))
(def score {")" 3 "]" 57 "}" 1197 ">" 25137})
(def score2 {\( 1 \[ 2 \{ 3 \< 4})

(defn checkline [line]
  (loop [doing line]
    (let [new (str/replace doing #"\(\)|\[\]|\{\}|\<\>" "")]
      (if (= new doing)
        new
        (recur new)))))

;part 1
(->> data
     (map checkline)
     (map #(re-find #"[\)\]\}\>]" %))
     (map #(get score % 0))
     (reduce +))

;part 2
(defn cal [l]
  (loop [todo (reverse l) res 0]
    (if (empty? todo)
      res
      (recur (rest todo) (+ (* 5 res) (score2 (first todo)))))))

(->> data
     (map checkline)
     (remove #(re-find #"[\)\]\}\>]" %))
     (map cal)
     ((fn [l] (nth (sort l) (/ (dec (count l)) 2)))))