;advent-of-code-2021.day03
(ns day03
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines))

;part 1
(let [data (parse "2021/in03")]
  (->> data
       (apply map vector)
       (map (fn [l] (map first (sort-by val > (frequencies l)))))
       (apply map vector)
       (map #(Integer/parseInt (apply str %) 2))
       (reduce *)))

;part 2
(defn cal [data comp equal]
  (loop [todo data idx 0]
    (if (= 1 (count todo))
      (Integer/parseInt (first todo) 2)
      (let [choice (->> (map #(subs % idx (inc idx)) todo)
                        frequencies
                        (sort-by val comp)
                        ((fn [[[a na] [_ nb]]] (if (= na nb) equal a))))]
        (recur (filter #(= choice (subs % idx (inc idx))) todo) (inc idx))))))

(let [data (parse "2021/in03")]
 (* (cal data > "1") (cal data < "0")))