;advent-of-code-2020.day02
(ns day02
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(->> (str/split % #"-|:? ")))))

(def data (parse "2020/in02"))

;part 1
(->> (for [[a b t w] data
           :let [a (read-string a)
                 b (read-string b)
                 res (count (filter #(= % (first t)) w))]
           :when (<= a res b)]
       1)
     (reduce +))

;part 2
(->> (for [[a b t w] data
           :let [a (read-string a)
                 b (read-string b)
                 target (first t)
                 v (vec w)
                 res (set [(v (dec a)) (v (dec b))])]
           :when (and (= (count res) 2) (res target))]
       1)
     (reduce +))