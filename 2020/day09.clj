;advent-of-code-2020.day09
(ns day09
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv #(read-string %))))

(def data (parse "2020/in09"))

;part 1
(loop [lst data]
  (let [l26 (take 26 lst)
        l25 (butlast l26)
        target (last l26)
        ok? (some true?
                  (for [a l25 b l25 :when (not= a b)]
                    (= (+ a b) target)))]
    (if ok?
      (recur (rest lst))
      target)))

;part 2
(def total 507622668)

(let [final (loop [lst data]
              (if-let [res (loop [n 0 sum 0 l lst]
                             (cond
                               (= sum total) (take n lst)
                               (> sum total) nil
                               :else (recur (inc n) (+ sum (first l)) (rest l))))]
                res
                (recur (rest lst))))]
  (+ (apply min final) (apply max final)))