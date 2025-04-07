;advent-of-code-2020.day01
(ns day01
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map read-string)))

(def data (vec (sort (parse "2020/in01"))))
(def len (count data))

;part 1
(loop [i 0]
  (if-let [res (loop [j (inc i)]
                 (cond
                   (= j len) nil
                   (> (+ (data i) (data j)) 2020) nil
                   (= (+ (data i) (data j)) 2020) [(data i) (data j)]
                   :else (recur (inc j))))]
    (apply * res)
    (recur (inc i))))

;part 2
(loop [i 0]
  (if-let [res (loop [j (inc i)]
                 (if (= j (dec len))
                   nil
                   (if-let [res2 (loop [k (inc j)]
                                   ;(println [(data i) (data j) (data k)])
                                   (cond
                                     (= k len) nil
                                     (> (+ (data i) (data j) (data k)) 2020) nil
                                     (= (+ (data i) (data j) (data k)) 2020) [(data i) (data j) (data k)]
                                     :else (recur (inc k))))]
                     res2
                     (recur (inc j)))))] 
    (apply * res)
    (recur (inc i))))