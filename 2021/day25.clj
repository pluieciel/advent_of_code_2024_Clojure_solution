;advent-of-code-2021.day25
(ns day25
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(defn move-east [m]
  (let [canmove (for [r (range h) c (range w)
                      :when (and (= \> (get-in m [r c]))
                                 (= \. (get-in m [r (mod (inc c) w)])))]
                  [r c])]
    (reduce
     (fn [acc [r c]]
       (-> acc (assoc-in [r c] \.) (assoc-in [r (mod (inc c) w)] \>)))
     m
     canmove)))

(defn move-south [m]
  (let [canmove (for [r (range h) c (range w)
                      :when (and (= \v (get-in m [r c]))
                                 (= \. (get-in m [(mod (inc r) h) c])))]
                  [r c])]
    (reduce
     (fn [acc [r c]]
       (-> acc (assoc-in [r c] \.) (assoc-in [(mod (inc r) h) c] \v)))
     m
     canmove)))

(def Map (parse "2021/in25"))
(def h (count Map))
(def w (count (first Map)))

(loop [m Map cnt 1]
  (let [newm (move-south (move-east m))]
    (if (= m newm)
      cnt
      (recur newm (inc cnt)))))