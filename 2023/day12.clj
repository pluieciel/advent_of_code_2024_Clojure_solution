;advent-of-code-2023.day12
(ns day12
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map (fn [line]
              (let [[a b] (str/split line #" ")]
                [(vec a) (map read-string (re-seq #"\d+" b))])))))

;part 1
(defn calc [lst curr nums]
  (cond
    (and (every? #{\. \?} lst) (empty? nums) (zero? curr)) 1
    (and (empty? lst) (= 1 (count nums)) (= curr (first nums))) 1
    (or (empty? lst) (empty? nums)) 0
    (and (zero? curr) (= \. (first lst))) (calc (rest lst) 0 nums)
    (and (zero? curr) (= \? (first lst))) (+ (calc (rest lst) 0 nums) (calc (rest lst) 1 nums))
    (and (or (= curr (first nums)) (and (zero? curr) (empty? nums))) (#{\. \?} (first lst))) (calc (rest lst) 0 (rest nums))
    (and (< curr (first nums)) (#{\# \?} (first lst))) (calc (rest lst) (inc curr) nums)
    :else 0))

(->> (parse "./2023/in12")
     (map (fn [[lst nums]]
       (calc lst 0 nums)))
     (reduce +))

;part 2
(def cal
  (memoize
   (fn [lst nums]
     (cond
       (and (empty? nums) (every? #{\? \.} lst)) 1
       (or (empty? nums) (< (count lst) (first nums))) 0
       (= (first lst) \#) (let [[n & r] nums]
                            (if (every? #{\# \?} (take n lst))
                              (cond
                                (= (count lst) n) (if (nil? r) 1 0)
                                (#{\. \?} (nth lst n)) (cal (drop (inc n) lst) r)
                                :else 0)
                              0))
       (= (first lst) \.) (cal (rest lst) nums)
       (= (first lst) \?) (let [[n & r] nums]
                            (if (every? #{\# \?} (take n lst))
                              (cond
                                (= (count lst) n) (if (nil? r) 1 0)
                                (#{\. \?} (nth lst n)) (+ (cal (drop (inc n) lst) r) (cal (rest lst) nums))
                                :else (cal (rest lst) nums))
                              (cal (rest lst) nums)))
       :else 0))))

(->> (parse "./2023/in12")
     (map
      (fn [[lst nums]]
        (cal
         (->> (apply str lst) (repeat 5) (str/join "?") vec)
         (apply concat (repeat 5 nums)))))
     (reduce +))