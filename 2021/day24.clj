;advent-of-code-2021.day24
(ns day24
  (:require [clojure.string :as str]))

(defn grow [z i c]
  (+ (* 26 z) i c))

(declare cando cando2 div div2 mul mul2)

;part 1
(defn div [idx lst z n]
  (let [i (- (mod z 26) n)]
    (if (<= 1 i 9)
      (if-let [newlst (cando (inc idx) (quot z 26) (conj lst i))]
        newlst
        nil)
      nil)))

(defn mul [idx lst z n]
  (loop [i 9]
    (if (zero? i)
      nil
      (if-let [newlst (cando (inc idx) (grow z i n) (conj lst i))]
        newlst
        (recur (dec i))))))

(defn cando [idx z lst]
  (case idx
    14 (let [i (- (mod z 26) 8)]
         (if (<= 1 i 9) (conj lst i) nil))
    13 (div idx lst z 4)
    12 (div idx lst z 5)
    11 (mul idx lst z 7)
    10 (div idx lst z 1)
    9 (mul idx lst z 6)
    8 (div idx lst z 13)
    7 (mul idx lst z 7)
    6 (div idx lst z 5)
    5 (div idx lst z 8)
    4 (mul idx lst z 11)
    3 (mul idx lst z 1)
    2 (mul idx lst z 11)
    1 (mul idx lst z 1)))

(->> (cando 1 0 [])
     (str/join ""))

;part 2
(defn div2 [idx lst z n]
  (let [i (- (mod z 26) n)]
    (if (<= 1 i 9)
      (if-let [newlst (cando2 (inc idx) (quot z 26) (conj lst i))]
        newlst
        nil)
      nil)))

(defn mul2 [idx lst z n]
  (loop [i 1]
    (if (= 10 i)
      nil
      (if-let [newlst (cando2 (inc idx) (grow z i n) (conj lst i))]
        newlst
        (recur (inc i))))))

(defn cando2 [idx z lst]
  (case idx
    14 (let [i (- (mod z 26) 8)]
         (if (<= 1 i 9) (conj lst i) nil))
    13 (div2 idx lst z 4)
    12 (div2 idx lst z 5)
    11 (mul2 idx lst z 7)
    10 (div2 idx lst z 1)
    9 (mul2 idx lst z 6)
    8 (div2 idx lst z 13)
    7 (mul2 idx lst z 7)
    6 (div2 idx lst z 5)
    5 (div2 idx lst z 8)
    4 (mul2 idx lst z 11)
    3 (mul2 idx lst z 1)
    2 (mul2 idx lst z 11)
    1 (mul2 idx lst z 1)))

(->> (cando2 1 0 [])
     (str/join ""))