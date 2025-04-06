;advent-of-code-2021.day24
(ns day24
  (:require [clojure.string :as str]))

(defn grow [z i c]
  (+ (* 26 z) i c))

(defn cando [idx z lst]
  (case idx
    14 (let [i (- (mod z 26) 8)]
         (if (<= 1 i 9) (conj lst i) nil))
    13 (let [i (- (mod z 26) 4)]
         (if (<= 1 i 9)
           (if-let [newlst (cando (inc idx) (quot z 26) (conj lst i))]
             newlst
             nil)
           nil))
    12 (let [i (- (mod z 26) 5)]
         (if (<= 1 i 9)
           (if-let [newlst (cando (inc idx) (quot z 26) (conj lst i))]
             newlst
             nil)
           nil))
    11 (loop [i 9]
         (if (zero? i)
           nil
           (if-let [newlst (cando (inc idx) (grow z i 7) (conj lst i))]
             newlst
             (recur (dec i)))))
    10 (let [i (- (mod z 26) 1)]
         (if (<= 1 i 9)
           (if-let [newlst (cando (inc idx) (quot z 26) (conj lst i))]
             newlst
             nil)
           nil))
    9 (loop [i 9]
        (if (zero? i)
          nil
          (if-let [newlst (cando (inc idx) (grow z i 6) (conj lst i))]
            newlst
            (recur (dec i)))))
    8 (let [i (- (mod z 26) 13)]
        (if (<= 1 i 9)
          (if-let [newlst (cando (inc idx) (quot z 26) (conj lst i))]
            newlst
            nil)
          nil))
    7 (loop [i 9]
        (if (zero? i)
          nil
          (if-let [newlst (cando (inc idx) (grow z i 7) (conj lst i))]
            newlst
            (recur (dec i)))))
    6 (let [i (- (mod z 26) 5)]
        (if (<= 1 i 9)
          (if-let [newlst (cando (inc idx) (quot z 26) (conj lst i))]
            newlst
            nil)
          nil))
    5 (let [i (- (mod z 26) 8)]
        (if (<= 1 i 9)
          (if-let [newlst (cando (inc idx) (quot z 26) (conj lst i))]
            newlst
            nil)
          nil))
    4 (loop [i 9]
        (if (zero? i)
          nil
          (if-let [newlst (cando (inc idx) (grow z i 11) (conj lst i))]
            newlst
            (recur (dec i)))))
    3 (loop [i 9]
        (if (zero? i)
          nil
          (if-let [newlst (cando (inc idx) (grow z i 1) (conj lst i))]
            newlst
            (recur (dec i)))))
    2 (loop [i 9]
        (if (zero? i)
          nil
          (if-let [newlst (cando (inc idx) (grow z i 11) (conj lst i))]
            newlst
            (recur (dec i)))))
    1 (loop [i 9]
        (if (zero? i)
          nil
          (if-let [newlst (cando (inc idx) (grow z i 1) (conj lst i))]
            newlst
            (recur (dec i)))))))

;part 1
(->> (cando 1 0 [])
     (str/join ""))

;part 2
(defn cando2 [idx z lst]
  (case idx
    14 (let [i (- (mod z 26) 8)]
         (if (<= 1 i 9) (conj lst i) nil))
    13 (let [i (- (mod z 26) 4)]
         (if (<= 1 i 9)
           (if-let [newlst (cando2 (inc idx) (quot z 26) (conj lst i))]
             newlst
             nil)
           nil))
    12 (let [i (- (mod z 26) 5)]
         (if (<= 1 i 9)
           (if-let [newlst (cando2 (inc idx) (quot z 26) (conj lst i))]
             newlst
             nil)
           nil))
    11 (loop [i 1]
         (if (= 10 i)
           nil
           (if-let [newlst (cando2 (inc idx) (grow z i 7) (conj lst i))]
             newlst
             (recur (inc i)))))
    10 (let [i (- (mod z 26) 1)]
         (if (<= 1 i 9)
           (if-let [newlst (cando2 (inc idx) (quot z 26) (conj lst i))]
             newlst
             nil)
           nil))
    9 (loop [i 1]
        (if (= 10 i)
          nil
          (if-let [newlst (cando2 (inc idx) (grow z i 6) (conj lst i))]
            newlst
            (recur (inc i)))))
    8 (let [i (- (mod z 26) 13)]
        (if (<= 1 i 9)
          (if-let [newlst (cando2 (inc idx) (quot z 26) (conj lst i))]
            newlst
            nil)
          nil))
    7 (loop [i 1]
        (if (= 10 i)
          nil
          (if-let [newlst (cando2 (inc idx) (grow z i 7) (conj lst i))]
            newlst
            (recur (inc i)))))
    6 (let [i (- (mod z 26) 5)]
        (if (<= 1 i 9)
          (if-let [newlst (cando2 (inc idx) (quot z 26) (conj lst i))]
            newlst
            nil)
          nil))
    5 (let [i (- (mod z 26) 8)]
        (if (<= 1 i 9)
          (if-let [newlst (cando2 (inc idx) (quot z 26) (conj lst i))]
            newlst
            nil)
          nil))
    4 (loop [i 1]
        (if (= 10 i)
          nil
          (if-let [newlst (cando2 (inc idx) (grow z i 11) (conj lst i))]
            newlst
            (recur (inc i)))))
    3 (loop [i 1]
        (if (= 10 i)
          nil
          (if-let [newlst (cando2 (inc idx) (grow z i 1) (conj lst i))]
            newlst
            (recur (inc i)))))
    2 (loop [i 1]
        (if (= 10 i)
          nil
          (if-let [newlst (cando2 (inc idx) (grow z i 11) (conj lst i))]
            newlst
            (recur (inc i)))))
    1 (loop [i 1]
        (if (= 10 i)
          nil
          (if-let [newlst (cando2 (inc idx) (grow z i 1) (conj lst i))]
            newlst
            (recur (inc i)))))))

(->> (cando2 1 0 [])
     (str/join ""))