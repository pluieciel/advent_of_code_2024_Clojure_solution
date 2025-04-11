;advent-of-code-2020.day05
(ns day05
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines))

(defn cal [lst l r len]
  (if (= len 1)
    l
    (let [flag (first lst)
          nlen (/ len 2)]
      (if (#{\F \L} flag)
        (cal (rest lst) l (- r nlen) nlen)
        (cal (rest lst) (+ l nlen) r nlen)))))

(defn seat-id [code]
  (let [row (take 7 code)
        col (drop 7 code)]
    (+ (* 8 (cal row 0 127 128)) (cal col 0 7 8))))

;part 1
(->> (map seat-id (parse "2020/in05"))
     (apply max))

;part 2
(->> (map seat-id (parse "2020/in05"))
     sort
     (partition 2 1)
     (remove #(= 1 (- (second %) (first %)))))