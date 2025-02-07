;advent-of-code-2022.day15
(ns day15
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(map read-string (re-seq #"-?\d+" %)))))

(defn dist [x1 y1 x2 y2]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))
;part 1
(let [data (parse "./2022/in15")
      target 2000000
      occ (atom '())
      cntB (atom #{})]
  (doseq [[x1 y1 x2 y2] data]
    (when (= y2 target) (swap! cntB conj [x2 y2]))
    (let [dia (dist x1 y1 x2 y2)
          dif (Math/abs (- target y1))
          more (- dia dif)]
      (when (>= more 0)
        (swap! occ conj [(- x1 more) (+ x1 more)]))))
  (let [ss (sort-by first @occ)]
    (->> (loop [todo (rest ss) res [(first ss)]]
           (if (empty? todo)
             res
             (let [[a b] (last res)
                   [c d] (first todo)]
               (cond
                 (> c b) (recur (rest todo) (conj res (first todo)))
                 (<= d b) (recur (rest todo) res)
                 :else (recur (rest todo) (conj (vec (butlast res)) [a d]))))))
         ((fn [[[a b]]]
            (inc (- b a))))
         (#(- % (count @cntB))))))

(defn ok? [[x y] [xx yy dis]]
  (> (dist x y xx yy) dis))

(defn inter [a b]
  [(/ (- b a) 2) (/ (+ a b) 2)])
;part 2
(let [data (parse "./2022/in15")]
  (let [occ (atom '())
        a (atom '())
        b (atom '())]
    (doseq [[x1 y1 x2 y2] data]
      (let [dia (dist x1 y1 x2 y2)]
        (swap! occ conj [x1 y1 dia])
        (swap! a conj (+ (- x1) y1 dia 1))
        (swap! a conj (+ (- x1) y1 (- dia) -1))
        (swap! b conj (+ x1 y1 dia 1))
        (swap! b conj (+ x1 y1 (- dia) -1))))
    (->> (for [i @a j @b
               :let [[p q] (inter i j)]
               :when (every? int? [p q])
               :when (and (<= 0 p 4000000) (<= 0 q 4000000))
               :when (every? (partial ok? [p q]) @occ)]
           [p q])
         first
         (#(let [[x y] %]
             (+ (* x 4000000) y))))))