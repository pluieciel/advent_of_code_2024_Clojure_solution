;advent-of-code-2019.day05
(ns day05
  (:require [clojure.string :as str]))

(def input
  (->> (str/split (slurp "2019/in05") #",")
       (map read-string)
       (map vector (range))
       (into {})))

(defn calone [op dict pos modes]
  (assoc dict
         (get dict (+ pos 3))
         (op (if (zero? (get modes 0 0))
               (dict (dict (+ pos 1)))
               (dict (+ pos 1)))
             (if (zero? (get modes 1 0))
               (dict (dict (+ pos 2)))
               (dict (+ pos 2))))))

(defn cal [dict pos]
  (if (= 99 (get dict pos))
    nil
    (let [op-long (get dict pos)
          [op & modes] (->> op-long
                            str
                            (re-seq #"\d\d$|\d")
                            reverse
                            (map read-string))
          modes (vec modes)]
      (case op
        1 (cal (calone + dict pos modes) (+ pos 4))
        2 (cal (calone * dict pos modes) (+ pos 4))
        3 (cal (assoc dict (dict (inc pos)) 1) (+ pos 2))
        4 (do (println (dict (dict (inc pos)))) (cal dict (+ pos 2)))))))

;; part1
(cal input 0)

;; part2
(defn calcmp [op dict pos modes]
  (assoc dict
         (get dict (+ pos 3))
         (if (op (if (zero? (get modes 0 0))
                   (dict (dict (+ pos 1)))
                   (dict (+ pos 1)))
                 (if (zero? (get modes 1 0))
                   (dict (dict (+ pos 2)))
                   (dict (+ pos 2))))
           1 0)))

(defn cal2 [dict pos]
  (if (= 99 (get dict pos))
    nil
    (let [op-long (get dict pos)
          [op & modes] (->> op-long
                            str
                            (re-seq #"\d\d$|\d")
                            reverse
                            (map Integer/parseInt))
          modes (vec modes)]
      (case op
        1 (cal2 (calone + dict pos modes) (+ pos 4))
        2 (cal2 (calone * dict pos modes) (+ pos 4))
        3 (cal2 (assoc dict (dict (inc pos)) 5) (+ pos 2))
        4 (do (println (dict (dict (inc pos)))) (cal2 dict (+ pos 2)))
        5 (cal2 dict (let [arg (if (zero? (get modes 0 0))
                                (dict (dict (+ pos 1)))
                                (dict (+ pos 1)))]
                      (if (zero? arg)
                        (+ pos 3)
                        (if (zero? (get modes 1 0))
                          (dict (dict (+ pos 2)))
                          (dict (+ pos 2))))))
        6 (cal2 dict (let [arg (if (zero? (get modes 0 0))
                                (dict (dict (+ pos 1)))
                                (dict (+ pos 1)))]
                      (if (zero? arg)
                        (if (zero? (get modes 1 0))
                          (dict (dict (+ pos 2)))
                          (dict (+ pos 2)))
                        (+ pos 3))))
        7 (cal2 (calcmp < dict pos modes) (+ pos 4))
        8 (cal2 (calcmp = dict pos modes) (+ pos 4))))))

(cal2 input 0)