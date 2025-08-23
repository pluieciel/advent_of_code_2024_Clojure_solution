;advent-of-code-2019.day03
(ns day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [l]
  (->> (str/split l #",")
       (map #(map read-string (re-seq #"[A-Z]|\d+" %)))))

(let [[l1 l2] (str/split-lines (slurp "2019/in03"))]
  (def l1 (parse l1))
  (def l2 (parse l2)))

(defn process [ops]
  (reduce
   (fn [acc [dir len]]
     (let [[[x y] step] (last acc)]
       (into acc
             (for [len (range 1 (inc len))]
               [(case dir
                  R [(+ x len) y]
                  L [(- x len) y]
                  U [x (+ y len)]
                  D [x (- y len)])
                (+ step len)]))))
   [[[0 0] 0]]
   ops))

(def intersections
  (->> (apply set/intersection (map #(set (map first (process %))) [l1 l2]))
       (#(disj % [0 0]))))

;; part 1
(->> intersections
     (map #(transduce (map Math/abs) + %))
     (apply min))

;; part 2
(defn make-map [pos-steps]
  (->> (for [[pos step] pos-steps
             :when (intersections pos)]
         {pos step})
       (apply merge-with min)))

(->> (merge-with +
                 (make-map (process l1))
                 (make-map (process l2)))
     (apply min-key val))


;; (defn process
;;   "[[[x1 y1] [x2 y2]] [[x2 y2] [x3 y3]] ...]"
;;   [ops]
;;   (->> (reduce
;;         (fn [acc [dir len]]
;;           (let [[x y] (last acc)]
;;             (conj acc
;;                   (case dir
;;                     R [(+ x len) y]
;;                     L [(- x len) y]
;;                     U [x (+ y len)]
;;                     D [x (- y len)]))))
;;         [[0 0]]
;;         ops)
;;        (partition 2 1)))

;; (let [[l1 l2] (map process [l1 l2])]
;;   (def intersections
;;     (for [[[a b] [c d]] l1 [[e f] [g h]] l2
;;           :when (or (and (= a c) (= f h)) (and (= b d) (= e g)))
;;           :let [[xv yv1 yv2] (if (= a c) [a b d] [e f h])
;;                 [xh1 xh2 yh] (if (= a c) [e g f] [a c b])
;;                 [yv1 yv2] (sort [yv1 yv2])
;;                 [xh1 xh2] (sort [xh1 xh2])]
;;           :when (and (< yv1 yh yv2) (< xh1 xv xh2))
;;           :when (not (= xv yh 0))]
;;       [xv yh])))

;; (->> intersections
;;      (map #(transduce (map Math/abs) + %))
;;      (apply min))