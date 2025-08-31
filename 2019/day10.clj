;advent-of-code-2019.day10
(ns day10
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "2019/in10")
       str/split-lines
       (mapv vec)))
(def W (count (first data)))
(def H (count data))
(def ast-pos
  (for [x (range W) y (range H)
        :when (= \# (get-in data [y x]))]
    [x y]))

(defn cal-half [[x y] poss]
  (->> (for [[xx yy] poss]
         (/ (- yy y) (- xx x)))
       set
       count))

(defn cal-mid [[_ y] poss]
  (->> poss
       (map second)
       (remove #(= % y))
       (group-by #(cond
                    (< % y) :up
                    (> % y) :down))
       count))

(defn cal-all [[x _ :as pos]]
  (let [{:keys [left middle right]} (group-by
                                     #(let [[xx _] %]
                                        (cond
                                          (< xx x) :left
                                          (= xx x) :middle
                                          (> xx x) :right))
                                     ast-pos)
        [left middle right] (map #(if (nil? %) [] %) [left middle right])]
    (+ (cal-mid pos middle) (cal-half pos left) (cal-half pos right))))

;; part1
(->> (map cal-all ast-pos)
     (apply max))

;; part2
(def position (apply max-key cal-all ast-pos))

(let [[x y] position
      {:keys [left up down right]} (group-by
                                    #(let [[xx yy] %]
                                       (cond
                                         (< xx x) :left
                                         (and (= xx x) (< yy y)) :up
                                         (and (= xx x) (> yy y)) :down
                                         (> xx x) :right))
                                    (remove #(= position %) ast-pos))
      [left up down right] (map #(if (nil? %) [] %) [left up down right])
      up [[0 (sort-by #(- (second %)) up)]]
      down [[0 (sort-by #(second %) down)]]
      left (->> (group-by #(let [[xx yy] %] (/ (- yy y) (- xx x))) left)
                (sort-by key)
                (mapv (fn [[k lst]]
                        [k (vec (sort-by (fn [[xx yy]]
                                           (transduce (map Math/abs) + [(- xx x) (- yy y)])) lst))])))
      right (->> (group-by #(let [[xx yy] %] (/ (- yy y) (- xx x))) right)
                 (sort-by key)
                 (mapv (fn [[k lst]]
                         [k (vec (sort-by (fn [[xx yy]]
                                            (transduce (map Math/abs) + [(- xx x) (- yy y)])) lst))])))]
  (loop [all [up right down left] phase 0 cnt 0]
    (let [doing (all phase)
          len (count doing)
          [done inner-cnt] (loop [v doing idx 0 inner-cnt cnt]
                             (if (< idx len)
                               (if (< inner-cnt 199)
                                 (recur (assoc-in v [idx 1] (rest (get-in v [idx 1]))) (inc idx) (inc inner-cnt))
                                 [(get-in v [idx 1]) 200])
                               [(vec (remove #(empty? (second %)) v)) inner-cnt]))]
      (if (< inner-cnt 199)
        (recur (assoc all phase done) (mod (inc phase) 4) inner-cnt)
        (->> done
             first
             (map * [100 1])
             (reduce +))))))