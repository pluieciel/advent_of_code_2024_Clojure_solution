;advent-of-code-2020.day17
(ns day17
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(def init (parse "2020/in17"))
(def h (count init))
(def w (count (first init)))

;part 1
(defn neighbors [x y z]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]
        :when (not= [0 0 0] [dx dy dz])]
    [(+ x dx) (+ y dy) (+ z dz)]))

(loop [round 0 x0 0 x1 w y0 0 y1 h z0 0 z1 1
       state (->> (for [x (range w) y (range h)
                        :when (= \# (get-in init [y x]))]
                    [x y 0])
                  set)]
  (if (= round 6)
    (count state)
    (let [nx0 (dec x0) nx1 (inc x1)
          ny0 (dec y0) ny1 (inc y1)
          nz0 (dec z0) nz1 (inc z1)]
      (recur (inc round)
             nx0 nx1
             ny0 ny1
             nz0 nz1
             (->> (for [x (range nx0 nx1) y (range ny0 ny1) z (range nz0 nz1)
                        :when (if (contains? state [x y z])
                                (#{2 3} (count (filter #(contains? state %) (neighbors x y z))))
                                (= 3 (count (filter #(contains? state %) (neighbors x y z)))))]
                    [x y z])
                  set)))))

;part 2
(defn neighbors2 [x y z w]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] dw [-1 0 1]
        :when (not= [0 0 0 0] [dx dy dz dw])]
    [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))

(loop [round 0 x0 0 x1 w y0 0 y1 h z0 0 z1 1 w0 0 w1 1
       state (->> (for [x (range w) y (range h)
                        :when (= \# (get-in init [y x]))]
                    [x y 0 0])
                  set)]
  (if (= round 6)
    (count state)
    (let [nx0 (dec x0) nx1 (inc x1)
          ny0 (dec y0) ny1 (inc y1)
          nz0 (dec z0) nz1 (inc z1)
          nw0 (dec w0) nw1 (inc w1)]
      (recur (inc round)
             nx0 nx1
             ny0 ny1
             nz0 nz1
             nw0 nw1
             (->> (for [x (range nx0 nx1) y (range ny0 ny1) z (range nz0 nz1) w (range nw0 nw1)
                        :when (if (contains? state [x y z w])
                                (#{2 3} (count (filter #(contains? state %) (neighbors2 x y z w))))
                                (= 3 (count (filter #(contains? state %) (neighbors2 x y z w)))))]
                    [x y z w])
                  set)))))