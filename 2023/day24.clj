;advent-of-code-2023.day24
(ns day24
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(map read-string (re-seq #"-?\d+" %)))))
;part 1
(defn calcross [[x1 y1 _ vx1 vy1 _] [x2 y2 _ vx2 vy2 _]]
  (if (= (/ vy1 vx1) (/ vy2 vx2))
    false
    (let [x (/ (+ (* (/ vx2 vx1) vy1 x1) (- (* vy2 x2)) (* vx2 (- y2 y1)))
               (- (* (/ vx2 vx1) vy1) vy2))
          y (+ (* (/ vy2 vx2) (- x x2)) y2)]
      (if (and (pos? (/ (- x x1) vx1))
               (pos? (/ (- x x2) vx2))
               (<= 200000000000000 x 400000000000000)
               (<= 200000000000000 y 400000000000000))
        true
        false))))

(let [data (parse "./2023/in24")]
  (->> data
       ((fn [d]
          (loop [todo d cnt 0]
            (if (= 1 (count todo))
              cnt
              (let [doing (first todo)
                    nxt (rest todo)
                    res (count (filter #(calcross doing %) nxt))]
                ;(println res)
                (recur nxt (+ cnt res)))))))))

;part 2
(defn cross [[x1 y1 z1] [x2 y2 z2]]
  [(- (*' y1 z2) (*' z1 y2))    ; x-component: y1*z2 - z1*y2
   (- (*' z1 x2) (*' x1 z2))    ; y-component: z1*x2 - x1*z2
   (- (*' x1 y2) (*' y1 x2))])  ; z-component: x1*y2 - y1*x2

(let [data (parse "./2023/in24")
      [[x0 y0 z0 vx0 vy0 vz0 :as stone0] stone1 stone2 :as three] (take 3 data)]
  (->> three
       (map #(map - % stone0)) ;; shift system relative to stone0
       ((fn [[_ [x1 y1 z1 vx1 vy1 vz1] [x2 y2 z2 vx2 vy2 vz2]]]
          (let [[a b c :as rockdir] (->> (cross (cross [x1 y1 z1] [vx1 vy1 vz1])
                                                (cross [x2 y2 z2] [vx2 vy2 vz2]))) ;; get rock direction (must go through origin)
                r1 (/ (- (* vx1 y1) (* vy1 x1)) (- (* vx1 b) (* vy1 a)))
                hit1 (map * rockdir (repeat r1))
                t1 (/ (- (* r1 a) x1) vx1)
                r2 (/ (- (* vx2 y2) (* vy2 x2)) (- (* vx2 b) (* vy2 a)))
                hit2 (map * rockdir (repeat r2))
                t2 (/ (- (* r2 b) y2) vy2)
                speed (map / (map - hit2 hit1) (repeat (- t2 t1)))
                init (map - hit2 (map * speed (repeat t2)))] 
            (reduce + (map + init stone0)))))))