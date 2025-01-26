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
(defn myabs [x]
  (if (neg? x) (- x) x))

(defn gcd [a b]
  (if (zero? b)
    (myabs a) ; GCD is always non-negative
    (gcd b (mod a b))))

(defn multi-gcd [nums]
  (reduce gcd (map myabs nums)))

(defn normalize-direction [dir]
  (let [d (multi-gcd dir)
        [x y z] dir]
    (when (zero? d) ; Handle invalid zero direction
      (throw (IllegalArgumentException. "Direction vector cannot be zero.")))
    [(quot x d) (quot y d) (quot z d)]))

(defn cross [[x1 y1 z1] [x2 y2 z2]]
  [(- (*' y1 z2) (*' z1 y2))    ; x-component: y1*z2 - z1*y2
   (- (*' z1 x2) (*' x1 z2))    ; y-component: z1*x2 - x1*z2
   (- (*' x1 y2) (*' y1 x2))])  ; z-component: x1*y2 - y1*x2

(let [data (parse "./2023/in24")
      [[x0 y0 z0 vx0 vy0 vz0 :as v0] v1 v2 :as three] (take 3 data)]
  (->> three
       (map #(map - % v0)) ;; shift system relative to stone0
       ((fn [[_ [x1 y1 z1 vx1 vy1 vz1] [x2 y2 z2 vx2 vy2 vz2]]]
          (let [[a b c] (->> (cross (cross [x1 y1 z1] [vx1 vy1 vz1])
                                    (cross [x2 y2 z2] [vx2 vy2 vz2]))
                             normalize-direction) ;; get rock direction (must go through origin)
                r1 (/ (- (* vx1 y1) (* vy1 x1)) (- (* vx1 b) (* vy1 a)))
                hit1 [(* r1 a) (* r1 b) (* r1 c)]
                t1 (repeat 3 (/ (- (* r1 a) x1) vx1))
                r2 (/ (- (* vx2 y2) (* vy2 x2)) (- (* vx2 b) (* vy2 a)))
                hit2 [(* r2 a) (* r2 b) (* r2 c)]
                t2 (repeat 3 (/ (- (* r2 b) y2) vy2))
                speed (map / (map - hit2 hit1) (map - t2 t1))
                init (map - hit2 (map * speed t2))] 
            (reduce + (map + init v0)))))))