;advent-of-code-2020.day12
(ns day12
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(vector (first %) (Integer/parseInt (subs % 1))))))

(def data (parse "2020/in12"))
(def dir [[1 0] [0 1] [-1 0] [0 -1]])

;part 1
(->> (reduce
      (fn [acc [op val]]
        (let [{face :face pos :pos} acc]
          (case op
            \N (assoc acc :pos (mapv + pos [0 val]))
            \S (assoc acc :pos (mapv - pos [0 val]))
            \E (assoc acc :pos (mapv + pos [val 0]))
            \W (assoc acc :pos (mapv - pos [val 0]))
            \R (assoc acc :face (mod (- face (/ val 90)) 4))
            \L (assoc acc :face (mod (+ face (/ val 90)) 4))
            \F (assoc acc :pos (mapv + pos (map #(* val %) (get dir face)))))))
      {:face 0 :pos [0 0]}
      data)
     :pos
     (map #(Math/abs %))
     (reduce +))

;part 2
(defn tl [[x y] n]
  (case n 0 [x y] 1 [(- y) x] 2 [(- x) (- y)] 3 [y (- x)]))

(defn tr [[x y] n]
  (case n 0 [x y] 1 [y (- x)] 2 [(- x) (- y)] 3 [(- y) x]))

(->> (reduce
      (fn [acc [op val]]
        (let [{pos :pos way :way} acc]
          (case op
            \N (assoc acc :way (mapv + way [0 val]))
            \S (assoc acc :way (mapv - way [0 val]))
            \E (assoc acc :way (mapv + way [val 0]))
            \W (assoc acc :way (mapv - way [val 0]))
            \R (assoc acc :way (tr way (mod (/ val 90) 4)))
            \L (assoc acc :way (tl way (mod (/ val 90) 4)))
            \F (assoc acc :pos (mapv + pos (map #(* val %) way))))))
      {:pos [0 0] :way [10 1]}
      data)
     :pos
     (map #(Math/abs %))
     (reduce +))