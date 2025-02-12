;advent-of-code-2022.day18
(ns day18
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(mapv read-string (re-seq #"\d+" %)))))
;part 1
(let [data (parse "./2022/in18")
      dict (set data)
      dims (apply map vector data)
      [[x1 x2] [y1 y2] [z1 z2]] (map (fn [dim] ((juxt #(apply min %) #(apply max %)) dim)) dims)
      cnt (atom 0)]
  (defn getmark [x y z]
    (if (dict [x y z]) 1 0))
  (doseq [y (range y1 (inc y2)) z (range z1 (inc z2))]
    (doseq [x (range (dec x1) (inc x2))]
      (when (= 1 (+ (getmark x y z) (getmark (inc x) y z)))
        (swap! cnt inc))))
  (doseq [y (range y1 (inc y2)) x (range x1 (inc x2))]
    (doseq [z (range (dec z1) (inc z2))]
      (when (= 1 (+ (getmark x y z) (getmark x y (inc z))))
        (swap! cnt inc))))
  (doseq [x (range x1 (inc x2)) z (range z1 (inc z2))]
    (doseq [y (range (dec y1) (inc y2))]
      (when (= 1 (+ (getmark x y z) (getmark x (inc y) z)))
        (swap! cnt inc))))
  @cnt)
;part 2
(let [data (parse "./2022/in18")
      dict (set data)
      dims (apply map vector data)
      [[x1 x2] [y1 y2] [z1 z2]] (map (fn [dim] ((juxt #(apply min %) #(apply max %)) dim)) dims)
      cnt (atom 0)
      dirs [[0 0 1] [0 0 -1] [0 1 0] [0 -1 0] [1 0 0] [-1 0 0]]
      out (atom #{})]
  (loop [todo [[(dec x1) (dec y1) (dec z1)]]] ;floodfill the outside space
    (when-not (empty? todo)
      (let [doing (first todo)
            nbs (map #(mapv + doing %) dirs)
            nbs (remove (fn [[x y z]]
                          (or (dict [x y z])
                              (@out [x y z])
                              (not (<= (dec x1) x (inc x2)))
                              (not (<= (dec y1) y (inc y2)))
                              (not (<= (dec z1) z (inc z2))))) nbs)]
        (doseq [nb nbs] (swap! out conj nb))
        (recur (reduce conj (rest todo) nbs)))))
  (defn getmark [x y z]
    (cond
      (dict [x y z]) 1
      (@out [x y z]) 0
      :else 2))
  (doseq [y (range y1 (inc y2)) z (range z1 (inc z2))]
    (doseq [x (range (dec x1) (inc x2))]
      (when (= 1 (+ (getmark x y z) (getmark (inc x) y z)))
        (swap! cnt inc))))
  (doseq [y (range y1 (inc y2)) x (range x1 (inc x2))]
    (doseq [z (range (dec z1) (inc z2))]
      (when (= 1 (+ (getmark x y z) (getmark x y (inc z))))
        (swap! cnt inc))))
  (doseq [x (range x1 (inc x2)) z (range z1 (inc z2))]
    (doseq [y (range (dec y1) (inc y2))]
      (when (= 1 (+ (getmark x y z) (getmark x (inc y) z)))
        (swap! cnt inc))))
  @cnt)