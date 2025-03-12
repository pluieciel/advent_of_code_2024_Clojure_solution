;advent-of-code-2021.day09
(ns day09
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv (fn [line] (mapv read-string (str/split line #""))))))

(def Map (parse "2021/in09"))
(def h (count Map))
(def w (count (first Map)))
(def dirs [[1 0] [-1 0] [0 1] [0 -1]])

;part 1
(->> (for [r (range h) c (range w)
           :let [val (get-in Map [r c])
                 neibs (map #(get-in Map (mapv + [r c] %) 9) dirs)]
           :when (every? #(> % val) neibs)]
       val)
     (transduce (map inc) +))

;part 2
(def visited (atom #{}))
(def res (atom []))
(defn floodfill [pos]
  (loop [todo [pos] cnt 0]
    (if (empty? todo)
      (swap! res conj cnt)
      (if (@visited (first todo))
        (recur (rest todo) cnt)
        (let [nxt (remove #(or (@visited %) (= 9 (get-in Map % 9))) (map #(mapv + (first todo) %) dirs))]
          (swap! visited conj (first todo))
          (recur (into (rest todo) nxt) (inc cnt)))))))

(doseq [r (range h)]
  (doseq [c (range w)]
    (if (or (@visited [r c]) (= 9 (get-in Map [r c] 9)))
      nil
      (floodfill [r c]))))
(->> @res
     (sort >)
     (take 3)
     (reduce *))