;advent-of-code-2023.day18
(ns day18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i) str/split-lines (map #(str/split % #" "))))

(def dirs {"R" [0 1] "D" [1 0] "L" [0 -1] "U" [-1 0]})

;(read-string "0x70c71")
;(vec (vals dirs))

(let [edge (loop [todo (parse "./2023/ex18") pos [0 0] done #{}]
             (if (empty? todo)
               done
               (let [[_ _ color] (first todo)
                     step (read-string (str "0x" (subs color 2 7)))
                     dir (get (vec (vals dirs)) (read-string (subs color 7 8)))
                     newposs (map #(mapv + pos (map (fn [n] (* n %)) dir)) (range (inc step)))
                     newpos (last newposs)]
                 (recur (rest todo) newpos (apply conj done newposs)))))
      edge (apply merge-with set/union (map (fn [[k v]] {k #{v}}) edge))]
  (get edge 0)
  (->> (for [[r cols] (sort-by first edge)]
         (->> cols
              sort
              (#(do (map-indexed (fn [i v] [v (- v i)]) %)))
              (partition-by second)
              (map #(map first %))
              ((fn [lst]
                 (loop [lst lst cnt 0 flag false pre nil]
                   (if (empty? lst)
                     cnt
                     (let [doing (first lst)
                           s (first doing) e (last doing)
                           prer (get edge (dec r) #{})
                           nxtr (get edge (inc r) #{})]
                       (recur (rest lst)
                              (+ cnt (count doing) (if flag (- s pre 1) 0))
                              (if (or (and (prer s) (nxtr e)) (and (prer e) (nxtr s)))
                                (not flag)
                                flag)
                              e))))))))
       (reduce +)))

(def dirs {"R" [0 1] "D" [1 0] "L" [0 -1] "U" [-1 0]})

(let [edge (loop [todo (parse "./2023/in18") pos [0 0] done #{}]
             (if (empty? todo)
               done
               (let [[d step _] (first todo)
                     step (read-string step)
                     dir (get dirs d)
                     newposs (map #(mapv + pos (map (fn [n] (* n %)) dir)) (range (inc step)))
                     newpos (last newposs)]
                 (recur (rest todo) newpos (apply conj done newposs)))))
      edge (apply merge-with set/union (map (fn [[k v]] {k #{v}}) edge))]
  (->> (for [[r cols] (sort-by first edge)]
         (->> cols
              sort
              (map-indexed (fn [i v] [v (- v i)]))
              (partition-by second)
              (map #(map first %))
              ((fn [lst]
                 (loop [lst lst cnt 0 flag false pre nil]
                   (if (empty? lst)
                     cnt
                     (let [doing (first lst)
                           s (first doing) e (last doing)
                           prer (get edge (dec r) #{})
                           nxtr (get edge (inc r) #{})]
                       (recur (rest lst)
                              (+ cnt (count doing) (if flag (- s pre 1) 0))
                              (if (or (and (prer s) (nxtr e)) (and (prer e) (nxtr s)))
                                (not flag)
                                flag)
                              e))))))))
       (reduce +)))