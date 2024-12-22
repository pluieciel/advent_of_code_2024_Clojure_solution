;advent-of-code-2024.day-21
(ns day21
  (:require [clojure.string :as str]))

(defn parse [input] (->> (slurp input) str/split-lines))

(defn digit->pos [d]
  (case d \0 [3 1] \A [3 2]
    (let [n (dec (Character/digit d 10))]
      [(- 2 (quot n 3)) (mod n 3)])))

(def move->pos {\A [0 2] \^ [0 1] \< [1 0] \v [1 1] \> [1 2]})

(defn ok? [l] (<= (count (partition-by identity l)) 2))

(def cal1 ;get path on digit pad
  (memoize
   (fn [[y1 x1 :as start] [y2 x2 :as end]]
     (cond
       (= start end) (list '())
       (= y1 y2) (list (if (< x1 x2)
                         (conj (first (cal1 [y1 (inc x1)] end)) \>)
                         (conj (first (cal1 [y1 (dec x1)] end)) \<)))
       (= x1 x2) (list (if (< y1 y2)
                         (conj (first (cal1 [(inc y1) x1] end)) \v)
                         (conj (first (cal1 [(dec y1) x1] end)) \^)))
       :else (filter ok?
                     (concat (if (< x1 x2)
                               (map #(conj % \>) (cal1 [y1 (inc x1)] end))
                               (if (not= start [3 1])
                                 (map #(conj % \<) (cal1 [y1 (dec x1)] end)) []))
                             (if (< y1 y2)
                               (if (not= start [2 0])
                                 (map #(conj % \v) (cal1 [(inc y1) x1] end)) [])
                               (map #(conj % \^) (cal1 [(dec y1) x1] end)))))))))

(def cal2 ;get path on movement pad
  (memoize
   (fn [[[y1 x1 :as start] [y2 x2 :as end]]]
     (cond
       (= start end) (list '())
       (= y1 y2) (list (if (< x1 x2)
                         (conj (first (cal2 [[y1 (inc x1)] end])) \>)
                         (conj (first (cal2 [[y1 (dec x1)] end])) \<)))
       (= x1 x2) (list (if (< y1 y2)
                         (conj (first (cal2 [[(inc y1) x1] end])) \v)
                         (conj (first (cal2 [[(dec y1) x1] end])) \^)))
       :else (filter ok?
                     (concat (if (< x1 x2)
                               (map #(conj % \>) (cal2 [[y1 (inc x1)] end]))
                               (if (not= start [0 1])
                                 (map #(conj % \<) (cal2 [[y1 (dec x1)] end])) []))
                             (if (< y1 y2)
                               (map #(conj % \v) (cal2 [[(inc y1) x1] end]))
                               (if (not= start [1 0])
                                 (map #(conj % \^) (cal2 [[(dec y1) x1] end])) []))))))))

(def cal3 ;core DP function
  (memoize
   (fn [pair deep]
     (if (= deep 1)
       (->> (cal2 (map move->pos pair)) (map count) (apply min) inc)
       (->> (cal2 (map move->pos pair))
            (map #(->> (concat '(\A) % '(\A))
                       (partition 2 1)
                       (map (fn [p] (cal3 p (dec deep))))
                       (reduce +)))
            (apply min))))))

(defn cal4 [lst deep]
  (->> (concat '(\A) lst '(\A)) (partition 2 1) (map #(cal3 % deep)) (reduce +)))

(defn cal-one [deep s]
  (->> (apply list s)
       (#(conj % \A))
       (map digit->pos)
       (partition 2 1) 
       (map (fn [lst] (->> lst (apply cal1) (map #(cal4 % deep)) (reduce min))))
       (reduce +)
       (* (Integer/parseInt (subs s 0 3)))))

;part 1
(->> (parse "input copy")
     (transduce (map (partial cal-one 2)) +)
     println)
;part 2
(->> (parse "input copy")
     (transduce (map (partial cal-one 25)) +)
     println)