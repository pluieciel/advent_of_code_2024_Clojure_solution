;advent-of-code-2023.day22
(ns day22
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(->> (str/split % #"~")
                  (map (fn [v] (map read-string (str/split v #","))))))))
;part 1
(let [data (sort-by #(last (first %)) (parse "./2023/in22"))
      tot (count data)
      dict (atom {})]
  (loop [todo (map-indexed (fn [i v] [i v]) data) done []]
    (if (empty? todo)
      (->> done
           (map last)
           (filter #(= 1 (count %)))
           (map first)
           set
           count
           (#(- tot %)))
      (let [[idx [[x1 y1 z1] [x2 y2 z2]]] (first todo)
            height (atom 0)
            tops (atom [])]
        (cond
          (not= x1 x2) (let [covers (for [x (range x1 (inc x2))] (get @dict [x y1] [0 nil]))
                             top-height (->> covers (map first) (apply max))
                             top (filter #(and (not (nil? (second %))) (= (first %) top-height)) covers)]
                         (reset! height (inc top-height))
                         (reset! tops (distinct (map second top)))
                         (doseq [x (range x1 (inc x2))]
                           (swap! dict assoc [x y1] [@height idx])))
          (not= y1 y2) (let [covers (for [y (range y1 (inc y2))] (get @dict [x1 y] [0 nil]))
                             top-height (->> covers (map first) (apply max))
                             top (filter #(and (not (nil? (second %))) (= (first %) top-height)) covers)]
                         (reset! height (inc top-height))
                         (reset! tops (distinct (map second top)))
                         (doseq [y (range y1 (inc y2))]
                           (swap! dict assoc [x1 y] [@height idx])))
          :else (let [[top-height top-idx] (get @dict [x1 y1] [0 nil])]
                  (reset! height (inc top-height))
                  (reset! tops (if (nil? top-idx) [] (list top-idx)))
                  (swap! dict assoc [x1 y1] [(+ @height (- z2 z1)) idx])))
        (recur (rest todo) (conj done [idx [x1 y1 @height] [x2 y2 (+ @height (- z2 z1))] @tops]))))))
;part 2
(let [data (sort-by #(last (first %)) (parse "./2023/in22"))
      tot (count data)
      dict (atom {})
      done (loop [todo (map-indexed (fn [i v] [i v]) data) done []]
             (if (empty? todo)
               done
               (let [[idx [[x1 y1 z1] [x2 y2 z2]]] (first todo)
                     height (atom 0)
                     tops (atom [])]
                 (cond
                   (not= x1 x2) (let [covers (for [x (range x1 (inc x2))] (get @dict [x y1] [0 nil]))
                                      top-height (->> covers (map first) (apply max))
                                      top (filter #(and (not (nil? (second %))) (= (first %) top-height)) covers)]
                                  (reset! height (inc top-height))
                                  (reset! tops (distinct (map second top)))
                                  (doseq [x (range x1 (inc x2))]
                                    (swap! dict assoc [x y1] [@height idx])))
                   (not= y1 y2) (let [covers (for [y (range y1 (inc y2))] (get @dict [x1 y] [0 nil]))
                                      top-height (->> covers (map first) (apply max))
                                      top (filter #(and (not (nil? (second %))) (= (first %) top-height)) covers)]
                                  (reset! height (inc top-height))
                                  (reset! tops (distinct (map second top)))
                                  (doseq [y (range y1 (inc y2))]
                                    (swap! dict assoc [x1 y] [@height idx])))
                   :else (let [[top-height top-idx] (get @dict [x1 y1] [0 nil])]
                           (reset! height (inc top-height))
                           (reset! tops (if (nil? top-idx) [] (list top-idx)))
                           (swap! dict assoc [x1 y1] [(+ @height (- z2 z1)) idx])))
                 (recur (rest todo) (conj done [idx [x1 y1 @height] [x2 y2 (+ @height (- z2 z1))] @tops])))))
      done (map (fn [[idx _ _ by]] [idx (set by)]) done)]
  ;done
  (->> (for [id (range tot)]
         (loop [fall #{id}]
           ;(println fall)
           (let [newfall (map first (filter (fn [[id supports]]
                                              (and (not (fall id))
                                                   (not-empty supports)
                                                   (set/subset? supports fall))) done))]
             (if (empty? newfall)
               (dec (count fall))
               (recur (reduce conj fall newfall))))))
       (reduce +)))