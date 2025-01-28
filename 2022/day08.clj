;advent-of-code-2022.day08
(ns day08
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i) str/split-lines (mapv (fn [line]
                                         (mapv #(Character/digit % 10) line)))))
;part 1
(let [Map (atom (parse "./2022/in08"))
      h (count @Map) w (count (first @Map))
      Mem (atom (vec (repeat h (vec (repeat w 0)))))]
  (defn cal []
    (doseq [[idx row] (map-indexed vector @Map)]
      (loop [todo row big -1 j 0]
        (when-not (empty? todo)
          (let [doing (first todo)]
            (when (> doing big)
              (swap! Mem assoc-in [idx j] 1))
            (recur (rest todo) (max doing big) (inc j)))))))
  (cal)
  (reset! Map (mapv #(vec (reverse %)) @Map))
  (reset! Mem (mapv #(vec (reverse %)) @Mem))
  (cal)
  (reset! Map (apply mapv vector @Map))
  (reset! Mem (apply mapv vector @Mem))
  (cal)
  (reset! Map (mapv #(vec (reverse %)) @Map))
  (reset! Mem (mapv #(vec (reverse %)) @Mem))
  (cal)
  (->> @Mem
       (transduce (map #(reduce + %)) +)))
;part 2
(let [Map (atom (parse "./2022/in08"))
      h (count @Map) w (count (first @Map))
      Mem (atom (vec (repeat h (vec (repeat w [])))))]
  (defn cal []
    (doseq [[idx row] (map-indexed vector @Map)]
      (if (#{0 (dec (count row))} idx)
        nil
        (let [stack (atom [])]
          (loop [todo (rest row) j 1]
            (when (> (count todo) 1)
              (let [doing (first todo)]
                (cond
                  (or (empty? @stack)
                      (< doing (first (last @stack))))
                  (do (swap! stack conj [doing j]) (recur (rest todo) (inc j)))
                  :else
                  (do (while (and (not-empty @stack) (>= doing (first (last @stack))))
                        (swap! Mem update-in [idx (second (last @stack))] conj (- j (second (last @stack))))
                        (swap! stack pop))
                      (swap! stack conj [doing j])
                      (recur (rest todo) (inc j)))))))
          (while (not-empty @stack)
            (swap! Mem update-in [idx (second (last @stack))] conj (- (dec w) (second (last @stack))))
            (swap! stack pop))))))
  (cal) 
  (reset! Map (mapv #(vec (reverse %)) @Map))
  (reset! Mem (mapv #(vec (reverse %)) @Mem))
  (cal)
  (reset! Map (apply mapv vector @Map))
  (reset! Mem (apply mapv vector @Mem))
  (cal)
  (reset! Map (mapv #(vec (reverse %)) @Map))
  (reset! Mem (mapv #(vec (reverse %)) @Mem))
  (cal)
  (->> @Mem
       rest
       butlast
       (map rest)
       (map butlast)
       (map (fn [line] (apply max (map (fn [one] (reduce * one)) line))))
       (apply max)))