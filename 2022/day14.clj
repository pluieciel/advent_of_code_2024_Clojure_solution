;advent-of-code-2022.day14
(ns day14
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(->> (re-seq #"\d+" %)
                  (map read-string)
                  (partition 2)
                  (partition 2 1)))))

;part 1
(let [data (parse "./2022/in14")
      Map (atom #{})
      bottom (atom 0)
      Sand (atom #{})
      start [500 0]]
  (doseq [lines data]
    (doseq [[[x1 y1] [x2 y2]] lines]
      (cond
        (= x1 x2) (let [[a b] (sort [y1 y2])]
                    (when (> b @bottom) (reset! bottom b))
                    (doseq [y (range a (inc b))]
                      (swap! Map conj [x1 y])))
        (= y1 y2) (let [[a b] (sort [x1 x2])]
                    (when (> y1 @bottom) (reset! bottom y1))
                    (doseq [x (range a (inc b))]
                      (swap! Map conj [x y1]))))))
  (loop [[x y] start]
    (cond
      (>= y @bottom) (count @Sand)
      (and (nil? (@Sand [x (inc y)])) (nil? (@Map [x (inc y)]))) (recur [x (inc y)])
      (and (nil? (@Sand [(dec x) (inc y)])) (nil? (@Map [(dec x) (inc y)]))) (recur [(dec x) (inc y)])
      (and (nil? (@Sand [(inc x) (inc y)])) (nil? (@Map [(inc x) (inc y)]))) (recur [(inc x) (inc y)])
      :else (do (swap! Sand conj [x y]) (recur start)))))

;part 2
(let [data (parse "./2022/in14")
      Map (atom #{})
      bottom (atom 0)
      Sand (atom #{})
      start [500 0]]
  (doseq [lines data]
    (doseq [[[x1 y1] [x2 y2]] lines]
      (cond
        (= x1 x2) (let [[a b] (sort [y1 y2])]
                    (when (> b @bottom) (reset! bottom b))
                    (doseq [y (range a (inc b))]
                      (swap! Map conj [x1 y])))
        (= y1 y2) (let [[a b] (sort [x1 x2])]
                    (when (> y1 @bottom) (reset! bottom y1))
                    (doseq [x (range a (inc b))]
                      (swap! Map conj [x y1]))))))
  (loop [[x y] start]
    (cond
      (= y (inc @bottom)) (do (swap! Sand conj [x y]) (recur start))
      (and (nil? (@Sand [x (inc y)])) (nil? (@Map [x (inc y)]))) (recur [x (inc y)])
      (and (nil? (@Sand [(dec x) (inc y)])) (nil? (@Map [(dec x) (inc y)]))) (recur [(dec x) (inc y)])
      (and (nil? (@Sand [(inc x) (inc y)])) (nil? (@Map [(inc x) (inc y)]))) (recur [(inc x) (inc y)])
      (= [x y] start) (inc (count @Sand))
      :else (do (swap! Sand conj [x y]) (recur start)))))