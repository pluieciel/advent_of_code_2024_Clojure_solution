;advent-of-code-2021.day11
(ns day11
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv (fn [s] (mapv read-string (str/split s #""))))))

(def Map (atom (parse "2021/in11")))

(defn neighbors [[r c]]
  (for [dx [-1 0 1] dy [-1 0 1]
        :let [x (+ r dx) y (+ c dy)]
        :when (and (not= 0 dx dy) (<= 0 x 9) (<= 0 y 9))]
    [x y]))

(defn step [_]
  (let [flashed (atom #{}) one (atom #{})]
    (loop [todo (set (for [r (range 10) c (range 10)] [r c]))]
      (if (seq todo)
        (let [[r c] (first todo)]
          (when (not (@one [r c]))
            (swap! Map update-in [r c] inc)
            (swap! one conj [r c]))
          (let [n (get-in @Map [r c])]
            (if (> n 9)
              (do
                (swap! Map assoc-in [r c] 0)
                (swap! flashed conj [r c])
                (let [nbs (remove @flashed (neighbors [r c]))]
                  (doseq [[r c] nbs]
                    (swap! Map update-in [r c] inc))
                  (recur (into (disj todo [r c]) nbs))))
              (recur (disj todo [r c])))))
        (count @flashed)))))

;part 1
(->> (map step (range 100)) (reduce +))

;part 2
(loop [cnt 0]
  (let [res (step 0)]
    (if (= res 100)
      (inc cnt)
      (recur (inc cnt)))))