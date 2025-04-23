;advent-of-code-2020.day15
(ns day15
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #",")
       (map read-string)))

(def data (parse "2020/in15"))
(def init (->> data
               (map-indexed (fn [i n] {n (list (inc i))}))
               (apply merge)))
(def dict (atom init))

(defn cal [step]
  (loop [cnt (inc (count data)) pre (last data)]
    (if (= (inc step) cnt)
      pre
      (cond
        (not (contains? @dict pre))
        (do (swap! dict assoc 0 (list cnt)) (recur (inc cnt) 0))
        (= 1 (count (get @dict pre)))
        (do (swap! dict assoc 0 (take 2 (conj (get @dict 0) cnt))) (recur (inc cnt) 0))
        :else
        (let [n (apply - (get @dict pre))]
          (swap! dict assoc n (take 2 (conj (get @dict n) cnt)))
          (recur (inc cnt) n))))))

;part 1
(cal 2020)

;part 2 ;;about 15s to run, a bit slow
(reset! dict init)
(cal 30000000)