;advent-of-code-2023.day15
(ns day15
  (:require [clojure.string :as str]
            [flatland.ordered.map :refer [ordered-map]]))

(defn parse [i]
  (str/split (slurp i) #","))

(defn myhash [s]
  (loop [curr 0 [a & bs] s]
    (let [new (-> curr (+ (int a)) (* 17) (mod 256))]
      (if bs
        (recur new bs)
        new))))
;part 1
(->> (parse "./2023/in15")
     (map myhash)
     (reduce +))

;part 2
(->> (parse "./2023/in15")
     ((fn [lst]
        (let [dict (into {} (for [i (range 256)] {i (atom (ordered-map))}))]
          (doseq [s lst]
            (let [t (re-find #"\w+" s)
                  box (myhash t)
                  omap (get dict box)
                  op (re-find #"=|-" s)]
              (case op
                "=" (swap! omap assoc t (read-string (re-find #"\d" s)))
                "-" (swap! omap dissoc t))))
          dict)))
     (map (fn [[k v]]
            (->> (map * (vals @v) (rest (range)) (repeat (inc k)))
                 (reduce +))))
     (reduce +))