;advent-of-code-2021.day14
(ns day14
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       ((fn [[init rules]]
          [(-> init (str/split #""))
           (->> rules
                str/split-lines
                (map #(let [[a b c] (re-seq #"[A-Z]" %)]
                        {(list a b) (list a c b)}))
                (apply merge))]))))

(let [[init rules] (parse "2021/in14")]
  (def init init) (def rules rules))
(def begin (first init))
(def end (last init))

(defn step [res]
  (->> (for [[pair cnt] res]
         (if-let [[a b c] (get rules pair)]
           {(list a b) cnt (list b c) cnt}
           {pair cnt}))
       (apply merge-with +)))

(defn cal [n]
  (loop [cnt 0 res (frequencies (partition 2 1 init))]
    (if (> n cnt)
      (recur (inc cnt) (step res))
      (->> res
           (mapcat (fn [[[a b] cnt]] [{a cnt} {b cnt}]))
           (apply merge-with +)
           ((fn [dict] (-> dict (update begin inc) (update end inc)))) 
           ((fn [dict]
              (let [[_ mi] (apply min-key val dict) [_ ma] (apply max-key val dict)]
                (- ma mi))))
           (#(/ % 2))))))

;part 1
(cal 10)

;part 2
(cal 40)