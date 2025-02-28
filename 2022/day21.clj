;advent-of-code-2022.day21
(ns day21
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(re-seq #"\w+|\d+|[\+\-\*\/]" %))))

;part 1
(let [dict (parse "2022/in21")
      promises (into {}
                     (map #(hash-map (first %) (promise))
                          dict))]
  (doseq [[k & v] dict]
    (if (= 1 (count v))
      (deliver (promises k) (read-string (first v)))
      (let [[a op b] v]
        (future
          (deliver (promises k)
                   ((resolve (read-string op))
                    @(promises a)
                    @(promises b)))))))
  @(promises "root"))