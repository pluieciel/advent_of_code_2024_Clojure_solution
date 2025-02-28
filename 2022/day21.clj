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

;part 2
(let [dict (parse "2022/in21")
      promises (into {}
                     (map #(hash-map (first %) (promise))
                          dict))]
  (doseq [[k & v] dict]
    (cond
      (= k "humn") nil
      (= 1 (count v)) (deliver (promises k) (read-string (first v)))
      :else
      (let [[a op b] v]
        (cond
          (= k "root")
          (do
            (future
              (let [left @(promises a)
                    target (promises b)]
                (when-not (realized? target) (deliver target left))))
            (future
              (let [right @(promises b)
                    target (promises a)]
                (when-not (realized? target) (deliver target right)))))
          (#{"+" "*"} op)
          (do
            (future
              (let [B @(promises a)
                    C @(promises b)
                    target (promises k)]
                (when-not (realized? target)
                  (deliver target (({"+" + "*" *} op) B C)))))
            (future
              (let [A @(promises k)
                    B @(promises a)
                    target (promises b)]
                (when-not (realized? target)
                  (deliver target (({"+" - "*" /} op) A B)))))
            (future
              (let [A @(promises k)
                    C @(promises b)
                    target (promises a)]
                (when-not (realized? target)
                  (deliver target (({"+" - "*" /} op) A C))))))
          (#{"-" "/"} op)
          (do
            (future
              (let [B @(promises a)
                    C @(promises b)
                    target (promises k)]
                (when-not (realized? target)
                  (deliver target (({"-" - "/" /} op) B C)))))
            (future
              (let [A @(promises k)
                    B @(promises a)
                    target (promises b)]
                (when-not (realized? target)
                  (deliver target (({"-" - "/" /} op) B A)))))
            (future
              (let [A @(promises k)
                    C @(promises b)
                    target (promises a)]
                (when-not (realized? target)
                  (deliver target (({"-" + "/" *} op) C A))))))))))
  @(promises "humn"))