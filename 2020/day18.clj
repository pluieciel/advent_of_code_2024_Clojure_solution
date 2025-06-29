;advent-of-code-2020.day18
(ns day18
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (map #(read-string
              (format "(%s)" %)))))

;part 1
(defn eval-expr [expr]
  (cond
    (number? expr) expr
    (= 1 (count expr)) (eval-expr (first expr))
    :else (let [[a op & r] expr]
            (case op
              * (eval-expr (conj (rest r) (* (eval-expr a) (eval-expr (first r)))))
              + (eval-expr (conj (rest r) (+ (eval-expr a) (eval-expr (first r)))))))))

(->> (parse "2020/in18")
     (map eval-expr)
     (reduce +))

;part 2
(defn eval-expr2 [expr]
  (cond
    (number? expr) expr
    (= 1 (count expr)) (eval-expr2 (first expr))
    :else (let [[a op & r] expr]
            (case op
              + (eval-expr2 (conj (rest r) (+ (eval-expr2 a) (eval-expr2 (first r)))))
              * (* (eval-expr2 a) (eval-expr2 r))))))

(->> (parse "2020/in18")
     (map eval-expr2)
     (reduce +))