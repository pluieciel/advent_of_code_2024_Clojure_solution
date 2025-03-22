;advent-of-code-2021.day18
(ns day18
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map read-string)))

(defn getnested [v]
  (loop [curr [0]]
    (if (or (> (count curr) 4) (empty? curr))
      curr
      (let [doing (get-in v curr)]
        (cond
          (vector? doing) (recur (conj curr 0))
          :else (let [newcurr (->> curr reverse (drop-while #(= 1 %)) reverse vec)]
                  (recur (if (empty? newcurr) newcurr (update newcurr (dec (count newcurr)) inc)))))))))

(defn getidx [v idx dir]
  (if (every? ({:left zero? :right #(= 1 %)} dir) idx) nil
    (let [start (->> idx reverse (drop-while ({:left zero? :right #(= 1 %)} dir)) reverse vec)
          start (update start (dec (count start)) ({:left dec :right inc} dir))]
      (loop [curr start]
        (if (vector? (get-in v curr))
          (recur (conj curr ({:left 1 :right 0} dir)))
          curr)))))

(defn explode [v]
  (if-let [idx (butlast (getnested v))]
    (let [[a b] (get-in v idx)
          leftidx (getidx v idx :left)
          rightidx (getidx v idx :right)]
      (-> v
          (assoc-in idx 0)
          ((fn [v] (if (every? zero? idx) v (update-in v leftidx #(+ % a)))))
          ((fn [v] (if (every? #(= 1 %) idx) v (update-in v rightidx #(+ % b)))))))
    v))

(defn getsplit [v]
  (loop [curr [0]]
    (cond
      (empty? curr) nil 
      (vector? (get-in v curr)) (recur (conj curr 0))
      (> (get-in v curr) 9) curr
      :else (let [newcurr (->> curr reverse (drop-while #(= 1 %)) reverse vec)]
              (recur (if (empty? newcurr) newcurr (update newcurr (dec (count newcurr)) inc)))))))

(defn split [v]
  (if-let [idx (getsplit v)]
    (let [num (get-in v idx)
          a (quot num 2)
          b (quot (inc num) 2)]
      (assoc-in v idx [a b]))
    v))

(defn doadd [a b]
  (let [res (vector a b)]
    (loop [state res]
      (let [exploded (explode state)]
        (if (not= state exploded)
          (recur exploded)
          (let [splitted (split exploded)]
            (if (not= state splitted)
              (recur splitted)
              state)))))))

(defn magnitude [v]
  (if (vector? v)
    (+ (* 3 (magnitude (first v))) (* 2 (magnitude (second v))))
    v))

(def data (parse "2021/in18"))

;part 1
(->> data
     (reduce doadd)
     magnitude)

;part 2
(->> (for [a data b data :when (not= a b)]
       (magnitude (doadd a b)))
     (apply max))