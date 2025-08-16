;advent-of-code-2020.day23
(ns day23
  (:require [clojure.string :as str]))

(def init (->> (slurp "2020/in23")
               (re-seq #"\d")
               (map read-string)))

;;part 1
(defn dest [curr exc]
  (cond
    (exc curr) (dest (dec curr) exc)
    (zero? curr) (dest 9 exc)
    :else curr))

(defn step [[a b c d & r]]
  (->> (cycle (conj r a))
       (drop-while #(not= % (dest (dec a) #{b c d})))
       (take 6)
       ((fn [[nf & nr]]
          (concat [nf b c d] nr)))
       cycle
       (drop-while #(not= % a))
       rest
       (take 9)))

(->> (iterate step init)
     (#(nth % 100))
     cycle
     (drop-while #(not= % 1))
     rest
     (take 8)
     (str/join ""))

;;part 2
(def init2 (concat init (range 10 1000001)))

(def link-map (into {} (map vec (partition 2 1 (conj init2 (last init2))))))

(defn dest2 [curr exc]
  (cond
    (exc curr) (dest2 (dec curr) exc)
    (zero? curr) (dest2 1000000 exc)
    :else curr))

(defn step2 [[curr link-map]]
  (let [a (link-map curr)
        b (link-map a)
        c (link-map b)
        d (link-map c)
        start (dest2 (dec curr) #{a b c})
        end (link-map start)]
    [d (-> link-map
         (assoc curr d)
         (assoc start a)
         (assoc c end))]))

(->> (iterate step2 [(first init2) link-map])
     (#(nth % 10000000))
     ((fn [[_ final-map]]
        (let [a (final-map 1)
              b (final-map a)]
          (* a b)))))