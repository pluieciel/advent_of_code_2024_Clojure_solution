;advent-of-code-2023.day07
(ns day07
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def dict (->> "AKQJT98765432" (#(map vector % (range 13 0 -1))) (into {})))

(defn getf [A]
  (->> (frequencies A)
       vals
       (sort >)))

(defn mycomp [A B]
  (let [[[a & ra] [b & rb]] (map getf [A B])]
    (cond
      (< a b) -1
      (> a b) 1
      (and (or (= a b 2) (= a b 3)) (< (first ra) (first rb))) -1
      (and (or (= a b 2) (= a b 3)) (> (first ra) (first rb))) 1
      :else (loop [a A b B]
              (cond
                (< (get dict (first a)) (get dict (first b))) -1
                (> (get dict (first a)) (get dict (first b))) 1
                :else (recur (rest a) (rest b)))))))
;part 1
(->> (slurp "./2023/in07")
     str/split-lines
     (map #(let [[s n] (str/split % #" ") n (read-string n)] [s n]))
     (sort-by first mycomp)
     (map second)
     (map * (rest (range)))
     (reduce +))
;part 2
(def dict2 (->> "AKQT98765432J" (#(map vector % (range 13 0 -1))) (into {})))

(defn getf2 [A]
  (let [freq (frequencies A)]
    (->> freq
         (#(dissoc % \J))
         vals
         (sort >)
         (#(if (empty? %)
             '(5)
             (let [[fi & re] %]
               (conj re (+ fi (get freq \J 0)))))))))

(defn mycomp2 [A B]
  (let [[[a & ra] [b & rb]] (map getf2 [A B])]
    (cond
      (< a b) -1
      (> a b) 1
      (and (or (= a b 2) (= a b 3)) (< (first ra) (first rb))) -1
      (and (or (= a b 2) (= a b 3)) (> (first ra) (first rb))) 1
      :else (loop [a A b B]
              (cond
                (< (get dict2 (first a)) (get dict2 (first b))) -1
                (> (get dict2 (first a)) (get dict2 (first b))) 1
                :else (recur (rest a) (rest b)))))))

(->> (slurp "./2023/in07")
     str/split-lines
     (map #(let [[s n] (str/split % #" ") n (read-string n)] [s n]))
     (sort-by first mycomp2)
     (map second)
     (map * (rest (range)))
     (reduce +))