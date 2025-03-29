;advent-of-code-2021.day19
(ns day19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (map (fn [scanner]
              (->> scanner
                   str/split-lines
                   (drop 1)
                   (map #(mapv read-string (str/split % #","))))))))

(def rotations
  [;; z-axis along z
   (fn [[x y z]] [x y z])           ; identity
   (fn [[x y z]] [(- y) x z])       ; 90° around z
   (fn [[x y z]] [(- x) (- y) z])   ; 180° around z
   (fn [[x y z]] [y (- x) z])       ; 270° around z
   ;; z-axis along -z
   (fn [[x y z]] [(- x) y (- z)])
   (fn [[x y z]] [y x (- z)])
   (fn [[x y z]] [x (- y) (- z)])
   (fn [[x y z]] [(- y) (- x) (- z)])
   ;; z-axis along x
   (fn [[x y z]] [(- z) y x])
   (fn [[x y z]] [z (- y) x])
   (fn [[x y z]] [y z x])
   (fn [[x y z]] [(- y) (- z) x])
   ;; z-axis along -x
   (fn [[x y z]] [z y (- x)])
   (fn [[x y z]] [(- z) (- y) (- x)])
   (fn [[x y z]] [y (- z) (- x)])
   (fn [[x y z]] [(- y) z (- x)])
   ;; z-axis along y
   (fn [[x y z]] [x (- z) y])
   (fn [[x y z]] [z x y])
   (fn [[x y z]] [(- x) z y])
   (fn [[x y z]] [(- z) (- x) y])
   ;; z-axis along -y
   (fn [[x y z]] [x z (- y)])
   (fn [[x y z]] [(- z) x (- y)])
   (fn [[x y z]] [(- x) (- z) (- y)])
   (fn [[x y z]] [z (- x) (- y)])])

(def valid (set (map #(mapv (fn [n] (if (neg? n) (+ 2 (- n)) (dec n))) (% [1 2 3])) rotations)))

(def centers (atom [[0 0 0]]))

(defn matchaxi [s1 s2 ori2]
  (let [getaxibyidx (fn [s2 n]
                      (let [[sx sy sz] (apply map vector s2)]
                        (case n
                          0 sx 1 sy 2 sz 3 (map - sx) 4 (map - sy) 5 (map - sz) (println "???"))))]
    (some (fn [rot]
            (let [ss2 (apply map vector (map (partial getaxibyidx s2) rot))]
              (when (>= (count (set/intersection (set s1) (set ss2))) 12)
                [ss2 (first (apply map vector (map (partial getaxibyidx [(mapv - [0 0 0] ori2)]) rot)))])))
          valid)))

(defn matchscanner [s1 s2]
  (loop [todo1 s1]
    (if (empty? todo1)
      nil
      (let [ori1 (last todo1)
            res (loop [todo2 s2]
                  (if (empty? todo2)
                    nil
                    (let [ori2 (last todo2)
                          ss1 (map #(mapv - % ori1) s1)
                          ss2 (map #(mapv - % ori2) s2)
                          [ss2-rot newori] (matchaxi ss1 ss2 ori2)]
                      (if (nil? ss2-rot)
                        (recur (butlast todo2))
                        [ss2-rot ori1 newori]))))]
        (if (nil? res)
          (recur (butlast todo1))
          res)))))

(defn combine [acc scanner]
  (let [res (loop [todo acc]
              (if (empty? todo)
                nil
                (let [s (last todo)
                      res (matchscanner (second s) (second scanner))]
                  (if (nil? res)
                    (recur (butlast todo))
                    [(first s) (first scanner) res]))))]
    (if (nil? res)
      acc
      (let [[idx1 idx2 [ss2-rot ori1 newori]] res
            _ (println idx1 idx2)]
        (swap! centers conj (mapv + ori1 newori))
        (conj acc [(first scanner) (map #(mapv + ori1 %) ss2-rot)])))))

(def visited (atom #{}))
(def data (parse "2021/in19"))

(def test1 0)
;part 1
(let [scanners (map-indexed #(vector %1 %2) data)]
  (loop [acc [(nth scanners test1)] scanners (remove #(= test1 (first %)) scanners)]
    ;(println (count scanners))
    (if (or (empty? scanners) (@visited scanners))
      [(count scanners) (count acc) (->> (map second acc) (apply concat) set count)]
      (let [s (first scanners)
            newacc (combine acc s)]
        (swap! visited conj scanners)
        (if (= acc newacc)
          (recur acc (conj (vec (rest scanners)) s))
          (recur newacc (rest scanners)))))))

;part 2
(let [len (count @centers)]
  (->> (for [i (range len) j (range (inc i) len)]
         (->> (map #(Math/abs (- %1 %2)) (@centers i) (@centers j))
              (reduce +)))
       (apply max)))