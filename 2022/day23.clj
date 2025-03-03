;advent-of-code-2022.day23
(ns day23
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)
       ((fn [M]
          (let [h (count M) w (count (first M))]
            (for [c (range w) r (range h) :when (= \# (get-in M [r c]))]
              [r c]))))
       set))

(def alldirs (for [x [-1 0 1] y [-1 0 1] :when (not (= x y 0))] [x y]))
(def M (parse "2022/in23"))

(defn check-dir [[r c] dir curr]
  (case dir
    :N (when-not (some curr (for [nc [-1 0 1]] [(dec r) (+ c nc)])) [(dec r) c])
    :S (when-not (some curr (for [nc [-1 0 1]] [(inc r) (+ c nc)])) [(inc r) c])
    :W (when-not (some curr (for [nr [-1 0 1]] [(+ r nr) (dec c)])) [r (dec c)])
    :E (when-not (some curr (for [nr [-1 0 1]] [(+ r nr) (inc c)])) [r (inc c)])))

(defn get-pos [[r c] order curr]
  (loop [todo order]
    (if (empty? todo)
      [[r c] [r c]]
      (let [dir (first todo)
            newpos (check-dir [r c] dir curr)]
        (if newpos
          [[r c] newpos]
          (recur (rest todo)))))))

(defn myp [final]
  (doseq [r (range 10)]
    (doseq [c (range 12)]
      (print
       (if (final [r c]) \# \.)))
    (println)))

(->> (loop [curr M order [:N :S :W :E] cnt 0]
       (if (= 10 cnt)
         curr
         (let [newpos (for [[r c] curr]
                        (if (some curr (map #(mapv + [r c] %) alldirs))
                          (get-pos [r c] order curr)
                          [[r c] [r c]]))
               cando (->> (map second newpos) frequencies (filter (fn [[k v]] (= 1 v))) keys set)
               new (->> newpos
                        (map (fn [[pre post]] (if (cando post) post pre)))
                        set)] 
           (recur new (conj (vec (rest order)) (first order)) (inc cnt)))))
     ((fn [final]
        (let [[rs cs] (apply map vector final)
              r-r (inc (- (apply max rs) (apply min rs)))
              r-c (inc (- (apply max cs) (apply min cs)))]
          (- (* r-r r-c) (count final))))))