;advent-of-code-2021.day16
(ns day16
  (:require [clojure.string :as str]))

(defn parse [i] (slurp i))

(defn parse-num [str dec] (Integer/parseInt str dec))

(defn hex->bin "'A'->'1010'" [h]
  (-> (parse-num h 16)
      Integer/toBinaryString
      (#(format "%4s" %))
      (str/replace " " "0")))

(def data (parse "2021/in16"))
(def bin (->> (str/split data #"")
              (map hex->bin)
              (apply str)))
(def versions (atom []))
(defn get-version-type [start]
  (let [v (-> (subs bin start (+ start 3)) (parse-num 2))
        t (-> (subs bin (+ start 3) (+ start 6)) (parse-num 2))]
    (swap! versions conj v)
    [t (+ start 6)]))

(defn get-all [start]
  (let [[t pos] (get-version-type start)]
    (if (= t 4)
      (let [[pos info] (loop [pos pos info '()]
                         (if (= (subs bin pos (+ pos 1)) "0")
                           [(+ pos 5) (conj info (parse-num (subs bin (inc pos) (+ pos 5)) 2))]
                           (recur (+ pos 5) (conj info (parse-num (subs bin (inc pos) (+ pos 5)) 2)))))]
        [pos (->> (map * info (iterate #(* % 16) 1)) (reduce +))])
      (let [ltype (subs bin pos (+ pos 1))
            pos (inc pos)
            [pos info] (if (= ltype "0")
                         (let [len (-> (subs bin pos (+ pos 15)) (parse-num 2))
                               pos (+ pos 15)
                               end (+ pos len)
                               [pos info] (loop [pos pos info []]
                                            (if (< pos end)
                                              (let [[pos newinfo] (get-all pos)]
                                                (recur pos (conj info newinfo)))
                                              [pos info]))]
                           [pos info])
                         (let [len (-> (subs bin pos (+ pos 11)) (parse-num 2))
                               pos (+ pos 11)
                               [pos info] (loop [pos pos info []]
                                            (if (< (count info) len)
                                              (let [[pos newinfo] (get-all pos)]
                                                (recur pos (conj info newinfo)))
                                              [pos info]))]
                           [pos info]))]
        [pos (case t
               0 (reduce + info)
               1 (reduce * info)
               2 (apply min info)
               3 (apply max info)
               5 (if (apply > info) 1 0)
               6 (if (apply < info) 1 0)
               7 (if (apply = info) 1 0))]))))

(let [[_ info] (get-all 0)]
  ;part 1
  (println (reduce + @versions))
  ;part 2
  info)