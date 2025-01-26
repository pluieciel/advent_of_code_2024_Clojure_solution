;advent-of-code-2023.day24
(ns day24
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(re-seq #"\w+" %))))
;part 1
(defn find-path [[start end] adj]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start [start]])
         visited #{start}]
    (when-not (empty? queue)
      (let [[current path] (peek queue)
            remaining-queue (pop queue)]
        (if (= current end)
          path ; Return the path if end is reached
          (let [neighbors (get adj current [])
                unvisited (remove visited neighbors)
                new-paths (map #(vector % (conj path %)) unvisited)]
            (recur (into remaining-queue new-paths)
                   (into visited unvisited))))))))

(let [data (parse "./2023/in25")
      nodes (set (apply concat data))
      edges (reduce (fn [acc line]
                      (let [[node & neighbors] line]
                        (reduce (fn [acc* neighbor]
                                  (-> acc*
                                      (update node conj neighbor)
                                      (update neighbor conj node)))
                                acc
                                neighbors)))
                    {}
                    data)]
  (defn choose [n coll] (take n (shuffle coll))) ;; random get 2 nodes
  (comment ->> (for [_ (range 100)] ;; Monte Carlo to get the MinCut 3 edges
         (->> (find-path (choose 2 nodes) edges)
              (partition 2 1)
              (map set)))
       (apply concat)
       frequencies
       (sort-by #(- (val %)))
       (take 4))
  
  (let [newedges (reduce (fn [acc [a b]]
                           (-> acc
                               (update a #(remove #{b} %))
                               (update b #(remove #{a} %))))
                         edges
                         [["vfh" "bdj"] ["bnv" "rpd"] ["ttv" "ztc"]]) ;; result from Monte Carlo
        total (count nodes)
        part (loop [queue (conj clojure.lang.PersistentQueue/EMPTY "vfh")
                    visited #{"vfh"}]
               (if (empty? queue)
                 (count visited)
                 (let [current-node (peek queue)
                       neighbors (get newedges current-node [])
                       unvisited (remove visited neighbors)]
                   (recur (into (pop queue) unvisited)
                          (into visited unvisited)))))]
    (* part (- total part))))