;advent-of-code-2024.day-06
(ns day06
  (:require [clojure.string :as str]))

(let [Map (->> (slurp "input") str/split-lines (mapv vec))
      start (first 
             (for [y (range (count Map)) x (range (count (first Map)))
                   :when (#{\^ \> \v \<} (get-in Map [y x]))]
               [y x]))
      dir-id ({\^ 0 \> 1 \v 2 \< 3} (get-in Map start))
      dirs [[-1 0] [0 1] [1 0] [0 -1]]
      path (loop [pos start dir dir-id path #{start}]
              (let [np (mapv + pos (dirs dir))
                    nmark (get-in Map np)
                    ndir (mod (inc dir) 4)]
                (case (if (#{\^ \> \v \<} nmark) \. nmark)
                  \. (recur np dir (conj path np))
                  \# (recur pos ndir path)
                  path)))
      candi (disj path start)
      detect-loop (fn [Map]
                    (loop [pos-dir [start dir-id] path #{[start dir-id]}]
                      (let [[pos dir] pos-dir
                            np (mapv + pos (dirs dir))
                            nmark (get-in Map np)
                            ndir (mod (inc dir) 4)]
                        (case (if (#{\^ \> \v \<} nmark) \. nmark)
                          \. (if (path [np dir]) true (recur [np dir] (conj path [np dir])))
                          \# (if (path [pos ndir]) true (recur [pos ndir] path))
                          false))))]
  ;part 1
  (->> path
       count
       println)
  
  ;part 2
  (->> candi
       (filter #(detect-loop (assoc-in Map % \#)))
       count
       println))