;advent-of-code-2024.day-06
;part 1
(let [Map (->> (slurp "input") clojure.string/split-lines)
      start (first
             (for [y (range (count Map)) x (range (count (first Map)))
                   :when (#{\^ \> \v \<} (get-in Map [y x]))]
               [y x]))
      dir-id ({\^ 0 \> 1 \v 2 \< 3} (get-in Map start))
      dirs [[-1 0] [0 1] [1 0] [0 -1]]]
  (->>
    (loop [pos start dir dir-id path #{start}]
      (let [np (mapv + pos (dirs dir))
            nmark (get-in Map np)]
        (case (if (#{\^ \> \v \<} nmark) \. nmark)
          \. (recur np dir (conj path np))
          \# (recur pos (mod (inc dir) 4) path)
          (count path))))
    println))

;part 2
(let [Map (->> (slurp "input") clojure.string/split-lines (mapv vec))
      start (first
             (for [y (range (count Map)) x (range (count (first Map)))
                   :when (#{\^ \> \v \<} (get-in Map [y x]))]
               [y x]))
      dir-id ({\^ 0 \> 1 \v 2 \< 3} (get-in Map start))
      dirs [[-1 0] [0 1] [1 0] [0 -1]]
      path (loop [pos-dir [start dir-id] path #{[start dir-id]}]
              (let [[pos dir] pos-dir
                    np (mapv + pos (dirs dir))
                    nmark (get-in Map np)
                    ndir (mod (inc dir) 4)]
                (case (if (#{\^ \> \v \<} nmark) \. nmark)
                  \. (recur [np dir] (conj path [np dir]))
                  \# (recur [pos ndir] path)
                  path)))
      candi (->> path (map first) (remove #(= % start)) set)
      detect-loop (fn [Map start dir-id]
                    (loop [pos-dir [start dir-id] path #{[start dir-id]}]
                      ;(println pos-dir)
                      (let [[pos dir] pos-dir
                            np (mapv + pos (dirs dir))
                            nmark (get-in Map np)
                            ndir (mod (inc dir) 4)]
                        (case (if (#{\^ \> \v \<} nmark) \. nmark)
                          \. (if (path [np dir]) true (recur [np dir] (conj path [np dir])))
                          \# (if (path [np dir]) true (recur [pos ndir] path))
                          false))))
      ]
  (->> 
    (for [c candi :when (detect-loop (assoc-in Map c \#)start dir-id)] 1)
    (reduce +)
    println))
