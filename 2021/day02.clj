;advent-of-code-2021.day02
(ns day02
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(let [[dir step] (re-seq #"\w+|\d+" %)] {dir (read-string step)}))))

(def data (parse "2021/ex02"))

;part 1
(let [{fo "forward" up "up" do "down"} (apply merge-with + data)]
  (* fo (- do up)))

;part 2
(loop [h 0 d 0 aim 0 todo data]
  (if (empty? todo)
    (* h d)
    (let [[dir step] (ffirst todo)]
      (case dir
        "forward" (recur (+ h step) (+ d (* aim step)) aim (rest todo))
        "up" (recur h d (- aim step) (rest todo))
        "down" (recur h d (+ aim step) (rest todo))))))