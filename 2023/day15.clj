;advent-of-code-2023.day15
(ns day15
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)))