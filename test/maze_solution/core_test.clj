(ns maze-solution.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [maze-solution.core :refer :all]))

(defn maze-read [class n]
  (-> (format "maze-%s-%s.txt" class n)
      (io/resource)
      (slurp)))

(defn solution-for [class n]
  (-> (maze-read class n)
      (parse-maze)
      (solve-maze)))

(defn path-of [& pairs]
  (mapcat (fn [[n d]] (repeat n d)) pairs))

(deftest normal-mode
  (is (= (path-of [9 :n]
                  [6 :e]
                  [2 :s]
                  [2 :e]
                  [2 :s]
                  [4 :w]
                  [4 :w]
                  [2 :n]
                  [2 :w]
                  [7 :s])
         (solution-for "normal" "001"))))
