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

(deftest finding-end-cell
  (let [cells [
               [\# \# \#]
               [\# \X \#]
               [\# \O \#]
               ]]
  (is (= [1 1]
         (find-endcell cells [[1 1] [0 1]])))
  (is (nil? (find-endcell cells [[0 0] [2 2]])))))

(deftest creating-path
  (is (= [:north :north :north]
         (create-steps [[5 5] [5 4] [5 3] [5 2]])))
  (is (= [:north :east :south :west]
         (create-steps [[5 5] [5 4] [6 4] [6 5] [5 5]]))))

(deftest normal-mode
  (is (= (path-of [9 :north]
                  [6 :east]
                  [2 :south]
                  [2 :east]
                  [2 :south]
                  [4 :west]
                  [2 :north]
                  [2 :west]
                  [7 :south])
         (solution-for "normal" "001")))
  (is (= (path-of [7 :east]
                  [4 :north]
                  [2 :west]
                  [2 :south]
                  [4 :west]
                  [2 :north]
                  [2 :east])
         (solution-for "normal" "002"))))
