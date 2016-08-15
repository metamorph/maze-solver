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

(defn path-of [dirs]
  (vec (mapcat (fn [[d n]] (repeat n d)) dirs)))

(deftest normal-mode
  (is (= (path-of [[:north 9]
                   [:east 6]
                   [:south 2]
                   [:east 2]
                   [:south 2]
                   [:west 4]
                   [:north 2]
                   [:west 2]
                   [:south 7]]) (solution-for "normal" "001")))
  (is (= (path-of [[:east x]
                   [:north x]
                   [:west x]
                   [:south x]
                   [:west x]
                   [:north x]
                   [:east x]]) (solution-for "normal" "002")))
  (is (= [] (solution-for "normal" "003")))
  (is (= [] (solution-for "normal" "004"))))
