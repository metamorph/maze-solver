(ns maze-solution.core)

(defn m-get
  "Get the value in a 2d array. Returns nil if out-of-bounds."
  [maze [x y]]
  (if (and (>= x 0)
           (>= y 0)
           (>= (count maze) y)
           (>= (count (maze y)) x))
    ((maze y) x)
    nil))

(defn neighbours
  "The coordinates around a cell"
  [xy]
  (map #(map + xy %) [[-1 0] [0 -1] [1 0] [0 1]]))

(defn to-directions
  "Convert a set of coordinates into n/e/s/w steps"
  [coords] coords)

(defn next-step
  "TODO: Perform the 'next step' - the next version - of the maze solution."
  [maze] maze)

(defn step [maze] (if (:done maze) maze
                      ;; Short-circuit until we've implemented 'next-step'
                      (next-step (assoc maze :done true))))

(defn parse-maze
  "Read a maze as string - convert internal representation"
  [data]
  (let [;; Split into 2d vector
        char-matrix    (->> data
                            (clojure.string/split-lines)
                            (map vec)
                            (vec))
        ;; Generate coord
        coords         (for [y (range (count char-matrix))
                             x (range (count (get char-matrix y)))] [x y])
        ;; Find the starting-point
        starting-point (first
                        (filter
                         (fn [xy] (= \O (m-get char-matrix xy)))
                         coords))]

    {:cells          char-matrix
     :starting-point starting-point
     :position       starting-point
     :trail          []
     :visited        #{starting-point}}))

(defn solve-maze [maze]
  (let [;; Drop all solution steps until a path have been found
        solution (->> (iterate step maze)
                      (drop-while #(not (:done %)))
                      (first))]
    ;; Prepend the starting point to the coords of the solution, and convert to 'directions'
    (to-directions (cons (:starting-point solution)
                         (:trail solution)))))
