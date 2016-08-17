(ns maze-solution.core)

(defn m-get
  "Get the value in a 2d array. Returns nil if out-of-bounds."
  [arr [x y]]
  (if (and (>= x 0)
           (>= y 0)
           (>= (count arr) y)
           (>= (count (arr y)) x))
    ((arr y) x)
    nil))

(defn neighbours
  "TODO: The coordinates around a cell."
  [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)])
       [[-1 0] [0 -1] [1 0] [0 1]]))

(defn remove-visited
  "Removes those neighbours that are in the set +visited+"
  [neighbours visited]
  (filter (fn [[x y]] (not (some? (visited [x y])))) neighbours))

(defn find-endcell
  "TODO: Return the end-cell if it's one of the neighbours"
  [cells neighbours] nil)

(defn create-steps
  "TODO: Convert a series of coordinates into a sequence of directional
  steps (:north, :south, :west, :east)"
  [cells] nil)

(defn next-step
  "Find the next step in the maze solution"
  [{:keys [position visited trail cells] :as maze}]
  (let [
        ;; Find the neighbours
        neighbours (neighbours position)
        ;; Remove visited cells
        neighbours (remove-visited neighbours visited)]
    (if (empty? neighbours)
      ;; All neighbours are visited - need to backtrack!
      ;; drop the last element in trail, and set that as position
      (-> maze
          (assoc :position (last trail)) ;; update current position
          (assoc :trail (drop-last trail)) ;; update trail
          (assoc :visited (conj visited position))) ;; set position as visited

      ;; Look for the end cell amongst the neighbours
      (if-let [end-cell (find-endcell cells neighbours)]
        ;; We found it! Add it to the trail and call it a day
        (-> maze
            (assoc :trail (conj trail end-cell))
            (assoc :done true))

        ;; Noop - select a random neighbour and carry on)
        (-> maze
            (assoc :position (rand-nth neighbours))
            (assoc :visited (conj visited position)))))))

(defn render-solution
  ;; TODO: Render the solution to stdout.
  [maze] "")


(defn step [maze] (if (:done maze) maze
                      ;; TODO: Short-circuit until we've implemented 'next-step'
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
    ;; The steps through the maze is in :trail
    ;; Convert that to steps
    (do
      (prn (render-solution solution))
      (create-steps (:trail solution)))))
