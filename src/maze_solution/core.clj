(ns maze-solution.core)

(defn m-get
  "Get the value in a 2d array. Returns nil if out-of-bounds."
  [arr [x y]]
  (if (and (>= x 0)
           (>= y 0)
           (> (count arr) y)
           (> (count (arr y)) x))
    ((arr y) x)
    nil))

(defn neighbours
  "TODO: Make prettier. The coordinates around a cell."
  [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)])
       [[-1 0] [0 -1] [1 0] [0 1]]))

(defn neighbours-with-direction
  "TODO: Duplication ..."
  [[x y]]
  (map (fn [[dx dy d]] [(+ x dx) (+ y dy) d])
       [[-1 0 :west] [0 -1 :north] [1 0 :east] [0 1 :south]]))

(defn remove-visited
  "Removes those neighbours that are in the set +visited+"
  [visited neighbours]
  (filter #(not (some? (visited %))) neighbours))

(defn find-endcell
  "Return the end-cell if it's one of the neighbours"
  [cells neighbours]
  (first (filter #(= \X (m-get cells %)) neighbours)))

(defn create-steps
  "Convert a series of coordinates into a sequence of directional
  steps (:north, :south, :west, :east)"
  [cells]
  (let [reducer (fn [c1 c2]
                  ;; Grab the neighbours to c1
                  ;; c2 must be one of them (or fail)
                  ;; return the direction dimension of the c2 neighbour
                  (let [neighbours (neighbours-with-direction c1)
                        [_ _ d] (first (filter (fn [[x y _]]
                                                 (= [x y] c2)) neighbours))]
                    (if d d
                        (throw (IllegalArgumentException. (format "Cells %s  and %s are not neighbours" c1 c2))))))]

    (map #(apply reducer %)
         ;; Map over a sliding window with size 2 and step 1
         (partition 2 1 cells))))

(defn next-step
  "Find the next step in the maze solution."
  [{:keys [position visited trail cells] :as maze}]
  (let [;; Find the neighbours
        neighbours (->> (neighbours position)
                        (remove #(nil? (m-get cells %)))
                        (remove #(= \# (m-get cells %)))
                        (remove-visited visited))]
    (if (empty? neighbours)
      ;; All neighbours are visited - need to backtrack!
      ;; drop the last element in trail, and set that as position
      (-> maze
          (assoc :position (first trail)) ;; update current position
          (assoc :trail (drop 1 trail)) ;; update trail
          (assoc :visited (conj visited position))) ;; set position as visited

      ;; Look for the end cell amongst the neighbours
      (if-let [end-cell (find-endcell cells neighbours)]
        ;; We found it! Add it to the trail and call it a day
        (-> maze
            (assoc :trail (conj (conj trail position) end-cell))
            (assoc :done true))

        ;; Noop - select a random neighbour and carry on)
        (-> maze
            (assoc :trail (conj trail position))
            (assoc :position (rand-nth neighbours))
            (assoc :visited (conj visited position)))))))

(defn render-solution
  ;; TODO: Render the solution to stdout.
  [maze] "")

(defn step [maze]
  (if (:done maze)
    maze
    (next-step maze)))

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
                        (filter #(= \O (m-get char-matrix %)) coords))]

    {:cells          char-matrix
     :starting-point starting-point
     :position       starting-point
     :trail          '()
     :visited        #{starting-point}}))

(defn solve-maze [maze]
  (let [;; Drop all solution steps until a path have been found
        solution (->> (iterate step maze)
                      (drop-while #(not (:done %)))
                      (first))]
    ;; The steps through the maze is in :trail
    ;; Convert that to steps
    (create-steps (reverse (:trail solution)))))
