(ns scratch
  (:gen-class))

(def screen [1 2 3 4 5 6 7 8 9])
(def stride 3)

(defn get-index
  "returns the index of a supplied coordinate"
  [coord]
  (if (nil? coord)
    nil 
    (+ (first coord) (* (second coord) stride))))

(defn out-of-bounds?
  "checks if the index is outside of the range of our screen array"
  [coord]
  (some #(or (<= stride %) (neg? %)) coord))

(defn coordinates
  "returns the coordinate of the supplied index"
  [index]
  (let [fixed-index (- index 1)]
    [(mod fixed-index stride) (int (/ fixed-index stride))]))

(defn add-to-element
  "adds a value to an element at an index and returns the resulting collection"
  [coll index value]
  (assoc coll index (+ (nth coll index) value)))

(defn neighbors
  "returns the neighbors in an axis"
  [coord index]
  (->> (range -1 2)
       (map (partial add-to-element coord index))))

(defn knight-neighbors
  "returns the neighbors that can be reached via a knights jump"
  [coord]
  (let [offsets (for [x [-1 1] 
                      y [2 -2]]
                  (vector x y))
        offsets (apply concat [offsets (mapv reverse offsets)])]
    (->> offsets 
         (map (partial map + coord))
         (filter (comp not out-of-bounds?))
         (map get-index)
         (map (partial + 1)))))

(defn around 
  "returns the elements around the index"
  [coord]
  (->> coord
       (#(neighbors % 0))
       (mapcat #(neighbors % 1))
       (filter (comp not out-of-bounds?))
       (map get-index)
       (mapv (partial + 1))))

(defn targetables
  "returns the valid moves around the starting point"
  [number]
  (let [coord (coordinates number)]
    (->> coord
         (around)
         (concat (knight-neighbors coord)))))

(defn distance
  "returns [deltaX deltaY]"
  [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn calculate-intermediate
  "calculates the position between start and end in a 2D array"
  [start end]
  (let [start-coord (coordinates start)
        end-coord (coordinates end)
        dist (distance start-coord end-coord)]
    (->> dist 
         (map #(/ % 2))
         (map-indexed (fn [idx itm] (+ (nth start-coord idx) itm)))
         (get-index)
         (+ 1))))

(defn valid-move
  "checks if a move is valid given a list of previous moves and a start/end"
  [prev start end]
  (if (some #{end} prev)
    false
    (let [available (targetables start)]
      (if (some #{end} available)
        true
        (let [intermediate (calculate-intermediate start end)]
          (some #{intermediate} prev))))))

(defn valid-path
  "checks to see if a path is valid by recursively stepping through the path and checking against its history"
  [path]
  (loop [remaining (rest path)
         previous [(first path)]]
    (let [next (first remaining)
          last (last previous)]
      (if (nil? next) true 
          (if (valid-move previous last next)
            (recur (rest remaining)
                   (conj previous (first remaining)))
            false)))))

(assert (true?  (valid-path [1 6 7 4])))
(assert (true?  (valid-path [2 1 3])))
(assert (false? (valid-path [1 9])))
(assert (false? (valid-path [1 2 3 2 1])))
(assert (false? (valid-path [0 1 2 3])))
