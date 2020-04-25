(ns scratch.common)

(defn distance
  "returns [deltaX deltaY]"
  [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn coordinates
  "returns the coordinate of the supplied index"
  [index stride]
  (let [fixed-index (- index 1)]
    [(mod fixed-index stride) (int (/ fixed-index stride))]))

(defn out-of-bounds?
  "checks if the index is outside of the range of our screen array"
  [stride coord]
  (some #(or (<= stride %) (neg? %)) coord))

(defn add-to-element
  "adds a value to an element at an index and returns the resulting collection"
  [coll index value]
  (assoc coll index (+ (nth coll index) value)))

(defn neighbors
  "returns the neighbors in an axis"
  [coord index]
  (->> (range -1 2)
       (map (partial add-to-element coord index))))

(defn around
  "returns the elements around the index"
  [stride coord]
  (->> coord
       (#(neighbors % 0))
       (mapcat #(neighbors % 1))
       (filter (comp not (partial out-of-bounds? stride)))))