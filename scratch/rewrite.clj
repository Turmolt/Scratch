(ns scrach.rewrite)

(defn check-winner
  "checks a subset to see if it contains just :x or :o"
  [subset]
  (->> subset
       (filter #(or (= % #{:x}) (= % #{:o})))
       ((fn [[x]] (first x)))))

(defn check-subsets
  "checks all subsets to see if they are winners"
  [& sets] 
  (first 
   (filter 
    (comp not nil?)
    (map check-winner sets))))

(defn rows
  "returns a set of the horizontal subsets from the board"
  [board]
  (map set board))

(defn columns
  "returns a set of the vertical subsets from the board"
  [board]
  (map set (apply map list board)))

(defn diagonal
  "returns a diagonal subset from the board"
  [board modifier]
  (list (set  (map #(nth (nth board %) (modifier %)) (range 3)))))

(defn tic-tac-toe
  "checks to see if there is a winner on a tic-tac-toe board"
  [board]
  (check-subsets
   (rows board)
   (columns board)
   (diagonal board (partial + 0))
   (diagonal board (partial - 2))))

(assert (= :x (tic-tac-toe [[:x :o :x] [:x :o :o] [:x :x :o]])))
(assert (= :o (tic-tac-toe [[:o :x :x] [:x :o :x] [:x :o :o]])))
(assert (nil? (tic-tac-toe [[:x :o :x] [:x :o :x] [:o :x :o]])))