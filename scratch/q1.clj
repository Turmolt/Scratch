(ns scratch.q1
  (:require [scratch.common :refer [distance coordinates out-of-bounds? around]]))

(def stride 3)

(defn get-index
  "returns the index of a supplied coordinate"
  [coord]
  (if (nil? coord)
    nil 
    (+ (first coord) (* (second coord) stride))))

(defn knight-neighbors
  "returns the neighbors that can be reached via a knights jump"
  [coord]
  (let [offsets (for [x [-1 1] 
                      y [2 -2]]
                  (vector x y))
        offsets (apply concat [offsets (mapv reverse offsets)])]
    (->> offsets 
         (map (partial map + coord))
         (filter (comp not (partial out-of-bounds? stride)))
         (map get-index)
         (map (partial + 1)))))

(defn targetables
  "returns the valid moves around the starting point"
  [number]
  (let [coord (coordinates number stride)]
    (->> coord
         (around stride)
         (map get-index)
         (mapv (partial + 1))
         (concat (knight-neighbors coord)))))

(defn calculate-intermediate
  "calculates the position between start and end in a 2D array"
  [start end]
  (let [start-coord (coordinates start stride)
        end-coord (coordinates end stride)
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