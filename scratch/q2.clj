(ns scratch.q2
  (:require [scratch.common :refer [around add-to-element distance]]))

(def stride (atom 0))

(defn map-coordinates
  "returns a vector of coordinates representing our letter matrix"
  [matrix]
  (for [y (range (count matrix))]
    (for [x (range (count (first matrix)))]
      (vector x y))))

(defn create-dictionary 
  "a hashmap with the coordinates of each letter"
  [matrix]
  (zipmap (apply concat (map-coordinates matrix))
          (apply concat matrix)))

(defn select-values
  "returns all the [keys value] pair of the desired value in a collection"
  [target query]
  (filter (fn [[_ value]] (= value query)) target))

(defn find-direction
  "searches neighbors for a letter and returns the direction of the letter if one is found"
  [dictionary start letter]
  (->> (around @stride start)
       (filter (comp (partial = letter) dictionary))
       (map (partial distance start))))

(defn check-direction
  "checks a supplied direction for the rest of the word we are searching for"
  [dictionary start direction letters]
  (if (nil? direction)
    false 
    (loop [position (map + start direction)
           query (first letters)
           next (rest letters)]
      (if (nil? query)
        true
        (if (= query (dictionary position))
          (recur (map + position direction)
                 (first next)
                 (rest next))
          false)))))

(defn check-all-directions
  "checks all the possible directions for the rest of the word"
  [dictionary directions letters index item]
  (check-direction dictionary
                   (first item)
                   (first (nth directions index))
                   letters))

(defn count-words-in-matrix
  "finds all occurances of the first letter of the matrix and then checks for the rest of the word around it"
  [matrix word]
  (reset! stride (count (first matrix)))
  (let [dictionary   (create-dictionary matrix)
        characters   (char-array word)
        possible     (select-values dictionary (first characters))
        letters      (rest characters)
        directions   (map (comp #(find-direction dictionary % (first letters)) first) possible)]
    (if (> (count possible) 0)
      (-> (if (< 0 (count letters))
            (->> possible
                 (map-indexed (partial check-all-directions dictionary directions letters))
                 (filter true?))
            possible)
          (count))
      false)))


