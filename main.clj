(ns scratch)

(def screen [1 2 3 4 5 6 7 8 9])
(def stride 3)

(defn coordinates
  "returns the coordinate of the supplied index"
  [index]
  [(mod index stride) (int (/ index stride))])

(defn around 
  "returns the elements around the index"
  [index]
  (-> index
   (coordinates)))