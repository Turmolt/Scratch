(ns scratch.core
  (:require [scratch.q1 :as q1]
            [scratch.q2 :as q2]))

;Question 1 Test Cases
(assert (true?  (q1/valid-path [1 6 7 4])))
(assert (true?  (q1/valid-path [2 1 3])))
(assert (false? (q1/valid-path [1 9])))
(assert (false? (q1/valid-path [1 2 3 2 1])))
(assert (false? (q1/valid-path [0 1 2 3])))

;Question 2 Test Cases
(def mat
  [[\A \O \T \D \L \R \O \W]
   [\L \C \B \M \U \M \L \U]
   [\D \R \U \J \D \B \L \J]
   [\P \A \Z \H \Z \Z \E \F]
   [\B \C \Z \E \L \F \H \W]
   [\R \K \U \L \V \P \P \G]
   [\A \L \B \L \P \O \P \Q]
   [\B \E \M \O \P \P \J \Y]]) 

(assert (= 2 (q2/count-words-in-matrix mat "HELLO")))
(assert (= 1 (q2/count-words-in-matrix mat "WORLD")))
(assert (= 2 (q2/count-words-in-matrix mat "BUZZ")))
(assert (= 0 (q2/count-words-in-matrix mat "CLOJURE")))
(assert (= 0 (q2/count-words-in-matrix mat "COWABUNGA")))