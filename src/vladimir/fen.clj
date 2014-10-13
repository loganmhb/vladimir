(ns vladimir.fen
  (:require [vladimir.board :refer [piece]]))

;; Basic functions and constants for interfacing with Forsyth-Edwards notation

(def start-fen
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defn expand-fen
  "Replaces numbers in FEN-style board with '.' to facilitate indexing"
  [board-string]
  (clojure.string/replace board-string #"[1-8]" #(apply str (repeat (bigint %1) "."))))

(defn character-to-piece
  "Defines character to piece equivalencies."
  [fen-char]
  (let [p (clojure.string/lower-case fen-char)]
    (get {"k" :king, "q" :queen, "r" :rook, "b" :bishop, "n" :knight, "p" :pawn}
         p
         nil)))

(defn parse-row
  "Takes fen-encoded row and returns a vector describing it in terms of empty
   squares and pieces."
  [row]
  (let [parse-char
        (fn [c] (if (character-to-piece c)
                  (piece :color (if (java.lang.Character/isUpperCase c)
                                    :white
                                    :black)
                         :type (character-to-piece c))
                  nil))]
    (vec (map parse-char row))))

(defn board-from-fen
  "Converts part 1 of FEN string to array representation of the board."
  [fen-part]
  (vec (reverse (map parse-row
                     (clojure.string/split (expand-fen fen-part) #"/")))))
