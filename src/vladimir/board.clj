;; Board representation

;; This file contains Vladimir's understanding of the rules of chess.
;; Vladimir uses a record to store game information, corresponding to
;; the six parts of Forsyth-Edwards notation plus a collection of
;; positions to compare against for threefold repetition (cleared
;; anytime a pawn is moved or (provided there are no pawns) a capture
;; is made).

;; The board itself is represented as vector of vectors -- each
;; first-level vector corresponding to a row, and each second-level
;; vector containing Piece records and nil placeholders for empty
;; squares. The vectors are constructed so that (0, 0) refers to a1
;; and (7, 7) to h8.

(ns vladimir.board
  (require [vladimir.fen :refer :all]))

(defrecord Game [board to-move castles en-passant halfmoves fullmoves])

(defn piece
  "Create a map representing a piece."
  [& args]
  (apply hash-map args))

(defn legal-castles
  "Given FEN part 3, returns a LegalCastles record."
  [castles]
  (let [scan (fn [pattern] (if (re-find pattern castles) true false))]
    {:white {:kside (scan #"K") :qside (scan #"Q")}
     :black {:kside (scan #"k") :qside (scan #"q")}}))

(defn game 
  "Creates a game object given a FEN representation."
  [fen]
  (let [[board to-move castles en-passant halfmoves fullmoves]
        (clojure.string/split fen #" ")]
    (Game. (board-from-fen board)
           (if (= to-move "w") :white :black)
           (legal-castles castles)
           (if (= en-passant "-") nil en-passant)
           (bigint halfmoves)
           (bigint fullmoves))))

(defn square
  "Convenience function for accessing the contents of a particular square."
  [game [rank file]]
  (((:board game) rank) file))

(defn update-square
  "Return a modified game with the contents of the given square updated."
  [game [rank file] new-square]
  (assoc game :board
         (assoc (:board game) rank
                (assoc ((:board game) rank) file new-square))))
