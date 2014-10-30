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
  (:gen-class))

;; Type definitions

(defrecord Game [board to-move castles en-passant halfmoves fullmoves])

(defrecord Piece [color type castle?])

;; Constructor functions

(defn create-piece
  "Friendlier syntax for creating a Piece record."
  [& {:keys [color type]}]
  (Piece. color type))

(defmacro create-game
  "Likewise for game records."
  [& args]
  (let [args (apply hash-map args)]
    `(Game. ~@(map #(% args)
                   [:board :to-move :castles :en-passant :halfmoves :fullmoves]))))

;; Basic functions and constants for interfacing with Forsyth-Edwards notation

(def start-fen
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defn expand-fen
  "Replaces numbers in FEN-style board with '.' to facilitate indexing"
  [board-string]
  (clojure.string/replace board-string #"[1-8]" #(apply str (repeat (bigint %1) "."))))

(defn char-to-piece
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
        (fn [c] (if (char-to-piece c)
                  (create-piece :color (if (java.lang.Character/isUpperCase c)
                                    :white
                                    :black)
                         :type (char-to-piece c))
                  nil))]
    (vec (map parse-char row))))

(defn board-from-fen
  "Converts part 1 of FEN string to array representation of the board."
  [fen-part]
  (vec (reverse (map parse-row
                     (clojure.string/split (expand-fen fen-part) #"/")))))

(defn legal-castles
  "Given FEN part 3, returns a LegalCastles record."
  [castles]
  (let [scan (fn [pattern] (if (re-find pattern castles) true false))]
    {:white {:kside (scan #"K") :qside (scan #"Q")}
     :black {:kside (scan #"k") :qside (scan #"q")}}))

(defn game-from-fen 
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
  "Utility function for accessing the contents of a particular square."
  [game [rank file]]
  (((:board game) rank) file))

(defn update-square
  "Return a modified game with the contents of the given square updated."
  [game [rank file] new-content]
  (update-in game [:board rank file] (fn [x] new-content)))
