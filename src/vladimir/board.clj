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

(ns vladimir.board)

;; Basic functions and constants for interfacing with Forsyth-Edwards notation

(def start-fen
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defn expand-fen
  "Replaces numbers in FEN-style board with '.' to facilitate indexing"
  [board-string]
  (clojure.string/replace board-string #"[1-8]" #(apply str (repeat (bigint %1) "."))))

;; The game object needs to have all of the information that the FEN
;; contains, but in a more computable form -- not just the locations
;; of all the pieces.

;; Vladimir stores games as maps:

(defrecord Game [board to-move castles enp halfmoves fullmoves])

(defrecord Piece [color type])

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
  (let [parse-char (fn [c] (if (character-to-piece c)
                             (Piece. (if (java.lang.Character/isUpperCase c)
                                       :white
                                       :black)
                                     (character-to-piece c))
                             nil))]
    (vec (map parse-char row))))

(defn board-from-fen
  "Converts part 1 of FEN string to array representation of the board."
  [fen-part]
  (vec (reverse (map parse-row
                     (clojure.string/split (expand-fen fen-part) #"/")))))

(defrecord LegalCastles [white black])

(defrecord CastleRights [kside qside])

(defn legal-castles
  "Given FEN part 3, returns a LegalCastles record."
  [castles]
  (let [scan (fn [pattern] (if (re-find pattern castles) true false))]
    (LegalCastles. (CastleRights. (scan #"K")
                                  (scan #"Q"))
                   (CastleRights. (scan #"k")
                                  (scan #"q")))))

(defn make-game 
  "Creates a game object given the data from a FEN string."
  [fen]
  (let [[board to-move castles enp halfmoves fullmoves]
        (clojure.string/split fen #" ")]
    (Game. (board-from-fen board)
           (if (= to-move "w") :white :black)
           (legal-castles castles)
           (if (= enp "-") nil enp)
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

;; Moves are stored as records. from and to fields should have vectors
;; of [rank file] indexed 0-7.

(defrecord Move [piece from to capture?])

;; Transformation functions for moving slider pieces (queen is just
;; rook + bishop)

(def transforms {:rook [#(vector (inc %1) %2)
                        #(vector %1 (inc %2))
                        #(vector (dec %1) %2)
                        #(vector %1 (dec %2))]
                 :bishop [#(vector (inc %1) (inc %2))
                          #(vector (inc %1) (dec %2))
                          #(vector (dec %1) (inc %2))
                          #(vector (dec %1) (dec %2))]})

(defn slider-moves
  "Given a transformation, return a vector of moves for a sliding piece."
  [transform game piece rank file]
  (loop [r rank
         f file
         moves []]
    (let [[to-r to-f] (transform r f)]
      ;; Short-circuiting in this if prevents out-of-bounds errors or
      ;; attempting to call :color on an empty square.
      (if (and (and (< to-f 8) (< to-r 8) (>= to-f 0) (>= to-r 0))
               (or (not (((:board game) to-r) to-f))
                   (= (:color (((:board game) to-r) to-f)))))
        (recur to-r
               to-f
               (conj moves
                     (Move. piece
                            [rank file]
                            [to-r to-f]
                            (if (((:board game) to-r) to-f) true false))))
        moves))))

(defn king-moves [game piece rank file])

(defn rook-moves [game piece rank file]
  (concat (map #(slider-moves % game piece rank file) (:rook transforms))))

(defn bishop-moves [game piece rank file]
  (concat (map #(slider-moves % game piece rank file (:bishop transforms)))))

(defn queen-moves [game piece rank file]
  (concat (map #(% game piece rank file) [rook-moves bishop-moves])))

(defn knight-moves [game piece rank file])

(defn pawn-moves [game piece rank file])

(defn generate-piece-moves
  "Generates a vector of all the Moves for a given Piece in a Game."
  [game piece rank file]
  (let [f (case (:type piece)
            :king king-moves
            :queen queen-moves
            :rook rook-moves
            :bishop bishop-moves
            :knight knight-moves
            :pawn pawn-moves)]
    (f game rank file)))

(defn generate-moves
  "Returns a vector of all legal moves in Move format."
  [game]
  (let [moves []]
    (flatten (for [rank (range 8)
                   file (range 8)
                   :let [piece (((:board game) rank) file)]]
               (when piece
                 (if (= (:color piece) (:to-move game))
                   (concat moves (generate-piece-moves game piece rank file))))))))

(defn alg-to-coord
  "Takes an algebraic square coordinate and converts it to [rank file] indices."
  [alg]
  [(- (read-string (str (last alg))) 1)
   (.indexOf "abcdefgh" (str (first alg)))])

(defn move-from-alg [game alg]
  (let [from (alg-to-coord (subs alg 0 2))
        to   (alg-to-coord (subs alg 2 4))
        promote (if (= (count alg) 5) (last alg) nil)]
    (Move. (((:board game) (first from)) (last from))
           from
           to
           (if (((:board game) (first to)) (last to))
             true
             false))))

(defn make-move
  "Returns the game after making the provided Move."
  [game move]
  (let [new-game game]
    (-> (update-square new-game (:to move) (:piece move))
        (update-square (:from move) nil))))

(defn make-moves
  "Given a sequence of moves, executes them sequentially."
  [game moves]
  (reduce make-move (cons game moves)))

(defn make-alg-move
  "Returns the game resulting from making the specified move in the provided game."
  [game alg]
  (make-move game (move-from-alg game alg)))

(defn make-alg-moves
  "Returns the game object created by making all of the moves in the vector
   (in long algebraic format)."
  [game moves]
  (make-moves game (map #(move-from-alg game %) moves)))
