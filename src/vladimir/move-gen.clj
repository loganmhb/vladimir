(ns vladimir.move-gen
  (:require [vladimir.board :refer [square update-square]])
  (:import vladimir.board.Game))

;; Moves are stored as records. :from and :to fields should have vectors
;; of [rank file] indexed 0-7.

(defrecord Move [from to castle? promotion])

(defn create-move
  "Create a Move object."
  [& {:keys [from to castle? promotion] :or [castle? false promotion nil]}]
  (Move. from to castle? promotion))

;; Transformation functions for sliding slider pieces (queen is just
;; rook + bishop)

(def transforms {:rook [#(vector (inc %1) %2)
                        #(vector %1 (inc %2))
                        #(vector (dec %1) %2)
                        #(vector %1 (dec %2))]
                 :bishop [#(vector (inc %1) (inc %2))
                          #(vector (inc %1) (dec %2))
                          #(vector (dec %1) (inc %2))
                          #(vector (dec %1) (dec %2))]})

(defn on-board?
  "Validates that coordinates exist."
  [[to-r to-f]]
  (and (< to-f 8) (< to-r 8) (>= to-f 0) (>= to-r 0)))

(defn valid-square?
  "Validates that target square exists and is either empty or an opponent's piece.
   Short-circuiting in the conditions prevents accessing out-of-bounds or nonexistent
   information."
  [game [to-r to-f]]
  (and (on-board? [to-r to-f])
       (let [contents (square game [to-r to-f])]
         (or (not contents)
             (not (= (:color contents) (:to-move game)))))))

(defn slider-moves
  "Given a transformation describing a direction, returns a vector of moves
   for a sliding piece."
  [transform game piece rank file]
  (loop [r rank
         f file
         moves []]
    (let [[to-r to-f] (transform r f)]
      (if (valid-square? game [to-r to-f])
        (recur to-r
               to-f
               (conj moves (create-move :from [rank file] :to [to-r to-f])))
        moves))))

(defn king-moves [game piece rank file]
  (concat))

(defn rook-moves [game piece rank file]
  (concat (map #(slider-moves % game piece rank file) (:rook transforms))))

(defn bishop-moves [game piece rank file]
  (concat (map #(slider-moves % game piece rank file (:bishop transforms)))))

(defn queen-moves [game piece rank file]
  (concat (map #(% game piece rank file) [rook-moves bishop-moves])))

(defn knight-moves [game piece rank file]
  (let [potentials (for [rmod [-2 -1 1 2]
                          fmod [-2 -1 1 2]
                         :when (not= (Math/abs rmod) (Math/abs fmod))]
                     `(fn [r f] [(+ ~rmod r) (+ ~fmod f)]))]
    (filter valid-square? (map #(% rank file) potentials))))

(defn pawn-moves [game piece rank file])

(defn generate-piece-moves
  "Generates a vector of all the Moves for a given Piece in a Game."
  [game [rank file]]
  (let [f (case (:type (square game [rank file]))
            :king king-moves
            :queen queen-moves
            :rook rook-moves
            :bishop bishop-moves
            :knight knight-moves
            :pawn pawn-moves)]
    (f game rank file)))

(defn generate-pseudolegal-moves
  "Returns a vector of all pseudolegal moves in Move format, without checking for pins"
  [game]
  (let [moves []]
    (flatten (for [rank (range 8)
                   file (range 8)
                   :let [piece (square game [rank file])]]
               (when piece
                 (if (= (:color piece) (:to-move game))
                   (concat moves (generate-piece-moves game piece rank file))))))))

(defn alg-to-rf
  "Takes an algebraic square coordinate  (i.e. 'e6') and converts it to a vector of
   [rank file] by index."
  [alg]
  [(- (read-string (str (last alg))) 1)
   (.indexOf "abcdefgh" (str (first alg)))])

(defn move-from-alg
  "Returns a Move record given an input game and algebraic coordinates."
  [game alg]
  (let [from (alg-to-rf (subs alg 0 2))
        to   (alg-to-rf (subs alg 2 4))
        promote-to (if (= (count alg) 5) (last alg) nil)]
    (create-move :from from
          :to to
          :castle? (if (= (:type (square game from))
                          :king)
                     (case (- (get from 1)
                              (get to 1))
                       2 :kside
                       -2 :qside
                       :else false)
                     false)
          :promotion promote-to)))

(defn update-castle-rights [game move])

(defn update-en-passant-rights [game move])

(defn make-move
  "Returns the game after making the provided Move."
  [game move]
  (let [new-game game]
    (-> (update-square new-game (:to move) (square game (:from move)))
        (update-square (:from move) nil)
        (update-castle-rights move)
        (update-en-passant-rights move))))

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
