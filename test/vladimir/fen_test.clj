(ns vladimir.fen-test
  (:require [clojure.test :refer :all]
            [vladimir.fen :refer :all]))

(deftest char-to-piece-test
  (is (map char-to-piece "kqrbnp")
      '(:king :queen :rook :bishop :knight :pawn)))

(deftest parse-row-test
  (is (parse-row "kq..rbnp")
      [{:color :black :type :king}
       {:color :black :type :queen}
       nil
       nil
       {:color :black :type :rook}
       {:color :black :type :bishop}
       {:color :black :type :knight}
       {:color :black :type :pawn}]))
