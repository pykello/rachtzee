#lang racket/gui

(require "gui-tableview.rkt")
(require "gui-dice.rkt")

(define example-table
  `(("" "You" "Bill")
    ("Ones" "" "")
    ("Twos" "" "")
    ("Threes" "" "")
    ("Fours" "" "")
    ("Fives" "" "")
    ("Sixes" "" "")
    ("Sum" "" "")
    ("Bonus" "" "")
    ("Three of a kind" "" "")
    ("Four of a kind" "" "40")
    ("Full House" "" ("red" "25"))
    ("Small Straight" "" "")
    ("Large Straight" "" "")
    ("Chance" "" "")
    ("YAHTZEE" "" "")
    ("TOTAL SCORE" "" "")))

(define application-frame (new frame% [label "Rachtzee"]))

(new table-view%
     [parent application-frame]
     [vert-margin 20]
     [horiz-margin 20]
     [table example-table]
     [callback (lambda (row col) (print col))])

(new dice-pad%
     [parent application-frame]
     [dices `((#f . 1) (#f . 2) (#t . 3) (#f . 4) (#f . 5) (#f . 6))]
     [callback (lambda (dice-idx) (print dice-idx))])

(send application-frame show #t)
