#lang racket/gui

(require "gui-tableview.rkt")

(define application-frame (new frame% [label "Rachtzee"]))

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


; Make a canvas that handles events in the frame
(new table-view%
     [parent application-frame]
     [vert-margin 20]
     [horiz-margin 20]
     [table example-table]
     [callback (lambda (row col) (print col))])

(send application-frame show #t)