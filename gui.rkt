#lang racket/gui

(require "gui-tableview.rkt")
(require "gui-dice.rkt")
(require "yahtzee.rkt")

(define players (list (player-init) (player-init)))

(define (score-table players) 1
  )

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

(define dice-pad
  (new dice-pad%
       [parent application-frame]
       [dices `((#f . 1) (#f . 2) (#t . 3) (#f . 4) (#f . 5))]
       [callback (lambda (dice-idx)
                   (set-field! dices dice-pad `((#f . 1) (#f . 2) (#t . 3) (#f . 4) (#t . 3))))]))

(send application-frame show #t)
