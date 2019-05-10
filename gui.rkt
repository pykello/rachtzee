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


(define (draw-dice dc x y n)
  (define dots
    `(((32 32))
      ((21 21) (43 43))
      ((32 32) (15 15) (49 49))
      ((17 17) (47 47) (17 47) (47 17))
      ((15 15) (49 49) (15 49) (49 15) (32 32))
      ((17 19) (47 45) (17 45) (47 19) (32 19) (32 45))))
  (send dc set-pen "black" 5 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-rounded-rectangle (+ x 2) (+ x 2) 64 64 4)
  (for ([p (list-ref dots (- n 1))])
    (define x (- (first p) 2))
    (define y (- (second p) 2))
    (send dc draw-rounded-rectangle x y 7 7 8))
  )
(define dice-view%
  (class canvas%
    (inherit get-dc)
    (define/override (on-paint)
      (define dc (get-dc))
      (draw-dice dc 0 0 6)
      )
    (super-new [min-height 256]
               [min-width 256])))

(new dice-view%
     [parent application-frame])

(send application-frame show #t)
