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


(define (draw-dice dc x y color n)
  (define dots
    `(((32 32))
      ((21 21) (43 43))
      ((32 32) (15 15) (49 49))
      ((17 17) (47 47) (17 47) (47 17))
      ((15 15) (49 49) (15 49) (49 15) (32 32))
      ((17 19) (47 45) (17 45) (47 19) (32 19) (32 45))))
  (send dc set-pen color 5 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-rounded-rectangle (+ x 2) (+ y 2) 64 64 4)
  (for ([p (list-ref dots (- n 1))])
    (define dx (- (first p) 2))
    (define dy (- (second p) 2))
    (send dc draw-rounded-rectangle (+ x dx) (+ y dy) 7 7 8))
  )

(define dice-pad%
  (class canvas%
    (inherit get-dc)
    (init-field dices)
    (define/override (on-paint)
      (define dc (get-dc))
      (for ([dice dices]
            [dx (range 2 350 70)])
        (define color (if (car dice) "red" "black"))
        (draw-dice dc dx 2 color (cdr dice))
      ))
    (super-new [min-height 80]
               [min-width 352])))

(new dice-pad%
     [parent application-frame]
     [dices `((#f . 1) (#f . 2) (#t . 3) (#f . 4) (#f . 5) (#f . 6))])

(send application-frame show #t)
