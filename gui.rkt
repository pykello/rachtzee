#lang racket/gui

(require "gui-tableview.rkt")
(require "gui-dice.rkt")
(require "yahtzee.rkt")

(define players (list (player-init) (player-init)))

(define (player-score-table p)
  (define s (player-scores p))
  (map ~v
       (append `("P")
               (scores-upper-section s)
               (list 0 0)
               (scores-lower-section s)
               (list (scores-yahtzee s))
               (list 0))))

(define (score-table players)
  (transpose
   (append `(("", "Ones", "Twos", "Threes", "Fours", "Fives", "Sixes", "Sum", "Bonus",
                 "Three of a kind", "Four of a kind", "Full House", "Small Straight",
                 "Large Straight", "Chance", "YAHTZEE", "TOTAL SCORE"))
           (map player-score-table players))))

(define (transpose xss)
  (apply map list xss))

(define application-frame (new frame% [label "Rachtzee"]))

(new table-view%
     [parent application-frame]
     [vert-margin 20]
     [horiz-margin 20]
     [table (score-table players)]
     [callback (lambda (row col) (print col))])

(define dice-pad
  (new dice-pad%
       [parent application-frame]
       [dices `((#f . 1) (#f . 2) (#t . 3) (#f . 4) (#f . 5))]
       [callback (lambda (dice-idx)
                   (set-field! dices dice-pad `((#f . 1) (#f . 2) (#t . 3) (#f . 4) (#t . 3))))]))

(send application-frame show #t)
