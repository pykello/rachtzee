#lang racket/gui

(require "gui-tableview.rkt")
(require "gui-dice.rkt")
(require "yahtzee.rkt")

(define players (list (player-init "Alice") (player-init "Bob")))

(define (render-scores scores funcs dice-values highlight-scores?)
  (for/list ([score scores]
             [func funcs])
    (cond
      [(number? score) `("black", (number->string score))]
      [highlight-scores? `("red", (number->string (func dice-values)))]
      [else ""])))

(define (player-score-table p highlight-scores?)
  (let* ([s (player-scores p)]
         [dice-values (player-dice-values p)]
         [upper-section
          (render-scores (scores-upper-section s) upper-section-funcs dice-values highlight-scores?)]
         [lower-section
          (render-scores (scores-lower-section s) lower-section-funcs dice-values highlight-scores?)]
         [yahtzee
          (render-scores (list (scores-yahtzee s)) `(score-yahtzee) dice-values highlight-scores?)])
    (append (list (player-name p))
            upper-section
            (list (number->string (scores-upper-section-total s))
                  (number->string (scores-bonus s)))
            lower-section
            yahtzee
            (list (number->string (scores-total-score s))))))

(define sample-dices `((#f . 1) (#f . 2) (#t . 3) (#f . 4) (#f . 5)))

(define (score-table players)
  (define labels `("", "Ones", "Twos", "Threes", "Fours", "Fives", "Sixes", "Sum", "Bonus",
                     "Three of a kind", "Four of a kind", "Full House", "Small Straight",
                     "Large Straight", "Chance", "YAHTZEE", "TOTAL SCORE"))
  (define score-tables
    (for/list ([p players])
      (player-score-table p #f)))
  (transpose (append (list labels) score-tables)))

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
       [dices sample-dices]
       [callback (lambda (dice-idx)
                   (set-field! dices dice-pad `((#f . 1) (#f . 2) (#t . 3) (#f . 4) (#t . 3))))]))

(send application-frame show #t)
