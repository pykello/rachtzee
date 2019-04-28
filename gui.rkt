#lang racket/gui

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
    ("Four of a kind" "" "")
    ("Full House" "" "")
    ("Small Straight" "" "")
    ("Large Straight" "" "")
    ("Chance" "" "")
    ("YAHTZEE" "" "")
    ("TOTAL SCORE" "" "")))

(define horizontal-margin 10)
(define vertical-margin 6)
(define double-seps `(0 7 9 16 17))

(define text-size-dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))

(define (text-height s)
  (define-values (width height c d) (send text-size-dc get-text-extent s))
  (exact-round height))

(define (text-width s)
  (define-values (width height c d) (send text-size-dc get-text-extent s))
  (exact-round width))

(define cell-height
  (+ (text-height "ABC") (* 2 vertical-margin)))

(define (cell-width s)
  (+ (text-width s) (* 2 horizontal-margin)))

(define (col-widths table)
  (define widths
    (for/list ([row table])
      (map cell-width row)))
  (foldl
   (lambda (a b) (for/list ([i a] [j b]) (exact-round (max i j))))
   (car widths)
   widths))

(define (table-height table)
  (+ 2 (* cell-height (length table))))

(define (table-width table)
  (+ 1 (apply + (col-widths table))))

(define my-canvas%
  (class canvas%
    (inherit get-dc get-width get-height)
    (define/override (on-event event)
      0)
    (define/override (on-paint)
      (define dc (get-dc))
      (define widths (col-widths example-table))
      (draw-scoreboard dc example-table widths))
    (super-new [min-height (table-height example-table)]
               [min-width (table-width example-table)])))

(define (draw-scoreboard dc table col-widths)
  (define col-offsets (cumulative-sum col-widths))
  (define row-offsets (cumulative-sum (map (const cell-height) table)))
  (define h (table-height table))
  (define w (table-width table))
  ; vertical lines
  (for ([col col-offsets])
    (send dc draw-line col 0 col h))
  ; horizontal lines
  (for ([row row-offsets])
    (send dc draw-line 0 row w row))
  ; double-sep
  (for ([idx double-seps])
    (define x (+ 1 (list-ref row-offsets idx)))
    (send dc draw-line 0 x w x))
  (send dc draw-line 1 0 1 h)
  (send dc draw-line (- w 2) 0 (- w 2) h)
  ; texts
  (for ([row table]
        [row-offset row-offsets])
    (for ([label row]
          [col-offset col-offsets])
      (define x (+ col-offset horizontal-margin))
      (define y (+ row-offset vertical-margin))
      (send dc draw-text label x y)))
  )

(define (cumulative-sum lst)
  (define (cumulative-sum-rec lst agg)
    (if (null? lst)
        lst
        (let ([fst (+ (car lst) agg)])
          (cons fst (cumulative-sum-rec (cdr lst) fst)))))
  (cons 0 (cumulative-sum-rec lst 0)))

; Make a canvas that handles events in the frame
(new my-canvas%
     [parent application-frame]
     [vert-margin 20]
     [horiz-margin 20])

(send application-frame show #t)