#lang racket/gui

(provide table-view%)

;; tableview widget
(define table-view%
  (class canvas%
    (init-field callback)
    (init-field table)
    (inherit get-dc get-width get-height)
    (define widths (col-widths table))
    (define/override (on-event event)
      (if (and (send event button-changed? 'left)
               (send event button-down? 'left))
          (let* ([x (send event get-x)]
                 [y (send event get-y)]
                 [row (exact-floor (/ y cell-height))])
            (callback row (x-to-col widths x)))
          0))
    (define/override (on-paint)
      (define dc (get-dc))
      (draw-table dc table widths))
    (super-new [min-height (table-height table)]
               [min-width (table-width table)])))

;; how to draw the tableview
(define (draw-table dc table col-widths)
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
    (for ([cell row]
          [col-offset col-offsets])
      (define x (+ col-offset horizontal-margin))
      (define y (+ row-offset vertical-margin))
      (send dc set-text-foreground (cell-color cell))
      (send dc draw-text (cell-text cell) x y)))
  )

;; geometry constants
(define horizontal-margin 10)
(define vertical-margin 6)
(define double-seps `(0 7 9 16 17))

;; table dimension functions
(define (table-height table)
  (+ 2 (* cell-height (length table))))

(define (table-width table)
  (+ 1 (apply + (col-widths table))))

(define (col-widths table)
  (define widths
    (for/list ([row table])
      (map cell-width row)))
  (foldl
   (lambda (a b) (for/list ([i a] [j b]) (exact-round (max i j))))
   (car widths)
   widths))

;; text size functions
(define text-size-dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))

(define (text-height s)
  (define-values (width height c d) (send text-size-dc get-text-extent s))
  (exact-round height))

(define (text-width s)
  (define-values (width height c d) (send text-size-dc get-text-extent s))
  (exact-round width))

;; cell related functions
(define cell-height
  (+ (text-height "ABC") (* 2 vertical-margin)))

(define (cell-text c)
  (if (list? c)
      (second c)
      c))

(define (cell-color c)
  (if (list? c)
      (first c)
      "black"))

(define (cell-width c)
  (+ (text-width (cell-text c)) (* 2 horizontal-margin)))

;; which column is x at in a table with given column widths?
(define (x-to-col widths x)
  (define (aux lst)
    (cond
      [(null? lst) -2]
      [(> (car lst) x) -1]
      [(+ 1 (aux (cdr lst)))]))
  (aux (cumulative-sum widths)))

;; cumulative sum of given list
(define (cumulative-sum lst)
  (define (cumulative-sum-rec lst agg)
    (if (null? lst)
        lst
        (let ([fst (+ (car lst) agg)])
          (cons fst (cumulative-sum-rec (cdr lst) fst)))))
  (cons 0 (cumulative-sum-rec lst 0)))
