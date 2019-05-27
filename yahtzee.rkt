#lang racket

(provide
 player-init
 (struct-out scores)
 (struct-out player))

(require struct-update)

;; unassigned score is #f, otherwise an integer
(struct scores
  (upper-section ;; list of size 6
   lower-section ;;
   yahtzee))
(define-struct-updaters scores)

(define ((score-upper num) dice-values)
  (* num (count (curry = num) dice-values)))

(define (score-small-straight dice-values)
  (let ([sorted (sort dice-values <)])
    ;; if first 4 or last 4 is ascending series, then 30, otherwise 0
    (if (or (ascending-series? (take sorted 4))
            (ascending-series? (cdr sorted)))
        30
        0)))

(define (score-large-straight dice-values)
  (let ([sorted (sort dice-values <)])
    (if (ascending-series? sorted)
        40
        0)))

(define (ascending-series? lst)
  (if (< (length lst) 2)
      #t
      (and (= (+ 1 (first lst)) (second lst))
           (ascending-series? (cdr lst)))))

(define (score-full-house dice-values)
  (let* ([sorted (sort dice-values <)]
         [v1 (first sorted)]
         [v2 (second sorted)]
         [v3 (third sorted)]
         [v4 (fourth sorted)]
         [v5 (fifth sorted)])
    (if (or (and (= v1 v2) (= v3 v4 v5))
            (and (= v1 v2 v3) (= v4 v5)))
        25
        0)))

(define (score-three-of-a-kind dice-values)
  (let* ([sorted (sort dice-values <)]
         [v1 (first sorted)]
         [v2 (second sorted)]
         [v3 (third sorted)]
         [v4 (fourth sorted)]
         [v5 (fifth sorted)])
    (if (or (= v1 v2 v3)
            (= v2 v3 v4)
            (= v3 v4 v5))
        (apply + dice-values)
        0)))

(define (score-four-of-a-kind dice-values)
  (let ([sorted (sort dice-values <)])
    (if (or (apply = (cdr sorted))
            (apply = (take sorted 4)))
        (apply + dice-values)
        0)))

(define (score-chance dice-values)
  (apply + dice-values))

(define (score-yahtzee dice-values)
  (if (apply = dice-values)
      50
      0))

(define upper-section-funcs (map score-upper (range 1 7)))
(define lower-section-funcs
  (list score-three-of-a-kind
        score-four-of-a-kind
        score-full-house
        score-small-straight
        score-large-straight
        score-chance))

(struct player
  (scores  ;; struct scores
   rolls   ;; integer. number of rolls done this round.
   dices)) ;; list of 6 (fixed: boolean, score:integer) pairs
(define-struct-updaters player)

(define scores-init (scores (map (const #f) upper-section-funcs)
                            (map (const #f) lower-section-funcs)
                            #f))

(define (list-total-score lst)
  (apply + (remq* `(#f) lst)))

(define (scores-total-score s)
  (let* ([lower-section-score (list-total-score (scores-lower-section s))]
         [upper-section-score (list-total-score (scores-upper-section s))]
         [bonus-score         (if (>= lower-section-score 63) 35 0)]
         [yahtzee-score       (scores-yahtzee s)])
    (+ lower-section-score upper-section-score bonus-score yahtzee-score)))

(define (player-init)
  (player scores-init 0 `((#f . 1) (#f . 2) (#f . 3) (#f . 4) (#f . 5) (#f . 6))))

(define (player-toggle-dice player idx)
  (let* ([dices         (player-dices player)]
         [dice          (list-ref dices idx)]
         [updated-dice  (cons (not (car dice)) (cdr dice))]
         [updated-dices (list-set dices idx updated-dice)])
    (player-dices-set player updated-dices)))

(define (player-roll-dices player)
  (let* ([dices         (player-dices player)]
         [updated-dices  (map roll-dice dices)])
    (player-dices-set player updated-dices)))

(define (player-dice-values player)
  (map cdr (player-dices player)))

(define (roll-dice dice)
  (let ([is-fixed  (car dice)]
        [new-value (random 1 7)])
    (cons is-fixed new-value)))

(define (internal-choose scores score-funcs idx dice-values)
  (let* ([score-func  (list-ref score-funcs idx)]
         [score       (score-func dice-values)])
    (list-set scores idx score)))

(define (player-choose-internal player
                                idx
                                section-select-fun
                                section-set-func
                                score-funcs)
  (let* ([scores          (player-scores player)]
         [section         (section-select-fun scores)]
         [dice-values     (player-dice-values player)]
         [updated-section (internal-choose section
                                           score-funcs
                                           idx
                                           dice-values)]
         [updated-scores  (section-set-func scores
                                            updated-section)])
    (player-scores-set player updated-scores)))

(define (player-choose-upper-section player idx)
  (player-choose-internal player idx
                          scores-upper-section
                          scores-upper-section-set
                          upper-section-funcs))

(define (player-choose-lower-section player idx)
  (player-choose-internal player idx
                          scores-lower-section
                          scores-lower-section-set
                          lower-section-funcs))

(define (player-choose-yahtzee player)
  (let* ([scores         (player-scores player)]
         [score          (score-yahtzee (player-dice-values player))]
         [updated-scores (scores-yahtzee-set score)])
    (player-scores-set player updated-scores)))

