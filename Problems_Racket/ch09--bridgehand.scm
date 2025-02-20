#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Interlude project between chapters 9 and 10, bridge hand scoring

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Implement bridge hand scoring by writing specific procedures in a
;;; bottom up manner.

(print "Chapter 9 -- bridge hand start...")

;; Bridge hands are sentences of 13 cards. A card is suit and rank, h5
;; for 5 of hearts, c10 for 10 of clubs, sq for quean of spades, etc.


;; `card-val' returns the bridge points for the card. number cards
;; are 0, ace 4, king 3, queen 2, jack 1.

(define (card-val x)
  (let ((rank (butfirst x)) (suit (first x)))
    (cond ((equal? rank 'a)          4)
          ((equal? rank 'k)          3)
          ((equal? rank 'q)          2)
          ((equal? rank 'j)          1)
          (else                      0))))

(check (card-val 'cq) => 2)
(check (card-val 'd9) => 0)
(check (card-val 'ha) => 4)


;; `high-card-points' takes a hand (sentence) and totals the points
;; for ranks. No distribution points are considered.

(define (high-card-points xs)
  (accumulate + (every card-val xs)))

(check (high-card-points '(sa s10 hq ck c4)) => 9)
(check (high-card-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) => 13)


;; `count-suit' takes a suit and hand as arguments and returns the
;; number of cards in the hand of that suit.

(define (count-suit s xs)
  (count (keep (lambda (c) (equal? (first c) s)) xs)))

(check (count-suit 's '(sa s10 hq ck c4)) => 2)
(check (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) => 2)
(check (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) => 5)


;; `suit-counts' given a hand, return a sentence of (#spades #hearts
;; #clubs #diamonds).

(define (suit-counts xs)
  (se (count-suit 's xs)
      (count-suit 'h xs)
      (count-suit 'c xs)
      (count-suit 'd xs)))

(check (suit-counts '(sa s10 hq ck c4)) => '(2 1 2 0))
(check (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) => '(5 3 2 3))
(check (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) => '(5 1 2 5))


;; `suit-dist-points' takes a number of cards and returns the points
;; for having that many cards of a particular suit.

(define (suit-dist-points n)
  (cond ((= n 2) 1)
        ((= n 1) 2)
        ((= n 0) 3)
        (else    0)))

(check (suit-dist-points 2) => 1)
(check (suit-dist-points 7) => 0)
(check (suit-dist-points 0) => 3)


;; `hand-dist-points' takes a hand and returns the number of points its
;; distribution is worth.

(define (hand-dist-points xs)
  (accumulate + (every suit-dist-points  (suit-counts xs))))

(check (hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) => 1)
(check (hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) => 3)


;; `bridge-val' takes a hand and returns its point total.

(define (bridge-val xs)
  (let ((hdp (hand-dist-points xs))
        (hcp (high-card-points xs)))
    (+ hdp hcp)))

(check (bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) => 14)
(check (bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) => 8)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 9 -- bridge hand end...")
