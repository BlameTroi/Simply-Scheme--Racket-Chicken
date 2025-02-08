#lang simply-scheme
;;; Simply Scheme
;;; Chapter 15 Project Poker Hands

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 15 project Poker Hands start...")

;; Create a relative score for poker hands. Or at least the ability to
;; compare two hands. Five card, no joker beyond ace being both the
;; low and high card. The usual four suits.

;; Hand values in descending order:

;; Royal Flush (A K J Q 10) same suit.
;; Straight Flush (? ? ? ? ?) five sequential cards of same suit.
;; Four of a Kind (? ? ? ? !) four of the same suit.
;; Full House (? ? ? # #) triple of same rank, and a pair of same rank.
;; Flush (? ? ? ? ?) five cards of the same suit
;; Straight (? ? ? ? ?) five sequential cards, any suits.
;; Three of a Kind (? ? ? ! !) triple of same rank.
;; Two Pair (? ? # # !) two pairs of two cards, each pair a different rank.
;; Pair (? ? ! ! !) two cards of same rank.
;; Bust (! ! ! ! !) none of the above.

;; Key: ? is card of value, ! is card of no value, # is card of value in
;; combinations such as Full House.


;; Bridge hands are sentences of 13 cards. A card is suit and rank, h5
;; for 5 of hearts, c10 for 10 of clubs, sq for quean of spades, etc.

;; The only time suit matters in when all five cards in the hand are
;; of the same suit.

;; `card-val' returns the bridge points for the card. number cards
;; are 0, ace 4, king 3, queen 2, jack 1.

;; Cards are represented as word: <suit><rank>. 
;; S pades C lubs H earts D iamonds
;; A ce    (either 1 or 14)
;; 2 3 4 5 6 7 8 9 10 J Q K (J = 11, Q = 12, K = 13)

;; Readable suit.
(define (suit card)
  (let ((s (first card)))
    (cond ((equal? s 's)      'spades)
          ((equal? s 'c)      'clubs)
          ((equal? s 'd)      'diamonds)
          ((equal? s 'h)      'hearts)
          (else               'ERROR))))

;; Sortable ranks. Ace is returned as 0 and will be handled
;; elsewhere.
(define (rank card)
  (let ((r (bf card)))
    (cond ((number? r)         r)
          ((equal? r 'a)       0)
          ((equal? r 'j)      11)
          ((equal? r 'q)      12)
          ((equal? r 'k)      13)
          (else               'ERROR))))

;; A hand is expected to be five cards, but there is no
;; explicit checking.
(define (lowest-rank-r so-far hand)
  (cond ((empty? hand)         so-far)
  		(else (if (< (rank so-far) (rank (first hand)))
        	       (lowest-rank so-far (bf hand))
            	   (lowest-rank (first hand) (bf hand))))))



(define (count-suit s xs)
  (count (keep (lambda (c) (equal? (first c) s)) xs)))

(check (count-suit 's '(sa s10 hq ck c4)) => 2)
(check (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) => 2)
(check (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) => 5)


;; `suit-counts' given a hand, return a sentence of (#spades #hearts
;; #clubs #diamonds).

(define (suit-counts xs)
  (se (count-suit 's xs)
      (count-suit 'c xs)
      (count-suit 'd xs)
      (count-suit 'h xs)))

(check (suit-counts '(sa s10 hq ck c4)) => '(2 1 2 0))
(check (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) => '(5 3 2 3))
(check (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) => '(5 1 2 5))



;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 15 project Poker Hands end...")
