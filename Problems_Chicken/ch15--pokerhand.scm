;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 15 Project Poker Hands

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load srfi 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 15 project Poker Hands start...")

;; Create poker hand evaluator. Given cards in the same format as the
;; bridge hand project, return the best possible evaluation for a hand.
;; IE, while a full house is also two pair and one pair and three of a
;; kind, full house should be returned.
;;
;; Five card stud rules, nothing wild beyond the ace's flexibility
;; in straights.
;;
;; Hand values in descending order:
;;
;; Royal Flush     (A K J Q 10) same suit.
;; Straight Flush  (? ? ? ? ?) five sequential cards of same suit.
;; Four of a Kind  (? ? ? ? !) four of the same suit.
;; Full House      (? ? ? # #) triple of same rank, and a pair of same rank.
;; Flush           (? ? ? ? ?) five cards of the same suit
;; Straight        (? ? ? ? ?) five sequential cards, any suits.
;; Three of a Kind (? ? ? ! !) triple of same rank.
;; Two Pair        (? ? # # !) two pairs of two cards, each pair a different rank.
;; Pair            (? ? ! ! !) two cards of same rank.
;; Bust            (! ! ! ! !) none of the above.
;;
;; Key: ? is card of value, ! is card of no value, # is card of value in
;; combinations such as Full House.
;;
;; Cards are represented as a word: <suit><rank>.
;; S pades C lubs H earts D iamonds
;; A ce    (either 1 or 14)
;; 2 3 4 5 6 7 8 9 10 J Q K (J = 11, Q = 12, K = 13)
;;
;; Bugs: No verification that a hand is legal. If someone puts an
;; extra ace of spades in a hand that already has that, it won't
;; be caught. As implemented, 5 aces is returned as three of a kind
;; because four of a kind requires that there be two unique ranks
;; in a hand.


;;; ----------------------------------------------
;;; Card interpretation:

;; Sortable ranks.
(define rank-ace    '14)          ;; Can be high or low.
(define rank-king   '13)          ;; Penultimate of high straight.
(define rank-queen  '12)          ;;
(define rank-jack   '11)          ;;
(define rank-ten    '10)          ;; Start of high straight.
(define rank-five    '5)          ;; Ultimate of low straight.
(define rank-two     '2)          ;; Second of low straight.


;; Return the rank (2->14) of CARD. Handles both suited and non-
;; suited cards by assuming a numeric is a valid rank.
(define (rank-of card)
  (if (number? card)
      card
      (let ((r (bf card)))
        (cond ((number? r)     r)
              ((equal?  r 'j)  rank-jack)
              ((equal?  r 'q)  rank-queen)
              ((equal?  r 'k)  rank-king)
              ((equal?  r 'a)  rank-ace)
              (else           'ERROR)))))


;;; ----------------------------------------------
;;; Sorting and supporting procedures.

;; Removes the first card matching CARD from a HAND. This
;; should work with and without suits.
(define (remove-card card hand)
  (cond ((empty? hand)              (se ))
        ((equal? card (first hand)) (se (bf hand)))
        (else (se (first hand) (remove-card card (bf hand))))))

(check (remove-card 'sa '(sa ca da ha sk)) => '(ca da ha sk))
(check (remove-card 'da '(sa ca da ha sk)) => '(sa ca ha sk))
(check (remove-card 'sk '(sa ca da ha sk)) => '(sa ca da ha))
(check (remove-card 'k  '(a a 3 k 4))      => '(a a 3 4))


;; Returns the card with the lowest rank regardless of suit from
;; HAND. Aces can be high or low, but default to high, so at this
;; level the 2 and not the ace is low, even though later we will
;; treat this as a straight.
;;
;; This should work with a suited or ranked only hand.
(define (lowest-rank-r so-far hand)
  (if (empty? hand)
      so-far
      (if (<= (rank-of so-far) (rank-of (first hand)))
          (lowest-rank-r so-far (bf hand))
          (lowest-rank-r (first hand) (bf hand)))))

(define (lowest-rank hand)
  (lowest-rank-r (first hand) (bf hand)))

(check (lowest-rank '(sa ca da ha sk))  => 'sk)
(check (lowest-rank '(d8 d9 d3 h2 hq))  => 'h2)
(check (lowest-rank '(h5 d2 c4 s3 ha))  => 'd2)
(check (lowest-rank '(d10 dj dq dk d9)) => 'd9)


;; Sort HAND by card ranks. Aces are high at this point. HAND may be
;; already stripped of its suits and this should still work.
(define (sort-hand hand)
  (cond ((empty? hand)        (se ))
        (else
         (let ((lowest (lowest-rank hand)))
           (se lowest (sort-hand (remove-card lowest hand)))))))

(check (sort-hand '(ca c2 c3 c4 c5))  => '(c2 c3 c4 c5 ca))
(check (sort-hand '(d10 sj hq ck ha)) => '(d10 sj hq ck ha))
(check (sort-hand '(2 14 3 13 9))     => '(2 3 9 13 14))


;;; ----------------------------------------------
;;; Hand interpretation (general attributes):

;; How many cards in a HAND are of a particular Suit?
(define (count-suit s hand)
  (count (keep (lambda (c) (equal? (first c) s)) hand)))

(check (count-suit 's '(sa s10 hq ck c4)) => 2)
(check (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) => 2)
(check (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) => 5)


;; Remove the suits from the cards in HAND. This should safely deal
;; with hands that already been stripped of suits.
(define (ranks-only hand)
  (if (empty? hand)
      (se )
      (se (rank-of (first hand)) (ranks-only (bf hand)))))

(check (ranks-only '(sa d2 d3 s5 h4))  => '(14 2 3 5 4))
(check (ranks-only '(da ck dj s10 hq)) => '(14 13 11 10 12))
(check (ranks-only '(c2 d7 h9 sj hq))  => '(2 7 9 11 12))
(check (ranks-only '(c2 d6 sa s9 ck))  => '(2 6 14 9 13))
(check (ranks-only '(2 6 9 13 14))     => '(2 6 9 13 14))
(check (ranks-only '(14 13 12 9 10))   => '(14 13 12 9 10))


;; Distinct ranks in a HAND. If there are four distinct ranks, there
;; must be a pair. Two distinct ranks, this is a full house or four
;; of a kind, etc. The results are sorted.
(define (unique-r ranks uniq)
  (if (empty? ranks)
      uniq
      (unique-r (bf ranks)
                (if (member? (first ranks) uniq)
                    uniq
                    (se uniq (first ranks))))))

(define (unique-ranks hand)
  (unique-r (ranks-only (sort-hand hand)) '()))

(check (unique-ranks '(sa d2 d3 s5 h4))  => '(2 3 4 5 14))
(check (unique-ranks '(da ck dj s10 hq)) => '(10 11 12 13 14))
(check (unique-ranks '(d2 c3 h5 h2 h3))  => '(2 3 5))
(check (unique-ranks '(d2 c3 c2 d3 h2))  => '(2 3))


;; How many times does a card of this RANK show up in the hand?
;; Assumes the hand has already been stripped of suits.
(define (occurs rank ranks ac)
  (cond ((empty? ranks)       ac)
        ((equal? rank (first ranks)) (occurs rank (bf ranks) (+ ac 1)))
        (else (occurs rank (bf ranks) ac))))


;; How many aces are in HAND?
(define (count-aces hand)
  (occurs rank-ace (ranks-only hand) 0))

(check (count-aces '(d2 h3 c4 c5 sa)) => 1)
(check (count-aces '(d2 h3 h4 dj dq)) => 0)
(check (count-aces '(sk ca d7 sa ha)) => 3)


;; Count pairs in HAND. Four of a kind is counted as only one pair.
(define (count-pairs-r uniq ranks)
  (if (empty? uniq)
      0
      (+ (count-pairs-r (bf uniq) ranks)
         (if (> (occurs (first uniq) ranks 0) 1) 1 0))))

(define (count-pairs hand)
  (let ((sorted (sort-hand hand)))
    (count-pairs-r (unique-ranks sorted) (ranks-only sorted))))

(check (count-pairs '(ca d3 h5 da ha)) => 1)
(check (count-pairs '(d2 h7 sk s8 s2)) => 1)
(check (count-pairs '(d3 h3 c3 sa da)) => 2)
(check (count-pairs '(d3 d4 c3 h3 s3)) => 1)


;;; ----------------------------------------------
;;; Hand classification:

;; These are written as predicates. As many classifications are
;; combinations of others, these call each other as needed. There
;; are no circular dependencies.
;;
;; Because I think it reads better, I do sometimes use an other
;; predicate as a filter. These predicates return true if the hand
;; satisfies the condition even if there is a better interpretation
;; of the hand available.
;;
;; For example, while a royal flush is also a straight flush, if you
;; check a hand for straight flush, you get that and not royal flush.
;; A full house checked against pair or two pair will return true for
;; the weaker classification.


;;; From strongest to weakest:


;; Royal Flush (A K J Q 10 same suit).
(define (royal-flush? hand)
  (let ((sorted (sort-hand hand)))
    (and
     (flush? sorted)
     (equal? (ranks-only sorted) '(10 11 12 13 14)))))


;; Straight Flush (? ? ? ? ?) five sequential cards of same suit.
(define (straight-flush? hand)
  (and (flush? hand)
       (straight? hand)))


;; Flush (? ? ? ? ?) five cards of the same suit
(define (flush? hand)
  (let ((cards (count hand)))
    (cond
      ((= (count-suit 'c hand) cards) #t)
      ((= (count-suit 'd hand) cards) #t)
      ((= (count-suit 'h hand) cards) #t)
      (else (= (count-suit 's hand) cards)))))


;; Four of a Kind (? ? ? ? !) four of the same rank.
(define (four-of-a-kind? hand)
  (let ((ranks (ranks-only (sort-hand hand))))
    (and
     (= (count (unique-ranks hand)) 2)
     (or
      (= 1 (occurs (first ranks) ranks 0))
      (= 4 (occurs (first ranks) ranks 0))))))


;; Full House (? ? ? # #) triple of same rank, and a pair of the
;; same rank.
(define (full-house? hand)
  (and
   (= (count (unique-ranks hand)) 2)
   (= (count-pairs hand) 2)
   (three-of-a-kind? hand)))


;; Straight (? ? ? ? ?) five sequential cards, any suits.
(define (straight? hand)
  (let ((sorted (sort-hand hand)))
    (let ((uniq (unique-ranks sorted))
          (ace (= 1 (count-aces sorted)))
          (ranks (ranks-only sorted)))
      (cond
        ((not (equal? (count uniq) 5))              #f)
        ((and ace (equal? ranks '(10 11 12 13 14))) #t) ;; Aces go at either
        ((and ace (equal? ranks '(2 3 4 5 14)))     #t) ;; end of the hand.
        (ace                                        #f) ;; But only there.
        (else     (= 4 (- (last ranks) (first ranks))))))))


;; Three of a Kind (? ? ? ! !) triple of same rank.
(define (three-of-a-kind?-r hand)
  (let ((count-ranks (count (unique-ranks hand)))
        (ranks       (ranks-only hand)))
    (cond
      ((= count-ranks 2)      #t)    ;; definite
      ((> count-ranks 3)      #f)    ;; also definite
      (else
       (or (= (item 1 ranks) (item 3 ranks))       ;; a a a b c
           (= (item 2 ranks) (item 4 ranks))       ;; a b b b c
           (= (item 3 ranks) (item 5 ranks)))))))
(define (three-of-a-kind? hand)
  (three-of-a-kind?-r (sort-hand hand)))


;; Two Pair (? ? # # !) two pairs of two cards, each pair of a
;; different rank.
(define (two-pair? hand)
  (and (< (count (unique-ranks hand)) 4)
       (= (count-pairs hand) 2)))


;; Pair (? ? ! ! !) at least two cards of the same rank.
(define (pair? hand)
  (and (< (count (unique-ranks hand)) 5)
       (> (count-pairs hand) 0)))


;; Bust (! ! ! ! !) none of the above.
(define (bust? hand)
  (and (not (flush? hand))
       (not (straight? hand))
       (= (count (unique-ranks hand)) 5)))


;;; -----------------------------------------------
;;; Hand classification tests:

;; Due to interdependcies, some of these tests can't be placed
;; immediately after their predicate's definition.

;; Royal Flush (A K J Q 10) same suit.
(check (royal-flush? '(sa sk sj sq s10)) => #t)
(check (royal-flush? '(d10 dj dq dk da)) => #t)
(check (royal-flush? '(cj cq c10 ca dk)) => #f)

;; Straight Flush (? ? ? ? ?) five sequential cards of same suit.
(check (straight-flush? '(h9 h10 hj hq hk)) => #t)
(check (straight-flush? '(ca c2 c3 c4 c5))  => #t)

;; Four of a Kind (? ? ? ? !) four of the same suit.
(check (four-of-a-kind? '(sa s9 c9 d9 h9)) => #t)
(check (four-of-a-kind? '(s2 dj c3 d3 h3)) => #f)
(check (four-of-a-kind? '(sa ca sk da ha)) => #t)

;; Full House (? ? ? # #) triple of same rank, and a pair of same rank.
(check (full-house? '(sa c4 da h4 ha))     => #t)
(check (full-house? '(c3 s3 d10 h10 s10))  => #t)
(check (full-house? '(c3 d10 h10 s10 c10)) => #f)

;; Flush (? ? ? ? ?) five cards of the same suit
(check (flush? '(s3 s4 s8 s9 cj))  => #f)
(check (flush? '(s2 s7 s10 s3 s8)) => #t)
(check (flush? '(ha h2 h10 h4 hk)) => #t)
(check (flush? '(cj cq c10 ca dk)) => #f)

;; Straight (? ? ? ? ?) five sequential cards, any suits.
(check (straight? '(ha h2 h3 h4 h5))  => #t)
(check (straight? '(ha hk hj h10 hq)) => #t)
(check (straight? '(ha h7 h8 h9 h10)) => #f)
(check (straight? '(cj cq c10 ca dk)) => #t)

;; Three of a Kind (? ? ? ! !) triple of same rank.
(check (three-of-a-kind? '(ha dk ck s3 sk)) => #t)
(check (three-of-a-kind? '(d3 da ha d5 ca)) => #t)

;; Two Pair (? ? # # !) two pairs of two cards, each pair a different rank.
(check (two-pair? '(ha d2 hk c2 ca))   => #t)
(check (two-pair? '(d10 dj h10 cj sa)) => #t)

;; Pair (? ? ! ! !) two cards of same rank.
(check (pair? '(ha hk hj hq sa))  => #t)
(check (pair? '(s3 d4 c2 d10 d3)) => #t)

;; Bust (! ! ! ! !) none of the above.
(check (bust? '(s3 s4 s8 s9 cj))  => #t)
(check (bust? '(d3 sj h4 c5 ha))  => #t)
(check (bust? '(sa dq dj c10 h9)) => #t)


;;; -----------------------------------------------
;;; Tie it all together.


;; Determine the strongest interpretation of a HAND.
(define (best-hand-value hand)
  (cond
    ((royal-flush? hand)        'royal-flush)
    ((straight-flush? hand)     'straight-flush)
    ((four-of-a-kind? hand)     'four-of-a-kind)
    ((full-house? hand)         'full-house)
    ((flush? hand)              'flush)
    ((straight? hand)           'straight)
    ((three-of-a-kind? hand)    'three-of-a-kind)
    ((two-pair? hand)           'two-pair)
    ((pair? hand)               'pair)
    (else                       'bust)))

;; And tests to beat up the decider:

(check (best-hand-value '(dj dq d10 da dk)) => 'royal-flush)
(check (best-hand-value '(sa s5 s3 s2 s4))  => 'straight-flush)
(check (best-hand-value '(s9 s10 sk sq sj)) => 'straight-flush)
(check (best-hand-value '(s10 cj hq dk sa)) => 'straight)
(check (best-hand-value '(cj cq c10 ca dk)) => 'straight)  ;; shows that a royal flush doesn't grab a straight
(check (best-hand-value '(dk sk ck da hk))  => 'four-of-a-kind)
(check (best-hand-value '(sa ca c3 da ha))  => 'four-of-a-kind)
(check (best-hand-value '(d3 h3 c3 dk hk))  => 'full-house)
(check (best-hand-value '(h7 hj d7 dj s7))  => 'full-house)
(check (best-hand-value '(s3 s5 sk sa s9))  => 'flush)
(check (best-hand-value '(dk d10 d3 d5 d7)) => 'flush)
(check (best-hand-value '(da d2 s3 c4 h5))  => 'straight)
(check (best-hand-value '(ha dj cq ck s10)) => 'straight)
(check (best-hand-value '(da d2 sa c7 ha))  => 'three-of-a-kind)
(check (best-hand-value '(c7 d8 c8 h8 c2))  => 'three-of-a-kind)
(check (best-hand-value '(c7 d8 h9 c8 d7))  => 'two-pair)
(check (best-hand-value '(sa d2 c2 d9 ca))  => 'two-pair)
(check (best-hand-value '(sa c2 c7 ca d6))  => 'pair)
(check (best-hand-value '(d9 hj hk hq h9))  => 'pair)
(check (best-hand-value '(d2 h3 c4 s5 d10)) => 'bust)
(check (best-hand-value '(dk dq sa s2 h9))  => 'bust)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 15 project Poker Hands end...")
