#lang simply-scheme
;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 14 Common Patters in Recursive Procedures

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 14 problem 8 start...")


;; ----------------------------------------------
;; 14.8 Write `expand' which takes and returns a sentence. If a number is found
;; in the sentence, return that many copies of the next word. Otherwise just
;; the word.
;;
;; I try to handle undefined cases (two adjacent numbers, just one number as
;; the only word in the sentence, etc.) sensibly.
;;
;; Trying to stick with the author's standard environment caused a me few
;; problems. I opted to wrap a few of their functions to make them behave the
;; way I want.

(define (repeater n xs)
  (cond ((not (number? n))   (se ))
        ((< n 1)             (se ))
        ((empty? xs)         (se ))
        (else (se (first xs) (repeater (- n 1) xs)))))

(check (repeater 3 '(fred)) => '(fred fred fred))
(check (repeater -1 '(fred)) => '())
(check (repeater 1 '(wilma)) => '(wilma))
(check (repeater 2 '()) => '())
(check (repeater 'x '(some thing)) => '())

(define (first-or-nil xs)
  (cond ((empty? xs)           (se ))
        (else             (first xs))))

(check (first-or-nil '()) => '())
(check (first-or-nil '(fred)) => 'fred)
(check (first-or-nil '(fred wilma)) => 'fred)

(define (second-or-nil xs)
  (cond ((empty? xs)             '())
        ((= (count xs) 1)        '())
        (else        (first (bf xs)))))

(check (second-or-nil '()) => '())
(check (second-or-nil '(fred)) => '())
(check (second-or-nil '(fred wilma)) => 'wilma)

(define (bf-or-nil xs)
  (cond ((empty? xs)             '())
        (else                (bf xs))))

(check (bf-or-nil '()) => '())
(check (bf-or-nil '(a)) => '())
(check (bf-or-nil '(a b)) => '(b))
(check (bf-or-nil '(a b c)) => '(b c))

(define (expand-r x xs)
  (cond ((empty? x)    (se ))
        ((equal? x "") (se ))
        ((empty? xs) (if (number? x)
                         (se )
                         (se x)))
        ((number? x) (se (repeater x xs) (expand-r (second-or-nil xs)
                                                   (bf-or-nil (bf-or-nil xs)))))
        (else        (se x (expand-r (first xs)
                                     (bf-or-nil xs))))))

(check (expand-r 1 '(is the loneliest number)) => '(is the loneliest number))
(check (expand-r 'we '(ate 3 apples)) => '(we ate apples apples apples))
(check (expand-r 3 '(one)) => '(one one one))
(check (expand-r 4 '(a b)) => '(a a a a b))
(check (expand-r 5 '()) => '())
(check (expand-r 'tail '()) => '(tail))
(check (expand-r '() '()) => '())

;; `expand' is part of Chicken as: <procedure (chicken.syntax#expand exp .
;; rest)> so I renamed it.

(define (s-expand sent)
  (cond ((empty? sent)           sent)
        ((= (count sent) 1)      sent)
        (else (se (expand-r (first-or-nil sent)
                            (bf-or-nil sent))))))

;; Problem provided tests:
(check (s-expand '(the 7 samuri))
       => '(the samuri samuri samuri samuri samuri samuri samuri))

(check (s-expand '(4 calling birds 3 french hens))
       => '(calling calling calling calling birds french french french hens))

;; A longer test and some edge cases.
(check (s-expand '(this 5 is what 3 max headroom 2 sounds like))
       => '(this is is is is is what max max max headroom sounds sounds like))

(check (s-expand '(hi there))
       => '(hi there))

(check (s-expand '(3 ho green giant))
       => '(ho ho ho green giant))

(check (s-expand '(0 zero one two))
       => '(one two))

;; The following are not defined in the problem statement, but here's what I
;; think they should do.

(check (s-expand '(one two 3 four 5))
       => '(one two four four four))

(check (s-expand '(one 2 3 four 5 six))
       => '(one 3 3 four six six six six six))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 14 problem 8 end...")
