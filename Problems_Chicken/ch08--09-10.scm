;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 8 High Order Functions

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load srfi 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 8 problem 9 & 10 start...")


;; ----------------------------------------------
;; 8.9 What procedures can you use as the function argument to `every', `keep',
;; and `accumulate' so that the output equals the input.

;;     every se ...
;;     keep se ...
;;     accumulate se ...

(check (every se '(a b c de fg)) => '(a b c de fg))
(check (keep se '(a b c de fg)) => '(a b c de fg))
(check (accumulate se '(a b c de fg)) => '(a b c de fg))


;; ----------------------------------------------
;; 8.10 Write procedure `true-for-all?' that takes a predicate and a sentence
;; and tests each word of the sentence against the predicate.

(define (true-for-all? pred sent)
  (cond ((empty? sent)              #f)
        (else (equal? (keep pred sent) sent))))

(define (tfa-pred x) (member x '(a b c)))

(check (true-for-all? tfa-pred '(a b c)) => #t)
(check (true-for-all? tfa-pred '(a b c d)) => #f)
(check (true-for-all? tfa-pred '()) => #f)



;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 8 problem 9 & 10 end...")
