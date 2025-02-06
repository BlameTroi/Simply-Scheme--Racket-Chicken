#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 8 High Order Functions


;; The problems I worked with lightweight testing to verify results
;; via srfi-78. This file should load into a new Scheme repl (only
;; Chicken tested) and report no failures.

;;; Set up the standard environment:

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)

;;; Set unit testing reporting levels and clear any dangling
;;; totals.

;; This should already be done, but just in case:

(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 08 problem 9 & 10 start...")


;; ----------------------------------------------
;; 8.9 What procedures can you use as the function argument to
;;     `every', `keep', and `accumulate' so that the output equals
;;     the input.

;;     every se ...
;;     keep se ...
;;     accumulate se ...

(check (every se '(a b c de fg)) => '(a b c de fg))
(check (keep se '(a b c de fg)) => '(a b c de fg))
(check (accumulate se '(a b c de fg)) => '(a b c de fg))


;; ----------------------------------------------
;; 8.10 Write procedure `true-for-all?' that takes a predicate and a
;;      sentence and tests each word of the sentence against the
;;      predicate.

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

(print "Chapter 08 problem 9 & 10 end...")
