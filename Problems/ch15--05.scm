#lang simply-scheme
;;; Simply Scheme
;;; Chapter 15 Advanced Recursion


;; The problems I worked with lightweight testing to verify results
;; via srfi-78. This file should load into a new Scheme repl (only
;; Chicken tested) and report no failures.

;;; Set up the standard environment:

;; Do this manually via (load "required.scm"), which currently
;; loads:
;;
;; (load "simply.scm")
;; (import srfi-78)
;; (import trace) <----- needed for some exercises in ch 13
;; (load "my-helpers.scm")

;;; Set unit testing reporting levels and clear any dangling
;;; totals.
(require srfi/78)
;;(load "my-helpers.scm")
;; This should already be done, but just in case:

(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 15 problem 05 start...")

;; 15.5 Given a phone number, come up with a clever way to spell out
;;      the number. Return all the possible spellings. That'll be
;;      huge. I'm using the E.161 table from chapters 8 and 11.
;;
;;      There are 2187 possible combinations (3^7, no q or z) so an
;;      exhaustive test is not advisable.
;;
;; (check (phone-spell 22235766) => '(aadjpmm aadjpmn ... ccflsoo))
;;
;; (check (phone-spell 23) => '(ad ae af bd be bf cd ce cf)
;;                            a   d
;;                            a    e
;;                            a     f
;;                            b   d
;;                            b    e
;;                            b     f ...

;; (digit->keypad d)
;;
;; Given a digit from a phone number, return a list of the possible
;; letter equivalents to make phonewords. Uses the older map with
;; no z or q. Returns (?) for illegal input.

(define (digit->keypad d)
  (cond
    ((= d 2) '(a b c))
    ((= d 3) '(d e f))
    ((= d 4) '(g h i))
    ((= d 5) '(j k l))
    ((= d 6) '(m n o))
    ((= d 7) '(p r s))   ;; q could go here
    ((= d 8) '(t u v))
    ((= d 9) '(w x y))   ;; z could go here
    (else    '(#\?))))   ;; have to do something, right?

;; (phone-spell num)
;;
;; Show all the possible spellings for a phone number using a standard
;; phone keypad (US E.161). The spec says we can assume that 0 and 1 are
;; not input.

(define (phone-spell-r x xs)
  (cond
    ((empty? xs)     (se ))

    ))
(define (deeper x xs)
  (se x (phone-spell xs)))

(define (doit xs ys)     ;; xs number->letters, ys remaining number
  (cond
    ((empty? xs)        (se ))
    (else (se (deeper (first xs) ys) (doit (bf xs) ys)))))



(define (phone-spell num)
  (let ((letters     (digit->keypad (first num))))
    (cond
      ((empty? num)                                   '())
      ((equal? (digit->keypad (first num)) #\?)       '())
      (else (se )   )
      )))



;; ----------------------------------------------
;; ----------------------------------------------
;; ----------------------------------------------
;; ----------------------------------------------

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 15 problem 05 end...")
