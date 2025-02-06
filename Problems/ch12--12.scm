#lang simply-scheme
;;; Simply Scheme
;;; Chapter 12 The Leap of Faith


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

(print "Chapter 12 problem 12 start...")


;; ----------------------------------------------
;; 12.12 Write `arabic' to convert Roman numerals to Arabic.
;;
;; I've gone one of these in elisp laying around somewhere that takes
;; a different approach, but I'll do this withing the framework of the
;; text.
;;
;; Assumes reasonable input (symbol or string) and that `butfirst" of
;; the last character in either returns "".
;;
;; Well, this is ugly but that's the nature of the problem, especially
;; since we don't know much about using let yet. Leaving the helpers
;; exposed.

(define (roman-digit->value letter)
  (cond ((equal? letter 'i) 1)
        ((equal? letter 'v) 5)
        ((equal? letter 'x) 10)
        ((equal? letter 'l) 50)
        ((equal? letter 'c) 100)
        ((equal? letter 'd) 500)
        ((equal? letter 'm) 1000)
        (else           'error)))

(define (roman->decimal-list x)
  (cond ((equal? x "")                 '())
        (else (se (roman-digit->value (first x)) (roman->decimal-list (bf x))))))

(define (add-roman accum prior xs)
  ;; (print "add-roman " accum " " prior " " xs)
  (cond ((empty? xs)               accum)
        ((not (number? (first xs)))   -1)
        ((< prior (first xs)) (add-roman (+ (- accum (* 2 prior)) (first xs)) (first xs) (bf xs)))
        (else                 (add-roman (+ accum (first xs)) (first xs) (bf xs)))))

(define (arabic roman)
  (let ((xs (roman->decimal-list roman)))
    (add-roman 0 (first xs) xs)))

(check (arabic 'x) => 10)
(check (arabic 'xvii) => 17)
(check (arabic 'viii) => 8)
(check (arabic 'ix) => 9)
(check (arabic 'mcmlxxi) => 1971)
(check (arabic 'mlxvi) => 1066)



;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 12 problem 12 end...")
