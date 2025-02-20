#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 8 High Order Functions

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 8 problem 11 start...")


;; ----------------------------------------------
;; 8.11 Write a GPA procedure that takes a sentence of letter +/-
;;      grades and returns the correct GPA.
;;
;;      (a-d) 4-0
;;      + adds 0.33, - subtracts 0.33

;; simple bad approach:
(define (grade->points g)
  (cond ((equal? g 'a+)           (+ 4.0 0.33))
        ((equal? g 'a)            4)
        ((equal? g 'a-)           (- 4.0 0.33))
        ((equal? g 'b+)           (+ 3.0 0.33))
        ((equal? g 'b)            3)
        ((equal? g 'b-)           (- 3.0 0.33))
        ((equal? g 'c+)           (+ 2.0 0.33))
        ((equal? g 'c)            2)
        ((equal? g 'c-)           (- 2.0 0.33))
        ((equal? g 'd+)           (+ 1.0 0.33))
        ((equal? g 'd)            1)
        ((equal? g 'd-)           (- 1.0 0.33))
        (else                                0)))

(define (grades->gpa sent)
  (/ (accumulate + (every grade->points sent)) (count sent)))

(check (grades->gpa '(a b c)) => 3)
(check (grades->gpa '(a b+ b- b+ b-)) => 3.2)

(define (2-decimal x) (exact->inexact (/ (round (+ 0.5 (* x 100))) 100)))
(check (2-decimal (grades->gpa '(a b+ b- b+ b- b+ b-))) => 3.15)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 8 problem 11 end...")
