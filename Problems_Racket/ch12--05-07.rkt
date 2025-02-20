#lang simply-scheme
;;; Simply Scheme
;;; Chapter 12 The Leap of Faith

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 12 problems 5, 6, 7 start...")


;; ----------------------------------------------
;; 12.5 (8.8 but recursive) Write the `exaggerate' procedure that
;;      exaggerates sentences.

(define (exaggerate sent)
  (if (empty? sent)
      sent
      (if (number? (first sent))
          (se (* (first sent) 2) (exaggerate (bf sent)))
          (se (first sent) (exaggerate (bf sent))))))

(check (exaggerate '(i ate 3 potstickers)) => '(i ate 6 potstickers))


;; ----------------------------------------------
;; 12.6 (8.11) Write GPA as defined in 8.11: Write a GPA procedure
;; that takes a sentence of letter +/- grades and returns the correct
;; GPA.
;;
;;      (a-d) 4-0
;;      + adds 0.33, - subtracts 0.33

;; simplistic:
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

(define (ch8-grades->gpa sent)
  (/ (accumulate + (every grade->points sent)) (count sent)))

;; I wanted to put this embedded in `grades->gpa' but we haven't
;; been introduced to let* or letrec yet.
(define (sum-grades xs)
  (if (empty? xs)
      0
      (+ (grade->points (first xs)) (sum-grades (bf xs)))))

(define (grades->gpa xs)
  (/ (sum-grades xs) (count xs)))

(define (2-decimal x)
  (exact->inexact (/ (round (+ 0.5 (* x 100))) 100)))

(check (grades->gpa '(a b c))
       => (ch8-grades->gpa '(a b c)))
(check (grades->gpa '(a b+ b- b+ b-))
       => (ch8-grades->gpa '(a b+ b- b+ b-)))
(check (2-decimal (grades->gpa '(a b+ b- b+ b- b+ b-)))
       => (2-decimal (ch8-grades->gpa '(a b+ b- b+ b- b+ b-))))


;; ----------------------------------------------
;; 12.7 Write `spell-number' that takes a number and returns a
;;      sentence of the digits spelled out. The authors provide
;;      a helper function (meh).

(define (spell-digit digit)
  (item (+ 1 digit)
  '(zero one two three four five six seven eight nine)))

(define (spell-number digits)
  (cond ((empty? digits)         '())
        ((not (number? digits))  '())
        (else (se (spell-digit (first digits)) (spell-number (bf digits))))))

(check (spell-number 1971) => '(one nine seven one))
(check (spell-number '007) => '(seven))
(check (spell-number "007") => '(zero zero seven))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 12 problems 5, 6, 7 end...")
