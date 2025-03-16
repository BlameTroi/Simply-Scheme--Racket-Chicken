#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 9 Lambda Land

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 9 problem 1, 2, 3 start...")


;; ----------------------------------------------
;; 9.1 What will the following print?

(lambda (x) (+ (* x 3) 4))        ;; I expect 'procedure ? x' (Chicken),
;; actual format #<procedure (? x)> in Chicken

((lambda (x) (+ (* x 3) 4)) 10)   ;; 34
;; correct

(every (lambda (wd) (word (last wd) (bl wd)))
       '(any time at all))        ;; 'nmal
;; wrong, '(yan etim ta lal)

;; ((lambda (x) (+ x 3)) 10 15)      ;; 28
;; wrong, bad argument count, so how do we write `+' then?


;; ----------------------------------------------
;; 9.2 Rewrite the following to make the implicit lambda explicit.
;;
;; I'm taking this to mean the lambda implied by define.

(define (second stuff)
  (first (bf stuff)))

(define lambda-second
  (lambda (stuff)
    (first (bf stuff))))

(check (second 'stuff) => (lambda-second 'stuff))

(define (make-adder num)
  (lambda (x) (+ num x)))

(define lambda-make-adder
  (lambda (num)
    (lambda (x) (+ num x))))

(define fna4 (make-adder 4))

(define lambda-fna4 (lambda-make-adder 4))

(check (fna4 3) => (lambda-fna4 3))


;; ----------------------------------------------
;; 9.3 What does this procedure do?

(define (let-it-be sent)                ;; returns sent
  (accumulate (lambda (x y) y) sent))
;; wrong, returns `last' of sent. That seems like a lot of work for a `last'
;; implementation.


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 9 problem 1, 2, 3 end...")
