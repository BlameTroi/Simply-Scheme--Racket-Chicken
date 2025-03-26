#lang simply-scheme
;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 14 Common Patters in Recursive Procedures

;; The #lang command loads the racket language definition for
;; the text. Then we just need SRFI-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 14 problems 9, 10 start...")


;; ----------------------------------------------
;; 14.9 Write `location' taking a word and a sentence. Return the position of
;; the word within the sentence or #f. Report only the first occurrence. I
;; could be convinced this is an accumulate or a keep.

(define (location-r n x xs)
  (cond ((empty? xs)              #f)
        ((equal? x (first xs))     n)
        (else (location-r (+ n 1) x (bf xs)))))

(define (location x xs)
  (cond ((empty? xs)            #f)
        ((empty? x)             #f)
        ((not (word? x))        #f)
        ((not (sentence? xs))   #f)
        (else (location-r 1 x xs))))

(check (location 'fred '(fred)) => 1)
(check (location 'fred '(wilma)) => #f)
(check (location 'fred '(wilma and fred are here)) => 3)
(check (location 'fred '(wilma fred)) => 2)
(check (location 'fred '(not here but fred and then fred again)) => 4)


;; ----------------------------------------------
;; 14.10 Write `count-adjacent-duplicates' which reports the words in the
;; sentence that are immediately followed by the same word.
;;
;; The behavior of 'a b b b b a' could be isn't clearly stated but that should
;; be a 3.
;;
;; This is an accumulate.

(define (second xs) (first (butfirst xs)))

(define (c-a-d-r n xs)
  (cond ((empty? xs)              n)
        ((= (count xs) 1)         n)
        (else (c-a-d-r (if (equal? (first xs) (second xs))
                           (+ n 1)
                           n)
                       (butfirst xs)))))

(define (count-adjacent-duplicates xs)
  (cond ((empty? xs)         0)
        ((= (count xs) 1)    0)
        (else (c-a-d-r 0 xs))))

(check (count-adjacent-duplicates '(x y z)) => 0)
(check (count-adjacent-duplicates '(x x)) => 1)
(check (count-adjacent-duplicates '(x)) => 0)

(check (count-adjacent-duplicates '(y a b b a d a b b a d o o)) => 3)
(check (count-adjacent-duplicates '(yeah yeah yeah)) => 2)
(check (count-adjacent-duplicates '(a b b b b a)) => 3)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 14 problems 9, 10 end...")
