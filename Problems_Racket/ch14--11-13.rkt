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

(print "Chapter 14 problem 11, 12, 13 start...")


;; ----------------------------------------------
;; 14.11 Write `remove-adjacent-duplicates' to remove any word from a sentence
;; that is immediately followed by the same word. Note the ordering.
;;
;; This is a keep operation.

(define (remove-adjacent-duplicates xs)
  (cond ((empty? xs)              '())
        ((= (count xs) 1)          xs)
        (else (se (if (equal? (first xs) (second xs))
                      '()
                      (first xs)) (remove-adjacent-duplicates (bf xs))))))

(check (remove-adjacent-duplicates '(yeah yeah yeah)) => '(yeah))
(check (remove-adjacent-duplicates '(y a b b a d a b b a d o o))
       => '(y a b a d a b a d o))


;; ----------------------------------------------
;; 14.12 Write `progressive-squares?' that takes a sentence of numbers and
;; determines if the numbers after the first one are squares of the prior
;; number.
;;
;; This is an accumulate.

(define (p-s-r x xs)
  (cond ((empty? xs)                  #t)
        ((not (= (* x x) (first xs))) #f)
        (else (p-s-r (first xs) (bf xs)))))

(define (progressive-squares? xs)
  (cond ((empty? xs)           #f)
        ((= (count xs) 1)      #f)
        (else (p-s-r (first xs) (butfirst xs)))))

(check (progressive-squares? '(2))           => #f)
(check (progressive-squares? '(1 1 1))       => #t)
(check (progressive-squares? '(2 4 16 256))  => #t)
(check (progressive-squares? '(3 9 81 6561)) => #t)
(check (progressive-squares? '(25 36 49 64)) => #f)


;; ----------------------------------------------
;; 14.13 What does `pigl' from chapter 11 do when given a word with no vowels,
;; such as 'frzzmlpt? Fix it so that it returns 'frzzmlptay.
;;
;; It goes into an infinite loop. The way I would fix this is to check for all
;; consonants, but that's probably now what they want here. Another approach is
;; to count the iterations and once the end is reached, just pass the word with
;; 'ay appended.

(define (ch11-pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (ch11-pigl (word (bf wd) (first wd)))))

(define (pigl-r n orig work)
  (cond ((= 0 n)                           (word orig 'ay))
        ((member? (first work) 'aeiou)     (word work 'ay))
        (else (pigl-r (- n 1) orig (word (bf work) (first work))))))

(define (pigl wd)
  (cond ((empty? wd)   '())
        (else           (pigl-r (count wd) wd wd))))

(check (pigl 'frzzmlpt) => 'frzzmlptay)
(check (pigl 'again) => (ch11-pigl 'again))
(check (pigl 'testy) => (ch11-pigl 'testy))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 14 problem 11, 12, 13 end...")
