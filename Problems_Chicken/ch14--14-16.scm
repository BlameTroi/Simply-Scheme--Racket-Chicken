;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 14 Common Patters in Recursive Procedures

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load srfi 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 14 problem 14, 15, 16 start...")


;; ----------------------------------------------
;; 14.14 Write predicate `same-shape?' that takes two sentences as
;;       arguments. It should return #tif two conditions are met:
;;
;;       (1) the sentences have the same number of words.
;;       (2) the corresponding words in each sentence have the
;;           same number of letters.

;; Accumulate. Again, they want more recursion than I would probably
;; use, I prefer guard clauses, but I'm doing it their way.

(define (same-shape? xs ys)
  (if (and (empty? xs) (empty? ys))
      #t
      (if (or (empty? xs) (empty? ys))
          #f
          (if (= (count (first xs)) (count (first ys)))
              (same-shape? (bf xs) (bf ys))
              #f))))


(check (same-shape? '(the fool on the hill) '(you like me too much)) => #t)
(check (same-shape? '(the fool on the hill) '(and your bird can sing)) => #f)
(check (same-shape? '(a) '(b)) => #t)
(check (same-shape? '(a b) '(a b c)) => #f)
(check (same-shape? '(a b) '()) => #f)
(check (same-shape? '() '()) => #t)


;; ----------------------------------------------
;; 14.15 Write `merge' which takes two sentences of numbers as
;;       arguments. Each sentence's numbers are in ascending order.
;;       Return one sentence with the numbers all in ascending order.

(define (merge xs ys)
  (cond ((empty? xs)                      ys)
        ((empty? ys)                      xs)
        ((<= (first xs) (first ys))      (se (first xs) (merge (bf xs) ys)))
        (else                            (se (first ys) (merge xs (bf ys))))))

(check (merge '(1 2) '(3 4)) => '(1 2 3 4))
(check (merge '(1 5 8 11) '(2 3 4 9 12 15))
       => '(1 2 3 4 5 8 9 11 12 15))
(check (merge '(4 7 18 40 99) '(3 6 9 12 24 36 50))
       => '(3 4 6 7 9 12 18 24 36 40 50 99))


;; ----------------------------------------------
;; 14.16 Write `syllables' which takes a word as its argument and
;;       returns the number of syllables counted according to the
;;       following rule:
;;
;;       The number of syllables is the number of vowels, except that
;;       consecutive vowels count as one.
;;
;;       This rule is obviously insufficient. Create test cases showing
;;       some of the errors.
;;
;; Yet another accumulator.

(define (syllables-r n p xs)
  (cond ((empty? xs)          n)
        (else (syllables-r (if (and (member? (first xs) 'aeiou)
                                    (not (member? p 'aeiou)))
                               (+ n 1)
                               n)
                           (first xs)
                           (butfirst xs)))))

(define (syllables xs)
  (syllables-r 0 '- xs))

(check (syllables 'wyoming) => 2) ;; should be 3
(check (syllables 'io) => 1)      ;; should be 2
(check (syllables 'riding) => 2)  ;; ok
(check (syllables 'government) => 3) ;; ok
(check (syllables 'try) => 0) ;; should be 1


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 14 problem 14, 15, 16 end...")
