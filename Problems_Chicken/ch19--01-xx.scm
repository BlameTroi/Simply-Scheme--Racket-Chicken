;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 19 Implementing High Order Functions.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load srfi 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 19 Implementing High Order Functions begin...")


;; ----------------------------------------------
;; 19.2 Write keep. Don't forget that keep has to return a sentence if its
;; second argument is a sentence, and a word if its second argument is a word.
;;
;; (Hint: it might be useful to write a combine procedure that uses either word
;; or sentence depending on the types of its arguments.)


;; ----------------------------------------------
;; 19.3 Write the three-argument version of accumulate that we described.
;;
;; (three-arg-accumulate + 0 '(4 5 6)) => 15
;;
;; (three-arg-accumulate + 0 '()) => 0
;;
;; (three-arg-accumulate cons '() '(a b c d e)) => (A B C D E)


;; ----------------------------------------------
;; 19.4 Our accumulate combines elements from right to left. That is:
;;
;; (accumulate - '(2 3 4 5)) computes 2−(3−(4−5)).
;;
;; Write left-accumulate, which will compute ((2−3)−4)−5 instead. (The result
;; will be the same for an operation such as +, for which grouping order
;; doesn't matter, but will be different for -.)


;; ----------------------------------------------
;; 19.5 Rewrite the true-for-all? procedure from Exercise 8.10. Do not use
;; every, keep, or accumulate.


;; ----------------------------------------------
;; 19.6 Write a procedure true-for-any-pair? that takes a predicate and a
;; sentence as arguments. The predicate must accept two words as its arguments.
;; Your procedure should return #t if the argument predicate will return true
;; for any two adjacent words in the sentence:
;;
;; (true-for-any-pair? equal? '(a b c b a)) => #f
;;
;; (true-for-any-pair? equal? '(a b c c d)) => #t
;;
;; (true-for-any-pair? < '(20 16 5 8 6)) => #t     ;; 5 is less than 8


;; ----------------------------------------------
;; 19.7 Write a procedure true-for-all-pairs? that takes a predicate and a
;; sentence as arguments. The predicate must accept two words as its arguments.
;; Your procedure should return #t if the argument predicate will return true
;; for every two adjacent words in the sentence:
;;
;; (true-for-all-pairs? equal? '(a b c c d)) => #f
;;
;; (true-for-all-pairs? equal? '(a a a a a)) => #t
;;
;; (true-for-all-pairs? < '(20 16 5 8 6)) => #f
;;
;; (true-for-all-pairs? < '(3 7 19 22 43)) => #t


;; ----------------------------------------------
;; 19.8 Rewrite true-for-all-pairs? (Exercise 19.7) using true-for-any-pair?
;; (Exercise 19.6) as a helper procedure. Don't use recursion in solving this
;; problem (except for the recursion you've already used to write
;; true-for-any-pair?). Hint: You'll find the not procedure helpful.


;; ----------------------------------------------
;; 19.9 Rewrite either of the sort procedures from Chapter 15 to take two
;; arguments, a list and a predicate. It should sort the elements of that list
;; according to the given predicate:
;;
;; (sort '(4 23 7 5 16 3) <) => (3 4 5 7 16 23)
;;
;; (sort '(4 23 7 5 16 3) >) => (23 16 7 5 4 3)
;;
;; (sort '(john paul george ringo) before?) => (GEORGE JOHN PAUL RINGO)


;; ----------------------------------------------
;; 19.10 Write tree-map, analogous to our deep-map, but for trees, using the
;; datum and children selectors.


;; ----------------------------------------------
;; 19.11 Write repeated. (This is a hard exercise!)


;; ----------------------------------------------
;; 19.12 Write tree-reduce. You may assume that the combiner argument can be
;; invoked with no arguments.
;;
;; (tree-reduce
;;  +
;;  (make-node 3 (list (make-node 4 '())
;;                     (make-node 7 '())
;;                     (make-node 2 (list (make-node 3 '())
;;                                        (make-node 8 '())))))) => 27


;; ----------------------------------------------
;; 19.13 Write deep-reduce, similar to tree-reduce, but for structured lists:
;;
;; (deep-reduce word '(r ((a (m b) (l)) (e (r))))) => 'rambler


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 19 Implementing High Order Functions end...")
