;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 19 Implementing High Order Functions.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load SRFI 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 19 Implementing High Order Functions begin 06-08...")


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

(define (true-for-any-pair? pred sent)
  (cond ((empty? sent) #f)                   ;; empty
        ((empty? (cdr sent)) #f)             ;; only one item, no pairs
        ((pred (car sent) (cadr sent))   #t) ;; satisfied at least once
        (else (true-for-any-pair? pred (cdr sent)))))

;; Boundary conditions:

(check (true-for-any-pair? equal? '()) => #f)
(check (true-for-any-pair? equal? '(1)) => #f)
(check (true-for-any-pair? equal? '(1 1)) => #t)
(check (true-for-any-pair? equal? '(1 1 1)) => #t)

;; And the provided test cases:

(check (true-for-any-pair? equal? '(a b c b a)) => #f)
(check (true-for-any-pair? equal? '(a b c c d)) => #t)
(check (true-for-any-pair? < '(20 16 5 8 6)) => #t)     ;; 5 is less than 8


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

(define (true-for-all-pairs? pred sent)
  (cond ((empty? sent) #f)                ;; there must be at least
        ((empty? (cdr sent)) #f)          ;; one pair.
        (else (true-for-all-pairs-r? pred sent))))

(define (true-for-all-pairs-r? pred sent)
  (cond ((empty? (cdr sent))       #t)    ;; empty and one element handled by wrapper
        ((not (pred (car sent) (cadr sent))) #f)
        (else (true-for-all-pairs-r? pred (cdr sent)))))

(check (true-for-all-pairs? equal? '(a b c c d)) => #f)
(check (true-for-all-pairs? equal? '(a a a a a)) => #t)
(check (true-for-all-pairs? < '(20 16 5 8 6)) => #f)
(check (true-for-all-pairs? < '(3 7 19 22 43)) => #t)


;; ----------------------------------------------
;; 19.8 Rewrite true-for-all-pairs? (Exercise 19.7) using true-for-any-pair?
;; (Exercise 19.6) as a helper procedure. Don't use recursion in solving
;; this problem (except for the recursion you've already used to write
;; true-for-any-pair?). Hint: You'll find the not procedure helpful.

;; I'm not seeing where they are going with this. I could replace the second
;; arm of the 'cond' with 'true-for-any-pairs?' but why? Is the idea to
;; call any-pairs with (not pred)?
;;
;; Of the two or three (out of approximately forty) repos on Github for these,
;; only a couple of other people have gone past the first ten or so chapters.
;; The solution for this problem is that -I need to read better-. The recursion
;; is from 'true-for-any-pair?' and not to use the recursion already in
;; 'true-for-all-pairs?'. Make 'true-for-all-pairs?' a wrapper over
;; 'true-for-any-pairs?' with a modified predicate using a lambda.

(define (true-for-all-pairs-2? pred sent)
  (cond ((empty? sent)            #f)
        ((empty? (cdr sent))      #f)
        (else (not (true-for-any-pair?
                     (lambda (a b) (not (pred a b)))
                     sent)))))

(check (true-for-all-pairs-2? equal? '(a b c c d)) => #f)
(check (true-for-all-pairs-2? equal? '(a a a a a)) => #t)
(check (true-for-all-pairs-2? < '(20 16 5 8 6)) => #f)
(check (true-for-all-pairs-2? < '(3 7 19 22 43)) => #t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 19 Implementing High Order Functions end 06-08...")
