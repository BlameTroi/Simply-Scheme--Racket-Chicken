;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 23 Vectors.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load SRFI 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 23 Files begin 08-xx...")

;; NOTE: Do not solve any of the following exercises by converting a vector
;; to a list, using list procedures, and then converting the result back to
;; a vector.

;; NOTE: Some of these may need the solutions to prior problems in this
;; chapter. Testing should be done in chapter-problem sequence.

;; ----------------------------------------------
;; 23.8 Modify the lap procedure to print "Car 34 wins!" when car 34
;; completes its 200th lap. (A harder but more correct modification is to
;; print the message only if no other car has completed 200 laps.)


;; ----------------------------------------------
;; 23.9 Write a procedure leader that says which car is in the lead right
;; now.


;; ----------------------------------------------
;; 23.10 Why doesn't this solution to Exercise 23.9 work?
;;
;; (define (leader)
;;   (leader-helper 0 1))
;;
;; (define (leader-helper leader index)
;;   (cond ((= index 100) leader)
;;         ((> (lap index) (lap leader))
;;          (leader-helper index (+ index 1)))
;;         (else (leader-helper leader (+ index 1)))))


;; ----------------------------------------------
;; 23.11 In some restaurants, the servers use computer terminals to keep
;; track of what each table has ordered. Every time you order more food,
;; the server enters your order into the computer. When you're ready for
;; the check, the computer prints your bill.
;;
;; You're going to write two procedures, order and bill. Order takes a
;; table number and an item as arguments and adds the cost of that item to
;; that table's bill. Bill takes a table number as its argument, returns
;; the amount owed by that table, and resets the table for the next
;; customers. (Your order procedure can examine a global variable *menu* to
;; find the price of each item.)
;;
;; (order 3 'potstickers)
;; (order 3 'wor-won-ton)
;; (order 5 'egg-rolls)
;; (order 3 'shin-shin-special-prawns)
;; (bill 3) => 13.85
;; (bill 5) => 2.75


;; ----------------------------------------------
;; 23.12 Rewrite selection sort (from Chapter 15) to sort a vector. This
;; can be done in a way similar to the procedure for shuffling a deck: Find
;; the smallest element of the vector and exchange it (using vector-swap!)
;; with the value in the first box. Then find the smallest element not
;; including the first box, and exchange that with the second box, and so
;; on. For example, suppose we have a vector of numbers:
;;
;; #(23 4 18 7 95 60)
;;
;; Your program should transform the vector through these intermediate
;; stages:
;;
;; #(4 23 18 7 95 60)   ; exchange 4 with 23
;; #(4 7 18 23 95 60)   ; exchange 7 with 23
;; #(4 7 18 23 95 60)   ; exchange 18 with itself
;; #(4 7 18 23 95 60)   ; exchange 23 with itself
;; #(4 7 18 23 60 95)   ; exchange 60 with 95


;; ----------------------------------------------
;; 23.13 Why doesn't this work?
;;
;; (define (vector-swap! vector index1 index2)
;;   (vector-set! vector index1 (vector-ref vector index2))
;;   (vector-set! vector index2 (vector-ref vector index1)))


;; ----------------------------------------------
;; 23.14 Implement a two-dimensional version of vectors. (We'll call one of
;; these structures a matrix.) The implementation will use a vector of
;; vectors. For example, a three-by-five matrix will be a three-element
;; vector, in which each of the elements is a five-element vector. Here's
;; how it should work:
;;
;; (define m (make-matrix 3 5))
;; (matrix-set! m 2 1 '(her majesty))
;; (matrix-ref m 2 1) => (HER MAJESTY)


;; ----------------------------------------------
;; 23.15 Generalize Exercise 23.14 by implementing an array structure that
;; can have any number of dimensions. Instead of taking two numbers as
;; index arguments, as the matrix procedures do, the array procedures will
;; take one argument, a list of numbers. The number of numbers is the
;; number of dimensions, and it will be constant for any particular array.
;; For example, here is a three-dimensional array (4×5×6):
;;
;; (define a1 (make-array '(4 5 6)))
;; (array-set! a1 '(3 2 3) '(the end))


;; ----------------------------------------------
;; 23.16 We want to reimplement sentences as vectors instead of lists.
;;
;; (a) Write versions of 'sentence', 'empty?', 'first', 'butfirst', 'last,
;; and 'butlast' that use vectors. Your selectors need only work for
;; sentences, not for words.
;;
;; (sentence 'a 'b 'c) => #(A B C)
;; (butfirst (sentence 'a 'b 'c)) => #(B C)
;;
;; (You don't have to make these procedures work on lists as well as vectors!)
;;
;; (b) Does the following program still work with the new implementation of
;; sentences? If not, fix the program.
;;
;; (define (praise stuff)
;;   (sentence stuff '(is good)))
;;
;; (c) Does the following program still work with the new implementation of
;; sentences? If not, fix the program.
;;
;; (define (praise stuff)
;;   (sentence stuff 'rules!))
;;
;; (d) Does the following program still work with the new implementation of
;; sentences? If not, fix the program. If so, is there some optional
;; rewriting that would improve its performance?
;;
;; (define (item n sent)
;;   (if (= n 1)
;;       (first sent)
;;       (item (- n 1) (butfirst sent))))
;;
;; (e) Does the following program still work with the new implementation of
;; sentences? If not, fix the program. If so, is there some optional
;; rewriting that would improve its performance?
;;
;; (define (every fn sent)
;;   (if (empty? sent)
;;     sent
;;     (sentence (fn (first sent))
;;               (every fn (butfirst sent)))))
;;
;; (f) In what ways does using vectors to implement sentences affect the
;; speed of the selectors and constructor? Why do you think we chose to use
;; lists?


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 23 Files end 08-xx...")
