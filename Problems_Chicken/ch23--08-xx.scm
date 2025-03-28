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

;; I decided that if the data comes in as a list, I would use the list
;; procedures if they make sense. The specific case I ran into was that
;; the way to write 'se' is (simplified) to '(lambda args (list->vector
;; args)'. This isn't quite sufficient as a sentence could be in the args,
;; but Scheme presents the full set of arguments as a list. I will work
;; to maintain the spirit, if not the letter, of their constraint.

;; NOTE: Some of these may need the solutions to prior problems in this
;; chapter. Testing should be done in chapter-problem sequence.

;; ----------------------------------------------
;; 23.8 Modify the lap procedure to print "Car 34 wins!" when car 34
;; completes its 200th lap. (A harder but more correct modification is to
;; print the message only if no other car has completed 200 laps.)

;; General utilities:

(define (max-vector v)
  (max-vector-r v (vector-length v) (vector-ref v 0)))

(define (max-vector-r v pos so-far)
  (if (= pos 0)
    so-far
    (let* ;; avoiding the nested lets
      ((p (- pos 1))
       (n (vector-ref v p)))
      (max-vector-r v p (max n so-far)))))

(define max-test-vector #(1 2 3 4 5 4 3 2 1))
(check (max-vector max-test-vector) => 5)

;; From the text:

(define (initialize-lap-vector index)
  (if (< index 0)
    'done
    (begin (vector-set! *lap-vector* index 0)
           (initialize-lap-vector (- index 1)))))

(define (original-lap car-number)
  (vector-set! *lap-vector*
               car-number
               (+ (vector-ref *lap-vector* car-number) 1))
  (vector-ref *lap-vector* car-number))

;; Global state collected:

(define *lap-vector* (make-vector 100))
(initialize-lap-vector 99)
(define *race-laps* 5)        ;; New, how long is the race?

;; Updated 'lap' to report the first car to complete the race:

(define (lap car-number)
  (let*
    ((laps (vector-ref *lap-vector* car-number)) ;; prior lap
     (next (+ laps 1))                           ;; not persisted yet
     (leader (max-vector *lap-vector*))          ;; the leader, don't care who
     (finished (>= leader *race-laps*)))         ;; is the race over
    (vector-set! *lap-vector* car-number next)    ;; persist this lap
    (if finished
      next
      (if (= next *race-laps*)
        (begin (show-line (se "Car" car-number "wins!")) next)
        next))))


;; ----------------------------------------------
;; 23.9 Write a procedure leader that says which car is in the lead right
;; now.

;; Ignoring the case of a tie and not creating a separate accessor though
;; problem 23.10 shows why one would be helpful.

(define (leader)
  (leader-r
    (max-vector *lap-vector*)           ;; furthest distance
    (vector-length *lap-vector*)))      ;; ..

(define (leader-r dist car-number)
  (if (= car-number 0)
    'error
    (if (= dist (vector-ref *lap-vector* (- car-number 1)))
      (- car-number 1)
      (leader-r dist (- car-number 1)))))


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

;; Well, the use of 'lap' to retrieve the current lap for a car updates the
;; *lap-vector*. We'd want a separate accessor function to pair with the
;; setter function.


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

(define *menu* '((egg-rolls 2.75)
                 (potstickers 4.00)
                 (wor-won-ton 2.75)
                 (shin-shin-special-prawns 6.10)))

(define *tables* (make-vector 10 0.00))

(define (open-diner) (vector-fill! *tables* 0.00))

(define (order table dish)
  (if (and (> table -1) (< table (vector-length *tables*)) (assoc dish *menu*))
    (vector-set! *tables* table (+ (vector-ref *tables* table) (cadr (assoc dish *menu*))))
    'error))

(define (bill table)
  (if (and (> table -1) (< table (vector-length *tables*)))
    (begin (show-line (se "Bill for table" table "is" (vector-ref *tables* table)))
           (vector-set! *tables* table 0.00))
    'error))

(open-diner)
(order 1 'potstickers)
(order 1 'potstickers)
(order 2 'wor-won-ton)
(order 1 'egg-rolls)
(order 2 'shin-shin-special-prawns)
(check (max-vector *tables*) => 10.75) ;; 2 potstickers 1 egg-rolls
(check (max-vector *tables*) => (vector-ref *tables* 1))
(bill 1) ;; clears table 1
(check (max-vector *tables*) => 8.85) ;; 2 potstickers 1 egg-rolls
(check (max-vector *tables*) => (vector-ref *tables* 2))
(bill 2) ;; clears table 1
(check (max-vector *tables*) => 0.00)


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
;;
;; I think of a selection sort as a bubble sort. That's how I learned the
;; algorithm in the 70s. Easy enough to remember:

;; My 'vector-swap!' implementation.

(define (vector-swap! v i j)
  (let ((k (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j k)
    v)) ;; a deviation from the text as '-set!' returns undefined value.

;; From Smalltalk I generally expect any procedure to return a value,
;; either explicitly nil, self, or the result of the procedure.

(check (vector-swap #(0 1 2 3 4 5 6 7 8 9) 2 7) => #(0 1 7 3 4 5 6 2 8 9))

;; And the solution for the problem:

(define (sort-vector! vec)
  (sort-vector!-r vec 0 (vector-length vec)))

(define (sort-vector!-r v beg end)
  (if (>= beg end)
    v
    (let ((bubble (index-least-in-vector v beg end)))
          (vector-swap! v beg bubble)
          (sort-vector!-r v (+ beg 1) end))))

(define (index-least-in-vector v beg end)
    (index-least-in-vector-r v beg (+ beg 1) end))

(define (index-least-in-vector-r v curr beg end)
  (if (>= beg end)
    curr
    (index-least-in-vector-r v
                     (if (< (vector-ref v beg) (vector-ref v curr))
                       beg
                       curr)
                     (+ 1 beg)
                     end)))

(check (index-least-in-vector #(10 9 8 1 2 3 7 6 5 4) 0 10) => 3)

(check (sort-vector! #(23 4 18 7 95 60)) => #(4 7 18 23 60 95)


;; ----------------------------------------------
;; 23.13 Why doesn't this work?
;;
;; (define (vector-swap! vector index1 index2)
;;   (vector-set! vector index1 (vector-ref vector index2))
;;   (vector-set! vector index2 (vector-ref vector index1)))

;; You need an intermediary field to do the swap. Otherwise you
;; just get a copy of one of the indices and lose the other:
;;
;; index1  index2
;;    1       4          iniital
;;    4 ----> 4          -set index1 to -ref index2
;;    4 <---- 4          -set index2 to -ref index1


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

;; This is fun. You have to watch for missed evaluations resulting
;; in multiple references to the same vector. Essentially, passing
;; (make-vector n m) as an argument on a function creates the one
;; instances. Reusing the argument on recursive calls in a helper
;; just copies the reference. You need to re-evaluate the 'make-vector'
;; each time you need a new vector. Depending on how much abstraction
;; you want, a factory method may be the best answer.

(define (make-matrix rows cols)
  (let ((v-rows (make-vector 3 '())))
    (make-matrix-r v-rows 0 rows cols)))

(define (make-matrix-r v-rows row rows cols)
  (if (= row rows)
    v-rows
    (make-matrix-r
      (begin (vector-set! v-rows row (make-vector cols '())) v-rows)
      (+ row 1)
      rows
      cols)))

(define (matrix-set! mat row col val)
  (vector-set! (vector-ref mat row) col val))

(define (matrix-ref mat row col)
  (vector-ref (vector-ref mat row) col))


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

;; Gotta noodle about how I want to do this. At first glance the impulse
;; is to create the 4 vector, then the 5 vectors, and then the 6 vectors,
;; but should we build from 6 to 5 to 4 instead? What is the base case?


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

;; (a) Write versions of 'sentence', 'empty?', 'first', 'butfirst', 'last,
;; and 'butlast' that use vectors. Your selectors need only work for
;; sentences, not for words.

;; This is definitely *not* robust.

;; predicates:

(define (sentence? x) (vector? x))

(define (word? thing)
  (cond ((sentence? thing) #f)
        ((list? thing) #f)
        (else #t)))

(define (empty? thing)
  (cond ((sentence? thing) (= (vector-ref thing 0) 0))
        (else (equal? thing '()))))

;; create, combine:

(define se (lambda parts (list->vector parts)))
;; The parts comes in as a list this way. Flatten and then scan for embedded
;; vectors, expand those to lists, then re squeeze.

;; query, extract:

(define (first sent)
  (cond ((sentence? sent) (vector-ref sent 0))
        ((list? sent) (car sent))
        (else 'error)))

(define (last sent)
  (cond ((sentence? sent) (vector-ref sent (- (vector-length sent) 1)))
        (else 'error)))

;; still to do

(define (bf sent) )

(define (bl sent) )



;; synonyms

(define sentence se)
(define butfirst bf)
(define butlast bl)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 23 Files end 08-xx...")
