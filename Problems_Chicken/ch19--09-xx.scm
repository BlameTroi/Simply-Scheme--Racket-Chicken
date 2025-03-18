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

(define (my-keep pred? stuff)
  (cond ((sentence? stuff) (my-keep-r pred? stuff se))
        (else              (my-keep-r pred? stuff word))))

(define (my-keep-r pred? stuff fn)
  (cond ((empty? stuff) (my-nil-for fn))
        (else (if (pred? (first stuff))
                (fn (first stuff) (my-keep-r pred? (bf stuff) fn))
                (my-keep-r pred? (bf stuff) fn)))))

(define (my-nil-for fn)
  (cond ((equal? fn se) '())
        ((equal? fn word) "")
        (else '())))

;; The authors have a 'combiner' in their standard framework, but it's hidden
;; from view. A sentence can take as arguments two words or two sentences, so
;; I'm not yet clear how I want to manage that. After some thought, we'll trust
;; the user to decide since the 'stuff' to 'keep' will be either a sentence or
;; a list.
;;
;; The authors' suggestion worked but I didn't like because it seemed a bit
;; redundant. The only thing that 'my-combiner' needed to do that the fn verb
;; didn't was handle the empty case. For 'word', empty is "", while for
;; 'sentence', the empty case is '().
;;
;; The solutions reads best with using the fn verb 'word' or 'se' as the sole
;; combiner, and determining the terminal value based on the verb. Rather than
;; inline that, I added a 'my-nil-for' to return the appropriate nil/empty
;; value.
;;
;; In the description of the 'three-arg-accumulate' they describe another way
;; to deal with base case values by checking to see if we have reached the last
;; element (car present, cdr empty) and if so the value of the last element is
;; the terminal value.
;;
;; That would work for this as well, but that's after problem 19.3.

;; Now test words and sentences.

(define (noise-word? x) (member? x '(a is be do at the this of)))
(check (my-keep
         noise-word?
         '(this is a sentence that has a mix of real indexable words and noise words))
       => '(this is a a of))
(check (my-keep vowel? 'superdupertrooper) => 'ueueooe)


;; ----------------------------------------------
;; 19.3 Write the three-argument version of accumulate that we described.
;;
;; (three-arg-accumulate + 0 '(4 5 6)) => 15
;;
;; (three-arg-accumulate + 0 '()) => 0
;;
;; (three-arg-accumulate cons '() '(a b c d e)) => (A B C D E)

(define (three-arg-accumulate verb terminator stuff)
  (cond ((empty? stuff)           terminator)
        (else (verb
                (first stuff)
                (three-arg-accumulate verb terminator (bf stuff))))))

;; Testing the three argument against their expected results and then
;; against their standard 'accumulate'. Interesting, their 'accumulate'
;; of cons '(list) returns '(<butlast list> . <last list>). So, we
;; get one failure reported but I matched the problem spec.

(check (three-arg-accumulate + 0 '(4 5 6)) => 15)
(check (three-arg-accumulate + 0 '(4 5 6)) => (accumulate + '(4 5 6)))

(check (three-arg-accumulate + 0 '()) => 0)
(check (three-arg-accumulate + 0 '()) => (accumulate + '()))

(check (three-arg-accumulate cons '() '(a b c d e)) => '(a b c d e))
;; (check (three-arg-accumulate cons '() '(a b c d e)) => (accumulate cons '(a b c d e)))


;; ----------------------------------------------
;; 19.4 Our accumulate combines elements from right to left. That is:
;;
;; (accumulate - '(2 3 4 5)) computes 2−(3−(4−5)).
;;
;; Write left-accumulate, which will compute ((2−3)−4)−5 instead. (The result
;; will be the same for an operation such as +, for which grouping order
;; doesn't matter, but will be different for -.)

;; The obvious 'cheat' is to reverse the input, but that isn't
;; quite right:

(define (left-accumulate-wrong verb stuff)
  (accumulate verb (reverse stuff)))

;; But this works correctly, but again with the difference in consing
;; between their standard 'accumulate' and the result expected by
;; 'three-arg-accumulate'.

(define (left-accumulate verb stuff)
  (cond ((empty? (bl stuff))            (last stuff))
        (else (verb
                (left-accumulate verb (bl stuff))
                (last stuff)))))

(check (left-accumulate - '(2 3 4 5)) => (- (- (- 2 3) 4) 5))

;;                                       -1 - 4 = -5 -5 = -10


;; ----------------------------------------------
;; 19.5 Rewrite the true-for-all? procedure from Exercise 8.10. Do not use
;; every, keep, or accumulate.

;; For reference:

(define (ch8-true-for-all? pred sent)
  (cond ((empty? sent)              #f)
        (else (equal? (keep pred sent) sent))))

(define (tfa-pred x) (member x '(a b c)))

(check (ch8-true-for-all? tfa-pred '(a b c)) => #t)
(check (ch8-true-for-all? tfa-pred '(a b c d)) => #f)
(check (ch8-true-for-all? tfa-pred '()) => #f)

;; And for the this problem:

(define (true-for-all? pred sent)
  (cond ((empty? sent) #f)
        ((not (pred (car sent))) #f)
        ((empty? (cdr sent)) #t)
        (else (true-for-all? pred (cdr sent)))))

(check (true-for-all? tfa-pred '(a b c)) => #t)
(check (true-for-all? tfa-pred '(a b c d)) => #f)
(check (true-for-all? tfa-pred '()) => #f)


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
        ((pred (car sent) (cadr sent))   #t) ;; satisifed at least once
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
;; Of the two or three (out of approximately forty) repos on Github for
;; these, only a couple of other people have gone past the first ten or
;; so chapters. The solutions is that -I need to read better-. The
;; recursion is from 'true-for-any-pair?' and not to use the recursion
;; already in 'true-for-all-pairs?'. Make 'true-for-all-pairs?' a wrapper
;; over 'true-for-any-pairs?' with a modified predicate using a lambda.

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

;; I lifted code from my workthrough of chapter 15 (ch15--01-04.scm) and
;; stripped my commentary. At first glance it seems that the 'merge' procedure
;; needs a predicate parameter. The question is how best to fit that in.
;;
;; I'll also use 'car' and 'cdr' instead of 'first' and 'butfirst', but I will
;; keep using 'sentence' as the aggregator.
;;
;; Nesting 'letrec' style would read more cleanly, but we haven't been properly
;; introduced to that yet. We were shown optional parameters in chapter 17 so I
;; will use them.

(define (mergesort sent . args)
  (let ((pred       (cond ((empty? args)        before?)
                          ((= (count args) 1) (car args))
                          (else 'bad-ordering-predicate))))
    (mergesort-r sent pred)))

(define (mergesort-r sent pred)
  (if (<= (count sent) 1)
    sent
    (merge pred (mergesort-r (one-half sent) pred)
           (mergesort-r (other-half sent) pred))))

(define (merge ordering xs ys)
  (cond ((empty? xs)                      ys)
        ((empty? ys)                      xs)
        ((ordering (car xs) (car ys)) (se (car xs) (merge ordering (cdr xs) ys)))
        (else                            (se (car ys) (merge ordering xs (cdr ys))))))

(define (one-half sent)
  (if (<= (count sent) 1)
      sent
      (se (car sent) (one-half (cdr (cdr sent))))))

(define (other-half sent)
  (if (<= (count sent) 1)
      '()
      (se (car (cdr sent)) (other-half (cdr (cdr sent))))))

(check (one-half '(a b c d e)) => '(a c e))
(check (other-half '(a b c d e)) => '(b d))

(check (mergesort '(e d c b a)) => '(a b c d e))
(check (mergesort '(a b c d e)) => '(a b c d e))
(check (mergesort '(a b c d e) (lambda (a b) (not (before? a b)))) => '(e d c b a))
(check (mergesort '(10 9 1 2 7 8 6 5 3 4 0) >) => '(10 9 8 7 6 5 4 3 2 1 0))


;; ----------------------------------------------
;; 19.10 Write tree-map, analogous to our deep-map, but for trees, using the
;; datum and children selectors.

;; My full set of their tree api.

(define make-node cons)
(define datum car)
(define children cdr)
(define (leaf datum)
  (make-node datum '()))
(define (cities name-list)
  (map leaf name-list))
(define (leaf? tree) (null? (cdr tree)))

;; The 'deep-map' procedure from the text.

(define (deep-map f structure)
  (cond ((word? structure) (f structure))
        ((null? structure) '())
        (else (cons (deep-map f (car structure))
                    (deep-map f (cdr structure))))))

;; At first glance, this is just a textual exercise, but I'm not sure about the
;; order of the 'cond'.

(define (tree-map f structure)
  (cond ((word? structure) (f structure))
        ((null? structure) '())
        (else (cons (tree-map f (datum structure))
                    (tree-map f (children structure))))))

;; They gave no test case, but they mentioned doing a 'deep square' so let's do
;; that and see what happens.

(define some-tree (make-node 3 (list (make-node 4 (cities '(5 6 7)))
                                     (make-node 8 (cities '(9 10 11))))))

(tree-map (lambda (n) (* n n)) some-tree)


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
