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

(print "Chapter 19 Implementing High Order Functions begin 09-10...")


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

(check (mergesort '(4 23 7 5 16 3) <) => '(3 4 5 7 16 23))
(check (mergesort '(4 23 7 5 16 3) >) => '(23 16 7 5 4 3))
(check (mergesort '(john paul george ringo) before?) => '(george john paul ringo))


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

;; The 'deep-map' procedure from the text and their description. Suppose we
;; have a structure full of numbers and we want to compute all of their
;; squares. We could write a specific procedure deep-square, but instead, we'll
;; write a higher-order procedure:

(define (deep-map f structure)
  (cond ((word? structure) (f structure))
        ((null? structure) '())
        (else (cons (deep-map f (car structure))
                    (deep-map f (cdr structure))))))

;; At first glance, this is just a textual exercise, but I'm not sure about the
;; order of the 'cond'.

(define (tree-map-1 f structure)
  (cond ((word? structure) (f structure))
        ((null? structure) '())
        (else (cons (tree-map-1 f (datum structure))
                    (tree-map-1 f (children structure))))))

;; They gave no test case, but they mentioned doing a 'deep square' so let's do
;; that and see what happens.

(define some-tree (make-node 3 (list (make-node 4 (cities '(5 6 7)))
                                     (make-node 8 (cities '(9 10 11))))))

(tree-map-1 (lambda (n) (* n n)) some-tree)

;; That preserved the structure of the tree and squared ever datum. Wondering
;; what I had missed, I checked some other solutions. There weren't many, but
;; they all only applied f to the leaves. It's just not clear to me which of
;; these the authors meant. I've included the leaves only version here for
;; reference.

(define (tree-map-2 f tree)
  (if (leaf? tree)
    (make-node (f (datum tree)) '())
    (cons (datum tree)
          (tree-map-2-in-forest f (children tree)))))

(define (tree-map-2-in-forest f tree)
  (if (null? tree)
    '()
    (cons (tree-map-2 f (car tree))
          (tree-map-2-in-forest f (cdr tree)))))

(tree-map-2 (lambda (n) (* n n)) some-tree)

;; Here's the output from each. I dunno which one they want, but both are
;; straight forward.
;;
;; (tree-map-1 (lambda (n) (* n n)) some-tree) => (9 (16 (25) (36) (49)) (64 (81) (100) (121)))
;;
;; (tree-map-2 (lambda (n) (* n n)) some-tree) => (3 (4 (25) (36) (49)) (8 (81) (100) (121)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 19 Implementing High Order Functions end 09-10...")
