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

(print "Chapter 23 Files begin 01-07...")

;; NOTE: Do not solve any of the following exercises by converting a vector
;; to a list, using list procedures, and then converting the result back to
;; a vector.


;; ----------------------------------------------
;; 23.1 Write a procedure sum-vector that takes a vector full of numbers as
;; its argument and returns the sum of all the numbers:
;;
;; (sum-vector '#(6 7 8)) => 21

(define (sum-vector v)
  (sum-vector-r v 0 0 (vector-length v)))

(define (sum-vector-r v accum curr len)
  (cond ((= curr len)
         accum)
        (else (sum-vector-r v (+ accum (vector-ref v curr)) (+ curr 1) len))))

(check (sum-vector #(1 2 3)) => 6)


;; ----------------------------------------------
;; 23.2 Some versions of Scheme provide a procedure vector-fill! that
;; takes a vector and anything as its two arguments. It replaces every
;; element of the vector with the second argument, like this:
;;
;; (define vec (vector 'one 'two 'three 'four))
;; vec => #(one two three four)
;;
;; (vector-fill! vec 'yeah)
;; vec => #(yeah yeah yeah yeah)
;;
;; Write vector-fill!. (It doesn't matter what value it returns.)

;; NOTE: 'vector-fill!' exists in Chicken 5, so I'm using the name
;; 'vector-flood!'.

(define (vector-flood! v x)
  (vector-flood!-r v x 0 (vector-length v)))

(define (vector-flood!-r v x curr len)
  (cond ((= curr len)
         curr)
        (else (vector-set! v curr x)
              (vector-flood!-r v x (+ curr 1) len))))

(define v (make-vector 10 0))
(check (sum-vector v) => 0)
(vector-flood! v 1)
(check (sum-vector v) => 10)


;; ----------------------------------------------
;; 23.3 Write a function vector-append that works just like regular
;; append, but for vectors:
;;
;; (vector-append '#(not a) '#(second time)) => #(not a second time)

;; I'm creating some vector infrastructure helpers here. This is probably
;; overkill for the assignments but I enjoy the practice.
;;
;; I'm inefficient in a few areas mostly to avoid the "two references same
;; object" issue that (append only-one-vector) would create. This could be
;; tightened up.

(define (dup-vector v)
  (dup-vector-r (make-vector (vector-length v) 0) v 0 (vector-length v)))

(define (dup-vector-r dup v curr len)
  (cond ((= curr len)
         dup)
        (else
          (vector-set! dup curr (vector-ref v curr))
          (dup-vector-r dup v (+ curr 1) len))))

(define v #(1 2 3))
(define u (dup-vector v))
(check (sum-vector v) => 6)
(check (sum-vector u) => 6)
(vector-set! v 0 -5)          ;; confirm that u is distinct from v
(check (sum-vector v) => 0)
(check (sum-vector u) => 6)

(define (copy-into-vector! v1 start len v2)
  (copy-into-vector!-r v1 start len 0 v2))

(define (copy-into-vector!-r v1 start len curr v2)
  (cond ((= len curr) v1)
        (else
          (vector-set! v1 (+ start curr) (vector-ref v2 curr))
          (copy-into-vector!-r v1 start len (+ curr 1) v2))))

(define v #(1 2 3 4 5 6 7 8 9 10))
(define u #(1 2 3 4 5))
(check (sum-vector v) => 55)
(copy-into-vector! v 5 5 u)
(check (sum-vector v) => 30)
(check (sum-vector u) => 15)

(define (append-2-vectors v1 v2)
  (if (empty? v2)
    (dup-vector v1)
    (let ((v3 (make-vector (+ (vector-length v1) (vector-length v2)) 0)))
      (copy-into-vector! v3 0 (vector-length v1) v1)
      (copy-into-vector! v3 (vector-length v1) (vector-length v2) v2)
      v3)))

(define v #(1 2 3 4 5))
(define u #(6 7 8 9 10))
(check (sum-vector v) => 15)
(check (sum-vector u) => 40)
(define w (append-2-vectors v u))
(check (sum-vector w) => 55)

(define (vector-append . vargs)
  (if (empty? vargs)              ;; should only happen if called with no args
    #()
    (if (= (count vargs) 1)       ;; the real terminal case
      (dup-vector (car vargs))
      (let
        ((v1 (car vargs))
         (v2 (cadr vargs))
         (vr (cdr (cdr vargs))))
        (apply vector-append (cons (append-2-vectors v1 v2) vr))))))

(define u #(1 2 3))
(define v #(4 5 6))
(define w #(7 8 9))
(check (vector-append) => #())
(check (vector-append u) => #(1 2 3))
(check (vector-append u v) => #(1 2 3 4 5 6))
(check (vector-append u v w) => #(1 2 3 4 5 6 7 8 9))

;; I think I've established that I'm doing deep copies in the helper tests.


;; ----------------------------------------------
;; 23.4  Write vector->list.

;; NOTE: 'vector->list' is in the Chicken base. I'm using the same name so
;; that if I need a 'vector->list' in the exercises, I'll be using mine.

(define (vector->list v)
  (vector->list-r v '() (vector-length v)))

(define (vector->list-r v accum curr)
  (if (= curr 0)
    accum
    (vector->list-r v (cons (vector-ref v (- curr 1)) accum) (- curr 1))))

(define v #(1 2 3 4))
(check (vector->list v) => '(1 2 3 4))


;; ----------------------------------------------
;; 23.5 Write a procedure vector-map that takes two arguments, a function
;; and a vector, and returns a new vector in which each box contains the
;; result of applying the function to the corresponding element of the
;; argument vector.

(define (vector-map fn v)
  (let ((dv (dup-vector v)))
    (vector-map-r fn dv (vector-length dv))))

;; I'm updating in place, but it's a copy of the original.
;;
;; As it turns out, this helper can be used for 'vector-map!' in the next
;; exercise.

(define (vector-map-r fn v curr)
  (if (= curr 0)
    v
    (let* ((i (- curr 1))
           (ov (vector-ref v i))
           (nv (fn ov)))
      (vector-set! v i nv)
      (vector-map-r fn v i))))

(define v #(1 2 3))
(define (p1 n) (+ n 1))
(check (vector-map p1 v) => #(2 3 4))
(check v => #(1 2 3))


;; ----------------------------------------------
;; 23.6 Write a procedure vector-map! that takes two arguments, a function
;; and a vector, and modifies the argument vector by replacing each element
;; with the result of applying the function to that element. Your procedure
;; should return the same vector.

(define (vector-map! fn v)
    (vector-map-r fn v (vector-length v)))

;; we can reuse the helper for 23.5 directly!

(define v #(1 2 3))
(define (p1 n) (+ n 1))
(check (vector-map! p1 v) => #(2 3 4))
(check v => #(2 3 4))


;; ----------------------------------------------
;; 23.7 Could you write vector-filter? How about vector-filter!? Explain
;; the issues involved.

;; I'm not convinced there is a reason to do so. What do you do with slots
;; that don't pass the filter? Does the filtering function receive the
;; current entry in the vector or the vector and an offset?
;;
;; While it seems possible to define 'vector-filter' and the filtering
;; function to have a useful and sensible behavior, the 'vector-filter!'
;; variant would require that we update the vector in place. Can we do
;; something analogous to the OOP 'become'? Do we left shift out the items
;; that don't pass the filter and have a way to truncate the vector?
;;
;; My conclusion is that 'vector-filter?' can be written such that the
;; filtering function receives only the value of the current entry in the
;; vector. The result would be a new vector made up of the elements that
;; pass the filter, but could be shorter than the original vector.
;;
;; The semantics of 'vector-filter!' just don't work for me. I wouldn't
;; write it.


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 23 Files end 01-07...")
