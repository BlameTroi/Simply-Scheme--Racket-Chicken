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

(print "Chapter 19 Implementing High Order Functions begin 11-13...")


;; ----------------------------------------------
;; 19.11 Write repeated. (This is a hard exercise!)

;; I disagree, it's only hard if you overthink it. Of course, I overthought it.
;;
;; As their runtime defines repeated, I'll write this as repeater.
;;
;; I'll pull examples from Chapter 8 to use as test cases.

;; the answer:

(define (repeater-r n fn a)
  (cond ((< n 1)
         a)                          ;; terminal case one
        ((= n 1)
         (fn a))                     ;; terminal case two
        (else (repeater-r (- n 1) fn (fn a)))))

(define (repeater fn n)
  (lambda (a) (repeater-r n fn a)))

;; For comparison, the definition in the standard environment. The = = - - bit
;; is something needed for the environment, they override many of the standard
;; operators. I can't decide if I like my version or theirs more.

(define repeated
  (let ((= =) (- -))
    (lambda (fn number)
      (if (= number 0)
	  (lambda (x) x)
	  (lambda (x)
	    ((repeated fn (- number 1)) (fn x)))))))

;; Functions required for the tests. They aren't using the full 'plural'
;; that we did in Chapter 6. Here they just append an 's. While 'item' is
;; in their standard environment, they provide a new definition in the
;; text.

(define (plural wd)
  (cond ((sentence? wd) (sentence (butlast wd) (plural (last wd))))
        ((number? wd)                                           wd)
        (else                                         (word wd 's))))

(define (item n sent)
  (first ((repeater bf (- n 1)) sent)))

;; The tests.

(check ((repeater bf 3) '(she came in through the bathroom window)) =>
       '(through the bathroom window))
(check ((repeater plural 4) 'computer) =>
       'computerssss)
(define (square n) (* n n))
(check ((repeater square 2) 3) =>
       81)
(check ((repeater square 4) 2) =>
       65536)
(check (item 1 '(a day in the life))
       => 'a)
(check (item 4 '(a day in the life))
       => 'the)


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

;; As 3 + 4 + 7 + 2 + 3 + 8 => 27 does this hint that my tree-map was
;; correct after all?
;;
;; Reminder: the children are each a node; a node is (datum (node*)).
;;
;; Functions that can take zero arguments seem to all return their "zero"
;; state. I believe I read that in the text. Regardless, it's true for the
;; combiners we use in the context of this text: (+) => 0, (*) => 1, (word) =>
;; "", (se) => ().
;;
;; = and / do not have a "zero" state. This makes sense when you think about
;; it.
;;
;; I'm not thrilled with this even though it works. The one solution I found
;; on Github did it in two functions by pulling tree-reduce-r into tree-reduce.
;; I get it, but I'll stick with my three function solution.

(define (tree-reduce fn tree)
  (tree-reduce-r fn (fn) tree))

(define (tree-reduce-r fn accum node)
  (cond ((empty? node)
         accum)
         (else (fn
                 accum
                 (fn
                   (datum node)
                   (children-r fn (fn) (children node)))))))

(define (children-r fn accum nodes)
  (cond ((empty? nodes)
         accum)
        (else (fn
                (tree-reduce fn (car nodes))
                (children-r fn (fn) (cdr nodes))))))

(define some-tree
  (make-node 3 (list
                 (make-node 4 '())
                 (make-node 7 '())
                 (make-node 2 (list
                                (make-node 3 '())
                                (make-node 8 '())))))) ;; => 27

(check (tree-reduce + some-tree) => 27)


;; ----------------------------------------------
;; 19.13 Write deep-reduce, similar to tree-reduce, but for structured lists:
;;
;; (deep-reduce word '(r ((a (m b) (l)) (e (r))))) => 'rambler

;; Allowing for combiner to accept zero or more arguments as in 19.12, I have
;; the following suspenders-and-belt implementation. Not all of the terminal
;; conditions I've coded for may be possible. I'll come back to this later,
;; maybe.

(define (deep-reduce fn thing)
  (deep-reduce-r fn (fn) thing))

(define (deep-reduce-r fn accum thing)
  (cond ((not thing)               ;; terminal one, nil
         accum)
        ((not (list? thing))       ;; terminal two, atom
         (fn accum thing))
        ((empty? thing)            ;; terminal three, empty thing
         accum)
        ((list? (car thing))
         (fn accum (deep-reduce fn (car thing)) (deep-reduce-r fn (fn) (cdr thing))))
        (else
          (fn accum (fn (car thing)) (deep-reduce-r fn (fn) (cdr thing))))))

(define rambler '(r ((a (m b) (l)) (e (r)))))

(check (deep-reduce word rambler) => 'rambler)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 19 Implementing High Order Functions end 11-13...")
