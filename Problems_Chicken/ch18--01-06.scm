;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 18 Trees

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load srfi 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 18 Trees begin...")

;; for test setup

(define make-node cons)
(define datum car)
(define children cdr)
(define (leaf datum)
  (make-node datum '()))
(define (cities name-list)
  (map leaf name-list))
(define (leaf? tree) (null? (cdr tree)))

;; end test setup

;; --------------------------------------------------------
;; 18.1 What does '((SAN FRANCISCO))' mean in the printout of world-tree? Why
;; two sets of parentheses?
;;
;; That the datum in question is a 'sentence', made up of more than one symbol.
;; The list still holds one datum(city), but the name itself is carried as a
;; list.


;; --------------------------------------------------------
;; 18.2 Suppose we change the definition of the tree constructor so that it
;; uses list instead of cons:
;;
;;       (define (make-node datum children)
;;         (list datum children))
;;
;; How do we have to change the selectors so that everything still works?

;; Answer:
;;
;; (define children cadr)
;;
;; instead of
;;
;; (define children cdr).
;;
;; The use of list just wraps the children (which is already a list) in another
;; list of one entry. Tested and working.


;; --------------------------------------------------------
;; 18.3 Write depth, a procedure that takes a tree as argument and returns the
;; largest number of nodes connected through parent-child links. That is, a
;; leaf node has depth 1; a tree in which all the children of the root node are
;; leaves has depth 2. Our world tree has depth 4 (because the longest path
;; from the root to a leaf is, for example, world, country, state, city).

(define (depth-r branches)
  (cond ((empty? branches) 0)
        (else (max (depth (car branches))
                   (depth-r (cdr branches))))))

(define (depth tree)
  (cond ((leaf? tree) 1)
        (else (+ 1 (depth-r (children tree))))))

(define one-deep (make-node 'one '()))
(define two-deep (make-node 'one (list (make-node 'two '()))))
(define three-deep
  (make-node 'one (list (make-node 'two (list (make-node 'three '()))))))
(define four-deep
  (make-node
    'world
    (list (make-node 'italy (cities '(rome venice naples))) ;; 3 deep
          (make-node 'USA
                     (list
                       (make-node 'ohio
                                  (cities '(cincinnati columbus cleveland))) ;; 4 deep
                       (make-node 'texas
                                  (cities '(houston dallas brownsville)))))
          (make-node 'france
                     (cities '(paris lyon dijon)))))) ;; 3 deep

(check (depth one-deep) => 1)
(check (depth two-deep) => 2)
(check (depth three-deep) => 3)
(check (depth four-deep) => 4)


;; --------------------------------------------------------
;; 18.4 Write count-nodes, a procedure that takes a tree as argument and
;; returns the total number of nodes in the tree. (Earlier we counted the
;; number of leaf nodes.)

(define (count-nodes-r tree)
  (cond ((empty? tree)  0)
        (else      (+ (count-nodes (car tree)) (count-nodes-r (cdr tree))))))

(define (count-nodes tree)
  (cond ((leaf? tree) 1)
        (else (+ 1 (count-nodes-r (cdr tree))))))

(check (count-nodes one-deep) => 1)
(check (count-nodes two-deep) => 2)
(check (count-nodes three-deep) => 3)
(check (count-nodes four-deep) => 18)


;; --------------------------------------------------------
;; 18.5 Write prune, a procedure that takes a tree as argument and returns a
;; copy of the tree, but with all the leaf nodes of the original tree removed.
;; (If the argument to prune is a one-node tree, in which the root node has no
;; children, then prune should return #f because the result of removing the
;; root node wouldn't be a tree.)

(define (prune tree)
  (cond ((leaf? tree) #f)
        (else (make-node (car tree) (prune-r (cdr tree))))))

(define (prune-r tree)
  (cond ((null? tree) '())
        ((leaf? (car tree)) (prune-r (cdr tree)))
        (else (make-node (prune (car tree)) (prune-r (cdr tree))))))

(define t0 (make-node 't0 '()))
(define t1 (make-node 't1 (list (make-node 's1 (cities '(c1 c2 c3)))
                                (make-node 's2 (cities '(c4 c5 c6))))))
(check (prune t0) => #f)
(check (prune t1) => '(t1 (s1) (s2)))
(check (prune (prune t1)) => '(t1))


;; --------------------------------------------------------
;; 18.6 Write a program parse-scheme that parses a Scheme arithmetic expression
;; into the same kind of tree that parse produces for infix expressions. Assume
;; that all procedure invocations in the Scheme expression have two arguments.
;;
;; The resulting tree should be a valid argument to compute:
;;
;;       (compute (parse-scheme '(* (+ 4 3) 2)))
;;       14
;;
;; (You can solve this problem without the restriction to two-argument
;; invocations if you rewrite compute so that it doesn't assume every branch
;; node has two children.)

;; ---------- begin supporting code from text for testing ----------
(define (compute tree)
  (if (number? (datum tree))
    (datum tree)
    ((function-named-by (datum tree))
     (compute (car (children tree)))
     (compute (cadr (children tree))))))

(define (function-named-by oper)
  (cond ((equal? oper '+) +) ((equal? oper '-) -)
        ((equal? oper '*) *) ((equal? oper '/) /)
        (else (error "no such operator as" oper))))

(define (parse expr) (parse-helper expr '() '()))

(define (parse-helper expr operators operands)
  (cond ((null? expr)
         (if (null? operators)
           (car operands)
           (handle-op '() operators operands)))
        ((number? (car expr))
         (parse-helper (cdr expr)
                       operators
                       (cons (make-node (car expr) '()) operands)))
        ((list? (car expr))
         (parse-helper (cdr expr)
                       operators
                       (cons (parse (car expr)) operands)))
        (else
          (if (or (null? operators) (> (precedence (car expr)) (precedence (car operators))))
            (parse-helper (cdr expr)
                          (cons (car expr) operators)
                          operands)
            (handle-op expr operators operands)))))

(define (handle-op expr operators operands)
  (parse-helper expr
                (cdr operators)
                (cons (make-node (car operators)
                                 (list (cadr operands) (car operands)))
                      (cddr operands))))

(define (operator? x) (member? x '(* / + -)))
(define (precedence oper) (if (member? oper '(+ -)) 1 2))

;; ---------- end supporting code from text for testing ----------

;; ------------ two argument support only ------------------------

;; a mini grammar:
;;
;;               addop := + | -
;;               mulop := * | /
;;              factor := (+ | -)* <digit>+
;;
;;               term  := <factor> [<mulop> <factor]*
;;
;; infix:     in_expr := <term> [<addop> <term>]
;;
;; prefix:   pre_expr := (<addop>|<mulop>) (<factor>|<pre_expr>) (<factor|pre_expr>)
;;
;; pretty simple, yeah?
;;
;; (* (+ 4 3) 2) becomes (* (+ (4) (3)) (2)) for 'compute'.

(define (parse-scheme expr)
  (parse-scheme-r expr '() '()))

(define (parse-scheme-r src op-stack term-stack)
  (cond ((null? src)
         (if (null? op-stack)
           (car term-stack)
           (make-node (car op-stack)
                      (list (cadr term-stack) (car term-stack)))))
        ((number? (car src))
         (parse-scheme-r (cdr src)
                         op-stack
                         (cons (make-node (car src) '()) term-stack)))
        ((list? (car src))
         (parse-scheme-r (cdr src)
                         op-stack
                         (cons (parse-scheme (car src)) term-stack)))
        (else
          (parse-scheme-r (cdr src)
                          (cons (car src) op-stack)
                          term-stack))))

;; Tests: Be sure to get the order on a subtract correct!

(define expr-1 '(* (+ 4 3) 2))       (define ans-1 14)
(define expr-2 '(- 1 2))             (define ans-2 -1)
(define expr-3 '(* 2 (* 2 (* 2 2)))) (define ans-3 16)

(check (compute (parse-scheme expr-1)) => ans-1)
(check (compute (parse-scheme expr-2)) => ans-2)
(check (compute (parse-scheme expr-3)) => ans-3)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 18 Trees end...")
