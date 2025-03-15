;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 17 Lists begin ...

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load srfi 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 17 Lists begin...")

;; A general note: We're starting to hit name collisions. To avoid accidentally
;; running built in code I'm weirding up the names a bit.


;; --------------------------------------------------------
;; 17.4 Presents a procedure and asks that we predict its output before testing
;; it. On reading this appears to create a new copy of its input. Wrong, it is
;; a reversed copy of its input.

(define (mystery-helper lst other)
  (if (null? lst)
    other
    (mystery-helper (cdr lst) (cons (car lst) other))))

(define (mystery lst)
  (mystery-helper lst '()))

(check (mystery '(1 2 3 4)) => '(4 3 2 1))


;; --------------------------------------------------------
;; 17.5 Implement (max ...) returning the maximum of an arbitrarily long list
;; of numbers. Do this in terms of (max2 a b) which returns the maximum of the
;; two numbers.

(define (max2 a b) (if (> a b) a b))

(define (maxi x . xs)
  (cond ((empty? xs)    x)
        (else (apply maxi (max2 x (car xs)) (cdr xs)))))

(check (max2 3 4) => 4)
(check (max2 4 3) => 4)
(check (maxi 1) => 1)
(check (maxi 1 2 3 4 9 8 7 6 5) => 9)


;; --------------------------------------------------------
;; 17.6 Use 'car', 'cdr', and 'cons' to write 'append'. First as a procedure
;; taking two arguments, and then generalize it for any number of arguments.
;;
;; While 'append' appends lists, I decided to add the ability to append atoms,
;; resulting in a list. This is done by converting an atom into a list with one
;; element. I don't see a way to do this without 'list?', but we can use 'cons'
;; to turn an atom into a list instead of the 'list' procedure.
;;
;; Unhappy about using 'butlast' and 'last', I looked for other ways to get
;; things in the right sequence and found that 'reverse' is an R5R6 built-in.

(define (append-2-r lfirst lsecond)
  (cond ((empty? lfirst)           lsecond)
        (else (append-2-r (cdr lfirst) (cons (car lfirst) lsecond)))))

(define (append-2 first second)
  (let ((lfirst (if (list? first) first (cons first '())))
        (lsecond (if (list? second) second (cons second '()))))
    (append-2-r (reverse lfirst) lsecond)))

(check (append-2 'a 'b) => '(a b))
(check (append-2 '(a b) 'c) => '(a b c))
(check (append-2 '(a b) '(c d)) => '(a b c d))

;; Rather than merge 'append-2' into the new 'append' I decided to reuse it.
;; This gives me the ability to append lists and atoms. From there, this is a
;; car-cdr style recursion, but it needs to work from back to front. So, once
;; again, 'reverse'. I picked the name 'appendage' to avoid collision with the
;; built-in.
;;
;; As we are working through the original arguments in reverse, I've put the
;; accumulated result as the first argument.

(define (appendage-r tail rest)
  (cond ((empty? rest)          tail)
        (else (appendage-r (append-2 (car rest) tail) (cdr rest)))))

(define (appendage . args)             ;; appendage is an available name.
  (if (empty? args)
    '()
    (let ((rargs (reverse args)))
      (let ((tail (car rargs)) (rest (cdr rargs)))
        (appendage-r tail rest)))))

(check (appendage) => '())
(check (appendage '()) => '())
(check (appendage '(a b) '(c d) '(e f)) => '(a b c d e f))


;; --------------------------------------------------------
;; 17.7 This assignment is to use out 'append' to write 'sentence'. I've
;; already acomplished that by dealing with atoms in addition to lists in my
;; 'append'. So, for me, this becomes:

(define (sentenced . args) (apply appendage args))


;; --------------------------------------------------------
;; 17.8 Write 'member' (not 'member?').

(define (remember x xs)
  (cond ((empty? xs)         #f)
        ((equal? x (car xs)) cdr xs)
        (else (remember x (cdr xs)))))

(check (remember 'a '(b c d)) => (member 'a '(b c d)))
(check (remember 'a '(b c a d)) => (member 'a '(b c a d)))


;; --------------------------------------------------------
;; 17.9 Write 'list-ref'.

(define (listless-ref xs n)
  (cond ((= 0 n) (car xs))
        (else (listless-ref (cdr xs) (- n 1)))))

(check (listless-ref '(a b c) 0) => (list-ref '(a b c) 0))
(check (listless-ref '(a b c) 2) => (list-ref '(a b c) 2))


;; --------------------------------------------------------
;; 17.10 Write 'length'.

(define (lengthen-r n xs)
  (cond ((empty? xs)          n)
        (else (lengthen-r (+ n 1) (cdr xs)))))

(define (lengthen xs)
  (lengthen-r 0 xs))

(check (lengthen '()) => 0)
(check (lengthen '(a b c)) => 3)


;; --------------------------------------------------------
;; 17.11 Write 'before-in-list?' taking a list and two elements, and return #t
;; if the first element appears before the second element, and #f if neither
;; element appears in the list.
;;
;; Not a requirement, but I'll use my versions of helpers.

(define (before-in-list-mk1? xs x y)
  (let ((xm (member? x xs))
        (ym (member? y xs)))
    (if (not (and xm ym))
      #f
      (let ((xr (remember x xs))
            (yr (remember y xs)))
        (> (length xr) (length yr))))))

(check (before-in-list-mk1? '(a b c d) 'a 'f) => #f)
(check (before-in-list-mk1? '(a b c d) 'x 'd) => #f)
(check (before-in-list-mk1? '(a b c d) 'd 'c) => #f)
(check (before-in-list-mk1? '(a b c d) 'c 'd) => #t)

;; Of couse the authors would prefer that we do this in a smaller recursive
;; procedure. Even though the above works, it can make four separate passes
;; through the target list. To reduce that, here is a recursive solution.

(define (bil?-r xs x y currpos xpos)
  (cond ((empty? xs) #f)
        ((and (= xpos -1) (equal? (car xs) x)) (bil?-r (cdr xs) x y currpos currpos))
        ((equal? (car xs) y) (not (= -1 xpos)))
        (else (bil?-r (cdr xs) x y (+ currpos 1) xpos))))

(define (before-in-list? xs x y)
  (cond ((empty? xs)       #f)
        ((equal? x y)      #f)
        (else (bil?-r xs x y 0 -1))))

(check (before-in-list? '(a b c d) 'a 'f) => #f)
(check (before-in-list? '(a b c d) 'x 'd) => #f)
(check (before-in-list? '(a b c d) 'd 'c) => #f)
(check (before-in-list? '(a b c d) 'c 'd) => #t)


;; --------------------------------------------------------
;; 17.12 Write a procedure called flatten that takes as its argument a list,
;; possibly including sublists, but whose ultimate building blocks are words
;; (not Booleans or procedures). It should return a sentence containing all the
;; words of the list, in the order in which they appear in the original:

(define (flatten-r xs)
  (cond ((not (list? xs)) (appendage xs))
        ((empty? xs)      '()) ;; counts on appendage being smart enough
        (else (appendage  (flatten-r (car xs)) (flatten-r (cdr xs))))))

(define (flattener xs)
  (appendage (flatten-r xs)))

(check (flattener '(((a b) c (d e)) (f g) ((((h))) (i j) k))) =>
'(a b c d e f g h i j k))
(check (flattener '(x)) => '(x))
(check (flattener '(x y z)) => '(x y z))
(check (flattener '((((a))))) => '(a))


;; --------------------------------------------------------
;; 17.13 Here is a procedure that counts the number of words anywhere within a
;; structured list:

(define (deep-count lst)
  (cond ((null? lst) 0)
        ((word? (car lst)) (+ 1 (deep-count (cdr lst))))
        (else (+ (deep-count (car lst))
                 (deep-count (cdr lst))))))

;; Although this procedure works, it's too complicated. Simplify it.

;; First try is pretty feeble. DIfferent, but not appreciably better.

(define (deeper-count lst)
  (if (null? lst)
    0
    (+ (if (word? (car lst))
         1
         (deeper-count (car lst)))
       (deeper-count (cdr lst)))))

;; Coming at the problem cold I'd have done a flatten and count. But after some
;; reflection, I think this is the solution they wanted us to find:

(define (deepest-count lst)
  (reduce +
    (map (lambda (x) (if (word? x) 1 (deepest-count x))) lst)))


;; --------------------------------------------------------
;; 17.14 Write a procedure branch that takes as arguments a list of numbers and
;; a nested list structure. It should be the list-of-lists equivalent of item,
;; like this:
;;
;;       (branch '(3) '((a b) (c d) (e f) (g h))) => '(e f))
;;       (branch '(3 2) '((a b) (c d) (e f) (g h))) => 'f)
;;       (branch '(2 3 1 2) '((a b) ((c d) (e f) ((g h) (i j)) k) (l m))) => 'h)
;;
;; In the last example above, the second element of the list is:
;;
;;         '((c d) (e f) ((g h) (i j)) k)
;;
;; The third element of that smaller list is:
;;
;;         '((g h) (i j))
;;
;; The first element of that is:
;;
;;         '(g h)
;;
;; And the second element of that is just:
;;
;;         'h

;; As is typical, I made this too hard by visualizing the problem incorrectly.
;; We're plucking items out of nested lists, and we have 'list-ref' already
;; available at 17.9 as 'listless-ref'. I was hacking my way through something
;; based upon 'member'.

(define (branch nums struct)
  (if (empty? nums)
    struct
    (branch (cdr nums) (listless-ref struct (- (car nums) 1)))))

(check (branch '(1) '(a b c d)) => 'a)
(check (branch '(3) '((a b) (c d) (e f) (g h))) => '(e f))
(check (branch '(3 2) '((a b) (c d) (e f) (g h))) => 'f)
(check (branch '(2 3 1 2) '((a b) ((c d) (e f) ((g h) (i j)) k) (l m))) => 'h)


;; --------------------------------------------------------
;; 17.15 Modify the pattern matcher to represent the known-values database as a
;; list of two-element lists, as we suggested at the beginning of this chapter.
;;
;; The original structure is (name data ! name data !). An association list is
;; the natural solution for this.

(define (lookup name known-values)
  (let ((there (assoc name known-values)))
    (if there
      there
      'no-value)))

(define (add name value known-values)
  (if (empty? name)
      known-values
      (cons (list name value) known-values)))


;; --------------------------------------------------------
;; 17.16 Write a predicate valid-infix? that takes a list as argument and
;; returns #t if and only if the list is a legitimate infix arithmetic
;; expression (alternating operands and operators, with parentheses—that is,
;; sublists—allowed for grouping).
;;
;;       (valid-infix? '(4 + 3 * (5 - 2))) => #t
;;       (valid-infix? '(4 + 3 * (5 2))) => #f
;;
;; I'm not seeing this as needing much beyond 'flatten(er)'. We aren't asked to
;; actually evaluate the expression, so this isn't a parsing problem. Nor do
;; they mention leaing negation, so...

(define (operator? x) (member? x '(* + / -)))

(define (alternating? prior exp)
   (cond ((empty? exp)          (not prior))
         ((equal? (operator? (car exp)) (not prior)) (alternating? (operator? (car exp)) (cdr exp)))
         (else #f)))

(define (valid-infix? exp)
  (alternating? #t (flattener exp)))

(check (valid-infix? '(4 + 3 * (5 - 2))) => #t)
(check (valid-infix? '(4 + 3 * (5 2))) => #f)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 17 Lists end...")
