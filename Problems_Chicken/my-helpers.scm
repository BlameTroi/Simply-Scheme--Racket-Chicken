;;; Establish an environemnt for the exercises for _Simply_Scheme_.

;; The authors provide their own extensions in "simply.scm". I then
;; add srfi-78 Lightweight Testing. Finally, as I come up with helpers
;; that might be useful later in the text, I pull them into this
;; loadable file.
;;
;; These are generally short predicates or converters and they do no
;; error checking.
;;
;; This file should load into a new Scheme repl and report no
;; failures. I have only tested against Chicken.

;;; Set up the standard environment:

;; For this problem set that's "simply.scm" and "srfi-78".
;; which will be loaded separately with this file.

;; (load "simply.scm")
;; (import srfi-78)

(check-set-mode! 'summary)

;;; Helpers that I might carry forward to other chapters.
;;; Assumes simply.scm and srfi-78.

;; Short reusable predicates and converters. These do no real error
;; checking.

(define (vowel? c)
  (member? c '(a e i o u)))

(define (consonant? c)
  (and (not (vowel? c)) (not (number? c))))

(check (vowel? 'a) => #t)
(check (consonant? 'a) => #f)
(check (consonant? 3) => #f)
(check (consonant? 't) => #t)

(define (divisible? dividend divisor)
  (= 0 (remainder dividend divisor)))

(check (divisible? 7 7) => #t)
(check (divisible? 7 6) => #f)

;; `second' is in "srfi-1", but not in standard Scheme. It is easy
;; enough to create in this context:

(define (second xs) (first (butfirst xs)))

(check (second 'asdf) => 's)
(check (second 1234) => 2)
(check (second '(this is a sentence)) => 'is)

(define (third xs) (first (butfirst (butfirst xs))))
(define (fourth xs) (first (butfirst (butfirst (butfirst xs)))))
(check (third 'asdf) => 'd)
(check (fourth 'asdf) => 'f)

;; Would these be generally useful? Half open can be at either end
;; in mathematics, but in programming it usually means open on the
;; right. [0 ... x) represents the interval of indices of subscripts
;; [1 ... x]. IE., subscripts 1-10 are indices 0-9.
;;
;; (a ... b) is opened                (define (in-opened? a b x) ...)
;; [a ... b] is closed                (define (in-closed? a b x) ...)
;; [a ... b) is left closed, right open        in-right-open
;; (a ... b] is left open, right closed        in-right-closed
;;
;; We don't have real state in Scheme explained yet, and I believe
;; it is avoided completley in this book, so I'm not sure how to
;; create a reusable "thing" here.
;;
;; What I want is to define an interval of one of the four types
;; with a and b provided. In OOP this would be an object. Then I
;; could interogate the interval. Is x in the interval?
;;
;; For now, I'll specify a and b at interogation.

(define (in-open?   a b x)       (and (<= a x) (>= b x)))
(define (in-closed? a b x)       (and (< a x) (> b x)))
(define (in-right-open? a b x)   (and (< a x) (>= b x)))
(define (in-right-closed? a b x) (and (<= a x) (> b x)))

(check (in-open? 0 10 3) => #t)
(check (in-open? 0 10 0) => #t)
(check (in-open? 0 10 10) => #t)
(check (in-open? 0 10 11) => #f)

(check (in-closed? 0 10 1) => #t)
(check (in-closed? 0 10 0) => #f)
(check (in-closed? 0 10 10) => #f)
(check (in-closed? 0 10 0) => #f)
(check (in-closed? 0 10 9) => #t)

(check (in-right-open? 0 10 0) => #f)
(check (in-right-open? 0 10 1) => #t)
(check (in-right-open? 0 10 10) => #t)
(check (in-right-open? 0 10 9) => #t)
(check (in-right-open? 0 10 11) => #f)

(check (in-right-closed? 0 10 10) => #f)
(check (in-right-closed? 0 10 9) => #t)
(check (in-right-closed? 3 5 3) => #t)


;; Return the type of an object, as defined in "simply.scm". Note that
;; the "simply.scm" environment thinks '() is a sentence, I disagree.
;; I also debate if a single word sentence is a sentence or a word,
;; but I'll accept their definition.

(define (type-of x)
  (cond ((and (word? x) (not (number? x)))             'word)
        ((number? x)                                 'number)
        ((and (sentence? x) (not (= (count x) 0))) 'sentence)
        ((equal? #t x)                              'boolean)
        ((equal? #f x)                              'boolean)
        (else                                       'unknown)))

(check (type-of 'fred)          => 'word)
(check (type-of 13)             => 'number)
(check (type-of "7")            => 'number)
(check (type-of '7)             => 'number)
(check (type-of '(nowhere man)) => 'sentence)
(check (type-of (= 3 3))        => 'boolean)
(check (type-of (= 3 4))        => 'boolean)
(check (type-of '())            => 'unknown)


;; Sometimes I want a rounded result. And I almost never want a rational
;; number expressed as a fraction. This has no error checking but will
;; return the value of `orignal' with `places' digits after the decimal.

(define (precision places original)
  (let ((shifter (expt 10 (truncate places))))
        (/ (floor (+ (* original shifter) 0.5)) shifter)))

(check (precision 2 3.14159265359) => 3.14)
(check (precision 3 3.14159265359) => 3.142)
(check (precision 4 3.14159265359) => 3.1416)
(check (precision 5 3.14159265359) => 3.14159)


;; When you are working with groups of n from a sentence.

(define (next-n n xs)
  (cond ((= n 0) (se ))
        (else (se (first xs) (next-n (- n 1) (butfirst xs))))))

(check (next-n 4 '(1 2 3 4 5 6 7 8)) => '(1 2 3 4))

(define (tail-after-n n xs)
  (cond ((= 0 n) xs)
        (else (tail-after-n (- n 1) (butfirst xs)))))

(check (tail-after-n 4 '(1 2 3 4 5 6 7 8)) => '(5 6 7 8))


;; When you have marker/sentinal/delimiter values in sentence.

(define (next-up-to x xs)
  (cond ((empty? xs) (se ))
        ((equal? (first xs) x) (se ))
        (else (se (first xs) (next-up-to x (butfirst xs))))))

(check (next-up-to 0 '(1 2 3 4 0 5 6 7 8 0)) => '(1 2 3 4))

(define (tail-after x xs)
  (cond ((empty? xs) (se ))
        ((equal? (first xs) x)  (se (butfirst xs)))
        (else (tail-after x (butfirst xs)))))

(check (tail-after 0 '(1 2 3 4 0 5 6 7 8 0)) => '(5 6 7 8 0))


;; I fat-finger these enough to justify investigating how to do the
;; following. I know a leading sign on digits is seen as part of the
;; number, but if it shows up in the function name position, I
;; probably meant 'sign<space>number'. As written these don't work due
;; to number of expected arguments. It's not clear how to make this
;; work, so I'll just leave this as a note for something to look for
;; later.

;; (define (+1 xs) (+ 1 . xs))
;; (define (-1 xs) (- 1 . xs))
;; (define (*1 xs) (* 1 . xs))
;; (define (/1 xs) (/ 1 . xs))
;; (define (=0 xs) (= 0 . xs))
;; (define (=1 xs) (= 1 . xs))


;; Check for errors and clear for next file.
(check-report)
(check-reset!)
(check-set-mode! 'report-failed)
