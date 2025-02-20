#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 8 High Order Functions

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 8 problem 3 start...")

;; ----------------------------------------------
;; 8.3 Describe each of the following functions in English. Include
;;     the domain and range of each.

;; This will lead into a discussion of the authors' recursion helpers.
;; I've left those notes in with 8.3.

(define (f a)
  (keep even? a))

;; The above is a selection of all the elements in `a' that are even
;; numbers. The domain of `a' is a sentence (list) of only numbers.
;; The range of `f' is all the even numbers in 'a'.

(define (g b)
  (every b '(blue jay way)))

(check (g first) => '(b j w))

;; The above returns the result of applying function `b' to each item
;; in the sentence '(blue jay way). The domain of `g' is any function
;; that accepts a single word argument. The range is a sentence of
;; the results of applying `b' to each item.

(define (h c d)
  (c (c d)))

;; The above mess defines function `h' as a function that takes two
;; arguments, both of which must be functions. The first function
;; argument `c' must be a function that accepts a single argument,
;; which itself must be a function that accepts a function as an
;; argument. Functions `h' and `c' both return a function that accepts
;; just one argument. The return from `h' is also a function that
;; accepts just one argument, which as this is written will be a
;; function that takes a single function argument and returns a
;; function that takes a single argument and ... so on, very
;; recursive.
;;
;; Better:
;;
;;            (define (d f1b) ...)
;;            (define (c f1a) ...)
;;            (define (h f1a f1b) ...)
;;
;; The domain of `h' is all single argument functions that take
;; functions as arguments and return like functions as arguments.

(define (i e)
  (/ (accumulate + e) (count e)))

;; At first glance, `i' is a function that takes a sentence/list of
;; numbers as an argument and returns the average of the values of
;; the list.
;;
;; Domain ... a non-empty list of numbers.
;; Range ... a single number.

;; accumulate

;; `accumulate' is a function that takes a two argument function and a
;; list of items that are in the domain of that two argument function.
;; it returns the result of applying the two argument function
;; pairwise through the list: (f (f first element, second element),
;; third element ...).
;;
;; Domain ... functions that two arguments of some 'thing' and return
;; something that can be used as an argument of the function.
;; Range ... Any one 'thing' produced by the function.

;; sqrt

;; Compute the square root of a number. Its domain is all numbers, as is
;; its range.

;; repeated

;; `repeated' is a function that takes as arguments a `function' and a
;; numeric `count' of times to apply that function to some `value'. It
;; returns a function that applies `function' to some value `count'
;; times in succession.
;;
;; Domain ... pretty much any function with a range compatible with
;;            its domain.
;; Range  ... A function with the same domain and range as its argument
;;            function.

;;(repeated sqrt 3)

;; A function that takes a number (domain of `sqrt') and applies
;; `sqrt' 3 times in succession. The functions domain and range are
;; the same as `sqrt'.

;;(repeated even? 2)

;; A function that I think won't work. The functions domain should be
;; numbers, and its range is a boolean. As a boolean lies outside the
;; domain of `even?', it errors.

;;(repeated first 2)

;; A function that takes a list (or some type that can be enumerated)
;; and returns the first of the first of the list/enumerable. I expect
;; that ((...) '(this is a test)) will return 't. While it might appear
;; that the each item in the list should also be enumerable, `first'
;; handles atoms rationally.
;;
;; Domain ... any enumerable type (list/sentence, word).
;; Range .... any item that could be in the domain.

;;(repeated (repeated bf 3) 2)

;;; and so ends the word problems ;;;


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 8 problem 3 end...")
