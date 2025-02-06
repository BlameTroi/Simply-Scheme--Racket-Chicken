#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 11 Introduction to Recursion


;; The problems I worked with lightweight testing to verify results
;; via srfi-78. This file should load into a new Scheme repl (only
;; Chicken tested) and report no failures.

;;; Set up the standard environment:

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)

;;; Set unit testing reporting levels and clear any dangling
;;; totals.

;; This should already be done, but just in case:

(check-reset!)
(check-set-mode! 'report-failed)


;;; Chapter 10 was a code walkthrough. Chapter 11
;;; is rather small and all the problems fit in one
;;; file.


;;; Scratch pad:

(define (explode wd)
  (if (= (count wd) 1)
      wd
      (se (first wd) (explode (bf wd)))))

;; The authors used `first-two' from chapter 5.

(define (letter-pairs wd)
  (if (< (count wd) 3)
      wd
      (se (word (first wd) (first (bf wd))) (letter-pairs (bf wd)))))


;;; Problem set:

(print "Chater 11 problems 1, 2, 3, 4, 5, 6, 7 start...")

;; ----------------------------------------------
;; 11.1 Write `downup4' in terms of only `word' and `sentence'.
;;      I must be dense, I don't see it. I need some way to break
;;      a word into letters. A quick search shows others considered
;;      `butlast' and 'first' acceptable in this problem. If the
;;      authors meant "without downup3 2 1 implementations" then
;;      that makes sense.

(define (downup4 wd)
  (sentence
   wd
   (word (first wd) (first (bf wd)) (first (bf (bf wd))))
   (word (first wd) (first (bf wd)))
   (first wd)
   (word (first wd) (first (bf wd)))
   (word (first wd) (first (bf wd)) (first (bf (bf wd))))
   wd
   ) ;; sentence
  ) ;; define

(check (downup4 'fred) => '(fred fre fr f fr fre fred))


;; 11.2 Rewrite `count-ums' from 8.12 using recursion.

(define (ch8-count-ums sent)
  (count (keep (lambda (wrd) (equal? wrd 'um)) sent)))

(define (count-ums sent)
  (if (= (count sent) 0)
      0
      (+ (count-ums (bf sent))
         (if (equal? (first sent) 'um)
             1
             0))))

(check (ch8-count-ums '(well um let us see um if um it is true)) => 3)
(check (count-ums '(well um let us see um if um it is true)) => 3)


;; ----------------------------------------------
;; 11.3 Rewrite `phone-unspell' from 8.13 using recursion.

;; helpers and the main procedure:
;; E.161 is the standard for letter-number mapping on phone
;; keypads. This is the US version.
(define E.161-MAP '( a2 b2 c2         ;; <spelled><unspelled>
                     d3 e3 f3
                     g4 h4 i4
                     j5 k5 l5
                     m6 n6 o6
                     p7 q7 r7 s7
                     t8 u8 v8
                     w9 x9 y9 z9
                     11 22 33
                     44 55 66
                     77 88 99
                     00))
(define (xlate c)
  (keep
   (lambda (cn) (equal? c (first cn)))
   E.161-MAP))

(define (ch8-phone-unspell spell-phone)
  (accumulate word (every last (every xlate (if (sentence? spell-phone)
                               (word spell-phone)
                               spell-phone)))))

(define (phone-unspell spell-phone)
  (if (= (count spell-phone) 0)
      ""    ;; was '() but `word' doesn't like that.
      (word  (last (first (xlate (first spell-phone))))
             (phone-unspell (bf spell-phone)))))

(check (ch8-phone-unspell 'popcorn) => '7672676)
(check (ch8-phone-unspell '888bigdogs) => '8882443647)
(check (phone-unspell 'popcorn) => '7672676)
(check (phone-unspell '888bigdogs) => '8882443647)


;; ----------------------------------------------
;; 11.4 I have no idea. That's a section header in the book.


;; ----------------------------------------------
;; 11.5 Write a procedure `initials' that takes a sentence as its
;;      argument and returns a sentence of the first letters in each
;;      of the sentence's words.

(define (initials sent)
  (if (= (count sent) 0)
      '()
      (se (first (first sent)) (initials (butfirst sent)))))

(check (initials '(if i need someone)) => '(i i n s))


;; ----------------------------------------------
;; 11.6 Write procedure `countdown' that works like this (passes the
;;      check below):

(define (countdown n)
  (if (= n 0)
      'blastoff!
      (se n (countdown (- n 1)))))

(check (countdown 10) => '(10 9 8 7 6 5 4 3 2 1 blastoff!))


;; ----------------------------------------------
;; 11.7 Write procedure `copies' that returns 'n' copies of
;;      a 'word'.

(define (copies n w)
  (if (or (not (number? n)) (= n 0))
      '()
      (se w (copies (- n 1) w))))

(check (copies 5 'boink) => '(boink boink boink boink boink))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chater 11 problems 1, 2, 3, 4, 5, 6, 7 end...")
