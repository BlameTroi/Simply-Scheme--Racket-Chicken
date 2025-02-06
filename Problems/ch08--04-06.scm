#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 8 High Order Functions


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

;;; Problem set:

(print "Chapter 08 problems 4, 5, and 6 start...")

;;; These are short enough to bundle up.


;; ----------------------------------------------
;; 8.4 Write procedure choose-beatles that takes a predicate function
;;     as its sole argument and returns a sentence of just those
;;     Beatles.

(define (choose-beatles fn)
  (keep fn '(john paul george ringo)))

(define (ch-j? wrd) (equal? (first wrd) 'j))

(check (choose-beatles ch-j?) => '(john))


;; ----------------------------------------------
;; 8.5 Write a procedure transform-beatles that takes a function,
;;     applies it to each of the beatles, and returns a sentence of
;;     the results.

(define (transform-beatles fn)
  (every fn '(john paul george ringo)))

(define (exclaim wrd) (word wrd '!))

(check (transform-beatles exclaim) => '(john! paul! george! ringo!))


;; ----------------------------------------------
;; 8.6 Write a procedure that takes a word and spells it out using
;;     the NATO phonetic alphabet.

(define phonetic-alphabet
  '(alfa bravo charlie delta echo foxtrot golf hotel india
         juliett kilo lima mike november oscar papa quebec romeo
         sierra tango uniform victor whiskey x-ray yankee zulu))

;; This reads best with helper functions.
(define (fn-find ltr)
  (lambda (wrd) (equal? ltr (first wrd))))
(define (for-letter ltr)
  (keep (fn-find ltr) phonetic-alphabet))
(define (phonetic wrd)
  (every for-letter (every se wrd)))

(check (for-letter 'o) => '(oscar))
(check (for-letter 'b) => '(bravo))
(check (phonetic 'kilo) => '(kilo india lima oscar))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 08 problems 4, 5, and 6 end...")
