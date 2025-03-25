;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 8 High Order Functions

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load SRFI 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 8 problems 4, 5, and 6 start...")


;; ----------------------------------------------
;; 8.4 Write procedure choose-beatles that takes a predicate function as its
;; sole argument and returns a sentence of just those Beatles.

(define (choose-beatles fn)
  (keep fn '(john paul george ringo)))

(define (ch-j? wrd) (equal? (first wrd) 'j))

(check (choose-beatles ch-j?) => '(john))


;; ----------------------------------------------
;; 8.5 Write a procedure transform-beatles that takes a function, applies it to
;; each of the Beatles, and returns a sentence of the results.

(define (transform-beatles fn)
  (every fn '(john paul george ringo)))

(define (exclaim wrd) (word wrd '!))

(check (transform-beatles exclaim) => '(john! paul! george! ringo!))


;; ----------------------------------------------
;; 8.6 Write a procedure that takes a word and spells it out using the NATO
;; phonetic alphabet.

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

(print "Chapter 8 problems 4, 5, and 6 end...")
