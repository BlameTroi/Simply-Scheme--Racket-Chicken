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

(print "Chapter 8 problem 13 start...")


;; ----------------------------------------------
;; 8.13 Write procedure 'phone-unspell' that takes a spelled out version of a
;; phone number and returns the correct phone number. POPCORN => 7672676.

;; Using the E.161 standard as it is in 2025.
;;
;; 1
;; 2 abc
;; 3 def
;; 4 ghi
;; 5 jkl
;; 6 mno
;; 7 pqrs
;; 8 tuv
;; 9 wxyz
;; 0

;; E.161 is the standard for letter-number mapping on phone keypads. This is
;; the US version.

(define E.161-MAP '( a2 b2 c2         ;; <spelled><unspelled>
                     d3 e3 f3
                     g4 h4 i4
                     j5 k5 l5
                     m6 n6 o6
                     p7 q7 r7 s7
                     t8 u8 v8
                     w9 x9 y9 z9
                     11 22 33     ;; these allow number and
                     44 55 66     ;; alpha to be mixed
                     77 88 99     ;; as in 1888buythis
                     00))

(define (xlate c)
  (keep
   (lambda (cn) (equal? c (first cn)))
   E.161-MAP))

(define (phone-unspell spell-phone)
  (accumulate word (every last (every xlate (if (sentence? spell-phone)
                               (word spell-phone)
                               spell-phone)))))

(check (phone-unspell 'popcorn) => '7672676)
(check (phone-unspell '888bigdogs) => '8882443647)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 8 problem 13 end...")
