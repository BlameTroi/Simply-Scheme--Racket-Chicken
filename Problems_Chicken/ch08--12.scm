;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 8 High Order Functions

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load srfi 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 8 problem 12 start...")


;; ----------------------------------------------
;; 8.12 Write procedure `count-ums' that counts the number of times 'um'
;; appears in a sentence.

(define (count-ums sent)
  (count (keep (lambda (wrd) (equal? wrd 'um)) sent)))

(check (count-ums '(well um uh um gee i dunno)) => 2)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 8 problem 12 end...")
