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

(print "Chapter 8 problem 14 start...")


;; ----------------------------------------------
;; 8.14 Write procedure `subword' that takes a word, a start, and end
;; position in the word, and returns the word made up from those characters.
;; (subword 'polythene' 5 8) => 'then.
;;
;; This is a job for `repeated'. Proof of concept: `((repeated bl 3) ((repeated
;; bf 3) '1234567890))' => '4567
;;
;; (define wrd 'polythene)
;; (count wrd) => 9
;; ((repeated bl 1) ((repeated bf 4) wrd)) => then

;; This has no error checking.

(define (subword b e wrd)
  (let ((rf (- b 1)) (rl (- (count wrd) e)))
    ((repeated bl rl) ((repeated bf rf) wrd))))

(check (subword 5 8 'polythene) => 'then)
(check (subword 6 8 'superman) => 'man)
(check (subword 2 11 'asteroidial) => 'steroidial)
(check (subword 2 8 'asteroidial) => 'steroid)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 8 problem 14 end...")
