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

(print "Chapter 8 problem 14 start...")


;; ----------------------------------------------
;; 8.14 Write procedure `subword' that takes a word, a start, and and end
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
