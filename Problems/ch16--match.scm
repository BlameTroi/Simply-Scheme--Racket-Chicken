#lang simply-scheme
;;; Simply Scheme
;;; Chapter 16 Example Pattern Matcher

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 16 Example Pattern Matcher start...")


;; Pattern matcher example from Simply Scheme Chapter 16. I am
;; unable to find a version of this that works. The match function
;; consistently returns '(), which is not #f in scheme. This is
;; returned for matches that should and shouldn't pass. I've done
;; a good bit of hunting this morning and given up. I'll have to
;; fix it as I work through the text.

;; I did some more experiments and named matches work, while
;; unnamed matches fail. This is regardless of the wild-card
;; in the pattern (*?!&). I'd wonder if this was a bug in the
;; simply-scheme language for Racket dealing with special
;; characters that have assigned procedures, but it also fails
;; in Chicken 5. Besides, only * is (re)defined in simply-scheme.

;; Checking this in as a base starting point so that I have
;; diff history of the debug and fix.


(define (match pattern sent)
  (match-using-known-values pattern sent '()))

(define (match-using-known-values pattern sent known-values)
  (cond ((empty? pattern)
	 (if (empty? sent) known-values 'failed))
	((special? (first pattern))
	 (let ((placeholder (first pattern)))
	   (match-special (first placeholder)
			  (bf placeholder)
			  (bf pattern)
			  sent
			  known-values)))
	((empty? sent) 'failed)
	((equal? (first pattern) (first sent))
	 (match-using-known-values (bf pattern) (bf sent) known-values))
	(else 'failed)))

(define (special? wd)
  (member? (first wd) '(* & ? !)))


(define (match-special howmany name pattern-rest sent known-values)
  (let ((old-value (lookup name known-values)))
    (cond ((not (equal? old-value 'no-value))
	   (if (length-ok? old-value howmany)
	       (already-known-match
		  old-value pattern-rest sent known-values)
	       'failed))
	  ((equal? howmany '?)
	   (longest-match name pattern-rest sent 0 #t known-values))
	  ((equal? howmany '!)
	   (longest-match name pattern-rest sent 1 #t known-values))
	  ((equal? howmany '*)
	   (longest-match name pattern-rest sent 0 #f known-values))
	  ((equal? howmany '&)
	   (longest-match name pattern-rest sent 1 #f known-values)))))

(define (length-ok? value howmany)
  (cond ((empty? value) (member? howmany '(? *)))
	((not (empty? (bf value))) (member? howmany '(* &)))
	(else #t)))

(define (already-known-match value pattern-rest sent known-values)
  (let ((unmatched (chop-leading-substring value sent)))
    (if (not (equal? unmatched 'failed))
	(match-using-known-values pattern-rest unmatched known-values)
	'failed)))

(define (chop-leading-substring value sent)
  (cond ((empty? value) sent)
	((empty? sent) 'failed)
	((equal? (first value) (first sent))
	 (chop-leading-substring (bf value) (bf sent)))
	(else 'failed)))

(define (longest-match name pattern-rest sent min max-one? known-values)
  (cond ((empty? sent)
	 (if (= min 0)
	     (match-using-known-values pattern-rest
				       sent
				       (add name '() known-values))
	     'failed))
	(max-one?
	 (lm-helper name pattern-rest (se (first sent))
		    (bf sent) min known-values))
	(else (lm-helper name pattern-rest
			 sent '() min known-values))))

(define (lm-helper name pattern-rest
		   sent-matched sent-unmatched min known-values)
  (if (< (length sent-matched) min)
      'failed
      (let ((tentative-result (match-using-known-values
			       pattern-rest
			       sent-unmatched
			       (add name sent-matched known-values))))
	(cond ((not (equal? tentative-result 'failed)) tentative-result)
	      ((empty? sent-matched) 'failed)
	      (else (lm-helper name
			       pattern-rest
			       (bl sent-matched)
			       (se (last sent-matched) sent-unmatched)
			       min
			       known-values))))))

;;; Known values database abstract data type

(define (lookup name known-values)
  (cond ((empty? known-values) 'no-value)
	((equal? (first known-values) name)
	 (get-value (bf known-values)))
	(else (lookup name (skip-value known-values)))))

(define (get-value stuff)
  (if (equal? (first stuff) '!)
      '()
      (se (first stuff) (get-value (bf stuff)))))

(define (skip-value stuff)
  (if (equal? (first stuff) '!)
      (bf stuff)
      (skip-value (bf stuff))))

(define (add name value known-values)
  (if (empty? name)
      known-values
      (se known-values name value '!)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

;; (trace match)
;; (trace special?)
;; (trace match-using-known-values)
;; (trace match-special)
;; (trace lookup)
;; (trace length-ok?)
;; (trace already-known-match)
;; (trace longest-match)

(check (special? (first '(* me *))) => #t)
(check (special? (first (bf '(* me *)))) => #f)
(check (match '(* me *) '(love me do)) => #t)






(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 16 Example Pattern Matcher end...")
