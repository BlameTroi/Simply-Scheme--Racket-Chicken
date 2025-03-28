;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 9 Lambda Land

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load SRFI 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 9 problem 4, 5, 6 start...")


;; ----------------------------------------------
;; 9.4 The following doesn't work. Fix it.

;;  NOTE: This will chatter in the log about top level variables being
;;  referenced but unbound, 'sent' in 'describe'. That's ok for this exercise.
;;
;; In Chicken 5 the error reports and the run can continue. In Racket
;; evaluation stops, so you'll need to comment the bad definitions out.

;; bad
;;(define (who sent)
  ;;(every describe '(peter roger john keith)))
;; bad
;;(define (describe person)
  ;;(se person sent))

;; fixed
(define (fixed-who sent)
  (every (lambda (person) (se person sent)) '(peter roger john keith)))

(define expected-who '(peter sells out roger sells out john sells out keith sells out))

(check (fixed-who '(sells out)) => expected-who)


;; ----------------------------------------------
;; 9.5 Write `prepend-every:

(define (prepend-every pre suf)
    (every (lambda (y) (word pre y)) suf))

(check (prepend-every 's '(he aid he aid))
       => '(she said she said))
(check (prepend-every 'anti '(dote pasto gone body))
       => '(antidote antipasto antigone antibody))


;; ----------------------------------------------
;; 9.6 Write `sentence-version' which takes a function F and returns a function
;; G. G should take a sentence as its argument and apply F to every item in the
;; sentence.

(define (sentence-version f)
  (lambda (sent) (every f sent)))

(check ((sentence-version first) '(if i fell))
       => '( i i f))

(check ((sentence-version (lambda (x) (* x x))) '(8 2 4 6))
       => '(64 4 16 36))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 9 problem 4, 5, 6 end...")
