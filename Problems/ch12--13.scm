#lang simply-scheme
;;; Simply Scheme
;;; Chapter 12 The Leap of Faith

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 12 problem 13 start...")


;; ----------------------------------------------
;; 12.13 Rewrite chapter 6 `describe-time' to produce more human
;;       readable output.
;;
;; Convert an absolutely silly duration in seconds to a more
;; meaningful duration for human consumption. IE, bigger units,
;; smaller numbers.

(define sec/sec       1)
(define sec/min       60)
(define sec/hour      (* sec/min 60))
(define sec/day       (* sec/hour 24))
(define sec/week      (* sec/day 7))
;; (define sec/month     (* sec/day 30))  ;; approx
(define sec/year      (* sec/day 365.25))
(define sec/century   (* sec/year 100))
(define sec/millenium (* sec/century 10))

(define (describe-time sec)
  (let ((q quotient) (r remainder) (s sentence))
    (cond
     ((< sec sec/sec)                                           '())
     ((> sec sec/millenium)  (s (q sec sec/millenium) 'millenia  (describe-time (r sec sec/millenium))))
     ((> sec sec/century)    (s (q sec sec/century)   'centuries (describe-time (r sec sec/century))))
     ((> sec sec/year)       (s (q sec sec/year)      'years     (describe-time (r sec sec/year))))
;;     ((> sec sec/month)      (s (q sec sec/month)     'months    (describe-time (r sec sec/month))))
     ((> sec sec/week)       (s (q sec sec/week)      'weeks     (describe-time (r sec sec/week))))
     ((> sec sec/day)        (s (q sec sec/day)       'days      (describe-time (r sec sec/day))))
     ((> sec sec/hour)       (s (q sec sec/hour)      'hours     (describe-time (r sec sec/hour))))
     ((> sec sec/min)        (s (q sec sec/min)       'minutes   (describe-time (r sec sec/min))))
     (else                                                       (s sec 'seconds)))))

(describe-time 45)
(describe-time 75)
(describe-time 930)
(describe-time (+ 1 sec/week))
(describe-time (+ sec/day sec/week))
(describe-time (* 14.2 sec/day)) ;; some weeks
(describe-time 3000000000)

(check (describe-time 22222)
       => '(6 hours 10 minutes 22 seconds))
(check (describe-time 4967189641)
       => '(1 centuries 57 years 20 weeks 6 days 8 hours 54 minutes 1 seconds))

;; I had to drop months and add 1/4 dayto year to match their result,
;; but the code works.


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 12 problem 13 end...")
