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

(print "Chapter 9 problem 15, 16, 17 start...")


;; ----------------------------------------------
;; 9.15 Write a function `type-check' that takes a function and a predicate
;; checks an argument for correct type. Return a function that wraps the call
;; to `function' and only calls it if an argument satisfies `predicate'. Return
;; #f if the type is invalid.

(define (type-check func pred)
  (lambda (arg)
    (if (pred arg)
        (func arg)
        #f)))

(define safe-sqrt (type-check sqrt number?))
(check (safe-sqrt 16) => 4)
(check (safe-sqrt 'boink) => #f)


;; ----------------------------------------------
;; 9.16 Write a function that creates a wrapper around a traditional single
;; argument function (eg, sqrt) and give it the APLish ability to work with
;; either a single number argument or a list (sentence) of number arguments and
;; return a list of the results of the application.

(define (aplize fn) ;; do it here
  (lambda (arg) (if (sentence? arg)
                    (every fn arg)
                    (fn arg))))

(define apl-sqrt (aplize sqrt))

(check (apl-sqrt 36) => 6)
(check (apl-sqrt '(36 9 4)) => '(6 3 2))


;; ----------------------------------------------
;; 9.17 Write `keep' in terms of `every' and `accumulate'.

(define (rekeep pred xs)
  (accumulate se (every (lambda (x) (if (pred x) x '())) xs)))

(check (rekeep number? '(a b c 3 4 8 d 2)) => '(3 4 8 2))
(check (rekeep number? '(3 a 2 b 1 c)) => '(3 2 1))
(check (rekeep number? '(a b c)) => '())


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 9 problem 15, 16, 17 end...")
