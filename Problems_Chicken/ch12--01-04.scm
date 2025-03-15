;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 12 The Leap of Faith

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load srfi 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 12 problems 1, 2, 3, 4 start...")


;; ----------------------------------------------
;; 12.1 This works but can be simplified. Do so.

(define (addup nums)
  (if (empty? (bf nums))
      (first nums)
      (+ (first nums) (addup (bf nums)))))

(define (addup-2 nums)
  (if (empty? nums)
      0
      (+ (first nums) (addup (bf nums)))))

(check (addup '(1 2 3 4)) => 10)
(check (addup-2 '(1 2 3 4)) => 10)


;; ----------------------------------------------
;; 12.2 Fix the following:

(define (bad-acronym sent)
  (if (- (count sent) 1)     ;; this will always be true, so
      (first sent)           ;; this clause executes once
      (word (first (first sent)) (bad-acronym (bf sent)))))

(check (bad-acronym '(this is a test)) => 'this)

(define (less-bad-acronym sent)
  (if (= (count sent) 1)    ;; this iis probably what the original
      (first (first sent))  ;; intended, but i prefer an empty base case
      (word (first (first sent)) (less-bad-acronym (bf sent)))))

(check (less-bad-acronym '(this is a test)) => 'tiat)

(define (acronym sent)
  (if (empty? sent)
      ""
      (word (first (first sent)) (acronym (bf sent)))))

(check (acronym '(this is a test)) => 'tiat)


;; ----------------------------------------------
;; 12.3 Can `factorial' have its base case reduced to -1 instead of 0? If not,
;; why? If so, show it:

(define (factorial n)              ;; original
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; It can not. While it can always return 1 for the base case, but if the base
;; case is -1, the step before will multiply by 0.


;; ----------------------------------------------
;; 12.4 Given this definition of a function f:
;;
;;            sent             if sent is empty
;; f(sent) = {
;;            sentence(f(butfirst(sent)),first(sent))
;;
;; Implement in Scheme. What does the function do? This looks to reverse the
;; words in a sentence.

(define (f sent)
  (if (empty? sent)
      sent
      (sentence (f(bf sent)) (first sent))))

;; Yep, that's what it does.


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 12 problems 1, 2, 3, 4 end ...")
