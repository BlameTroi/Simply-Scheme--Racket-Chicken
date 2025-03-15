;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 7 Variables

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load srfi 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 7 problems 1, 2, 3, 4 start...")


;; ----------------------------------------------
;; 7.1 Use `let' to remove redundancies from the following:
(define (vowel? c)
  (member? c '(a e i o u)))

(define (consonant? c)
  (and (not (vowel? c)) (not (number? c))))

(check (vowel? 'a) => #t)
(check (consonant? 'a) => #f)
(check (consonant? 3) => #f)
(check (consonant? 't) => #t)

(define (gertrude wd)
  (se (if (vowel? (first wd)) 'an 'a)
      wd
      'is
      (if (vowel? (first wd)) 'an 'a)
      wd
      'is
      (if (vowel? (first wd)) 'an 'a)
      wd))

(define (let-gertrude wd)
  (let ((article (if (vowel? (first wd)) 'an 'a))
        (wd-is (se wd 'is)))
    (let ((being-silly (se article wd-is)))
          (se being-silly being-silly article wd))))

(check (gertrude 'rose)       => '(a rose is a rose is a rose))
(check (gertrude 'iguana)     => '(an iguana is an iguana is an iguana))
(check (let-gertrude 'rose)   => (gertrude 'rose))
(check (let-gertrude 'iguana) => (gertrude 'iguana))


;; ----------------------------------------------
;; 7.2 Add missing parentheses.
;;
;; (let pi 3.14159
;;      pie 'lemon meringue
;;   se 'pi is pi 'but pie is pie)
;; expected result:
;; '(pi is 3.14159 but pie is lemon meringue)

(define (pipie)
  (let ((pi 3.14159)
        (pie '(lemon meringue)))
    (se '(pi is) pi '(but pie is) pie)))

(check (pipie) => '(pi is 3.14159 but pie is lemon meringue))


;; ----------------------------------------------
;; 7.3 This doesn't work as expected, fix it.

(define expected7.3 '(dumbest exercise))

(define (bad7.3 adjective word)
  (se (word adjective 'est) word))

;; variable and a function have the same name.

(define (superlative adj wd)
  (se (word adj 'est) wd))

(check (superlative 'dumb 'exercise) => expected7.3)


;; ----------------------------------------------
;; 7.4 What does this do? Explain why it works?

(define (sum-square a b)
  (let ((+ *)
        (* +))
    (* (+ a a) (+ b b))))

;; This takes advantage of the evaluation order within `let' to swap the
;; functions + and *. In a `let' all the values (right hand side of a
;; definition) are done before their values are bound to the corresponding
;; names (left hand side). This prevents side effects while computing values.
;;
;; This could not be done outside the `let' because the (+ *) would take effect
;; before (* +) is evaluated. Using the substition model, at the end of the
;; sequence, + would be *, and * would be ... *. Within the scope of the `let'
;; their meanings are exchanged.

;; Chapter 7 is more of a preface. On to chapter 8.


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 7 problems 1, 2, 3, 4 end...")
