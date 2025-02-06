#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 9 Lambda Land


;; The problems I worked with lightweight testing to verify results
;; via srfi-78. This file should load into a new Scheme repl (only
;; Chicken tested) and report no failures.

;;; Set up the standard environment:

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)

;;; Set unit testing reporting levels and clear any dangling
;;; totals.

;; This should already be done, but just in case:

(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 09 problem 11, 12, 13, 14 start...")


;; ----------------------------------------------
;; 9.11 Write `unabbrev' which takes two arguments. The first is a
;;      sentence to un-abbreviate, and the second is a sentence with
;;      replacements for any numbers found in the first.
;;
;;      The number 1 gets the first word from the second sentence, and
;;      number 5 gets the fifth, and so on.
;;
;;      `item' is defined in the standard environment but I did my
;;      own implementation.

(define (unabbrev target replacements)
  (let ((get-item (lambda (n) (first ((repeated bf (- n 1)) replacements)))))
    (let ((replacer (lambda (x) (if (number? x) (get-item x) x))))
      (every replacer target))))

(check (unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey))
       => '(john bill wayne fred joey))
(check (unabbrev '(i 3 4 tell 2) '(do you want to know a secret?))
       => '(i want to tell you))


;; ----------------------------------------------
;; 9.12 Write `first-last' which takes a sentance and returns only
;;      the words with the same first and last letter.

(define (first-last sent)
  (keep (lambda (wrd) (equal? (first wrd) (last wrd))) sent))

(check (first-last '(indiana ohio dad mom brother sister aunt kick))
       => '(ohio dad mom kick))


;; ----------------------------------------------
;; 9.13 Write a procedure `compose' which takes two functions, f and
;;      g, and returns a function composed of f and g and computes
;;      f(g(x)) when passed argument x.
;;
;;      Note: `compose' is also defined in the Chicken base with this
;;      general functionality, but it supports more than two argument
;;      functions. To avoid collision, I'm renaming this.

(define (compositor f g) (lambda (x) (f (g x))))

(check ((compositor sqrt abs) -25) => 5)
(define second (compositor first bf))
(check (second '(higher order function)) => 'order)


;; ----------------------------------------------
;; 9.14 Write `substitute' taking three parameters, new-word,
;;      old-word, and target-sent. Replace every occurance of old-word
;;      with new-word. I'd prefer a different ordering, but meh. It's
;;      a variation the above, with `letterwords' being a good
;;      template.

(define (substitute new-word old-word sent)
  (every
   (lambda (wrd)
     (if (equal? old-word wrd)
         new-word
         wrd))
   sent))

(check (substitute 'maybe 'yeah '(she loves you yeah yeah yeah))
       => '(she loves you maybe maybe maybe))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 09 problem 11, 12, 13, 14 start...")
