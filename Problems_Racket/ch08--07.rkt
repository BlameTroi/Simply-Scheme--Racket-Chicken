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

(print "Chapter 8 problem 7 start...")


;; ----------------------------------------------
;; 8.7 Write a procedure letter-count that takes a sentence and returns the
;; number of letters in the sentece.

;; One is the loneliest number:
(define (count-1 ignored) 1)

;; Split a `word' into a `sentence' of non-blank characters:
(define (word->characters wrd) (every se wrd))

;; And all together now:
(define (character-count-x sent)
  (accumulate +
              (every count-1
                     (every word->characters sent))))

;; And now a cleaner version with some better selection options. The special
;; characters seem to parse differently between Emacs and Chicken w/breadline,
;; quite a mess, but this should demonstrate the idea.
;;
;; The period is expanded out of word->characters as "." and not ., which makes
;; sense as with a naked . it would be a botched pair syntax.

;; Character selection predicates:

(define (pred-letters c)
  (member? c
           '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(define (pred-digits c)
  (member? c
           '(0 1 2 3 4 5 6 7 8 9)))

(define (pred-special c)
  (member? c
        '( ! @ $ % ^ & * - = _ + : / < > , #\. ? #\; #\# #\\ #\( #\) #\{ #\} #\[ #\] #\< #\> )))

(define (pred-everything c)
  #t)

;; Count the characters requested, this does not capture separators between
;; words in the sentence. NOTE: `lambda' is the subject of the next chapters
;; but it showed up in some of the text of 7 and 8 so I felt free to use it.

(define (character-count pred sent)
  (let ((count-1 (lambda (c) (if (pred c) 1 0)))
        (word->characters (lambda (wrd) (every se wrd))))
    (accumulate + (every count-1 (every word->characters sent)))))

;; A test sentence:

(define test-sent '( this is a sentence with 1.2 special < > ! and digits 0123 ))

;; And verification:

(check (character-count pred-letters test-sent)    => 35)
(check (character-count pred-digits test-sent)     => 5)
(check (character-count pred-special test-sent)    => 3) ;; does not catch . in 1.2
(check (character-count pred-everything test-sent) => 44)
(check (character-count
        (lambda (c) (or (pred-special c) (pred-digits c)))
        test-sent) => 8)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 8 problem 7 end...")
