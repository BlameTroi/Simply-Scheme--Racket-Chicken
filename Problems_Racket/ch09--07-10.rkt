#lang simply-scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 9 Lambda Land

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 09 problem 7, 8, 9, 10 start...")


;; ----------------------------------------------
;; 9.7 Write function letterwords that takes a letter and a sentence
;;     and returns a sentence with only the words containing the
;;     letter.

(define (letterwords ltr sent)
  (keep (lambda (wrd) (member? ltr wrd)) sent))

(check (letterwords 'o '(got to get you into my life))
       => '(got to you into))
(check (letterwords 't '(got to get you into my life))
       => '(got to get into))


;; ----------------------------------------------
;; 9.8 Write a display function for a game of hangman that takes a
;;     word and the letters guessed so far by the player. Display
;;     the word with blanks (underscores) in the positions not
;;     exposed by the player guesses.

(define (hangman-display solution guessed)
  (accumulate
   word
   (every
    (lambda (ltr) (if (member? ltr guessed) ltr '_))
    solution)))

(check (hangman-display 'potstickers '(a e i o u s))
  => '_o_s_i__e_s)


;; ----------------------------------------------
;; 9.9 Write function `common-words' that takes two sentences and
;;     returns a sentence containing only items common to both.
;;
;;     `member?' will get the job done. The check for list length
;;     isn't really needed, but it feels more efficient.
;;
;;     That probably means it isn't :)

(define (common-words xs ys)
  (let ((shorter (if (<= (count xs) (count ys)) xs ys))
        (longer (if (<= (count xs) (count ys)) ys xs)))
    (keep (lambda (x) (member? x longer)) shorter)))

(check (common-words '(a b c d) '(b d e g))   => '(b d))
(check (common-words '() '(a b c))            => '())
(check (common-words '(a b c d) '(e f g h i)) => '())
(check (common-words 'fred 'frederick)        => 'fred)


;; ----------------------------------------------
;; 9.10 Implement `appearances' from chapter two. How many times
;;      does the first argument appear in the second? This is a
;;      variation of the `character-count' problem from chapter 8.

(define (appearances-p x xs)
  (accumulate +
   (every (lambda (i) 1) (keep (lambda (i) (equal? x i)) xs))))

(check (appearances-p 'a 'abbracadabra)
       => (appearances 'a 'abbracadabra))
(check (appearances-p 'and '(it is a bit of this and a bit of that
                              and a dash of the other thing)) => 2)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 9 problem 7, 8, 9, 10 end...")
