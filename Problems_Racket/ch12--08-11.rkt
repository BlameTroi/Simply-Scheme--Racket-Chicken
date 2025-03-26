#lang simply-scheme
;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 12 The Leap of Faith

;; The #lang command loads the racket language definition for
;; the text. Then we just need SRFI-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 12 problems 8, 9, 10, 11 start...")


;; ----------------------------------------------
;; 12.8 Write `numbers' that takes a sentence and returns a sentence made up of
;; only the numbers in the sentence.

(define (numbers sent)
  (let ((number-or-nil (lambda (x) (if (number? x) x '()))))
    (cond ((empty? sent)                '())
          (else (se (number-or-nil (first sent)) (numbers (bf sent)))))))

(check (numbers '(76 trombones in the parade on 23rd street))
       => '(76))
(check (numbers '(two 4 six 8 10 twelve)) => '(4 8 10))


;; ----------------------------------------------
;; 12.9 Write `real-words' to filter noise words out of a sentence. Use the
;; definition of real words from chapter 1.

(define (real-word? wd) (not (member? wd '(a the an in of and for to with))))
(define (real-words sent)
  (let ((real-or-nil (lambda (x) (if (real-word? x) x '()))))
  (cond ((empty? sent)                 '())
        (else (se (real-or-nil (first sent)) (real-words (bf sent)))))))

(check (real-words '(this that and the other thing))
       => '(this that other thing))
(check (real-words '(the united nations command for law and enforcement))
       => '(united nations command law enforcement))


;; ----------------------------------------------
;; 12.10 Write `remove' that takes a word and sentence and returns the sentence
;; with all occurrences of word removed.

(define (remove wd sent)
  (let ((wd->nil (lambda (x) (if (equal? x wd) '() x))))
    (cond ((empty? sent)                '())
          (else (se (wd->nil (first sent)) (remove wd (bf sent)))))))

(check (remove 'if '(if you can you can if you cant you cant))
       => '(you can you can you cant you cant))
(check (remove 'and '(this and that and the other thing))
       => '(this that the other thing))


;; ----------------------------------------------
;; 12.11 Write `count' to return the number of words in a sentence or letters
;; in a word.
;;
;; This relies on first and bf working on either words or sentences correctly.
;; `my-count' as `count' is part of the standard environment.

(define (my-count xs)
  (cond ((empty? xs)            0)
        (else (+ 1 (my-count (bf xs))))))

(check (my-count 'word) => (count 'word))
(check (my-count 'fork) => 4)
(check (my-count '(this is a sentence)) => (count '(this is a sentence)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 12 problems 8, 9, 10, 11 end...")
