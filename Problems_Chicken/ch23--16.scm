;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 23 Vectors.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load SRFI 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 23 Files begin 16...")

;; NOTE: Do not solve any of the following exercises by converting a vector
;; to a list, using list procedures, and then converting the result back to
;; a vector.

;; I decided that if the data comes in as a list, I would use the list
;; procedures if they make sense. The specific case I ran into was that
;; the way to write 'se' is (simplified) to '(lambda args (list->vector
;; args)'. This isn't quite sufficient as a sentence could be in the args,
;; but Scheme presents the full set of arguments as a list. I will work
;; to maintain the spirit, if not the letter, of their constraint.

;; NOTE: Some of these may need the solutions to prior problems in this
;; chapter. Testing should be done in chapter-problem sequence.

;; ----------------------------------------------
;; 23.16 We want to reimplement sentences as vectors instead of lists.
;;
;; (a) Write versions of 'sentence', 'empty?', 'first', 'butfirst', 'last,
;; and 'butlast' that use vectors. Your selectors need only work for
;; sentences, not for words.
;;
;; (sentence 'a 'b 'c) => #(A B C)
;; (butfirst (sentence 'a 'b 'c)) => #(B C)
;;
;; (You don't have to make these procedures work on lists as well as vectors!)
;;
;; (b) Does the following program still work with the new implementation of
;; sentences? If not, fix the program.
;;
;; (define (praise stuff)
;;   (sentence stuff '(is good)))
;;
;; (c) Does the following program still work with the new implementation of
;; sentences? If not, fix the program.
;;
;; (define (praise stuff)
;;   (sentence stuff 'rules!))
;;
;; (d) Does the following program still work with the new implementation of
;; sentences? If not, fix the program. If so, is there some optional
;; rewriting that would improve its performance?
;;
;; (define (item n sent)
;;   (if (= n 1)
;;       (first sent)
;;       (item (- n 1) (butfirst sent))))
;;
;; (e) Does the following program still work with the new implementation of
;; sentences? If not, fix the program. If so, is there some optional
;; rewriting that would improve its performance?
;;
;; (define (every fn sent)
;;   (if (empty? sent)
;;     sent
;;     (sentence (fn (first sent))
;;               (every fn (butfirst sent)))))
;;
;; (f) In what ways does using vectors to implement sentences affect the
;; speed of the selectors and constructor? Why do you think we chose to use
;; lists?

;; (a) Write versions of 'sentence', 'empty?', 'first', 'butfirst', 'last,
;; and 'butlast' that use vectors. Your selectors need only work for
;; sentences, not for words.

;; This is definitely *not* robust. I decided to reuse my copy-into-vector
;; from 23.3 to help with butfirst and butlast:

;; NOTE: No attempt to stay compatible with the standard environment is
;; made. You almost certainly want to restart your Scheme after you are
;; done with this code:

;; Utilities from other problems:

(define (copy-into-vector! v1 start len v2)
  (copy-into-vector!-r v1 start len 0 v2))

(define (copy-into-vector!-r v1 start len curr v2)
  (cond ((= len curr) v1)
        (else
          (vector-set! v1 (+ start curr) (vector-ref v2 curr))
          (copy-into-vector!-r v1 start len (+ curr 1) v2))))

;; Predicates:

(define (sentence? x) (vector? x))

(define (word? thing)
  (cond ((sentence? thing) #f)
        ((list? thing) #f)
        (else #t)))

(define (empty? thing)
  (cond ((sentence? thing) (= (vector-ref thing 0) 0))
        (else (equal? thing '()))))

;; Create and combine:

(define se (lambda parts (list->vector parts)))
;; The parts comes in as a list this way. Flatten and then scan for embedded
;; vectors, expand those to lists, then re squeeze.

;; Query and extract:

(define (first sent)
  (cond ((sentence? sent) (vector-ref sent 0))
        ((list? sent) (car sent))
        (else 'error)))

(define (last sent)
  (cond ((sentence? sent) (vector-ref sent (- (vector-length sent) 1)))
        (else 'error)))

;; still to do

(define (bf sent) )

(define (bl sent) )



;; synonyms

(define sentence se)
(define butfirst bf)
(define butlast bl)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 23 Files end 16...")
