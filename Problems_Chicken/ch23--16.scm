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

;; Some rules:
;;
;; A sentence is a vector holding zero or more words.
;; A word is pretty much what the standard environment thinks one is.
;; A list may be passed to the sentence constructor.
;; A list may contain a nested sentence.
;; A sentence may not contain a nested sentence.
;; A sentence may be empty (zero length).

;; Predicates and queries:

(define (sentence? thing)
  (vector? thing))

(check (sentence? 'a) => #f)
(check (sentence? '(a b c)) => #f)
(check (sentence? #(a b c)) => #t)


(define (sentence-length thing)
  (if (sentence? thing)
    (vector-length thing)
    'error))

(check (sentence-length (list 1 2 3)) => 'error)
(check (sentence-length 'word) => 'error)
(check (sentence-length #(a b c d)) => 4)
(check (sentence-length #()) => 0)


(define (word? thing)
  (and (not (sentence? thing)) (not (list? thing))))

(check (word? 'thing) => #t)
(check (word? #(a b c)) => #f)
(check (word? '(a b c)) => #f)
(check (word? 3.14159) => #t)


(define (empty? thing)
  (if (sentence? thing)
    (= (sentence-length thing) 0)
    (if (list? thing)
      (= (count thing) 0)
      (equal? thing '()))))

(check (empty? #()) => #t)
(check (empty? '()) => #t)
(check (empty? 'word) => #f)
(check (empty? #(a b)) => #f)
(check (empty? '(c d)) => #f)


;; Some accessors are needed before we define the constructors:

(define (first sent)
  (cond ((sentence? sent) (vector-ref sent 0))
        ((list? sent) (car sent))
        (else 'error)))

(check (first #(a b c)) => 'a)
(check (first '(a b c)) => 'a)
(check (first 'abc) => 'error)      ;; we've had to break word support

(define (last sent)
  (cond ((sentence? sent) (vector-ref sent (- (vector-length sent) 1)))
        (else 'error)))

(check (last #(a b c)) => 'c)
(check (last '(a b c)) => 'error)      ;; not doing last for lists atm
(check (last 'abc) => 'error)          ;; and as before, word support broken

(define (bf sent)
  (if (empty? sent)
    #()
    (bf-r 1 sent (make-vector (- (vector-length sent) 1)))))

(define butfirst bf)

(define (bf-r idx sent1 sent2)
  (let ((my-vec-set! (lambda (v s a) (vector-set! v s a) v)))
    (if (= idx (vector-length sent1))
      sent2
      (bf-r (+ idx 1)
            sent1
            (my-vec-set! sent2
                         (- idx 1)
                         (vector-ref sent1 idx))))))

(check (bf #(a b c)) => #(b c))
(check (bf #(a)) => #())
(check (bf #()) => #())

;; Create and combine:

;; A sentence constructor can include other sentences. This leads to a mix
;; of lists (for the variable arity arguments) that can hold sentences
;; (vectors). Flatten the list in case the user has mixed in "old style"
;; sentences along with the vector variety. Then move each "word" into
;; a new vector large enough to hold the words.
;;
;; (se 'a 'b #(c d) '(e f #(g h))) becomes
;; (se 'a 'b #(c d) 'e 'f #(g h)) becomes
;; #(a b c d e f g h)
;;
;; Sentences (list or vector variety) are not really allowed hold a nested
;; sentence.

(define se
  (lambda parts
    (let* ((flat (flatten parts))
           (needed (words-and-sentences-in flat))
           (alloced (make-vector needed '())))
      (se-r alloced 0 flat))))

(define sentence se)

(define (words-and-sentences-in parts)
  (w-a-s-i-r 0 parts))

(define (w-a-s-i-r count xs)
  (cond ((empty? xs)
         count)
        ((vector? (car xs))
         (w-a-s-i-r (+ count (vector-length (car xs))) (cdr xs)))
        (else (w-a-s-i-r (+ count 1) (cdr xs)))))

(define (se-r vec slot words)
  (let ((my-vec-set! (lambda (v s a) (vector-set! v s a) v)))
        (cond ((empty? words)
               vec)
              ((not (sentence? (car words)))
               (begin (show vec) (show slot) (show (car words)))
               (se-r
                 (my-vec-set! vec slot (car words)) ;; returns vec
                 (+ slot 1)
                 (cdr words)))
              ;; from here we know we have a vector to empty into the sentence
              ((empty? (car words))          ;; we've emptied the vector
               (se-r vec slot (cdr words)))
              (else    ;; take word from front of vector
                (se-r
                  (my-vec-set! vec slot (first (car words)))
                  (+ slot 1)
                  (cons (bf (car words)) (cdr words)))))))

;; Query and extract:

;; still to do

(define (bl sent) )

(define butlast bl)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 23 Files end 16...")
