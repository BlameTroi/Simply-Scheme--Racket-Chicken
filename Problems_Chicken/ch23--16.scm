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

;; NOTE: No attempt to stay compatible with the standard environment is
;; made. You almost certainly want to restart your Scheme after you are
;; done with this code:
;; -->
;; THIS IS NOT ROBUST!

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


(define (bl sent)
  (if (empty? sent)
    #()
    (bl-r 0 sent (make-vector (- (vector-length sent) 1)))))

(define butlast bl)

(define (bl-r idx sent1 sent2)
  (let ((my-vec-set! (lambda (v s a) (vector-set! v s a) v)))
    (if (= idx (vector-length sent2))
      sent2
      (bl-r (+ idx 1)
            sent1
            (my-vec-set! sent2
                         idx
                         (vector-ref sent1 idx))))))

(check (bl #(a b c)) => #(a b))
(check (bl #(a)) => #())
(check (bl #()) => #())


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
           (sent (make-vector needed '())))
      (se-r sent 0 flat))))

(define sentence se)

(define (words-and-sentences-in flat)
  (w-a-s-i-r 0 flat))

(define (w-a-s-i-r count flat)
  (cond ((empty? flat)
         count)
        ((vector? (car flat))
         (w-a-s-i-r (+ count (vector-length (car flat))) (cdr flat)))
        (else (w-a-s-i-r (+ count 1) (cdr flat)))))

(define (se-r sent slot flat)
  (let ((my-vec-set! (lambda (v s a) (vector-set! v s a) v)))
        (cond ((empty? flat)
               sent)
              ((not (sentence? (car flat)))
               (se-r
                 (my-vec-set! sent slot (car flat)) ;; returns sent
                 (+ slot 1)
                 (cdr flat)))
              ;; from here we know we have a vector to empty into the sentence
              ((empty? (car flat))          ;; we've emptied the vector
               (se-r sent slot (cdr flat)))
              (else    ;; take word from front of vector
                (se-r
                  (my-vec-set! sent slot (first (car flat)))
                  (+ slot 1)
                  (cons (bf (car flat)) (cdr flat)))))))

(check (se 'a 'b 'c) => #(a b c))
(check (se ) => #())
(check (se 'a 'b 'c '(d e f) '(g h i) #(q w e r t y)) => #(a b c d e f g h i q w e r t y))
(check (se 'a 'b (se 'c 'd)) => #(a b c d))


;; The tests listed in the problem:

(check (sentence 'a 'b 'c) => #(a b c))
(check (butfirst (sentence 'a 'b 'c)) => #(b c))


;; (b) Does the following program still work with the new implementation of
;; sentences? If not, fix the program.
;;
;; (define (praise stuff)
;;   (sentence stuff '(is good)))

;; Mine does. Possibilities with less comprehensive implementations are (se
;; stuff #(is good)) and (se stuff 'is 'good)


;; (c) Does the following program still work with the new implementation of
;; sentences? If not, fix the program.
;;
;; (define (praise stuff)
;;   (sentence stuff 'rules!))

;; Mine does. See (b) above.


;; (d) Does the following program still work with the new implementation of
;; sentences? If not, fix the program. If so, is there some optional
;; rewriting that would improve its performance?
;;
;; (define (item n sent)
;;   (if (= n 1)
;;       (first sent)
;;       (item (- n 1) (butfirst sent))))

;; Yes, it works with my implementation and probably with the less
;; comprehensive one that most would write. The obvious optimization is
;; that we have direct access via index into the vector, so a better
;; implementation (but also more tightly coupled to a sentence definition)
;; is:

(define (item n sent)
  (vector-ref sent (- n 1)))


;; (e) Does the following program still work with the new implementation of
;; sentences? If not, fix the program. If so, is there some optional
;; rewriting that would improve its performance?
;;
;; (define (every fn sent)
;;   (if (empty? sent)
;;     sent
;;     (sentence (fn (first sent))
;;               (every fn (butfirst sent)))))

;; It works, a better solution would be to duplicate the sentence and
;; loop through the duplicate item by item and apply the function.


;; (f) In what ways does using vectors to implement sentences affect the
;; speed of the selectors and constructor? Why do you think we chose to use
;; lists?

;; I think it's a wash for grabbing off the front of a sentence, and even
;; for grabbing off the end. My constructor has the overhead of casting the
;; a list item by item into a vector, but I preallocated the vector.
;;
;; It is easier to grow and shrink a list based sentence. If you are
;; combining sentence parts, list operators probably be faster.
;;
;; A vector implementation has to choose between an exact fit of items
;; (words) to slots, or a [count][word]....[maybe free space] structure.
;; The easier implementation to code is that a sentence is an exactly sized
;; vector, and any changes are applied to a copy which can grow or shrink.
;;
;; The list implementation is much simpler in concept and code. You
;; couldn't present the 'lisp/scheme-ness of scheme' using vectors and
;; state.


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 23 Files end 16...")
