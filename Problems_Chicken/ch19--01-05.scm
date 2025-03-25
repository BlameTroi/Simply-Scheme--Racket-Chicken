;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 19 Implementing High Order Functions.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load SRFI 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 19 Implementing High Order Functions begin 01-05...")


;; ----------------------------------------------
;; 19.2 Write keep. Don't forget that keep has to return a sentence if its
;; second argument is a sentence, and a word if its second argument is a word.
;;
;; (Hint: it might be useful to write a combine procedure that uses either word
;; or sentence depending on the types of its arguments.)

(define (my-keep pred? stuff)
  (cond ((sentence? stuff) (my-keep-r pred? stuff se))
        (else              (my-keep-r pred? stuff word))))

(define (my-keep-r pred? stuff fn)
  (cond ((empty? stuff) (my-nil-for fn))
        (else (if (pred? (first stuff))
                (fn (first stuff) (my-keep-r pred? (bf stuff) fn))
                (my-keep-r pred? (bf stuff) fn)))))

(define (my-nil-for fn)
  (cond ((equal? fn se) '())
        ((equal? fn word) "")
        (else '())))

;; The authors have a 'combiner' in their standard framework, but it's hidden
;; from view. A sentence can take as arguments two words or two sentences, so
;; I'm not yet clear how I want to manage that. After some thought, we'll trust
;; the user to decide since the 'stuff' to 'keep' will be either a sentence or
;; a list.
;;
;; The authors' suggestion worked but I didn't like because it seemed a bit
;; redundant. The only thing that 'my-combiner' needed to do that the fn verb
;; didn't was handle the empty case. For 'word', empty is "", while for
;; 'sentence', the empty case is '().
;;
;; The solutions reads best with using the fn verb 'word' or 'se' as the sole
;; combiner, and determining the terminal value based on the verb. Rather than
;; inline that, I added a 'my-nil-for' to return the appropriate nil/empty
;; value.
;;
;; In the description of the 'three-arg-accumulate' they describe another way
;; to deal with base case values by checking to see if we have reached the last
;; element (car present, cdr empty) and if so the value of the last element is
;; the terminal value.
;;
;; That would work for this as well, but that's after problem 19.3.

;; Now test words and sentences.

(define (noise-word? x) (member? x '(a is be do at the this of)))
(check (my-keep
         noise-word?
         '(this is a sentence that has a mix of real indexable words and noise words))
       => '(this is a a of))
(check (my-keep vowel? 'superdupertrooper) => 'ueueooe)


;; ----------------------------------------------
;; 19.3 Write the three-argument version of accumulate that we described.
;;
;; (three-arg-accumulate + 0 '(4 5 6)) => 15
;;
;; (three-arg-accumulate + 0 '()) => 0
;;
;; (three-arg-accumulate cons '() '(a b c d e)) => (A B C D E)

(define (three-arg-accumulate verb terminator stuff)
  (cond ((empty? stuff)           terminator)
        (else (verb
                (first stuff)
                (three-arg-accumulate verb terminator (bf stuff))))))

;; Testing the three argument against their expected results and then
;; against their standard 'accumulate'. Interesting, their 'accumulate'
;; of cons '(list) returns '(<butlast list> . <last list>). So, we
;; get one failure reported but my solution matched the problem spec.

(check (three-arg-accumulate + 0 '(4 5 6)) => 15)
(check (three-arg-accumulate + 0 '(4 5 6)) => (accumulate + '(4 5 6)))

(check (three-arg-accumulate + 0 '()) => 0)
(check (three-arg-accumulate + 0 '()) => (accumulate + '()))

(check (three-arg-accumulate cons '() '(a b c d e)) => '(a b c d e))
;; (check (three-arg-accumulate cons '() '(a b c d e)) => (accumulate cons '(a b c d e)))


;; ----------------------------------------------
;; 19.4 Our accumulate combines elements from right to left. That is:
;;
;; (accumulate - '(2 3 4 5)) computes 2−(3−(4−5)).
;;
;; Write left-accumulate, which will compute ((2−3)−4)−5 instead. (The result
;; will be the same for an operation such as +, for which grouping order
;; doesn't matter, but will be different for -.)

;; The obvious 'cheat' is to reverse the input, but that isn't
;; quite right:

(define (left-accumulate-wrong verb stuff)
  (accumulate verb (reverse stuff)))

;; But this works correctly, but again with the difference in consing
;; between their standard 'accumulate' and the result expected by
;; 'three-arg-accumulate'.

(define (left-accumulate verb stuff)
  (cond ((empty? (bl stuff))            (last stuff))
        (else (verb
                (left-accumulate verb (bl stuff))
                (last stuff)))))

(check (left-accumulate - '(2 3 4 5)) => (- (- (- 2 3) 4) 5))

;;                                       -1 - 4 = -5 -5 = -10


;; ----------------------------------------------
;; 19.5 Rewrite the true-for-all? procedure from Exercise 8.10. Do not use
;; every, keep, or accumulate.

;; For reference:

(define (ch8-true-for-all? pred sent)
  (cond ((empty? sent)              #f)
        (else (equal? (keep pred sent) sent))))

(define (tfa-pred x) (member x '(a b c)))

(check (ch8-true-for-all? tfa-pred '(a b c)) => #t)
(check (ch8-true-for-all? tfa-pred '(a b c d)) => #f)
(check (ch8-true-for-all? tfa-pred '()) => #f)

;; And for the this problem:

(define (true-for-all? pred sent)
  (cond ((empty? sent) #f)
        ((not (pred (car sent))) #f)
        ((empty? (cdr sent)) #t)
        (else (true-for-all? pred (cdr sent)))))

(check (true-for-all? tfa-pred '(a b c)) => #t)
(check (true-for-all? tfa-pred '(a b c d)) => #f)
(check (true-for-all? tfa-pred '()) => #f)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 19 Implementing High Order Functions end 01-05...")
