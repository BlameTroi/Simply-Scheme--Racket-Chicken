#lang simply-scheme
;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 15 Advanced Recursion

;; The #lang command loads the racket language definition for
;; the text. Then we just need srfi-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 15 problems 1, 2, 3, 4, plus preamble start...")

;; ----------------------------------------------
;; Work along text examples....

;; A good quick introductory example that will lead to a merge sort later.
;; `remove-once' is from an earlier chapter.

(define (sort sent)
  (if (empty? sent)
      (se )
      (se (earliest-word sent) (sort (remove-once (earliest-word sent) sent)))))

(define (earliest-word sent)
  (earliest-word-r (first sent) (bf sent)))

(define (earliest-word-r so-far rest)
  (cond ((empty? rest) so-far)
        ((before? so-far (first rest)) (earliest-word-r so-far (bf rest)))
        (else (earliest-word-r (first rest) (bf rest)))))

(define (remove-once wd sent)
  (cond ((empty? sent) (se ))
        ((equal? wd (first sent)) (bf sent))
        (else (se (first sent) (remove-once wd (bf sent))))))

(check (earliest-word '(this is not the first word)) => 'first)
(check (remove-once 'this '(this is not the last word this is)) => '(is not the last word this is))
(check (sort '(gamma delta bravo alpha charlie beta echo)) => '(alpha beta bravo charlie delta echo gamma))


;; I really didn't like how they explained the binary to decimal conversion.
;; Their walk through takes something easy and mangles it into something
;; difficult to follow.

(define (binary-r accum xs)
  (cond ((empty? xs)     accum)
        (else (binary-r (+ (* 2 accum) (if (equal? (first xs) '1) 1 0)) (bf xs)))))

(define (from-binary bits)
  (binary-r 0 bits))

;; You would think the binary input would have to be a symbol or string but
;; their abstraction layer does what is reasonable and (first 10) is 1, and so
;; on.

(check (from-binary 0) => 0)
(check (from-binary 10) => 2)
(check (from-binary 1111) => 15)
(check (from-binary 10000) => 16)


;; One thing they do consistently stress is that when thinking about recursive
;; algorithms, take a leap of faith and trust that some small bit of recursion
;; will just work. That's along the lines of functional
;; decomposition/recomposition.
;;
;; Once you have a procedure that works, trust it to work.


;; They now provide one of the best descriptions and walkthroughs of a merge
;; sort that I have ever seen. This is precisely because of that leap of faith
;; concept.

(define (mergesort sent)
  (if (<= (count sent) 1)
      sent
      (merge (mergesort (one-half sent))
             (mergesort (other-half sent)))))

;; We wrote a merge for 14.15...lifting my version as I know it works but
;; changing the predicate from <= to before? My tests were all numeric in
;; chapter 14.

(define (merge xs ys)
  (cond ((empty? xs)                      ys)
        ((empty? ys)                      xs)
        ((before? (first xs) (first ys)) (se (first xs) (merge (bf xs) ys)))
        (else                            (se (first ys) (merge xs (bf ys))))))

;; Now all we need is a way to get the first and then the second half of the
;; current sentence. Their approach to this works but is redundant. Either way
;; we have to pass over the sentence twice, once from the front and once from
;; the back, but they do the whole sentence each time while only half the
;; sentence is needed. I'd redo this if efficiency was an issue.
;;
;; There's also the temptation to introduce global variables and persistent
;; state, but I'm not going there. I can see a way to do this in one pass with
;; nested lets though.

(define (one-half sent)
  (if (<= (count sent) 1)
      sent
      (se (first sent) (one-half (bf (bf sent))))))

(define (other-half sent)
  (if (<= (count sent) 1)
      '()
      (se (first (bf sent)) (other-half (bf (bf sent))))))

(check (one-half '(a b c d e)) => '(a c e))
(check (other-half '(a b c d e)) => '(b d))


;; A decent walkthrough of figuring out how to get every combination of the
;; letters in a word is followed by a rare mention of efficiency. The obvious
;; solution has a redundancy that matters and so show how to cache the result
;; without ever saying 'cache' or 'memoize'.

(define (prepend-every ltr sent)
  (if (empty? sent)
      (se )
      (se (word ltr (first sent)) (prepend-every ltr (bf sent)))))

;; works but inefficient -- calls itself twice with the same arguments (bf wd).

(define (subsets-old wd)
  (if (empty? wd)
      (se "")
      (se (subsets (bf wd))                    ; <-- first call
          (prepend-every (first wd)
                         (subsets (bf wd)))))) ; <-- second call same args

;; better -- factor out the call to subsets so it only happens once and caches
;; the result.

(define (subsets wd)
  (if (empty? wd)
      (se "")
      (let ((smaller (subsets (bf wd))))
        (se smaller (prepend-every (first wd) smaller)))))

(check (subsets 'fred) => '("" d e ed r rd re red f fd fe fed fr frd fre fred))


;; ----------------------------------------------
;; 15.1 Write `to-binary'.
;;
;; (find-power-of-2 num pow)
;;
;; Finds the power of 2 where 2^pow >= num. This is meant to be called from
;; wrapper routines and has no real error checking. On the first call pow
;; should be 0.

(define (find-power-of-2 num pow)
  (cond ((> num (expt 2 pow))     (find-power-of-2 num (+ pow 1)))
        (else (if (< num (expt 2 pow))
                  (- pow 1)         ;; n = 2^p exactly
                  pow))))

(check (find-power-of-2 8 0) => 3)
(check (find-power-of-2 127 0) => 6)

;; (to-binary-r num pow)
;;
;; Recursively returns a word representing the bits of num in binary. On the
;; initial call 2^pow >= num. If a 'word' pow is "too large" leading zeros are
;; issued but the result is otherwise correct.
;;
;; As with `find-power-of-2', this is not meant to be called directly and so it
;; has no guards for bad input.

;; NOTE: Due to the "strings are numbers" behavior of the book framework, a
;; result with leading zeros is returned as a string. If the result is properly
;; fitted and the first digit is a one, the result appears to be a binary
;; number but is really in whatever radix is active in your repl. This is not
;; an issue within the book framework.

(define (to-binary-r num pow)
  (if (< pow 0)
      (word )
      (if (< num (expt 2 pow))
          (word '0 (to-binary-r num (- pow 1)))
          (word '1 (to-binary-r (max 0 (- num (expt 2 pow))) (- pow 1))))))

(check (to-binary-r 4 2) => 100)
(check (to-binary-r 7 2) => 111)
(check (to-binary-r 9 3) => 1001)

;; (to-binary num)
;;
;; Return the binary representation of a non-negative integer num.

(define (to-binary num)
  (if (< num 1)
      (word '0)
      (word (to-binary-r num (find-power-of-2 num 0)))))

(check (to-binary 9) => '1001)
(check (to-binary 23) => '10111)
(check (to-binary 529) => '1000010001)


;; ----------------------------------------------
;; 15.2 Write predicate (palindrome? sent) that determines if a sentence reads
;; the same forwards and backwards by letters.
;;
;; (sentence->word sent)
;;
;; Concatenate all the words that make up the sentence into one long word.

(define (sentence->word sent)
  (cond ((empty? sent) (word ))
        (else (word (first sent) (sentence->word (bf sent))))))

(check (sentence->word '(flee to me remote elf)) => 'fleetomeremoteelf)

;; (palindromic-word? wd)
;;
;; Is wd a palindrome?

(define (palindromic-word? wd)
  (cond ((< (count wd) 2) #t)
        ((not (equal? (first wd) (last wd))) #f)
        (else (palindromic-word? (bf (bl wd))))))

(check (palindromic-word? 'asdf) => #f)
(check (palindromic-word? 'asdffdsa) => #t)
(check (palindromic-word? 'asdfgfdsa) => #t)

;; (palindrome? sent-or-wd)
;;
;; Is the argument a palindrome? If the argument is a sentence, all spaces are
;; removed to create one long word, which is then checked.

(define (palindrome? sent-or-wd)
  (cond ((sentence? sent-or-wd)       (palindromic-word? (sentence->word sent-or-wd)))
        ((word? sent-or-wd)           (palindromic-word? sent-or-wd))
        (else #f)))

(check (palindrome? '(flee to me remote elf)) => #t)
(check (palindrome? '(flee to me remote control)) => #f)
(check (palindrome? 'asdf) => #f)
(check (palindrome? 'asdffdsa) => #t)
(check (palindrome? 'asdfgfdsa) => #t)


;; ----------------------------------------------
;; 15.3 Write (substrings wd) which returns a sentence of all the possible
;; substrings in the word. Not subsets, but substrings. rat and at are
;; substrings of brat, ar is a subset, as is btra.
;;
;; (substrings-r wd)
;;
;; Recursive helper for substrings that returns the substrings in wd by
;; repeatedly triming the tail.

(define (substrings-r wd)
  (cond ((< (count wd) 1)               (se ))
        (else (se wd  (substrings-r (bl wd))))))

;; (substrings wd)
;;
;; Return all the substrings within word.

(define (substrings wd)
  (cond ((< (count wd) 1)               (se ))
        (else (se wd (substrings-r (bl wd)) (substrings (bf wd))))))

(check (substrings 'ab) => '(ab a b))
(check (substrings 'bird) => '(bird bir bi b ird ir i rd r d))


;; ----------------------------------------------
;; 15.4 Write predicate (substring? wd). See 15.3 for the definition of
;; substring.
;;
;; (prefix? pre str)
;;
;; Does str start with pre?

(define (prefix? pre str)
  (cond ((empty? pre)                               #t)
        ((> (count pre) (count str))                #f)
        ((not (equal? (first pre) (first str)))     #f)
        (else               (prefix? (bf pre) (bf str)))))

(check (prefix? 'asdf 'asdfijkl) => #t)
(check (prefix? 'asdf 'asdej) => #f)
(check (prefix? 'ijkl 'ijk) => #f)

;; (substring? sub str)
;;
;; Is sub contained with str?

(define (substring? sub str)
  (cond ((empty? sub)                  #t)       ;; or not, needs a definition
        ((> (count sub) (count str))   #f)
        ((prefix? sub str)             #t)
        (else    (substring? sub (bf str)))))

(check (substring? 'ab 'ab) => #t)
(check (substring? 'rat 'brat) => #t)
(check (substring? 'bri 'brat) => #f)
(check (substring? 'bright 'brite) => #f)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 15 problems 1, 2, 3, 4, plus preamble end...")
