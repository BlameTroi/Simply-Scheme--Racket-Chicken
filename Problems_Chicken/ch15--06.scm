;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 15 Advanced Recursion

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load SRFI 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 15 problem 6 start...")

;; -----------------------------------------------
;; 15.6 Write procedure 'unscramble' that will unwrap a nested sentence into a
;; more readable but the same semantically sentence. We are allowed to assume
;; the structure of the input will be as in the examples below, with no special
;; cases such as "that /jack/ built" or "that lay /in/ the house.
;;
;;           222222222 1111111111111 111111
;; '(this is the roach the gladiator killed) =>
;; '(this is the gladiator that killed the roach)
;;
;;           5555555 4444444 3333333 2222222 11111111 111 22222 333333 444
;; '(this is the rat the cat the dog the boy the girl saw owned chased bit) =>
;; '(this is the girl that saw the boy that owned the dog that chased the cat that bit the rat))
;;
;; So we have 'the noun' some number of times, followed by 'verb's, one fewer
;; time, and the whole thing is preceded by "this is" which is mostly noise,
;; and the pivot of "that" is inserted before the nouns.
;;
;; It's kind of a stack operation:
;;
;; Remove noise (this is, the)
;; Collect nouns, collect verbs,
;; Pop noun pop verb (except for last noun)
;; Build new sentence as
;; (se 'this 'is 'the noun 'that verb 'the next-noun 'that next-verb 'the last-noun)


;; Second should really be part of the standard environment ...

(define (second sent) (first (bf sent)))

;; Predefined for manual testing.
(define sent-gladiator '(this is the roach the gladiator killed))
(define sent-girl '(this is the rat the cat the dog the boy the girl saw owned chased bit))


;; Collect the nouns from the sentence. They can be identified by a preceding
;; "the". Pull from the sentence two at a time, saving only the noun, until we
;; either hit the end (which is an error) or a non "the".

(define (collect-nouns sent)
  (cond ((empty? sent)      (se ))
        ((equal? (first sent) 'the) (se (first (bf sent)) (collect-nouns (bf (bf sent)))))
        (else (collect-nouns (bf sent)))))


;; Collect verbs from the sentence. We are given the original sentence and the
;; list of NOUNS found previously. For every noun, skip two words ("the" and
;; noun) in the sentence. Once the nouns are exhausted, the remainder of the
;; sentence should be only the verbs.

(define (collect-verbs nouns sent)
  (cond ((empty? nouns) sent)
        (else (collect-verbs (bf nouns) (bf (bf sent))))))


;; Rebuild the sentence from the NOUNS and VERBS to fit the desired pattern
;; (see unscramble). The NOUNS sentence was reversed so that we can use 'first'
;; and 'butfirst' on both NOUNS and VERBS.

(define (rebuild nouns verbs)
  (cond ((empty? nouns)      (se ))                   ;; done
        ((empty? verbs)      (se 'the (first nouns))) ;; one fewer verb than nouns expected
        (else                                         ;; otherwise the noun that verb ...
         (se 'the (first nouns) 'that (first verbs) (rebuild (bf nouns) (bf verbs))))))


;; Unscramble a nested sentence of the pattern:
;;
;; this is the noun 3 the noun 2 the noun 1 verb 1 verb 2
;;
;; The order is reversed from the ends: IE, verb 1 goes with noun 1, verb 2 with
;; noun 2, and noun 3 follows noun 2 with no verb. More cleanly stated, a two
;; stacks, pushing down from the ends, meeting in the middle.
;;
;; Unscrambled that would be:
;;
;; this is the noun 1 that verb 1 the noun 2 that verb 2 verb 3.
;;
;; There is no error checking and we are allowed to assume that the noun
;; pattern will be 'the noun' throughout.

(define (unscramble sent)
  (cond ((empty? sent) #f)
        ((not (and (equal? 'this (first sent)) (equal? 'is (second sent)))) #f)
        (else
         (let ((nouns (collect-nouns (bf (bf sent)))))
           (let ((verbs (collect-verbs nouns (bf (bf sent)))))
             (se 'this 'is (rebuild (reverse nouns) verbs)))))))

;; Tests provided in the text:

(check (unscramble '(this is the roach the gladiator killed)) => '(this is the gladiator that killed the roach))
(check (unscramble '(this is the rat the cat the dog the boy the girl saw owned chased bit))
       => '(this is the girl that saw the boy that owned the dog that chased the cat that bit the rat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 15 problem 6 end...")
