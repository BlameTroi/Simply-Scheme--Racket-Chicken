;; a scratchpad for work through testing

;; Troy Brumley, blametroi@gmail.com, early 2025.

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load SRFI 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

(import trace)

;; ! -- exactly one word
;; ? -- at most one word
;; & -- at least one word
;; * -- any number of words

(define (match? pattern sent)
  (cond ((empty? pattern)        (empty? sent))
        ((empty? sent)           #f)
        ((equal? (first pattern) '?)
         (if (empty? sent)
           (match? (bf pattern) '())            ;; if empty, does the next of pattern match empty?
           (or (match? (bf pattern) (bf sent))  ;; either rest pattern matches rest sentence
               (match? (bf pattern) sent))))    ;; or rest pattern matches sentence
        ((equal? (first pattern) '*)
         (*-longest-match (bf pattern) sent))
        ((equal? (first pattern) '!)
         (match? (bf pattern) (bf sent)))
        ((equal? (first pattern) (first sent))
         (match? (bf pattern) (bf sent)))
        (else #f)))

(define (*-longest-match pattern-rest sent)
  (*-lm-helper pattern-rest sent '()))

(define (*-lm-helper pattern-rest sent-matched sent-unmatched)
  (cond ((match? pattern-rest sent-unmatched) #t)
        ((empty? sent-matched) #f)
        (else (*-lm-helper pattern-rest
                           (bl sent-matched)
                           (se (last sent-matched) sent-unmatched)))))

