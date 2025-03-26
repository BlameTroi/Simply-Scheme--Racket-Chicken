#lang simply-scheme
;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 14 Common Patters in Recursive Procedures

;; The #lang command loads the racket language definition for
;; the text. Then we just need SRFI-78.
(require srfi/78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 14 problems 1, 2, 3, 4, 5, 6, and 7 start...")


;; Identify a "pattern" (every/keep/accumulate) that leads to a solution. Write
;; a procedure implementing the solution.

;; ----------------------------------------------
;; 14.1 Only remove one item. To me using their terminology this is a variation
;; on keep.

(define (remove-once wd sent)
  (cond ((empty? sent)                   '())         ;; base case
        ((empty? wd)                     sent)        ;; or no word
        ((not (equal? wd (first sent)))  (se (first sent) (remove-once wd (bf sent))))
        (else                            (bf sent)))) ;; terminal case

(check (remove-once 'morning '(good morning good morning))
       => '(good good morning)) ;; Or '(good morning good)
;;                              ;; if approached backwards.

;; This is waaay to much testing:

(check (remove-once 'morning '(good good good)) => '(good good good))
(check (remove-once 'morning '(morning is good every morning)) => '(is good every morning))
(check (remove-once 'morning '(morning)) => '())
(check (remove-once 'morning '(morning time)) => '(time))
(check (remove-once 'morning '(good morning)) => '(good))
(check (remove-once 'morning '(morning morning)) => '(morning))
(check (remove-once 'morning '()) => '())
(check (remove-once "" '(good morning fred)) => '(good morning fred))


;; ----------------------------------------------
;; 14.2 Build a word or sentence up letter by letter. Iterate over each letter,
;; or keep, but we're accumulating a sentence. So accumulate is probably the
;; best answer.

(define (up-r sofar togo)
  (if (or (equal? togo '()) (equal? togo ""))
      (se )
      (se (word sofar (first togo))
          (up-r (word sofar (first togo)) (butfirst togo)))))

(define (up wrd)
  (if (or (equal? wrd "") (empty? wrd))
      (se )
      (se (first wrd) (up-r (first wrd) (butfirst wrd)))))

;; edge cases are empty or single letter. this happens to do something somewhat
;; reasonable for a sentence.

(check (up 'town) => '(t to tow town))
(check (up 'x) => '(x))
(check (up '()) => '())
(check (up '(town)) => '(town))
(check (up '(down town jackie brown)) => '(down downtown downtownjackie downtownjackiebrown))


;; ----------------------------------------------
;; 14.3 Remove duplicates from a sentence. This is a variation on keep. Or
;; maybe accumulate. I'm accumulating a sentence and replacing some items, so
;; now that I think on it, this is an every where duplicates a made nil.

(define (add-if-not-member x xs)
  ;; If x is already in xs, don't add it again.
  (cond ((empty? xs) (se x))      ;; first time, start sentence
        ((empty? x) xs)           ;; last time, probably not called
        ((member? x xs) xs)       ;; already there
        (else (se xs x))))        ;; not there, append

(define (remdup-r xs ys)
  (cond ((empty? ys)   xs)
        (else
         (se
          (remdup-r
           (add-if-not-member (first ys) xs)   ;; add front of remainder to accum if not there
           (butfirst ys))))))                  ;; trim front of remainder

(define (remdup sent)
  (se (remdup-r '() sent)))

(check (remdup '(ob la di ob la da)) => '(ob la di da))


;; ----------------------------------------------
;; 14.4 Odds. This is mostly a keep.

(define (odds-r x xs)
  (cond ((empty? xs)     '())
        ((even? x)       (se (odds-r (+ 1 x) (bf xs))))
        (else            (se (first xs) (odds-r (+ 1 x) (bf xs))))))

(define (odds xs)
  (se (odds-r 1 xs)))

(check (odds '(i lost my little girl)) => '(i my girl))


;; ----------------------------------------------
;; 14.5 Write `letter-count' that takes a sentence and returns the total number
;; of letters in the sentence. This is an accumulate.

(define (lower-case? x)
  (member? x '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
(define (upper-case? x)
  (member? x '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
(define (letter? x)
  (or (lower-case? x) (upper-case? x)))
(define (decimal-digit? x)
  (member? x '(0 1 2 3 4 5 6 7 8 9)))

(define (letters-in-word x)
  (cond ((empty? x)                      0)
        ((number? x)                     0)
        ((letter? (first x))  (+ 1 (letters-in-word(bf x))))
        (else                 (+ 0 (letters-in-word(bf x))))))

(check (letters-in-word 'asdf) => 4)
(check (letters-in-word '1234) => 0)
(check (letters-in-word 'big-deal) => 7)
(check (letters-in-word 'asdf1234asdf) => 8)

(define (letter-count xs)
  (cond ((empty? xs)               0)
        (else (+ (letters-in-word (first xs))
                 (letter-count    (bf xs))))))

(check (letter-count '(fred wilma barney3)) => 15)
(check (letter-count '(fixing a hole)) => 11)


;; ----------------------------------------------
;; 14.6 Write `member?'. An every for only one.

(define (x_member? x xs)
  (if (empty? xs)
      #f
      (if (equal? x (first xs))
          #t
          (x_member? x (butfirst xs)))))

(check (x_member? 'a '(d c b a)) => #t)
(check (x_member? 'asdf '(ijkl qwerty asdf zxcv)) => #t)
(check (x_member? 'asdf '(a s d f)) => #f)
(check (x_member? 'asdf '()) => #f)
(check (x_member? 'a 'fdsa) => #t)


;; ----------------------------------------------
;; 14.7 Write `differences' which takes a sentence of numbers and returns a
;; sentence of the differences between adjacent numbers (first subtracted from
;; second, second from third, ...). There will be one fewer items in the new
;; sentence than the original.
;;
;; An accumulator over every.

(define (differences-r prev this rest)
  (cond ((empty? rest) (se (- this prev)))
        (else          (se (- this prev)
                           (differences-r this
                                               (first rest)
                                               (bf rest))))))

(define (differences xs)
  (cond ((empty? xs)                     '()) ; undefined behavior
        ((< (count xs) 2)                '()) ; undefined behavior
        (else (se (differences-r (first xs) (first (bf xs)) (bf (bf xs)))))))

(check (differences '(4 23 9 87 6 12)) => '(19 -14 78 -81 6))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 14 problems 1, 2, 3, 4, 5, 6, and 7 end...")
