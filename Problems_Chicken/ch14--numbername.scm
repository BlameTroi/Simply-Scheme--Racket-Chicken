;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 14-15 interlude --- spelling names of huge numbers

;; For Chicken 5, load "required.scm" before this to establish the
;; text book environment for Simply Scheme. We load srfi 78 in the
;; exercises to support testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 14 project -- number name...")

;; ----------------------------------------------
;; A mini project to speak an arbitrarily long string of digits into mostly
;; proper English. The sentence/word wrappers ended up working well here.
;;
;; Groupings of three digits for long numbers are called periods. A natural
;; approach is to speak each period with its magnitude (10^3 = thousand, 10^6 =
;; million, and so on).
;;
;; There are two irregularities, but only the handling for values in the teens
;; is at all tedius. The magnitude for units (10^0) is not spoken.


;;-----------------------------------------------
;; One of the test cases is a rather large factorial.

(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

;; ----------------------------------------------
;; Given a digit in a position of a period (one/digit, ten, hundred) return the
;; proper string. Depending upon position and context a '4 will return '(four
;; hundred) or '(forty) or '(four) or '(fourteen).
;;
;; Zeros are never spoken but are returned here as placeholders.

(define (one n)
  (cond ((< n 0) 'ERROR)
        ((> n 9) 'ERROR)
        (else
         (item (+ n 1)
               '(zero one two three four five six seven eight nine)))))

(check (one 3) => 'three)
(check (one 11) => 'ERROR)

;; Hundred's are suffixed 'hundred' later in the process. Here they are the
;; same as digits.

(define (hundred n)
  (one n))

(check (hundred 4) => 'four)

;; Ten is actually teen and is used to trigger replacement for '(ten five) to
;; 'fifteen, and so on.

(define (ten n)
  (cond ((< n 0) 'ERROR)
        ((> n 9) 'ERROR)
        (else
         (item (+ n 1)
               '(zero ten twenty thirty forty fifty sixty seventy eighty ninety)))))

(check (ten 0) => 'zero)
(check (ten 1) => 'ten)
(check (ten 3) => 'thirty)

;; Since we don't have associative arrays yet, a long cond sequence will map a
;; digit to its teen.

(define (ones->teens x) ;; at this point ...
  (cond ((equal? x 'zero)  'ten)
        ((equal? x 'one)   'eleven)
        ((equal? x 'two)   'twelve)
        ((equal? x 'three) 'thirteen)
        ((equal? x 'four)  'fourteen)
        ((equal? x 'five)  'fifteen)
        ((equal? x 'six)   'sixteen)
        ((equal? x 'seven) 'seventeen)
        ((equal? x 'eight) 'eighteen)
        ((equal? x 'nine)  'nineteen)
        (else              'ERROR)))

(check (ones->teens 'zero) => 'ten)
(check (ones->teens 'seven) => 'seventeen)


;; ----------------------------------------------
;; Return a sentence of the periods that make up the number n, ordered least to
;; most significant: 21057321 becomes '(321 57 21).

(define (periodically-r n)
  (cond ((= n 0) (se '()))
        (else
         (se (remainder n 1000) (periodically-r (quotient n 1000))))))

(define (periodically n)
  (cond ((= n 0) (se 0))
        (else (se (periodically-r n)))))

;; These test cases check behavior at the edges and confirm that periods are
;; ordered least -> most.

(check (periodically 0) => '(0))
(check (periodically 1) => '(1))
(check (periodically 10) => '(10))
(check (periodically 100) => '(100))
(check (periodically 1000) => '(0 1))
(check (periodically 213) => '(213))
(check (periodically 9) => '(9))
(check (periodically 19) => '(19))
(check (periodically 20) => '(20))
(check (periodically 21) => '(21))
(check (periodically 3002001) => '(1 2 3))
(check (periodically 1002003) => '(3 2 1))
(check (periodically 21057321) => '(321 57 21))
(check (periodically 5513345) => '(345 513 5))
(check (periodically (factorial 20)) => '(0 640 176 8 902 432 2))


;; ----------------------------------------------
;; Regardless of the magnitude of the period, each period is spoken the same
;; way. The magnitude is appended as a suffix later. Here the periods are
;; normalized. Each digit (digit, ten, hundred) is given a text value, even
;; leading zeros. '111 is '(one ten one), and will eventually become '(one
;; hundred eleven). '456 is '(four fifty six). '7 is '(zero zero seven).

(define (normalize-period n)
  (cond ((< n 0)   (se n 'ERROR 'ERROR 'ERROR))
        ((> n 999) (se n 'ERROR 'ERROR 'ERROR))
        ((= n 0)   (se n 'zero 'zero 'zero))
        (else      (se n (hundred (quotient n 100))
                       (ten (quotient (remainder n 100) 10))
                       (one (remainder n 10))))))

(check (normalize-period 1) => '(1 zero zero one))
(check (normalize-period 10) => '(10 zero ten zero))
(check (normalize-period 100) => '(100 one zero zero))
(check (normalize-period 512) => '(512 five ten two))
(check (normalize-period 13)  => '(13 zero ten three))
(check (normalize-period 21)  => '(21 zero twenty one))

;; When you are working with groups of n from a sentence.

(define (next-n n xs)
  (cond ((= n 0) (se ))
        (else (se (first xs) (next-n (- n 1) (butfirst xs))))))

(check (next-n 4 '(1 2 3 4 5 6 7 8)) => '(1 2 3 4))

(define (tail-after-n n xs)
  (cond ((= 0 n) xs)
        (else (tail-after-n (- n 1) (butfirst xs)))))

(check (tail-after-n 4 '(1 2 3 4 5 6 7 8)) => '(5 6 7 8))

;; When you have marker/sentinal/delimiter values in sentence.

(define (next-up-to x xs)
  (cond ((empty? xs) (se ))
        ((equal? (first xs) x) (se ))
        (else (se (first xs) (next-up-to x (butfirst xs))))))

(check (next-up-to 0 '(1 2 3 4 0 5 6 7 8 0)) => '(1 2 3 4))

(define (tail-after x xs)
  (cond ((empty? xs) (se ))
        ((equal? (first xs) x)  (se (butfirst xs)))
        (else (tail-after x (butfirst xs)))))

(check (tail-after 0 '(1 2 3 4 0 5 6 7 8 0)) => '(5 6 7 8 0))

;; ----------------------------------------------
;; Take the normalized form and return a clean human readable form. '(21 zero
;; twenty one) becomes '(twenty one FM), '(100 one zero zero) becomes '(one
;; hundred FM), '(310 three ten zero) becomes '(three hundred ten FM), and so
;; on. The 'FM is the end of period marker to help find where to insert
;; magnitudes.
;;
;; Handling teens makes this uglier than I would like. Both the handlers for
;; tens and ones have to be aware of the other.

(define (humanize-hundreds xs)
  (let ((this (first xs)))
    (if (equal? this 'zero)
        (se )
        (se this 'hundred))))

(define (humanize-tens xs)
  (let ((this (first (butfirst xs))))
    (if (or (equal? this 'zero) (equal? this 'ten))
        (se ) ;; here ten is dropped and handled later
        (se this))))

(define (humanize-ones xs)
  (let ((that (first (butfirst xs)))
        (this (first (butfirst (butfirst xs)))))
    (cond ((and (equal? that 'ten) (equal? this 'zero)) (se 'ten))
          ((equal? that 'ten) (se (ones->teens this)))
          ((equal? this 'zero) (se ))
          (else (se this)))))

(define (humanize xs)
  (se (humanize-hundreds (bf xs))
      (humanize-tens (bf xs))
      (humanize-ones (bf xs))
      'FM))

(check (humanize '(11 zero ten one)) => '(eleven FM))
(check (humanize '(321 three twenty one)) => '(three hundred twenty one FM))


;; ----------------------------------------------
;; Periods are in a sentence, least to most significant. Each period is
;; delimited by a marker. Match each period with its appropriate magnitude and
;; return the English sentence as one would speak it.
;;
;; The least significant period (units) has no suffix, so a dummy value is
;; added to the magnitude list as a place holder.

(define (insert-magnitudes periods magnitudes)
  (cond ((empty? periods)
         (se ))
        ((empty? magnitudes)
         (se 'ERROR))
        (else
         (se (insert-magnitudes (tail-after 'FM periods) (butfirst magnitudes))
             (se (next-up-to 'FM periods)
                 (if (equal? (first magnitudes) 'IGNORE) (se ) (se (first magnitudes))))))))


;; ----------------------------------------------
;; The process breaks naturaly into four pieces. Break the number into periods
;; (the groups of three digits between commas), spell out the digits
;; (normalize) and make them properly worded (humanize by adding 'hundred' and
;; dealing with teens) and then inserting the magnitudes (thousand, million,
;; billion, ...) after each period.


;; Magnification places a magnitude indicator between each period. Is this
;; period thousands, millions, ...

(define (magnify-periods xs)
  (insert-magnitudes
   xs
   '(IGNORE thousand million billion
            trillion quadrillion quintillion
            sextillion septillion octillion
            nonillion decillion)))


;; Humanization takes the normalized periods and makes them properly speakable
;; (add 'hundred', remove leading zeros, handle the teens).

(define (humanize-periods xs)
  (cond ((empty? xs) (se ))
        (else (se (humanize (next-n 4 xs))
                  (humanize-periods (tail-after-n 4 xs))))))


;; Normalization takes the period and speaks it (poorly) into English. Each
;; period will be comprised of three words in a sentence followed by a
;; delimiter, for a total of four.

(define (normalize-periods xs)
  (cond ((empty? xs) (se ))
        (else (se (normalize-period (first xs))
                  (normalize-periods (butfirst xs))))))


;; Split into digits of the number into groups of three, right to left.

(define (create-periods n)
  (periodically n))


;; ----------------------------------------------
;; The assignment is to write `number-name' which takes a positive whole number
;; and returns a sentence of the spelled out number with magnitudes for every
;; period.

(define (number-name n)
  (magnify-periods
   (humanize-periods
    (normalize-periods
     (create-periods n)))))


;; ----------------------------------------------
;; Tests from the text:

(check (number-name 5513345) =>
       '(five million five hundred thirteen thousand three hundred forty five))

(check (number-name (factorial 20)) =>
       '(two quintillion
             four hundred thirty two quadrillion
             nine hundred two trillion
             eight billion
             one hundred seventy six million
             six hundred forty thousand))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)
