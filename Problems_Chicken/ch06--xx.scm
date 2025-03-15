;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Simply Scheme
;;; Chapter 6

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load srfi 78 in the exercises to support
;; testing.
(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;; These were work along through the text and not from a specific problem set.
;; Future problem sets will be better structured.
;;
;; Several of the procedures here can and will be reused in later problems. In
;; strictly file based Chicken Scheme I would just load a file of helpers, but
;; that doesn't work with Racket. Modules are the correct solution but they
;; don't fit with the level we are supposed to be working from in these texts.
;;
;; There will be copy-and-paste reuse. Sorry-not sorry.


;;; Problem(s) and Solution(s) here:
(print "Chapter 6 various start...")


;; ----------------------------------------------
;; Create a more readable list by inserting 'and' between the last items.

(define (insert-and sl)
  (se (butlast sl) 'and (last sl)))

(check (insert-and '(john paul george ringo))
   => '(john paul george and ringo))
(check (insert-and '(spick span))
   => '(spick and span))


;; ----------------------------------------------
;; Write a responder for a game of 'buzz'. Count numbers and when any number is
;; divisible by 7, say 'buzz'.

(define (divisible? dividend divisor)
  (= 0 (remainder dividend divisor)))

(check (divisible? 7 7) => #t)
(check (divisible? 7 6) => #f)

(define (buzz num)
  (if (or (member? 7 num) (divisible? 7 num))
      'buzz
      num))

(check (buzz 2)
   => 2)
(check (buzz 7)
   => 'buzz)


;; ----------------------------------------------
;; Convert a declarative sentence into a query by swapping the first two words
;; and add a ? to the last word.

(define (second sent) (first (butfirst sent)))
(define (declarative->query sent)
   (se (second sent) (first sent)
   (butlast (butfirst (butfirst sent))) (word (last sent) '?)))

(check (declarative->query '(we are ok)) => '(are we ok?))
(check (declarative->query '(we are going to school))
   => '(are we going to school?))


;; ----------------------------------------------
;; Pluralize a verb to demonstrate 'cond':

(define (pluralize-verb verb)
   (cond ((equal? verb 'be) 'is)
         ((equal? (last verb) 'o) (word verb 'es))
         (else (word verb 's))))

(check (pluralize-verb 'be) => 'is)
(check (pluralize-verb 'go) => 'goes)
(check (pluralize-verb 'move) => 'moves)


;; ----------------------------------------------
;; Scientific notation for those numbers with a lot of zeros. This can be
;; tricky depending upon your Scheme's handling of exact and inexact numbers.

(define (scientific b p)
  (* b (expt 10 p)))
(define (sci-exponent n)
  (floor (/ (log n) (log 10))))
(define (sci-coefficient n)
  (/ n (expt 10 (sci-exponent n))))

;; Chicken is returning a rational number here and the comparison doesn't work
;; with the equivalent (ok, approximately equivalent) irrational number.

(check (scientific 5127 -5) => 5127/100000)

;; And these both came back as irrational numbers, but while 0.051271 does not
;; equal 5127/100000, 4 does equal 4.0 in Chicken but this fails as 4.0 not
;; equaling 4 in Racket. I updated the expected result to account for this.

(check (sci-coefficient 21300) => 2.13)
(check (sci-exponent 21300)    => 4.0)


;; ----------------------------------------------
;; Time conversion routines for America <-> European time formats, hours only.
;; Watch for edge cases.
;;
;; Style note. I almost always use guards to keep bad input out of processing
;; logic, it may require more procedures but the clarity is worth it.

;; helpers for ranges [) and (] and such.
(define (in-open?   a b x)       (and (<= a x) (>= b x)))
(define (in-closed? a b x)       (and (< a x) (> b x)))
(define (in-right-open? a b x)   (and (< a x) (>= b x)))
(define (in-right-closed? a b x) (and (<= a x) (> b x)))

;; Is a time such as '(13 PM) a valid American formatted time?
(define (american-time? ts)
  (cond
    ((equal? 'noon ts)          #t)
    ((equal? 'midnight ts)      #t)
    ((not (= 2 (count ts)))     #f)
    ((not (number? (first ts))) #f)
    ((not (member? (last ts) '(am pm AM PM))) #f)
    ((not (in-open? 0 12 (first ts)))         #f)
    (else #t)))

(check (american-time? '(1 pm)) => #t)
(check (american-time? '(12 pm)) => #t)
(check (american-time? '(12 AM)) => #t)
(check (american-time? '(13 am)) => #f)
(check (american-time? '(1 pst)) => #f)
(check (american-time? 'noon) => #t)
(check (american-time? 'midnight) => #t)

;; Is a time such as noon or 17 a valid European formatted time?
(define (european-time? ts)
  (cond
    ((equal? 'noon ts)          #t)
    ((equal? 'midnight ts)      #t)
    ((not (word? ts))           #f)
    ((not (number? ts))         #f)
    ((not (in-open? 0 23 ts))   #f)
    (else #t)))

(check (european-time? '(1 pm)) => #f)
(check (european-time? '(12 15)) => #f)
(check (european-time? '12) => #t)
(check (european-time? '13) => #t)
(check (european-time? '24) => #f)
(check (european-time? '0) => #t)
(check (european-time? 11) => #t)
(check (european-time? 25) => #f)
(check (european-time? 'noon) => #t)
(check (european-time? 'midnight) => #t)

;; Now that we can verify input validity, convert an American formatted time to
;; European format.

(define (european-time ts)
  (if (not (american-time? ts))
      #f
      (cond ((equal? 'noon ts) 12)
            ((equal? 'midnight ts) 0)
            ((equal? '(12 am) ts) 0)
            ((equal? '(12 pm) ts) 12)
            (else (+ (first ts)
                     (if (equal? (last ts) 'am)
                         0
                         12))))))

(check (european-time '(-1 am)) => #f)
(check (european-time  '(0 am)) =>  0)
(check (european-time  '(1 am)) =>  1)
(check (european-time '(11 am)) => 11)
(check (european-time '(12 pm)) => 12)
(check (european-time  '(1 pm)) => 13)
(check (european-time  '(2 pm)) => 14)
(check (european-time '(11 pm)) => 23)
(check (european-time '(12 am)) =>  0)
(check (european-time    '(am)) => #f)
(check (european-time '(13 am)) => #f)
(check (european-time '(13 pm)) => #f)

;; And now a European formatted time to American format.

(define (american-time ts)
  (if (not (european-time? ts))
      #f
      (cond ((equal? 'noon ts)               '(12 pm))
            ((equal? 'midnight ts)           '(12 am))
            ((equal? 0 ts)                   '(12 am))
            ((equal? 12 ts)                  '(12 pm))
            ((> ts 12)       (sentence (- ts 12) 'pm))
            (else            (sentence        ts 'am)))))

(check (american-time -4) => #f)
(check (american-time 0) => '(12 am))
(check (american-time 12) => '(12 pm))
(check (american-time 13) => '(1 pm))
(check (american-time 23) => '(11 pm))
(check (american-time 24) => #f)
(check (american-time 'noon) => '(12 pm))
(check (american-time 'midnight) => '(12 am))


;; ----------------------------------------------
;; Return the type of an object, as defined in "simply.scm". Note that the
;; "simply.scm" environment thinks '() is a sentence, I disagree in principle
;; but will go along with their definition. I also debate if a single word
;; sentence is a sentence or a word, but I'll accept their definition.

(define (type-of x)
  (cond ((and (word? x) (not (number? x))) 'word)
        ((number? x)                       'number)
        ((sentence? x)                     'sentence)
        ((equal? #t x)                     'boolean)
        ((equal? #f x)                     'boolean)
        (else                              'unknown)))

(check (type-of 'fred)          => 'word)
(check (type-of 13)             => 'number)
(check (type-of "7")            => 'number)
(check (type-of '7)             => 'number)
(check (type-of '(nowhere man)) => 'sentence)
(check (type-of (= 3 3))        => 'boolean)
(check (type-of (= 3 4))        => 'boolean)
(check (type-of '())            => 'sentence)


;; ----------------------------------------------
;; What is the indefinite article of a noun (phrase)?
;;
;; We don't have to worry about silent consonant rules for 'a' vs 'an'. For a
;; noun (phrase) of indenfite identity (people or person vs a name):
;;
;; For count nouns, a or an.
;;
;; For non-count nouns, nothing
;;
;; All things in general, nothing.
;;
;; Specific thing, the.
;;
;; The rules are more complex than we are expected to catch, but giving it a
;; try.

(define (valid-noun-phrase? np)
  (cond ((number? np)                                    #f)
        ((word? np)                                      #t)
        ((and (sentence? np) (not (number? (first np)))) #t)
        (else                                            #f)))

(define (first-letter np)
  (cond ((word? np)               (first np))
        (else              (first (first np)))))

(define (vowel? c)
  (member? c '(a e i o u)))

(define (consonant? c)
  (and (not (vowel? c)) (not (number? c))))

(define (vowel-sound? c)
  (vowel? c))

(define (indef-article np)
  (cond ((not (valid-noun-phrase? np))    (sentence 'ERROR np))
        ((vowel-sound? (first-letter np))    (sentence 'an np))
        (else                                 (sentence 'a np))))

(check (indef-article 'beatle)          => '(a beatle))
(check (indef-article 'album)           => '(an album))
(check (indef-article '7)               => '(ERROR 7))
(check (indef-article '(marching band)) => '(a marching band))


;; ----------------------------------------------
;; Convert a noun to singular or plural depending upon `thismany' of them there
;; are.
;;
;; We are asked to improve the `plural' function from earlier in the text
;; elsewhere in these problems, I'll do that here and reuse it for the
;; `thismany' problem.
;;
;; I'm cheating by using a recursive call for a sentence, which isn't in the
;; spec but it's for a case that wouldn't be used at this point in the text.
;;
;; To make a plural noun, add 's' most of the time, but if a noun ends in 's',
;; 'x', 'z', 'sh', 'ch' add 'es', nouns ending in '<consonant> y' change the
;; 'y' to 'i' and add 'es'.
;;
;; Which leads to some helpers:

(define (last2 x) (word (last (butlast x)) (last x))) ;; bug--won't do sentence
(check (last2 'asdf) => 'df)

(define (es-plural? wd)
  (cond ((member? (last wd) '(s x z))           #t)
        ((member? (last2 wd) '(sh ch))          #t)
        ((member? (last2 wd) '(ay ey iy oy uy)) #f)
        ((equal? (last wd) 'y)                  #t)
        (else #f)))

(check (es-plural? 'box) => #t)
(check (es-plural? 'boxy) => #t)
(check (es-plural? 'wish) => #t)
(check (es-plural? 'sandwich) => #t)
(check (es-plural? 'boy) => #f)
(check (es-plural? 'day) => #f)
(check (es-plural? 'kitty) => #t)
(check (es-plural? 'boink) => #f)

(define (ies-plural? wd)
  (cond ((not (equal? (last wd) 'y))            #f)
        ((member? (last2 wd) '(ay ey iy oy uy)) #f)
        (else                                   #t)))

(check (ies-plural? 'toy) => #f)
(check (ies-plural? 'kitty) => #t)
(check (ies-plural? 'witch) => #f)

(define (plural wd)
  (cond ((sentence? wd) (sentence (butlast wd) (plural (last wd))))
        ((number? wd)                                           wd)
        ((ies-plural? wd)                 (word (butlast wd) 'ies))
        ((es-plural? wd)                             (word wd 'es))
        (else                                         (word wd 's))))

(check (plural 'box) => 'boxes)
(check (plural 'boy) => 'boys)
(check (plural 'witch) => 'witches)
(check (plural 'baby) => 'babies)
(check (plural 'fish) => 'fishes)

;; And after all that work on plurals, the actual problem.
(define (thismany count wd)
  (if (= count 1)
      (sentence count wd)
      (sentence count (plural wd))))

(check (thismany 1 'partridge) => '(1 partridge))
(check (thismany 2 'partridge) => '(2 partridges))
(check (thismany 3 'french-hen) => '(3 french-hens))
(check (thismany 1 'baby) => '(1 baby))
(check (thismany 2 'baby) => '(2 babies))
(check (thismany 3 'witch) => '(3 witches))
(check (thismany 1 'toy) => '(1 toy))
(check (thismany 9 'toy) => '(9 toys))


;; ----------------------------------------------
;; Write a date validation predicate for numerical dates '(mm dd yyyy).
;;
;; There is a lot of (probably too much) functional decomposition here. It
;; isn't as bad as it initially appears, there are a lot of unit tests with the
;; functions.
;;
;; In a real application this would be refactored and I might flip some of the
;; tests from negative to positive.

;; Accessors for parts of a date.

(define (month mdy)
  (first mdy))
(define (day mdy)
  (second mdy))
(define (year mdy)
  (last mdy))

(check (month '(1 2 1930)) => 1)
(check (day '(1 2 1930)) => 2)
(check (year '(1 2 1930)) => 1930)

;; There are some quick checks I can perform for each unit of the date. Doing
;; so allows me to ignore error checking within the actual validation. Think of
;; this as checking syntax before semantics.

(define (obviously-bad-month? mdy)
  (or (not (number? (month mdy)))
      (< (month mdy) 1)
      (> (month mdy) 12)))

(check (obviously-bad-month? '(1 2 3)) => #f)
(check (obviously-bad-month? '(12 2 3)) => #f)
(check (obviously-bad-month? '(15 16 1718)) => #t)

(define (obviously-bad-day? mdy)
  (or (not (number? (day mdy)))
      (< (day mdy) 1)
      (> (day mdy) 31)
      (and (equal? (month mdy) 2)
           (> (day mdy) 29))))

(check (obviously-bad-day? '(1 15 1920)) => #f)
(check (obviously-bad-day? '(2 30 1920)) => #t)
(check (obviously-bad-day? '(12 32 1920)) => #t)

(define (obviously-bad-year? mdy)
  (or (not (number? (year mdy)))
      (< (year mdy) 1)))

(check (obviously-bad-year? '(7 15 1943)) => #f)
(check (obviously-bad-year? '(7 15 0)) => #t)

;; Here the accessors and unit checks are combined with some basic formatting
;; checks. Throw out the malformed dates.

(define (obviously-bad-date? mdy)
  (or (not (sentence? mdy))
      (not (equal? (count mdy) 3))
      (not (number? (month mdy)))
      (not (number? (day mdy)))
      (not (number? (year mdy)))
      (obviously-bad-month? mdy)
      (obviously-bad-day? mdy)
      (obviously-bad-year? mdy)))

;; We have lexical scope in Scheme. The authors frequently abbreviate their
;; function names and I've started doing the same for these problem sets. Here
;; I reuse `obd?' as an abbreviation of `obviously-bad-date?'. The `check's
;; following in the source code invoke `obviously-bad-date' while any `check's
;; prior to this define still invoke `obviously-bad-day?'.
;;
;; This isn't quite shadowing as both definitions are at the same nesting
;; level.

(check (obviously-bad-date? '(1 23 1945)) => #f)
(check (obviously-bad-date? '(1 14)) => #t)
(check (obviously-bad-date? '(feb 13 1980)) => #t)
(check (obviously-bad-date? '(13 14 1950)) => #t)
(check (obviously-bad-date? '(1 32 1960)) => #t)
(check (obviously-bad-date? '(2 28 1960)) => #f)
(check (obviously-bad-date? '(2 30 1960)) => #t)
(check (obviously-bad-date? '(30 2 1960)) => #t)
(check (obviously-bad-date? '(1942 6 4)) => #t)

;; And the ever problematic 'what is a leap year': /4 and /400 but not !/100

(define (leap-year? yyyy)
  (cond ((divisible? yyyy 100) (divisible? yyyy 400))
        (else (divisible? yyyy 4))))

(check (leap-year? 1960) => #t)
(check (leap-year? 1984) => #t)
(check (leap-year? 1999) => #f)
(check (leap-year? 2000) => #t)
(check (leap-year? 1900) => #f)

(define (days-in-february mdy)
  (if (leap-year? (year mdy)) 29 28))

;; "Thirty days hath September..." table lookups are more readable than `if' or
;; `cond' chains.

(define (thirty-day-month? mdy) (member? (month mdy) '(9 4 6 11)))
(define (thirty-one-day-month? mdy) (member? (month mdy) '(1 3 5 7 8 9 10 12)))

(define (too-many-days? mdy)
  (cond ((and (thirty-day-month? mdy) (<= (day mdy) 30))        #f)
        ((and (thirty-one-day-month? mdy) (<= (day mdy) 31))    #f)
        (else (if (<= (day mdy) (days-in-february mdy))      #f #t))))

;; And after that we have a four 'line' function to see if a date is valid. I
;; mostly wrote this bottom-up but my thought process was somewhere closer to
;; top-down. I don't think of it as middle-out.

(define (valid-date? mdy)
  (cond ((obviously-bad-date? mdy)   #f)
        ((too-many-days? mdy)        #f)
        (else                        #t)))

(check (valid-date? '(1 23 1945)) => #t)
(check (valid-date? '(12 31 1927)) => #t)
(check (valid-date? '(11 31 1980)) => #f)
(check (valid-date? '(2 28 1960)) => #t)
(check (valid-date? '(2 29 1960)) => #t)
(check (valid-date? '(2 30 1960)) => #f)
(check (valid-date? '(2 29 1900)) => #f)
(check (valid-date? '(2 29 2000)) => #t)


;; ----------------------------------------------
;; Improve `greet' so that it handles honorifics, suffixes, and some commonly
;; known names associated with titles.

;; Is this an honorific? Several are missing, of course. In a real application
;; I'd have to account for military ranks and who knows what else.

(define (honorific? wd)
  (member?
   wd
   '(mr mrs miss ms mx sir dame dr lady lord professor
        father sister fr sr mister missus brother mother doctor)))

(check (honorific? 'mr) => #t)
(check (honorific? 'queen) => #f)

;; Is this a suffix? As with honorifics, several are missing.

(define (suffix? wd)
  (member?
   wd
   '(esq esquire jr sr i ii iii iv)))

(check (suffix? 'esq) => #t)
(check (suffix? 'jr) => #t)
(check (suffix? 'first) => #f)

;; Have we reduced the name to a single word?

(define (one-word? nm)
  (cond ((word? nm)                                 #t)
        ((and (sentence? nm) (equal? (count nm) 1)) #t)
        (else                                       #f)))

(check (one-word? 'fred) => #t)
(check (one-word? '()) => #f)
(check (one-word? '(wilma flintstone)) => #f)

;; Allow for some famous special cases.

(define (famous? nm)
  (cond ((member? (first nm) '(queen pope king president))       #t)
        ((equal? nm '(david livingstone))                        #t)
        (else                                                    #f)))

(define (greet-famous nm)
  (cond
   ((equal? nm '(david livingstone))        '(dr livingstone i presume))
   ((equal? (first nm) 'queen)              '(hello your majesty))
   ((equal? (first nm) 'king)               '(hello your majesty))
   ((equal? (first nm) 'pope)               '(hello holy father))
   ((equal? (first nm) 'president)          '(hello mr president))
   (else                                    '(oh gosh, its you? i dont know what to say!))))

;; Weave it all together.

(define (greet nm)
  (cond ((one-word? nm)                (sentence 'hi (if (word? nm) nm (first nm))))
        ((famous? nm)                  (greet-famous nm))
        ((suffix? (last nm))           (greet (butlast nm))) ;; cheating a bit with recursion here
        ((honorific? (first nm))       (sentence 'hello (first nm) (last nm)))  ;; flawed
        (else (sentence 'hello (first nm)))))

(check (greet '(betty)) => '(hi betty))
(check (greet 'mary) => '(hi mary))
(check (greet '(john lennon)) => '(hello john))
(check (greet '(dr marie curie)) => '(hello dr curie))
(check (greet '(dr martin luther king jr)) => '(hello dr king))
(check (greet '(queen elisabeth)) => '(hello your majesty))
(check (greet '(david livingstone)) => '(dr livingstone i presume))


;; ----------------------------------------------
;; Convert an absolutely silly duration in seconds to a more meaningful
;; duration for human consumption. IE, bigger units, smaller numbers.

(define sec/min 60)
(define sec/hour (* sec/min 60))
(define sec/day (* sec/hour 24))
(define sec/week (* sec/day 7))
;;(define sec/month (* sec/day 30))  ;; approx
(define sec/year (* sec/day 365))

;; A very simplistic first pass. Months are too approximate for this so I
;; pulled them out.

(define (describe-time sec)
  (cond ((> sec sec/year)        (sentence (/ sec sec/year) 'years))
;;        ((> sec sec/month)       (sentence (/ sec sec/month) 'months))
        ((> sec sec/week)        (sentence (/ sec sec/week) 'weeks))
        ((> sec sec/day)         (sentence (/ sec sec/day) 'days))
        ((> sec sec/hour)        (sentence (/ sec sec/hour) 'hours))
        ((> sec sec/min)         (sentence (/ sec sec/min) 'minutes))
        (else (sentence sec 'seconds))))

(describe-time 45)
(describe-time 75)
(describe-time 930)
(describe-time (+ 1 sec/week))
(describe-time (+ sec/day sec/week))
(describe-time (* 14.2 sec/day)) ;; some weeks
(describe-time 3000000000)


;; ----------------------------------------------
;;; And that's the end of this section. Report test results and reset
;;; counters.
(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 6 various end...")
