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

(print "Chapter 15 problem 05 start...")

;; -----------------------------------------------
;; 15.5 Given a phone number, come up with a clever way to spell out the
;; number. Return all the possible spellings. That'll be huge. I'm using the
;; E.161 table from chapters 8 and 11.
;;
;; There are 2187 possible combinations (3^7, no q or z) so an exhaustive test
;; is not advisable.
;;
;; (check (phone-spell 22235766) => '(aadjpmm aadjpmn ... ccflsoo))
;;
;; (check (phone-spell 23) => '(ad ae af bd be bf cd ce cf)
;;
;;                            a   d
;;                            a    e
;;                            a     f
;;                            b   d
;;                            b    e
;;                            b     f ...
;;
;; (digit->keypad d)
;;
;; Given a digit from a phone number, return a list of the possible letter
;; equivalents to make phonewords. Uses the older map with no z or q. Returns
;; '? for illegal input other than 0 or 1, which are returned as 0 or 1.

(define (digit->keypad d)
  (cond
    ((not (number? d)) (se '?))
    ((= d 2)          '(a b c))
    ((= d 3)          '(d e f))
    ((= d 4)          '(g h i))
    ((= d 5)          '(j k l))
    ((= d 6)          '(m n o))
    ((= d 7)          '(p r s))   ;; q goes here in full set
    ((= d 8)          '(t u v))
    ((= d 9)          '(w x y))   ;; z goes here in full set
    (else              (se d))))  ;; have to do something, right?


;; (phone-spell num)
;;
;; Show all the possible spellings for a phone number using a standard phone
;; keypad (US E.161). The spec says we can assume no illegal input.


;; And here we do the compiling of the key letters. KEY is just one key, and
;; REST is a sentence of what prior KEYs have been appended to.
;;
;; For 23     '(a b c) '(d e f) '(g h i)
;;    each of g h i is combined with nothing,
;;    then each of d e f is combined with each of g h i
;;    then each of a b c is combined with each of dg dh di eg eh ei fg fh fi
(define (combine-keys key rest)
  (cond
    ((empty? rest)  (se ))
    (else           (se (word key (first rest))
                        (combine-keys key (bf rest))))))
(trace combine-keys)

;; KEYS come in as a sentence of (usually) 3 letters. REST ends up being the
;; KEYS triple for the next digit of NUM. This helper is mainly a dispatcher.

(define (for-each-key keys rest)
  (cond
    ((empty? keys)  (se ))
    (else           (se (for-each-key (bf keys) rest)
                        (combine-keys (first keys) rest)))))
(trace for-each-key)

;; NUM should be digits only of a phone number. Each digit becomes a sentence
;; of (usually) 3 letters. Dispatch to a helper to drive the process for each
;; letter and the remainder of NUM.

(define (phone-spell num)
  (cond
    ((empty? num)   (se ""))
    (else           (se (for-each-key (digit->keypad (first num)) (phone-spell (bf num)))))))
(trace phone-spell)

(check (phone-spell '234) => (reverse '(adg adh adi
                                            aeg aeh aei
                                            afg afh afi
                                            bdg bdh bdi
                                            beg beh bei
                                            bfg bfh bfi
                                            cdg cdh cdi
                                            ceg ceh cei
                                            cfg cfh cfi)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 15 problem 05 end...")
