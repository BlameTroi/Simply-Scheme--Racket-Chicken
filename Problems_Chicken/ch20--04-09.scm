;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 19 Input and Output.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load srfi 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 20 Input and Output begin 04-09...")


;; ----------------------------------------------
;; 20.4 Write a program that carries on a conversation like the following
;; example. User input is between > < since I don't have boldface for
;; program source.

;; > (converse) <
;; Hello, I'm the computer.  What's your name? > Brian Harvey <
;; Hi, Brian.  How are you? > I'm fine. <
;; Glad to hear it.

(define (converse . rest)
  (show-line (se "Hi, I'm a computer. What's your name?"))
  (let ((name (read-line)))
    (show-line (se "Heya" name ". How's it hanging?"))
    (let ((resp (read-line)))
      (show-line (se "Well goodness gracious!")))))

;; Parameters are strictly needed, but ". rest" will allow parameters to be
;; put on the procedure call, avoiding a wrong number of arguments error.


;; ----------------------------------------------
;; 20.5 Our name-table procedure uses a fixed width for the column
;; containing the last names of the people in the argument list. Suppose
;; that instead of liking British-invasion music you are into late romantic
;; Russian composers, or perhaps you like jazz. Modify name-table so that
;; it figures out the longest last name in its argument list, adds two for
;; spaces, and uses that number as the width of the first column.
;;
;; Editorial comment: name-table is a horrible name. print-name-table or
;; something else would be more descriptive.

;; Here are the original name-table and the three name lists:

(define (name-table names)
  (if (null? names)
    'done
    (begin (display (align (cadar names) 11))
           (show (caar names))
           (name-table (cdr names)))))

(define names-beatles '((john lennon)
                        (paul mccartney)
                        (george harrison)
                        (ringo starr)))

(define names-classical '((piotr tchaikovsky)
                          (nicolay rimsky-korsakov)
                          (sergei rachmaninov)
                          (modest musorgsky)))

(define names-jazz '((bill evans)
                     (paul motian)
                     (scott lefaro)))

;; Modify name-table so that it figures out the longest last name in its
;; argument list, adds two for spaces, and uses that number as the width of
;; the first column.

;; The standard environment provides word->string, and Scheme provides
;; string-length. In the environment, symbols and numbers are strings, but
;; not in standard Scheme. Testing shows that 1, '1, "1" are all considered
;; words by the standard environment. I tried to use word->string and
;; string-length in a for-each and that threw errors somewhere down in the
;; standard environment. After some hammering, my word-length works.

(define (word-length x)
  (if (not (word? x))
         -1               ;; better than zero imo
        (count x)))

;; The surname is the cadar of each name (a sentence). Return the length of
;; the longest name found. The standard environment provides max (more
;; correctly, it overrides max).

(define (longest-surname-length-r n names)
  (cond ((empty? names)
         n)
        (else (longest-surname-length-r
                (max n (word-length (cadar names)))
                (cdr names)))))

(define (longest-surname-length names)
  (cond ((empty? names)
         -1)
        (else (longest-surname-length-r 0 names))))

(define (my-name-table-r alignment names)
  (if (empty? names)
    'done
    (begin
      (display (align (cadar names) alignment))
      (show (caar names))
      (my-name-table-r alignment (cdr names)))))

(define (my-name-table names)
  (if (null? names)
    'done
    (let ((alignment    (+ 2 (longest-surname-length names))))
      (my-name-table-r alignment names))))

;; Testing the assignment is a visual verification, but we can test
;; the helpers.

(check (word-length 'mccartney) => 9)
(check (word-length (car '(lefaro scott))) => 6)
(check (word-length "rimsky-korsakov") => 15)
(check (longest-surname-length names-beatles) => 9)
(check (longest-surname-length names-jazz) => 6)
(check (longest-surname-length names-classical) => 15)


;; ----------------------------------------------
;; 20.6 The procedure ask-user isn't robust. What happens if you type
;; something that isn't anumber, or isn't between 1 and 9? Modify it to
;; check that what the user types is a number between 1 and 9. If not, it
;; should print a message and ask the user to try again.

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (read-a-char '(1 2 3 4 5 6 7 8 9)))

;; The only gotcha here is remembering what was read. State can not always
;; be avoided.

(define (read-a-char valid-chars)
  (let ((ch      (read)))
    (if (member? ch valid-chars)
      ch
      (begin (show "invalid, try again:")
             (read-a-char valid-chars)))))

;; This is another one that we can't test with check.


;; ----------------------------------------------
;; 20.7 Another problem with ask-user is that it allows a user to request a
;; square that isn't free. If the user does this, what happens? Fix
;; ask-user to ensure that this can't happen.

;; The position is a nine character word. Only '_' positions are valid.

(define (valid-positions position)
  (valid-positions-r '() 1 position))

(define (valid-positions-r accum n position)
  (cond ((empty? position)
         accum)
        (else
          (valid-positions-r
            (if (equal? (first position) '_)
              (se accum n)
              accum)
            (+ n 1)
            (bf position)))))

(check (valid-positions '_________) => '(1 2 3 4 5 6 7 8 9))
(check (valid-positions '_xxxxxxxx) => '(1))
(check (valid-positions 'xxxxxxxx_) => '(9))
(check (valid-positions '1234_67__) => '(5 8 9))

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (read-a-char (valid-positions position)))

;; And again, we don't have a scriptable test.


;; ----------------------------------------------
;; 20.8 At the end of the game, if the computer wins or ties, you never
;; find out which square it chose for its final move. Modify the program to
;; correct this. (Notice that this exercise requires you to make
;; play-ttt-helper non-functional.)

;; Some of this is from the text.

;; strategy = first open square

(define (stupid-ttt position letter)
  (location '_ position))

(define (location letter word)
  (if (equal? letter (first word))
    1
    (+ 1 (location letter (bf word)))))

;; Run the game

(define (play-ttt x-strat o-strat)
  (play-ttt-helper x-strat o-strat '_________ 'x))

(define (play-ttt-helper x-strat o-strat position whose-turn)
  (cond ((already-won? position (opponent whose-turn))
         (list (opponent whose-turn) 'wins!))
        ((tie-game? position) '(tie game))
        (else (let ((square (if (equal? whose-turn 'x)
                              (x-strat position 'x)
                              (o-strat position 'o))))
                (play-ttt-helper x-strat
                                 o-strat
                                 (add-move square whose-turn position)
                                 (opponent whose-turn))))))

;; Update the game board:

(define (add-move square letter position)
  (if (= square 1)
    (word letter (bf position))
    (word (first position)
          (add-move (- square 1) letter (bf position)))))

;; I need to write a bunch of code but I'm not in the mood for this
;; particular project. Deferring.


;; ----------------------------------------------
;; 20.9 The way we invoke the game program isn't very user-friendly. Write
;; a procedure game that asks you whether you wish to play x or o, then
;; starts a game. (By definition, x plays first.) Then write a procedure
;; games that allows you to keep playing repeatedly. It can ask "do you
;; want to play again?" after each game. (Make sure that the outcome of
;; each game is still reported, and that the user can choose whether to
;; play x or o before each game.)

;; Same as 20.8.


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 20 Input and Output end 04-09...")
