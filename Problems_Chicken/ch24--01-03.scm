;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 24 Spreadsheet Application.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load SRFI 78 in the exercises to support
;; testing.

;; In addition to "required.scm" load "ch24--spread.scm" then run the
;; application with '(spreadsheet)'. The interactive entry is horked for
;; me, but I suspect that's my readline support for the Chicken REPL.
;; Reading from a file works fine.
;;
;; This is only marginally Scheme. While the spreadsheet commands resemble
;; s expressions, they aren't really. I'll work these in separate files and
;; pull my answers in here.
;;
;; I attempted to quickly add a comment command but after five minutes I
;; gave up and instead use "(put 'some-long-text-one-symbol a30)" which
;; will let me leave a command as a comment in a file if I want and only
;; trash one cell I'm not likely to use.
;;
;; Spreadsheet dimensions are a1..z30.
;;
;; I added Vim style motion commands HJKL as synonyms for the BNPF from
;; Emacs that the authors used.


;; For each of the following exercises, the information to hand in is the
;; sequence of spreadsheet commands you used to carry out the assignment.
;; You will find the load command helpful in these exercises.


;; -----------------------------------------------
;; 24.1 Set up a spreadsheet to keep track of the grades in a course. Each
;; column should be an assignment; each row should be a student. The last
;; column should add the grade points from the individual assignments. You
;; can make predictions about your grades on future assignments and see
;; what overall numeric grade each prediction gives you.

;; As a lookup table is not a capability, grades will be standard
;; percentages.

(put "24.1 class grades" a30)

(put "student" a3)
(put "        q1" b3)
(put "        q2" c3)
(put "       mid" d3)
(put "        q3" e3)
(put "       prj" f3)
(put "     final" g3)
(put "     grade" h3)

(put "John" a4)
(put  87 b4)
(put  95 c4)
(put  75 d4)
(put  93 e4)
(put 100 f4)
(put  97 g4)

(put "Paul" a5)
(put  97 b5)
(put  92 c5)
(put  85 d5)
(put  93 e5)
(put  80 f5)
(put  87 g5)

(put "George" a6)
(put  97 b6)
(put  95 c6)
(put  95 d6)
(put  93 e6)
(put 100 f6)
(put 100 g6)

(put "Ringo" a7)
(put  87 b7)
(put  85 c7)
(put  85 d7)
(put  83 e7)
(put 100 f7)
(put  87 g7)

(put (/ (+ (cell b) (cell c) (cell d) (cell e) (cell f) (cell g)) 6) h)

(put "yawn" a30)


;; -----------------------------------------------
;; 24.2 Make a table of tax and tip amounts for a range of possible costs
;; at a restaurant. Column a should contain the pre-tax amounts, starting
;; at 50 cents and increasing by 50 cents per row. (Do this without
;; entering each row separately!) Column b should compute the tax, based on
;; your state's tax rate. Column c should compute the 15% tip. Column d
;; should add columns a through c to get the total cost of the meal. Column
;; e should contain the same total cost, rounded up to the next whole
;; dollar amount.

(put "24.2 tax and tip" a30)
(put "      meal" a1)
(put "       tax" b1)
(put "       tip" c1)
(put "     total" d1)
(put "    next $" e1)
(put 0.50 a2)
(put (+ 0.50 (cell <0 <1)) a)
(put (* 0.05 (cell a)) b)
(put (* 0.15 (cell a)) c)
(put (+ (cell a) (cell b) (cell c)) d)
(put (round (+ (cell d) 0.50)) e)


;; -----------------------------------------------
;; 24.3 Make a spreadsheet containing the values from Pascal's triangle:
;; Each element should be the sum of the number immediately above it and
;; the number immediately to its left, except that all of column a should
;; have the value 1, and all of row 1 should have the value 1.

(put "24.3 Pascal's Triangle" z30)
(put 1.00 a)
(put 1.00 1)
(put (+ (cell <1 <0) (cell <0 <1)) 2)
(put (+ (cell <1 <0) (cell <0 <1)) 3)
(put (+ (cell <1 <0) (cell <0 <1)) 4)
(put (+ (cell <1 <0) (cell <0 <1)) 5)
(put "and so on" z30)

;;;;;;;;;;;;
;; end chapter 24 1-3, no real code or tests.
