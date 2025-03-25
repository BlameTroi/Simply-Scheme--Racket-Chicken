;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 22 Files.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load srfi 78 in the exercises to support
;; testing.

(import srfi-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 22 Files begin 01-08...")

;; NOTE: I'm ignoring return values from top level procedures. Whatever
;; comes out, comes out.

;; ----------------------------------------------
;; 22.1 Write a concatenate procedure that takes two arguments: a list of
;; names of input files, and one name for an output file. The procedure
;; should copy all of the input files, in order, into the output file.

;; I'd prefer to use a prototype like (concatenate . files) instead
;; of explicitly making the input a list. Lop off the last file name
;; and use it for output.

(define (concatenate files combined)
  (let ((output (open-output-file combined)))
    (concatenate-r files output)
    (close-output-port output)))

(define (concatenate-r files output)
  (cond ((empty? files)      '())
        (else (let ((input (open-input-file (car files))))
                (begin
                  (copy-in-out input output)
                  (close-input-port input)
                  (concatenate-r (cdr files) output))))))

;; There doesn't seem to be much in the way of standard error reporting for
;; i/o. I expect a runtime error if any problems occur.

(define (copy-in-out input output)
  (let ((got (read-char input)))
    (if (eof-object? got)
      '()
      (begin
        (write-char got output)
        (copy-in-out input output)))))


;; ----------------------------------------------
;; 22.2 Write a procedure to count the number of lines in a file. It should
;; take the filename as argument and return the number.

;; I'm pretty sure the expected solutions to these are expecting us to use
;; the different behavior of 'read' and 'read-line' but for this and
;; character counting, 'read-char' is fine answer. I'll be lazy on the word
;; counting procedure.

(define (line-count file)
  (let ((input (open-input-file file)))
    (let ((n (line-count-r input 0)))
    (close-input-port input)      ;; no need to (begin ) this
    n)))

(define (line-count-r input n)
  (let ((ch (read-char input)))
    (if (eof-object? ch)
      n
      (line-count-r input (+ n
                             (if (char=? ch #\newline) 1 0))))))


;; ----------------------------------------------
;; 22.3 Write a procedure to count the number of words in a file. It should
;; take the filename as argument and return the number.

;; This is the way to count words given what we know from the text. The
;; other option would be to 'read-string' and create a more correct but
;; also more complex solution. This has two bugs:
;;
;; 1. Anything in parentheses counts as a single word.
;; 2. An open parentheses without a matching close will error out.

(define (word-count file)
  (let ((input (open-input-file file)))
    (let ((n (word-count-r input 0)))
    (close-input-port input)      ;; no need to (begin ) this
    n)))

(define (word-count-r input n)
  (let ((wd (read input)))
    (if (eof-object? wd)
      n
      (word-count-r input (+ n 1)))))


;; ----------------------------------------------
;; 22.4 Write a procedure to count the number of characters in a file,
;; including space characters. It should take the filename as argument and
;; return the number.

(define (char-count file)
  (let ((input (open-input-file file)))
    (let ((n (char-count-r input 0)))
    (close-input-port input)      ;; no need to (begin ) this
    n)))

(define (char-count-r input n)
  (let ((ch (read-char input)))
    (if (eof-object? ch)
      n
      (char-count-r input (+ n 1)))))


;; ----------------------------------------------
;; 22.5 Write a procedure that copies an input file to an output file but
;; eliminates multiple consecutive copies of the same line. That is, if the
;; input file contains the lines:
;;
;; John Lennon
;; Paul McCartney
;; Paul McCartney
;; George Harrison
;;
;;
;; Paul McCartney
;; Ringo Starr
;;
;; then the output file should contain
;;
;; John Lennon
;; Paul McCartney
;; George Harrison
;;
;; Paul McCartney
;; Ringo Starr

(define (dedup infile outfile)
  (let ((input (open-input-file infile))
        (output (open-output-file outfile)))
    (begin
      (dedup-r "" input output)
      (close-input-port input)
      (close-output-port output))))

(define (dedup-r last-line input output)
  (let ((this-line (read-line input)))
    (if (eof-object? this-line)
      '()
      (begin
        (if (equal? this-line last-line)
          '() ; nop
          (show-line this-line output))
        (dedup-r this-line input output)))))


;; ----------------------------------------------
;; 22.6 Write a lookup procedure that takes as arguments a filename and a
;; word. The procedure should print (on the screen, not into another file)
;; only those lines from the input file that include the chosen word.

(define (word-in file wd)
  (let ((input (open-input-file file)))
    (begin
      (word-in-r input wd)
      (close-input-port input))))

(define (word-in-r input wd)
  (let ((line (read-line input)))
    (if (eof-object? line)
      '()
      (begin
        (if (has-word? line wd)
          (show-line line)
          '())
        (word-in-r input wd)))))

(define (has-word? line wd)
  (member? wd line))


;; ----------------------------------------------
;; 22.7 Write a page procedure that takes a filename as argument and prints
;; the file a screenful at a time. Assume that a screen can fit 24 lines;
;; your procedure should print 23 lines of the file and then a prompt
;; message, and then wait for the user to enter a (probably empty) line. It
;; should then print the most recent line from the file again (so that the
;; user will see some overlap between screenfuls) and 22 more lines, and so
;; on until the file ends.

(define (by-page file)
  )


;; ----------------------------------------------
;; 22.8 A common operation in a database program is to join two databases,
;; that is, to create a new database combining the information from the two
;; given ones. There has to be some piece of information in common between
;; the two databases. For example, suppose we have a class roster database
;; in which each record includes a student's name, student ID number, and
;; computer account name, like this:

;;   full-name            sid   sname
;; ((john alec entwistle) 04397 john)
;; ((keith moon) 09382 kmoon)
;; ((peter townshend) 10428 pete)
;; ((roger daltrey) 01025 roger)

;; We also have a grade database in which each student's grades are stored
;; according to computer account name:

;;  sname grades
;; (john 87 90 76 68 95)
;; (kmoon 80 88 95 77 89)
;; (pete 100 92 80 65 72)
;; (roger 85 96 83 62 74)

;; We want to create a combined database like this:

;; joined on sname
;; ((john alec entwistle) 04397 john 87 90 76 68 95)
;; ((keith moon) 09382 kmoon 80 88 95 77 89)
;; ((peter townshend) 10428 pete 100 92 80 65 72)
;; ((roger daltrey) 01025 roger 85 96 83 62 74)

;; in which the information from the roster and grade databases has been
;; combined for each account name.
;;
;; Write a program join that takes five arguments: two input filenames, two
;; numbers indicating the position of the item within each record that
;; should overlap between the files, and an output filename. For our
;; example, we'd say

> (join "class-roster" "grades" 3 1 "combined-file")

;; In our example, both files are in alphabetical order of computer account
;; name, the account name is a word, and the same account name never
;; appears more than once in each file. In general, you may assume that
;; these conditions hold for the item that the two files have in common.
;; Your program should not assume that every item in one file also appears
;; in the other. A line should be written in the output file only for the
;; items that do appear in both files.

(define (join file1 file2 item1 item2 combined)
  )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 21 Files end 01-08...")
