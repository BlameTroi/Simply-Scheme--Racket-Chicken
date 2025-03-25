;;; Simply Scheme

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 21 Input and Output.

;; For Chicken 5, load "required.scm" before this to establish the text book
;; environment for Simply Scheme. We load SRFI 78 in the exercises to support
;; testing.

(import SRFI-78)
(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

;; NOTE: While I've left the SRFI-78 test wrapping of the problems in
;; place, there were no unit tests for this chapter. Instead the changes to
;; the 'functions' program were worked out here and then merged in to the
;; original "functions.scm" as "ch21--functions.scm".

(print "Chapter 21 Input and Output begin 01-09...")

;; ----------------------------------------------
;; 21.1 The get-args procedure has a let that creates the variable first,
;; and then that variable is used only once inside the body of the let. Why
;; doesn't it just say the following?
;;
;; This +won't+ might not work:
;;
;; (define (get-args n)
;;   (if (= n 0)
;;     '()
;;     (cons (get-arg) (get-args (- n 1)))))
;;
;; As written, which will work:
;;
;; (define (get-args n)
;;   (if (= n 0)
;;       '()
;;       (let ((first (get-arg)))
;;         (cons first (get-args (- n 1))))))

;; You can't guarantee the order that the arguments to 'cons' will be
;; evaluated. As 'get-arg' has side effects (display, read), its order
;; matters. Reference prior chapter, section A High Order Procedure for
;; Sequencing 'for-each'.


;; ----------------------------------------------
;; 21.2 The domain-checking function for equal? is
;;
;; (lambda (x y) #t)
;;
;; This seems silly; it's a function of two arguments that ignores both
;; arguments and always returns #t. Since we know ahead of time that the
;; answer is #t, why won't it work to have equal?'s entry in the a-list be
;;
;; (list 'equal? equal? 2 #t)

;; The type predicate (which is what this is in the a-list) has to be
;; evaluated (executed) to check the arguments. This is done with 'apply'
;; which won't work well. In my Scheme this returns 'call of non
;; procedure'.


;; ----------------------------------------------
;; 21.3 Every time we want to know something about a function that the user
;; typed in, such as its number of arguments or its domain-checking
;; predicate, we have to do an assoc in *the-functions*. That's
;; inefficient. Instead, rewrite the program so that get-fn returns a
;; function's entry from the a-list, instead of just its name. Then rename
;; the variable fn-name to fn-entry in the functions-loop procedure, and
;; rewrite the selectors scheme-procedure, arg-count, and so on, so that
;; they don't invoke assoc.

;; This would be easier with a global variable, but we haven't seen how to
;; set those yet.
;;
;; And I know the authors would prefer that we didn't use global variables.
;;
;; Change 'get-fn' to return the entry in the a-list instead of the name.
;; Then functions-loop will invoke the accessors as before, but passing the
;; entry instead of the name. In the accessors, remove the 'assoc' wrapping
;; leaving just the c????r procedure call. Also, create an 'fn-name'
;; accessors function.

(define (functions-loop)
  (let ((fn-entry (get-fn)))
    (if (equal? (fn-name fn-entry) 'exit)
      "Thanks for using FUNCTIONS!"
      (let ((args (get-args (arg-count fn-entry))))
        (if (not (in-domain? args fn-entry))
          (show "Argument(s) not in domain.")
          (show-answer (apply (scheme-function fn-entry) args)))
        (functions-loop)))))

(define (get-fn)
  (display "Function: ")
  (let ((line (read-line)))
    (cond ((empty? line)
           (show "Please type a function!")
           (get-fn))
          ((not (= (count line) 1))
           (show "You typed more than one thing!  Try again.")
           (get-fn))
          ((not (valid-fn-name? (first line)))
           (show "Sorry, that's not a function.")
           (get-fn))
          (else (function-entry (first line))))))

(define (function-entry x)
  (valid-fn-name? x))

(define (scheme-function fn-entry)
  (cadr fn-entry))

(define (arg-count fn-entry)
  (caddr fn-entry))

(define (type-predicate fn-entry)
  (cadddr fn-entry))

(define (in-domain? args fn-entry)
  (apply (type-predicate fn-entry) args))

(define (fn-name fn-entry)
  (car fn-entry))


;; ----------------------------------------------
;; 21.4 Currently, the program always gives the message "argument(s) not in
;; domain" when you try to apply a function to bad arguments. Modify the
;; program so that each record in *the-functions* also contains a specific
;; out-of-domain message like "both arguments must be numbers," then modify
;; functions to look up and print this error message along with
;; "argument(s) not in domain."

;; I would prefer to create another a-list for the 'in-domain?' function to
;; query, but (1) that's not the assignment, and (2) parallel a-lists can
;; get out of sync (and thus the program is less robust).
;;
;; Add a string to the end of each entry in the *the-functions* a-list. Then
;; add a new accessor. Finally, change the failure arm of the domain check
;; in 'functions-loop' from (show "literal") to (show (domain-error-msg
;; fn-entry), assuming the changes in 21.3 have already been made.

;; The new accessor:
(define (domain-error-msg fn-entry)
  (caddddr fn-entry))

;; The changed line in functions-loop:
(show (domain-error-msg fn-entry))

;; And a sample entry in the *the-functions*:
(list '/ / 2 can-divide? "arguments must be two numbers, the second must be non-zero")


;; ----------------------------------------------
;; 21.5 Modify the program so that it prompts for the arguments this way:
;;
;; Function: if
;; First Argument: #t
;; Second Argument: paperback
;; Third Argument: writer
;;
;; The result is: PAPERBACK
;;
;; but if there's only one argument, the program shouldn't say First:
;;
;; Function: sqrt
;; Argument: 36
;;
;; The result is 6

;; Add the tag parameter, a string with a trailing space, as a
;; parameter.

(define (get-arg tag)
  (display tag)
  (display "Argument: ")
  (let ((line (read-line)))
    (cond ((empty? line)
           (show "Please type an argument!")
           (get-arg tag))
          ((and (equal? "(" (first (first line)))
                (equal? ")" (last (last line))))
           (let ((sent (remove-first-paren (remove-last-paren line))))
             (if (any-parens? sent)
               (begin
                 (show "Sentences can't have parentheses inside.")
                 (get-arg tag))
               (map booleanize sent))))
          ((any-parens? line)
           (show "Bad parentheses")
           (get-arg tag))
          ((empty? (bf line)) (booleanize (first line)))
          (else (show "You typed more than one argument!  Try again.")
                (get-arg tag)))))

;; the max number of arguments in the table is currently three (if). If a
;; function taking four arguments is added (I can't think of one off the
;; top of my head) then 'get-args-r' will fail as 'item' checks the item
;; number against the target list size. In a real program, the *arg-tags*
;; list would be kept adjacent to *the-functions* in the source code.

(define *arg-prompt-tags* (list  "First " "Second " "Third "))

(define (get-args-r n m)
  (if (> m n)
    '()
    (let ((tag (if (= n 1)
                   ""
                   (item m *arg-prompt-tags*))))
      (let ((first (get-arg tag)))
          (cons first (get-args-r n (+ m 1)))))))

;; The binding of 'ignored' is just a quick and dirty way to catch an error
;; if *the-functions* and *arg-prompt-prefix* aren't compatible. I prefer
;; to throw the error here instead of making the user enter the first few
;; parameters and then fail part way through entry.

(define (get-args n)
  (let ((ignored (item n *arg-prompt-tags*)))
    (get-args-r n 1)))


;; ----------------------------------------------
;; 21.6 The assoc procedure might return #f instead of an a-list record.
;; How come it's okay for arg-count to take the caddr of assoc's return
;; value if (caddr #f) is an error?

;; Indeed, (caddr #f) is an error. However, 'get-fn' will not return
;; anything but the name of a function (or after the above, an entry from
;; the alist). Until the user enters a function name in the a-list, 'get-fn'
;; will not return.


;; ----------------------------------------------
;; 21.7 Why is the domain-checking predicate for the word? function:
;;
;; (lambda (x) #t)
;;
;; instead of the following procedure?
;;
;; (lambda (x) (word? x))

;; Because the domain of 'word?' is any one thing, list, sentence, number,
;; and so on. As long as one argument is provided, 'word?' will handle it
;; correctly.


;; ----------------------------------------------
;; 21.8 What is the value of the following Scheme expression?
;;
;; (functions)

;; Whatever 'functions-loop' returns when the user enters the function name
;; of 'exit'. In this case, the string "Thanks for using FUNCTIONS!".


;; ----------------------------------------------
;; 21.9 We said in the recursion chapters that every recursive procedure
;; has to have a base case and a recursive case, and that the recursive
;; case has to somehow reduce the size of the problem, getting closer to
;; the base case. How does the recursive call in get-fn reduce the size of
;; the problem?

;; The base case is binary, either #t or #f. I'm not sure I see it as
;; 'reducing' the size of the problem. We know that the last user input is
;; not valid, so we discard it and get new input.
;;
;; That's a long way of saying loop until done.


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

(print "Chapter 21 Input and Output end 01-09...")
