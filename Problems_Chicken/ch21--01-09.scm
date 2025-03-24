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
;; Change 'get-fn' to return the entry in the a-list instead of the name.
;; Then functions-loop will invoke the accessors as before, but passing the
;; entry instead of the name. In the accessors, remove the 'assoc' wrapping
;; leaving jsut the c????r procedure call. Also, create an 'fn-name'
;; accessor function.

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

;; I would prefer to create another alist for the 'in-domain?' function to
;; query, but (1) that's not the assignment, and (2) parallel alists can
;; get out of sync (and thus the program is less robust).
;;
;; Add a string to the end of each entry in the *the-functions* alist. Then
;; add a new accessor. Finally, change the failurearm of the domain check
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


(define (get-arg tag)
  (if tag (display tag) '())
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

;; the max number of arguments in the table is currently three (if).

(define a-tags (list  (list 3 "First " "Second " "Third ")
                    (list 2 "First " "Second ")))

(define (tag-for n m) ;; n=>number of args  m=>number this arg
  (let ((tags (assoc n a-tags)))
    (if (or (< m 1) (> m n) (not tags))
      #f
      (cadr tags))))

(define (get-args-r n m)
  (if (= m 0)
    '()
    (let ((first (get-arg (tag-for n m))))
          (cons first (get-args-r n (- m 1))))))

(define (get-args n)
  (get-args-r n n))

;; 2 2
;; 2 1
;;
;; 1 1
;;
;; 3 3
;; 3 2
;; 3 1


;; ----------------------------------------------
;; 21.6 The assoc procedure might return #f instead of an a-list record.
;; How come it's okay for arg-count to take the caddr of assoc's return
;; value if (caddr #f) is an error?

;; Indeed, (caddr #f) is an error. However, 'get-fn' will not return
;; anything but the name of a function (or after the above, an entry from
;; the alist). Until the user enters a function name in the alist, 'get-fn'
;; will not return.


;; ----------------------------------------------
;; 21.7 Why is the domain-checking predicate for the word? function
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
;; 'exit'. In this case, the string "Thanks for using FUNCTIONS!".


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
