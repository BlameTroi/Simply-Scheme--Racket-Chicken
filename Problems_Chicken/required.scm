;;; required.scm -- standard environment setup for _Simply_Scheme_


;;; Commentary:

;; The authors provide a standard set of abstractions and a few sample
;; projects. The standard abstractions are a consistent requirement as
;; you work through the text.
;;
;; I have found it helpful to load a couple of other libraries.
;;
;; Troy Brumley -- blametroi@gmail.com -- February 2025.


;;; Environment Requirements:

;; I've only tried Chez, Guile, and Chicken. The first two don't work.
;; Chicken 5 supports the Scheme language as in R5RS which was the
;; active standard when _Simply_Scheme_ was published.


;;; The rational behind the authors' abstractions:

;; The authors provide an abstraction layer that I agree helps someone
;; new to Scheme and Functional Programming manage all the new ideas.
;; Lists and atoms are sentences and words, and typing of the atoms
;; (words) is very flexible, as you would see in Logo or Rexx. The
;; car/cdr and basic iteration are first, last, keep, every,
;; accumulate, and so on.
;;
;; For the beginner, there are new and big ideas to learn when they
;; start programming:
;;
;; data types -- the text abstracts them to sentences, words, strings,
;;     and numbers. (+ "7" 3 '8) works in their framework.
;;
;; literalness of programming -- the idea that the computer does what
;;     it was told to do and not what the programmer meant is an early
;;     source of frustration. By abstracting some of the details at
;;     first, the opportunity for the "told vs meant" bugs is reduced
;;     and the possible causes of the difference are reduced,
;;     speeding debugging and learning.
;;
;; control structures -- are limited and common idioms are given names
;;     that make sense without forcing the learning to cope with the
;;     details of car/cdr recursion.
;;
;; The authors are quite clear to the reader about what they are
;; doing, and the abstractions begin to be lifted about halfway
;; through the text. Once the reader is used to functional/scheme
;; conventions they are ready to learn the standard terminology.
;;
;; Expecting the reader to climb only one learning curve at time
;; improves the learning experience.
;;
;; I believe SICP takes a similar approach, but not to this level.


;;; Code:


;;; simply.scm -- the _Simply_Scheme_ abstraction layer:

(load "../From_Text/simply.scm")
;;(load "simply.so")

;;; Additional environment setup:

;; Chicken makes many packages available via their "chicken-install". I
;; use two regularly: SRFI-78 for testing, and trace for a simple
;; execution trace.
;;
;; I don't know the correct commands to grab these for other Scheme
;; implementations.


;;; SRFI-78 -- extremely lightweight testing:

;; My personal style of development is to have many small tests. The
;; framework in SRFI-78 is very lightweight, requiring nor providing
;; for any real setup. This gives enough "test first" goodness without
;; significant overhead. It can be as simple as "(check (some
;; function) => expected result)"

(import srfi-78)


;;; trace -- run time tracing:

;; Around chapter 13 the authors begin to expose details of recursion
;; and they use trace to do so. The Chicken trace is pretty simple and
;; after loading the text's directions just work for tracing in the
;; REPL.

(import trace)


;;; My extensions:

;; Some common code I've repeated (or expected to repeat) across
;; multiple problem sets.

(load "my-helpers.scm")


;;; Examples and projects:

;; Any of the following can be uncommented to work with that
;; particular set of procedures in the text. Or more simply, just type
;; the load command into the REPL after this file has been loaded.
;;
;; In my limited testing, these don't all play together nicely and the
;; authors did not mean for anyone to load all of these at once. I
;; treat simply.scm as a prerequisite for any one of the following, and if I
;; switch from say functions.scm to ttt.scm, I restart the REPL
;; session.
;;
;; (load "functions.scm")
;; (load "spread.scm")
;; (load "match.scm")
;; (load "ttt.scm")
;; (load "database.scm")

;;; required.scm ends here.
