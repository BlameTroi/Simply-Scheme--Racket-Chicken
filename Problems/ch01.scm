;;; Simply Scheme
;;; Problems from Simply Scheme, running on Racket with the Simply Scheme
;;; language setting.

;; Troy Brumley, blametroi@gmail.com, early 2025.

;;; Chapter 10 tic tac toe walkthrough


;; The problems I worked with lightweight testing to verify results
;; via srfi-78. This file should load into a new Scheme repl (only
;; Chicken tested) and report no failures.

;;; Set up the standard environment:

;; Do this manually via (load "required.scm"), which currently
;; loads:
;;
;; (load "simply.scm")
;; (import srfi-78)
;; (import trace)
;; (load "my-helpers.scm")

;;; Set unit testing reporting levels and clear any dangling
;;; totals.

;; This should already be done, but just in case:

(check-reset!)
(check-set-mode! 'report-failed)

;;; Problem set:

(print "Chapter 10 nothing to do...")

;; ----------------------------------------------

;; Actually there's nothing to do here. The chapter is a code walk through
;; of `ttt': tic-tac-toe implemented in Scheme with strategy for the
;; computer player. There are a few trivial questions and then a joke (?)
;; problem to "now implement a game of chess". :)

;; ----------------------------------------------

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; And that's the end of this section. Report test results and reset
;;; counters.

(check-report)
(check-reset!)
(check-set-mode! 'report-failed)
