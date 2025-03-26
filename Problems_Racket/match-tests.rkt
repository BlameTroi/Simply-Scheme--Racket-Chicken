;; required setup ...

;; for chicken:
;; (load "required.scm")
;; (load "match.scm")

;; for racket:
;; #lang simply-scheme
;; (require srfi/78)

;; clear test counters
(check-reset!)
(check-set-mode! 'report-failed)

(check (match '(* me *) '(love me do)) => '())
(check (match '(? me) '(love me)) => '())
(check (match '(? me) '(me)) => '())
(check (match '(! me) '(me)) => 'failed)         ;; ! = 1 word
(check (match '(! me) '(x me)) => '())
(check (match '(love ? do) '(love you do)) => '())
(check (match '(love * do) '(love you do)) => '())
(check (match '(love ! do) '(love you do)) => '())
(check (match '(love & do) '(love you do)) => '())
(check (match '(& me &) '(love me do)) => '())
(check (match '(&x me &y) '(love me do)) => '(x love ! y do !)) ;; captures
(check (match '(*x me *y) '(love me do)) => '(x love ! y do !)) ;; captures
(check (match '(*x me *y) '(me do)) => '(x ! y do !)) ;; captures
(check (match '(*x me *y) '(love me)) => '(x love ! y !))
(check (match '(*x me *y) '(me)) => '(x ! y !))
(check (match '(love *) '(love me do)) => '())
(check (match '(love &) '(love me do)) => '())
(check (match '(love &) '(love me)) => '())
(check (match '(love *) '(love me)) => '())
(check (match '(&x me &y) '(love me do)) => '(x love ! y do !))
(check (match '(&x me &y) '(he loves me he do)) => '(x he loves ! y he do !))

;; report and clear
(check-report)
(check-reset!)
(check-set-mode! 'report-failed)

