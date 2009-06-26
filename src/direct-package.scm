;;; Directly executable parser in Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main file.

(define-structure direct-debug
  (export (assert :syntax)
	  (debug :syntax)
	  adder-PDA)
  (open scheme-with-scsh
	pp)
  (files direct/debug))

(define-structure direct-pda-record
  (export sexp->PDA PDA->sexp)
  (open scheme-with-scsh
	srfi-1 ; list=
	srfi-8 ; receive
	srfi-23 ; error
	direct-debug
	defrec-package
	ascii)
  (files direct/pda-record)
)
