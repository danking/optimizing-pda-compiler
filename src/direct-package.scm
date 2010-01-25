;;; Directly executable parser in Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main file.

(define-structure direct-debug
  (export (assert :syntax)
	  (debug :syntax))
  (open scheme-with-scsh
	pp)
  (files direct/debug))

(define-structure direct-pda-record
  (export sexp->PDA 
	  PDA->sexp 
	  pda-static-check 
	  compile-pda 
	  ast->code
	  adder-PDA
	  adder-PDA-record
	  new-adder-PDA
	  new-adder-PDA-record
	  number-test
	  next-token
	  )
  (open scheme-with-scsh
	srfi-1 ; list=
	srfi-8 ; receive
	srfi-23 ; error
	direct-debug
	defrec-package
	ascii)
  (files direct/pda-record)
)

(define-structure direct-pda-compiler
  (export (compile-pda-to-code :syntax)
	  (compile-pda-record-to-code :syntax))
  (open list-lib scheme-with-scsh)
  (for-syntax (open direct-pda-record))  
  (begin
    (define-syntax compile-pda-record-to-code ast->code))
)