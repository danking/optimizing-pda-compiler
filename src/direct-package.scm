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
  (export pda
	  adder-PDA
	  new-adder-PDA
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

(define-structure direct-pda-record
  (export pda
	  adder-PDA
	  new-adder-PDA
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
  (export (pda :syntax)
	  next-token)
  (open list-lib scheme-with-scsh direct-pda-record)
  (for-syntax (open direct-pda-record)) 
  (begin
    (define-syntax pda pda))
) 


(define-structure lalr-macro
  (export ;; semantic-action.scm
	  compile-actions-macro-strict
	  compile-actions-macro-cps
	  ;; cfg.scm
	  cfg->pda-macro-strict
	  cfg->pda-macro-cps
	  ;; engine.scm
	  parse/pda-macro)

  (open list-lib string-lib tables scheme-with-scsh defrec-package)
  (files semantic-action lalr cfg engine))

(define-structure lalr-temp
  (export (compile-actions :syntax) (compile-actions-cps :syntax)
	  (cfg->pda :syntax) (cfg->pda-cps :syntax)
	  (parse/pda :syntax))
  (open list-lib scheme-with-scsh)
  (for-syntax (open lalr-macro))  
  (begin
    (define-syntax compile-actions compile-actions-macro-strict)
    (define-syntax compile-actions-cps compile-actions-macro-cps)
    (define-syntax cfg->pda cfg->pda-macro-strict)
    (define-syntax cfg->pda-cps cfg->pda-macro-cps)
    (define-syntax parse/pda parse/pda-macro)))

(define-structure lalr
  (export next-token
	  (compile-actions :syntax) 
	  (compile-actions-cps :syntax)
	  (cfg->pda :syntax) 
	  (cfg->pda-cps :syntax)
	  (parse/pda :syntax)
	  (parse/cfg :syntax) 
	  (parse/compiled-cfg :syntax)
	  (convert-to-pda :syntax) 
	  (parser :syntax))
  (open scheme lalr-temp direct-pda-compiler)
  (files direct/direct-parser-glue))