(define-structure lalr
  (export ;; lalr.scm
	  create-lalr-parser
	  ;; cfg.scm
	  ;(cfg :syntax)
	  ;; utilities.scm
	  print-LR-program
	  ;; engine.scm
	  pda-engine simple-parse-error-func
	  ;; semantic-actions.scm
	  null-action-evaluator scheme-action-evaluator
	  compile-actions
	  ;; examples.scm
	  tiger-grammar
	  ;; debug.scm
	  border-string old->new)

  (open list-lib string-lib tables scheme-with-scsh defrec-package)

  (files records cfg lalr utilities semantic-action engine examples debug))
