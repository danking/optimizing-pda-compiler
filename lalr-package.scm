(define-structure lalr
  (export ; lalr.scm
	  create-lalr-parser

	  make-cfg cfg?
	  cfg:terminals cfg:eoi cfg:error cfg:start cfg:rules
	  set-cfg:terminals set-cfg:eoi set-cfg:error set-cfg:start
	  set-cfg:rules

	  make-cfg-precedence cfg-precedence?
	  cfg-precedence:associativity cfg-precedence:terminals
	  set-cfg-precedence:associativity set-cfg-precedence:terminals

	  make-cfg-rule cfg-rule?
	  cfg-rule:left-side cfg-rule:right-side cfg-rule:precedence
	  cfg-rule:action
	  set-cfg-rule:left-side set-cfg-rule:right-side set-cfg-rule:precedence
	  set-cfg-rule:action

	  make-LR-program LR-program?
	  LR-program:terminals LR-program:eoi LR-program:error LR-program:rules
	  LR-program:states
	  set-LR-program:terminals set-LR-program:eoi set-LR-program:error
	  set-LR-program:rules set-LR-program:states

	  make-LR-rule LR-rule?
	  LR-rule:left-side LR-rule:right-side LR-rule:action
	  set-LR-rule:left-side set-LR-rule:right-side set-LR-rule:action

	  make-LR-state LR-state?
	  LR-state:shift-reduce-table LR-state:goto-table LR-state:items
	  set-LR-state:shift-reduce-table set-LR-state:goto-table
	  set-LR-state:items
	  ; cfg.scm
	  cfg
	  ; utilities.scm
	  print-LR-program
	  ; engine.scm
	  pda-engine simple-parse-error-func
	  ; semantic-actions.scm
	  null-action-evaluator scheme-action-evaluator
	  compile-actions
	  ; examples.scm
	  tiger-grammar
	  ; debug.scm
	  border-string old->new)

  (open list-lib string-lib tables scheme-with-scsh defrec-package)

  (files cfg lalr utilities semantic-action engine examples debug))
