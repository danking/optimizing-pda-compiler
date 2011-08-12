
(define-structure olins-defrec-macro
  (export define-record)
  (for-syntax (open scheme error-package srfi-8 records))
  (open scheme records record-types)
  (files defrec))

(define-structure lalr-macro
  (export ;; semantic-action.scm
	  compile-actions-macro-strict
	  compile-actions-macro-cps
	  ;; cfg.scm
	  cfg->pda-macro-strict
	  cfg->pda-macro-cps
	  ;; engine.scm
	  parse/pda-macro)

  (for-syntax (open olins-defrec-macro))
  (for-syntax (files cfg))
  (open srfi-1 error-package tables scheme-with-scsh olins-defrec-macro)
  (files semantic-action lalr cfg engine))

(define-structure lalr-temp
  (export (compile-actions :syntax) (compile-actions-cps :syntax)
	  (cfg->pda :syntax) (cfg->pda-cps :syntax)
	  (parse/pda :syntax))
  (open srfi-1 scheme-with-scsh error-package)
  (for-syntax (open lalr-macro))
  (begin
    (define-syntax compile-actions compile-actions-macro-strict)
    (define-syntax compile-actions-cps compile-actions-macro-cps)
    (define-syntax cfg->pda cfg->pda-macro-strict)
    (define-syntax cfg->pda-cps cfg->pda-macro-cps)
    (define-syntax parse/pda parse/pda-macro)))

(define-structure lalr
  (export (compile-actions :syntax) (compile-actions-cps :syntax)
          (cfg->pda :syntax) (cfg->pda-cps :syntax)
          (parse/pda :syntax)
          (parse/cfg :syntax) (parse/compiled-cfg :syntax)
          (convert-to-pda :syntax) (compile+convert-to-pda :syntax))
  (open scheme lalr-temp)
  (files macro-glue))
