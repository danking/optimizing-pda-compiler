(define-syntax cfg->pda
  (syntax-rules (comment tokens no-shift end-of-parse non-term =>
		 non right left eos error)
    ((cfg->pda cfg)
     (cfg->pda-hp () #f #f () () () cfg))))

(define-syntax cfg->pda-hp
  (syntax-rules (comment tokens no-shift end-of-parse non-term =>
		 non right left eos error)
    ((cfg->pda-hp terminals err eoi eop ns rules
		  ((comment whatever ...) rest ...))
     (cfg->pda-hp terminals err eoi eop ns rules (rest ...)))

;     ((cfg->pda-hp terminals err eoi eop (ns ...) rules
; 		  ((no-shift terms ...) rest ...))
;      (cfg->pda-hp terminals err eoi eop (ns ... terms ...) rules (rest ...)))

;     ((cfg->pda-hp terminals err eoi (eop ...) ns rules
; 		  ((end-of-parse terms ...) rest ...))
;      (cfg->pda-hp terminals err eoi (eop ... terms ...) ns rules (rest ...)))

;     ((cfg->pda-hp terminals #f eoi eop ns rules
; 		  ((tokens (error ident) token-decl ...) rest ...))
;      (cfg->pda-hp terminals ident eoi eop ns rules ((tokens token-decl ...) rest ...)))
;     ((cfg->pda-hp terminals err eoi eop ns rules
; 		  ((tokens (error ident) token-decl ...) rest ...))
;      (error "Multiple error token declarations."))

;     ((cfg->pda-hp terminals err #f eop ns rules
; 		  ((tokens (eos ident) token-decl ...) rest ...))
;      (cfg->pda-hp terminals err ident eop ns rules ((tokens token-decl ...) rest ...)))
;     ((cfg->pda-hp terminals err eoi eop ns rules
; 		  ((tokens (eos ident) token-decl ...) rest ...))
;      (error "Multiple End-of-Stream token declarations."))

;     ((cfg->pda-hp (terminals ...) err eoi eop ns rules
; 		  ((tokens token-decl1 token-decl ...) rest ...))
;      (cfg->pda-hp (terminals ... token-decl1) err ident eop ns rules ((tokens token-decl ...) rest ...)))

;     ((cfg->pda-hp terminals err eoi eop ns rules
; 		  ((tokens) rest ...))
;      (cfg->pda-hp terminals err eoi eop ns rules (rest ...)))

;     ((cfg->pda-hp terminals err eoi eop ns rules
; 		  ((non-term ident nt-clause1 nt-clause ...) rest ...))
;      (process-nt-dec (terminals err eoi eop ns rules rest ...) ident () nt-clause1 nt-clause ...))

;     ((cfg->pda-hp terminals err eoi eop ns rules ())
;      (cfg->pda/args terminals err eoi eop ns rules))
))

(define-syntax process-nt-dec
  (syntax-rules (comment tokens no-shift end-of-parse non-term =>
		 non right left eos error)
    ((process-nt-dec args nt (clauses ...) (comment whatever ...) nt-clause ...)
     (process-nt-dec args nt (clauses ...) nt-clause ...))

    ((process-nt-dec args nt (clauses ...) (=> stuff ...) nt-clause ...)
     (process-nt-dec args nt (clauses ... (stuff ...)) nt-clause ...))

    ((process-nt-dec (terminals err eoi eop ns (rules ...) rest ...) nt (clauses ...))
     (cfg->pda-hp terminals err eoi eop ns (rules ... (nt clauses ...)) rest ...))))

(define-syntax cfg->pda/args
  (lambda (form rename compare)
    form))
