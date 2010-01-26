(define-syntax parse/cfg
  (syntax-rules ()
    ((parse/cfg get-token drop-token token-case cfg)
     (compile-actions-cps cfg parse/compiled-cfg get-token drop-token token-case))
    ((parse/cfg get-token drop-token token-case parse-error cfg)
     (compile-actions-cps cfg parse/compiled-cfg get-token drop-token token-case parse-error))))

(define-syntax parse/compiled-cfg
  (syntax-rules ()
    ((parse/cfg-temp get-token drop-token token-case cfg)
     (cfg->pda-cps cfg parse/pda get-token drop-token token-case))
    ((parse/cfg-temp get-token drop-token token-case parse-error cfg)
     (cfg->pda-cps cfg parse/pda get-token drop-token token-case parse-error))))

(define-syntax convert-to-pda
  (syntax-rules ()
    ((get-pda cfg)
     (cfg->pda-cps cfg pda))))

(define-syntax parser
  (syntax-rules ()
    ((parser cfg)
     (compile-actions-cps cfg convert-to-pda))))
