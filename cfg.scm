; The input grammar is '( terminals production+ ).
;
; terminals is '( <sym|prec>+ )
;   sym is a symbol (for undefined associativity)
;   prec is '( <left|right|non> sym+ )
;
; production is '( sym rule+ )
;   rule is <'((sym*) action) | '((prec sym) (sym*) action)>
;     action is not-interpreted
;     prec is a syntax-keyword
;
; The output grammar is
; (make-cfg '(<sym|prec>+)
;           `(,(make-rule sym (sym+) <sym| #f> action))

(define-syntax cfg-terminals
  (syntax-rules ()
    ((cfg-terminals (x ...) ())
     (list x ...))
    ((cfg-terminals (x ...) ((assoc terminals ...) rest ...))
     (cfg-terminals (x ... (make-precedence 'assoc '(terminals ...))) (rest ...)))
    ((cfg-terminals (x ...) (symbol rest ...))
     (cfg-terminals (x ... 'symbol) (rest ...)))))

(define-syntax cfg-rules
  (syntax-rules (prec)
    ((cfg-rules (x ...) ())
     (list x ...))
    ((cfg-rules (x ...) ((lhs) rest ...))
     (cfg-rules (x ...) (rest ...)))
    ((cfg-rules (x ...) ((lhs (rhs action) rules ...) rest ...))
     (cfg-rules (x ... (make-rule 'lhs 'rhs '#f 'action)) ((lhs rules ...) rest ...)))
    ((cfg-rules (x ...) ((lhs ((prec sym) rhs action) rules ...) rest ...))
     (cfg-rules (x ... (make-rule 'lhs 'rhs 'sym 'action)) ((lhs rules ...) rest ...)))))

(define-syntax cfg
  (syntax-rules (prec)
    ((cfg terminals (start whatever ...) rest ...)
     (make-cfg (cfg-terminals () terminals)
	       '*EOI*
	       'start
	       (cfg-rules () ((start whatever ...) rest ...))))))

; (define-syntax cfg
;   (syntax-rules (prec)
;     ((cfg terminals productions ...)
;      (let-syntax ((cfg-terminals
; 		   (syntax-rules ()
; 		     ((cfg-terminals (x ...) ())
; 		      (list x ...))
; 		     ((cfg-terminals (x ...) ((assoc terminals ...) rest ...))
; 		      (cfg-terminals (x ... (make-precedence 'assoc '(terminals ...))) (rest ...)))
; 		     ((cfg-terminals (x ...) (symbol rest ...))
; 		      (cfg-terminals (x ... 'symbol) (rest ...)))))

; 		  ((cfg-rules
; 		    (syntax-rules (prec)
; 		      ((cfg-rules (x ...) ())
; 		       (list x ...))
; 		      ((cfg-rules (x ...) ((lhs) rest ...))
; 		       (cfg-rules (x ...) (rest ...)))
; 		      ((cfg-rules (x ...) ((lhs (rhs action) rules ...) rest ...))
; 		       (cfg-rules (x ... (make-rule 'lhs 'rhs '#f 'action)) ((lhs rules ...) rest ...)))
; 		      ((cfg-rules (x ...) ((lhs ((prec sym) rhs action) rules ...) rest ...))
; 		       (cfg-rules (x ... (make-rule 'lhs 'rhs 'sym 'action)) ((lhs rules ...) rest ...)))))))

;        (make-cfg (cfg-terminals () terminals)
; 		 (cfg-rules () (productions ...)))))))
