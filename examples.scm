;;; Shivers' grammar for Appel's Tiger language.

(define tiger-grammar
  '((tokens ID INT STRING COMMA COLON SEMICOLON
	    LPAREN RPAREN LBRACK RBRACK LBRACE
	    RBRACE DOT 
	    ARRAY IF WHILE
	    FOR TO LET IN END BREAK NIL
	    FUNCTION VAR TYPE

	    (left UMINUS)
	    (left TIMES DIVIDE)
	    (left MINUS PLUS)
	    (left LT LE GT GE)
	    (non EQ NEQ)
	    (left AND)
	    (left OR)
	    (left OF)
	    (right THEN ELSE DO ASSIGN)

	    (error *Error)
	    (eoi *EOI))

    ;; Productions
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Un burro es un animal. La plume de ma tante.
    ;; Wo yao mai zhongguo ditu. Uni programma es uno espresso.
    (non-term program
	      (=> (exp) #f))
    
    ;; Possibly empty sequence of declarations.
    (non-term decls
	      (=> (decls decl) #f)
	      (=> ()           #f))
    
    (non-term decl
	      (=> (tydec)  #f)
	      (=> (vardec) #f)
	      (=> (fundec) #f))
    
    (non-term tydec
	      (=> (TYPE ID EQ ty) #f))
    (non-term ty
	      (=> (ID)                     #f)
	      (=> (LBRACE tyfields RBRACE) #f)
	      (=> (ARRAY OF ID)            #f))
    
    ;; Comma-separated list of type fields, possibly empty.
    (non-term tyfields
	      (=> (tyfieldscomma tyfield) #f)
	      (=> ()                      #f))
    (non-term tyfieldscomma
	      (=> (tyfieldscomma tyfield COMMA) #f)
	      (=> ()                            #f))
    
    (non-term tyfield
	      (=> (ID COLON ID) #f))
    
    (non-term vardec
	      (=> (VAR ID COLON ID ASSIGN exp) #f)
	      (=> (VAR ID ASSIGN exp)          #f))
    
    (non-term fundec
	      (=> (FUNCTION ID LPAREN tyfields RPAREN EQ exp)          #f)
	      (=> (FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp) #f))
    
    ;; This <idbrack> and the <lvalue>/<lvaluea> hackery is to eliminate 
    ;; conflict when disambiguating between
    ;;     <lvalue> : <id> [ <exp> ]
    ;;     <exp>    : <id> [ <exp> ] of <exp>
    (non-term idbrack
	      (=> (ID LBRACK exp RBRACK) #f))
    
    (non-term exp
	      (=> (IF exp THEN exp ELSE exp)		#f)
	      (=> (IF exp THEN exp)			#f)
	      (=> (WHILE exp DO exp)			#f)
	      (=> (FOR ID ASSIGN exp TO exp DO exp)	#f)
	      (=> (lvalue ASSIGN exp)			#f)
	      (=> (idbrack OF exp)			#f)
	      (=> (LET decls IN expseq END)		#f)
	      (=> (ID LPAREN args RPAREN)			#f)
	      (=> (lvalue)				#f)
	      (=> (INT)					#f)
	      (=> (STRING)				#f)
	      (=> (binop)					#f)
	      (=> (LPAREN expseq RPAREN)			#f)
	      (=> UMINUS (MINUS exp)			#f)
	      (=> (ID LBRACE fieldassigns RBRACE)		#f)
	      (=> (BREAK)					#f)
	      (=> (NIL)					#f))
    
    ;; Comma-separated list of expressions, possibly empty.
    (non-term args
	      (=> (argscomma exp)  #f)
	      (=> ()               #f))
    (non-term argscomma
	      (=> (argscomma exp COMMA) #f)
	      (=> ()                    #f))
    
    ;; Semicolon-separated list of expressions, possibly empty.
    (non-term expseq
	      (=> (expseqsemi exp)  #f)
	      (=> ()                #f))
    (non-term expseqsemi
	      (=> (expseqsemi exp SEMICOLON)  #f)
	      (=> ()                          #f))
    
    (non-term binop
	      (=> (exp PLUS exp)    #f)
	      (=> (exp MINUS exp)   #f)
	      (=> (exp TIMES exp)   #f)
	      (=> (exp DIVIDE exp)  #f)
	      (=> (exp LT exp)  	  #f)
	      (=> (exp LE exp)  	  #f)
	      (=> (exp EQ exp)  	  #f)
	      (=> (exp GE exp)  	  #f)
	      (=> (exp GT exp)      #f)
	      (=> (exp NEQ exp)     #f)
	      (=> (exp AND exp)     #f)
	      (=> (exp OR exp)      #f))
    
    ;; Comma-separated list of field assignments, possibly empty.
    (non-term fieldassigns
	      (=> (fieldassignscomma fieldassign) #f)
	      (=> ()                              #f))
    (non-term fieldassignscomma
	      (=> (fieldassignscomma fieldassign COMMA) #f)
	      (=> ()                                    #f))
    (non-term fieldassign
	      (=> (ID EQ exp) #f))
    
    (non-term lvalue
	      (=> (ID)      #f)
	      (=> (lvaluea) #f))
    (non-term lvaluea
	      (=> (ID DOT ID)                  #f)
	      (=> (idbrack)                    #f)
	      (=> (lvaluea DOT ID)             #f)
	      (=> (lvaluea LBRACK exp RBRACK)  #f))))
