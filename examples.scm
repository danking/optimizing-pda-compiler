;;; Shivers' grammar for Appel's Tiger language.

(define tiger-grammar (cfg
 ;; Terminals/tokens & precedence declarations
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Note the RIGHT declaration below. IF, WHILE, FOR & := expressions 
 ;; have "tail recursive" syntax -- each one's defining production is of
 ;; the form 
 ;;     this-exp : stuff... stuff... stuff... exp
 ;; Making them lowest precedence means that final exp is made as large
 ;; as possible. E.g., we want to parse
 ;;     while test do x + y
 ;; as
 ;;     while test do (x+y)
 ;; not
 ;;     (while test do x) + y
 ;; Using RIGHT instead of LEFT resolves the dangling-else ambiguity
 ;; in the desired direction, as well.
 (ID INT STRING COMMA COLON SEMICOLON
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
  (right THEN ELSE DO ASSIGN))

 ;; Productions
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Un burro es un animal. La plume de ma tante.
 ;; Wo yao mai zhongguo ditu. Uni programma es uno espresso.
 (program ((exp) #f))
 
 ;; Possibly empty sequence of declarations.
 (decls ((decls decl) #f)
	(()           #f))
 
 (decl ((tydec)  #f)
       ((vardec) #f)
       ((fundec) #f))
 
 (tydec ((TYPE ID EQ ty) #f))
 (ty ((ID)                     #f)
     ((LBRACE tyfields RBRACE) #f)
     ((ARRAY OF ID)            #f))
 
 ;; Comma-separated list of type fields, possibly empty.
 (tyfields ((tyfieldscomma tyfield) #f)
	   (()                      #f))
 (tyfieldscomma ((tyfieldscomma tyfield COMMA) #f)
		(()                            #f))
 
 (tyfield ((ID COLON ID) #f))
 
 (vardec ((VAR ID COLON ID ASSIGN exp) #f)
	 ((VAR ID ASSIGN exp)          #f))
 
 (fundec ((FUNCTION ID LPAREN tyfields RPAREN EQ exp)          #f)
	 ((FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp) #f))
 
 ;; This <idbrack> and the <lvalue>/<lvaluea> hackery is to eliminate 
 ;; conflict when disambiguating between
 ;;     <lvalue> : <id> [ <exp> ]
 ;;     <exp>    : <id> [ <exp> ] of <exp>
 (idbrack ((ID LBRACK exp RBRACK) #f))
 
 (exp ((IF exp THEN exp ELSE exp)        #f)
      ((IF exp THEN exp)                 #f)
      ((WHILE exp DO exp)                #f)
      ((FOR ID ASSIGN exp TO exp DO exp) #f)
      ((lvalue ASSIGN exp)               #f)
      ((idbrack OF exp)           	 #f)
      ((LET decls IN expseq END)  	 #f)
      ((ID LPAREN args RPAREN)    	 #f)
      ((lvalue)  			 #f)
      ((INT)     			 #f)
      ((STRING)  			 #f)
      ((binop)   			 #f)
      ((LPAREN expseq RPAREN)            #f)
      ((prec UMINUS) (MINUS exp)         #f)
      ((ID LBRACE fieldassigns RBRACE)   #f)
      ((BREAK)                           #f)
      ((NIL)                             #f))
 
 ;; Comma-separated list of expressions, possibly empty.
 (args ((argscomma exp)  #f)
       (()               #f))
 (argscomma ((argscomma exp COMMA) #f)
	    (()                    #f))
 
 ;; Semicolon-separated list of expressions, possibly empty.
 (expseq ((expseqsemi exp)  #f)
	 (()                #f))
 (expseqsemi ((expseqsemi exp SEMICOLON)  #f)
	     (()                          #f))
 
 (binop ((exp PLUS exp)    #f)
	((exp MINUS exp)   #f)
 	((exp TIMES exp)   #f)
 	((exp DIVIDE exp)  #f)
 	((exp LT exp)  	   #f)
 	((exp LE exp)  	   #f)
 	((exp EQ exp)  	   #f)
 	((exp GE exp)  	   #f)
 	((exp GT exp)      #f)
 	((exp NEQ exp)     #f)
 	((exp AND exp)     #f)
 	((exp OR exp)      #f))
 
 ;; Comma-separated list of field assignments, possibly empty.
 (fieldassigns ((fieldassignscomma fieldassign) #f)
	       (()                              #f))
 (fieldassignscomma ((fieldassignscomma fieldassign COMMA) #f)
		    (()                                    #f))
 (fieldassign ((ID EQ exp) #f))
 
 (lvalue ((ID)      #f)
	 ((lvaluea) #f))
 (lvaluea ((ID DOT ID)                  #f)
	  ((idbrack)                    #f)
	  ((lvaluea DOT ID)             #f)
	  ((lvaluea LBRACK exp RBRACK)  #f))))
