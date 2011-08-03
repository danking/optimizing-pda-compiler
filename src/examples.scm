(define (get-token stream lookahead)
  (if (null? stream)
      '*EOF*
      (car stream)))

(define-syntax token-case
  (syntax-rules ()
    ((token-case token rest ...)
     (case (let ((t token))
	     (if (number? t)
		 'NUM
		 (case t
		   ((#\() 'L-PAREN)
		   ((#\)) 'R-PAREN)
		   ((#\;) 'SEMICOLON)
		   ((+) 'PLUS)
		   ((-) 'MINUS)
		   ((*) 'TIMES)
		   ((/) 'DIVIDE)
		   ((*EOF*) t)
		   (else 'LEXER-ERROR))))
       rest ...))))

(define (parse-error state token)
  (display "Parse error in state: ") (display state)
  (display " on token: ") (display token) (newline)
  #t)

(define calculator
  (parse/cfg get-token cdr token-case parse-error
    ((tokens NUM L-PAREN R-PAREN SEMICOLON
	     (left TIMES DIVIDE)
	     (left PLUS MINUS)
	     (error *ERROR*)
	     (eos *EOF*))

     (non-term program
	       (=> (s-list)				s-list)
	       (=> (s-list exp)				(begin (display exp) (newline) (cons exp s-list)))
	       (=> (s-list *ERROR*)			(cons (if #f #f) s-list)))
     (non-term s-list
	       (=> ()					'())
	       (=> (s-list statement)			(cons statement s-list)))
     (non-term statement
	       (=> (exp SEMICOLON)			(begin (display exp) (newline) exp))
	       (=> (*ERROR* SEMICOLON)			(if #f #f)))
     (non-term exp
	       (=> (NUM)				NUM)
	       (=> (exp PLUS exp)			(+ exp-1 exp-2))
	       (=> (exp MINUS exp)			(- exp-1 exp-2))
	       (=> ((expA exp) TIMES (expB exp))	(* expA expB))
	       (=> ((expA exp) DIVIDE exp)		(quotient expA exp))
	       (=> (L-PAREN exp R-PAREN)		exp)))))
