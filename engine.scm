; This is the engine for the parser.  No sanity checking is done on any of the
; arguments.  They are all assumed to be correct and if this is not the case,
; this function can blow-up.
;
; Arguments:
; program = The LR-program record that defines the parser table.
; action-func = A function that will handle the semantic actions for the actions
;     in 'program.  The function should take two arguments.  The first is the
;     action to execute (taken directly out of 'program).  The second is the
;     list of values for each element in that rule.  This list will have one
;     element for each right-hand-side symbol in the rule.
;     semantic-actions.scm has a few sample semantic action functions.
; lexer-func = A function that returns terminals symbols.  This function should
;     take no arguments and return 2 values.  The first value is a terminal
;     symbol and the second value is the semantic value for that terminal.
; error-func = This function will be called whenever the parser encounters a
;     syntax error.  It should take 3 arguments.  The first is the current
;     lookahead token.  The second is the list of tokens that are valid in the
;     current state.  The last argument is the current state number.  The
;     function should return either true or false which indicates whether the
;     parser should continue parsing or not.
;     A simple error-func is available called 'simple-parse-error-func
(define (pda-engine program action-func lexer-func error-func)
  (let ((terminals (LR-program:terminals program))
	(eoi-symbol (LR-program:eoi program))
	(error-symbol (LR-program:error program))
	(rules (LR-program:rules program))
	(states (LR-program:states program)))
    (let lexer-loop ((state-stack '(0)) (value-stack '()) (error-distance 0))
      (receive
       (lookahead value) (lexer-func)
       (let reduction-loop ((state-stack state-stack) (value-stack value-stack))
	 (let* ((state (vector-ref states (car state-stack)))
		(sr-table (LR-state:shift-reduce-table state))
		(action (lookup-action lookahead sr-table)))
	   (cond ((eq? action #f)
		  (if (or (> error-distance 0)
			  (error-func lookahead (map car sr-table)
				      (car state-stack)))
		      (let error-loop ((state-stack state-stack)
				       (value-stack value-stack))
			(let ((action (lookup-action
				       error-symbol
				       (LR-state:shift-reduce-table
					(car state-stack)))))
			  (cond ((and action (eq? (cadr action) 'shift))
				 (lexer-loop (cons (caddr action) state-stack)
					     (cons #f value-stack) 3))
				((null? (cdr state-stack))
				 #f)
				(else
				 (error-loop (cdr state-stack)
					     (cdr value-stack))))))
		      #f))
		 ((eq? (car action) 'shift)
		  (lexer-loop (cons (cadr action) state-stack)
			      (cons value value-stack)
			      (- error-distance 1)))
		 ((eq? (car action) 'reduce)
		  (let* ((rule (vector-ref rules (cadr action)))
			 (num-to-pop (vector-length (LR-rule:right-side rule)))
			 (new-value-stack (run-action value-stack
						      num-to-pop
						      action-func
						      (LR-rule:action rule))))
		    (reduction-loop
		     (cons (cadr (lookup-action (LR-rule:left-side rule)
						(LR-state:goto-table state)))
			   (drop state-stack num-to-pop))
		     new-value-stack)))
		 (else ;Assume accept
		  (car value-stack)))))))))

(define (lookup-action lookahead action-list)
  (let loop ((action-list action-list) (default #f))
    (cond ((null? action-list)
	   default)
	  ((eq? (caar action-list) lookahead)
	   (cdar action-list))
	  ((eq? (caar action-list) #t)
	   (loop (cdr action-list) (cdar action-list)))
	  (else
	   (loop (cdr action-list) default)))))

(define (run-action value-stack num-to-pop action-evaluator action)
  (let loop ((to-pop num-to-pop) (stack-top '()) (value-stack value-stack))
    (if (> to-pop 0)
	(loop (- to-pop 1) (cons (car value-stack) stack-top) (cdr value-stack))
	(cons (action-evaluator action stack-top) value-stack))))

(define (simple-parse-error-func lookahead expected state)
  (display "Parse Error in state: ") (display state) (newline)
  (display "  Found: ") (display lookahead) (newline)
  (display "  Expected one of:") (for-each (lambda (item)
					     (display " ")
					     (display item))
					   expected)
  (newline)
  #t)
