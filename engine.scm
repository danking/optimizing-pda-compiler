(define-record pda-state
  name
  sr-table
  default
  goto-table
  error-state)

; get-token drop-token token-case [parse-error] cfg
(define parse/pda
  (lambda (form rename compare)
    (if (not (member (length form) '(4 5)))
	(error "Wrong number of arguments to parse/pda"))
    (let ((get-token (first form))
	  (drop-token (second form))
	  (token-case (third form))
	  (parse-error (if (= (length form) 5)
			   (fourth form)
			   `(,(rename 'lambda) (x y)
			      (,(rename 'display) "Parse Error")
			      (,(rename 'newline))
			      #f)))
	  (pda (last form)))
      (if (not (list? pda))
	  (error "PDA specification must be a list."))
      (receive (no-shift rules states start-state)
	       (parse-pda pda)
	(construct-parser get-token drop-token token-case parse-error no-shift
			  rules states start-state rename)))))

(define (parse-pda form)
  (let loop ((no-shift '()) (rules '()) (states '()) (start-state #f)
	     (error-sym #f) (form form))
    (cond ((null? form)
	   (if start-state
	       (values no-shift rules (parse-states states error-sym)
		       start-state)
	       (error "No States declared in the PDA")))
	  ((not (pair? (car form)))
	   (error "Elements of PDA langauge must be keyword-delimited lists"))
	  (else
	   (case (caar form)
	     ((COMMENT TOKENS)
	      (loop no-shift rules states start-state error-sym (cdr form)))
	     ((ERROR)
	      (cond ((not (= (length (car form)) 2))
		     (error "ERROR declartion takes 1 argument"))
		    (error-sym
		     (error "ERROR symbol redeclared"))
		    (else
		     (loop no-shift rules states start-state (cadr form)
				(cdr form)))))
	     ((NO-SHIFT)
	      (loop (append no-shift (cdar form)) rules states start-state
		    error-sym (cdr form)))
	     ((RULE)
	      (if (= (length (car form)) 5)
		  (let ((rule (receive (name non-term arity action)
				       (apply values (cdar form))
				 (if (list? arity)
				     (list name non-term (length arity) action)
				     (cdar form)))))
		    (loop no-shift (cons rule rules) states start-state
			  error-sym (cdr form)))
		  (error "RULE declarations take 4 parameters")))
	     ((STATE)
	      (if (< (length (car form)) 2)
		  (error "STATE delcarations must have a name")
		  (loop no-shift rules (cons (cdar form) states)
			(if start-state start-state (cadar form)) error-sym
			(cdr form))))
	     (else
	      (error "Unrecognized Keyword: " (caar form))))))))

(define (parse-states state-list error-sym)
  (let state-loop ((state-list state-list) (states '()))
    (if (null? state-list)
	states
	(let action-loop ((sr-table '()) (default #f) (goto-table '())
			  (error-state #f) (actions (cdar state-list)))
	  (cond ((null? actions)
		 (state-loop (cdr state-list)
			     (cons (make-pda-state (caar state-list) sr-table
						   default goto-table
						   error-state)
				   states)))
		((not (pair? (car actions)))
		 (error "Actions must be keyword-delimted lists in state:"
			(caar state-list)))
		(else
		 (case (caar actions)
		   ((COMMENT)
		    (action-loop sr-table default goto-table error-state
				 (cdr actions)))
		   ((GOTO)
		    (if (= (length (car actions)) 3)
			(action-loop sr-table default
				     (cons (cdar actions) goto-table)
				     error-state (cdr actions))
			(error "GOTO requires 2 arguments.  State:"
			       (caar state-list))))
		   ((ACCEPT)
		    (cond ((not (= (length (car actions)) 2))
			   (error "ACCEPT actions require 2 arguments.  State:"
				  (caar state-list)))
			  ((null? (cadar actions))
			   (if default
			       (error
				"Default action already declared for state:"
				(caar state-list))
			       (action-loop sr-table (list 'ACCEPT) goto-table
					    error-state (cdr actions))))
			  ((not (= (length (cadar actions)) 1))
			   (error "LR(k>1) not supported."))
			  (else
			   (action-loop (cons (car actions) sr-table) default
					goto-table error-state (cdr actions)))))
		   ((SHIFT REDUCE)
		    (cond ((not (= (length (car actions)) 3))
			   (error (string-append (symbol->string (caar actions))
						 " actions require 3 arguments"
						 ".  State:")
				  (caar state-list)))
			  ((null? (cadar actions))
			   (if default
			       (error
				"Default action already declared for state:"
				(caar state-list))
			       (action-loop sr-table (list (caar actions)
							   (caddar actions))
					    goto-table error-state
					    (cdr actions))))
			  ((not (= (length (cadar actions)) 1))
			   (error "LR(k>1) not supported."))
			  ((eqv? (caadar actions) error-sym)
			   (cond ((not (eqv? (caar actions) 'SHIFT))
				  (action-loop sr-table default goto-table
					       error-state (cdr actions)))
				 (error-state
				  (error
				   "Error action already declared for state:"
				   (caar state-list)))
				 (else
				  (action-loop sr-table default goto-table
					       (caddar actions)
					       (cdr actions)))))
			  (else
			   (action-loop (cons (car actions) sr-table) default
					goto-table error-state (cdr actions)))))
		   (else
		    (error "Unknown keyword:" (caar actions)
			   "in state:" (caar state-list))))))))))

(define (print-args get-token drop-token token-case parse-error
			  no-shift rules states start-state
			  rename)
  (display "GET-TOKEN: ") (display get-token) (newline)
  (display "DROP-TOKEN: ") (display drop-token) (newline)
  (display "TOKEN-CASE: ") (display token-case) (newline)
  (display "PARSE-ERROR: ") (display parse-error) (newline)
  (display "NO-SHIFT: ") (display no-shift) (newline)
  (display "RULES: ") (display rules) (newline)
  (display "STATES: ") (display states) (newline)
  (display "START-STATE: ") (display start-state) (newline)
  (display "RENAME: ") (display rename) (newline))

(define (construct-parser get-token drop-token token-case parse-error
			  no-shift rules states start-state
			  rename)
  (let* (;;Built-in functions
	 (%+ (rename '+))
	 (%< (rename '<))
	 (%<= (rename '<=))
	 (%= (rename '=))
	 (%apply (rename 'apply))
	 (%car (rename 'car))
	 (%case (rename 'case))
	 (%cdr (rename 'cdr))
	 (%cond (rename 'cond))
	 (%cons (rename 'cons))
	 (%drop (rename 'drop))
	 (%error (rename 'error))
	 (%if (rename 'if))
	 (%lambda (rename 'lambda))
	 (%let (rename 'let))
	 (%let* (rename 'let*))
	 (%letrec (rename 'letrec))
	 (%null? (rename 'null?))
	 (%or (rename 'or))
	 (%set! (rename 'set!))
	 (%values (rename 'values))
	 (%vector (rename 'vector))
	 (%vector-ref (rename 'vector-ref))

	 ;; Rename internal functions to avoid conlicts
	 (%get-token (rename 'get-token))
	 (%drop-token (rename 'drop-token))
	 (%parse-error (rename 'parse-error))
	 (%good-parse (rename 'good-parse))
	 (%accept (rename 'accept))
	 (%reduce (rename 'reduce))
	 (%pda-error (rename 'pda-error))
	 (%value-s (rename 'value-s))
	 (%goto-s (rename 'goto-s))
	 (%error-s (rename 'error-s))
	 (%stream (rename 'stream))
	 (%dist (rename 'dist))
	 (%token (rename 'token))

	 ;; Internal function declared here so I don't have to repeat the renames
	 (construct-state
	  (lambda (state)
	    (let ((shift (lambda (state)
			   `(,state (,%cons ,%token ,%value-s) ,%goto-s 
				    ,%error-s (,%drop-token ,%stream)
				    (,%+ ,%dist 1))))
		  (reduce (lambda (rule)
			    `(,%reduce ,%value-s ,%goto-s ,%error-s ,%stream
				       ,%dist ,rule)))
		  (accept (lambda ()
			    `(,%accept ,%value-s ,%stream))))
	      `(,(pda-state:name state)
		(,%lambda (,%value-s ,%goto-s ,%error-s ,%stream ,%dist)
		  (,%let* ((,%goto-s
			    (,%cons (,%lambda (v g e s d non-term)
				      (,%case non-term
					,@(map (lambda (goto)
						 `((,(car goto))
						   (,(cadr goto) v g e s d)))
					       (pda-state:goto-table state))
					(else (,%error "Internal Error"))))
				    ,%goto-s))
			   (,%error-s (,%cons ,(pda-state:error-state state)
					      ,%error-s))
			   (,%token (,%get-token ,%stream 1)))
		    (,token-case ,%token
		       ,@(map (lambda (action)
				`(,(cadr action) ,(case (car action)
						    ((SHIFT)
						     (shift (caddr action)))
						    ((REDUCE)
						     (reduce (caddr action)))
						    ((ACCEPT)
						     (accept)))))
			      (pda-state:sr-table state))
		       (else ,(let ((default (pda-state:default state)))
				(if default
				    (case (car default)
				      ((SHIFT) (shift (cadr default)))
				      ((REDUCE) (reduce (cadr default)))
				      ((ACCEPT) (accept)))
				    `(,%pda-error ,%value-s ,%goto-s ,%error-s
						  ,%stream ,%dist
						  ',(pda-state:name
						     state)))))))))))))

    `(,%lambda (input-stream)
       (,%letrec (;; Global values
		  (,%get-token ,get-token)
		  (,%drop-token ,drop-token)
		  (,%parse-error ,parse-error)
		  (,%good-parse #t)
		  ;; Global functions
		  (,%accept
		    (,%lambda (value-s stream)
		      (,%let loop ((result (,%if #f #f))
				   (value-s value-s))
			(,%if (,%null? value-s)
			      (,%values ,%good-parse result stream)
			      (loop (,%car value-s) (,%cdr value-s))))))
		  (,%reduce
		    (,%lambda (value-s goto-s error-s stream dist rule)
		      (,%let* ((non-terminal (,%vector-ref rule 0))
			       (arity (,%vector-ref rule 1))
			       (action (,%vector-ref rule 2))
			       (goto-s (,%drop goto-stack arity))
			       (error-s (,%drop goto-stack arity)))
			(,%let loop ((i 0) (value-s value-s) (call-values '()))
			  (,%if (,%< i arity)
				(loop (,%+ i 1) (,%cdr value-stack)
				      (,%cons (,%car value-stack) call-values))
				((,%car error-stack)
				 (,%cons (,%apply action call-values)
					 value-stack)
				 error-stack stream error-distance
				 non-terminal))))))
		  (,%pda-error
		    (,%lambda (value-s goto-s error-s stream distance state)
		      (,%set! clean-parse #f)
		      (,%let* ((unspecific (,%if #f #f))
			       (token (,%get-token stream 1))
			       (stream (,%cond ((,%= distance 0)
						(,token-case token
						  (,no-shift #f)
						  (else (,%drop-token stream))))
					       ((,%or (,%<= distance 3)
						      (,%parse-error state
								     token))
						stream)
					       (else
						#f))))
			(,%if stream
			      (,%let loop ((value-s value-s) (goto-s goto-s)
					   (error-s error-s))
				(,%cond ((,%null? error-s)
					 (,%values #f unspecific stream))
					((,%car error-s)
					 ((,%car error-s)
					  (,%cons unspecific value-s)
					  goto-s error-s stream 0))
					(else
					 (loop (,%cdr value-s) (,%cdr goto-s)
					       (,%cdr error-s)))))
			      (,%values #f unspecific stream)))))

		  ,@(map (lambda (rule)
			   `(,(car rule) (,%vector ',(cadr rule)
						   ,@(cddr rule))))
			 rules)

		  ,@(map construct-state states))
	 (,start-state '() '() '() input-stream #f)))))
