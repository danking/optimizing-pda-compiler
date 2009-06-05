; Make a directly-executable parser from a PDA record

#|
; pull GOTOs out of states when building records -- each goto has a NonTerm FromState ToState
;(define-struct goto (nonterm from to)
;(make-goto 'EXP s14 s3)
;(STATE s14
	...
	(GOTO exp s3))

|#


;================================
;= Hard-coded parser and source =
;================================

; A StackFrame is a (make-sframe Symbol InputVal StateCall)
(define-record-type sframe :sframe
  (make-sframe type val state)
  sframe?
  (type  sframe-type)
  (val   sframe-val)
  (state sframe-state))

;; An InputVal is one of Number, Symbol, String

;; This adder takes inputs of the form "33+12" and adds them.
(define adder
  (compile+convert-to-pda
   ((tokens DIGIT
	    (left PLUS)
	    (error *ERROR*)
	    (eos *EOF*))
    (non-term exp
	      (=> (num PLUS num)   (+ num-1 num-2)))
    (non-term num
	      (=> (num DIGIT)      (+ (* num 10)
				      (- (char->ascii DIGIT)
					 (char->ascii #\0))))
	      (=> ()               0))
    )))

;; adder-source : Number String -> (values Symbol Character Number)
;; Produce characters from a string source.
(define (adder-source state stream)
  (cond ((> state (string-length stream)) (error "Lexer error: index too large."))
	((= state (string-length stream)) (values '*EOF*
						  '_
						  state))
	(else (let* ((look (string-ref stream state)))
		  (case char
		    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 'DIGIT)
		    ((#\+) 'PLUS)
		    (else (error "Invalid character in input stream.")))))))

;; adder-parser : Source Number -> Number
;; Parse adder input using a lexing source.
(define (adder-parser source lex-state)
  (letrec 
      (
       ; Rules are copied verbatim. They are what is preserved entirely in the CFG->Scheme process.
       (RULE-1 #f)
       (RULE-2 (lambda (num-1 PLUS num-2)
		 (+ num-1 num-2)))
       (RULE-3 (lambda (num DIGIT)
		 (+ (* num 10)
		    (- (char->ascii DIGIT)
		       (char->ascii #\0)))))
       (RULE-4 (lambda () 0))

       ; Accept and error states
       (accept (lambda (value)
		 value)) ; kind of pointless
       (error (lambda (stack lex-state)
		(error "Parsing failed."))) ; TODO: something interesting
       
       ; Goto lambdas: One for each non-terminal, pulled from states
       (GOTO-num (lambda (old)
		   (cond ((eq? old STATE-0) STATE-2)
			 ((eq? old STATE-4) STATE-5)
			 (else (error "Bad reduction on 'num.")))))
       (GOTO-exp (lambda (old)
		   (cond ((eq? old STATE-0) STATE-1)
			 (else (error "Bad reduction on 'exp.")))))
       
       ; State-lambdas take a stack and a lex state and either shift or reduce
       (STATE-0 (lambda (stack lex-state)
		  ; No lookahead required to choose an action.

		  ; Reduce: Nullary
		  (let* ((args '()))
		    
		    (let* ((tos (car stack))
			   (bottom (sframe-state tos))) ; grab the state that we reduced to
		    
        	      ; r4 is known to produce a 'num
		      (let* ((produced (RULE-4)) ; run rule against stuff from stack
			     (stack (cons (make-sframe 'num produced bottom) ; shift the semantic value onto the stack
					  stack))
			     (shift-to (GOTO-num bottom))) ; lookup what state follows the reduction
			(shift-to stack lex-state)))))) ; shift to next state
       
       ; This one accepts unconditionally.
       (STATE-1 (lambda (stack lex-state))
		; No lookahead required

		; Accept
		(accept (sframe-val (car stack))))
       
       ; Several shifts
       (STATE-2 (lambda (stack lex-state)
		  ; First, grab the lookahead
		  (receive (toktype semval lex-state) (source lex-state)
			   (case toktype
			     ; perform a shift
			     ((DIGIT)
			      (STATE-3 (cons (make-sframe toktype semval STATE-2) ; put new value and self on stack
					     stack)
				       lex-state))
			     
			     ; perform a shift
			     ((PLUS)
			      (STATE-4 (cons (make-sframe toktype semval STATE-2) ; put new value and self on stack
					     stack)
				       lex-state))
			     
			     ; otherwise start recovery... or whatever
			     (else (error (string-append "Parser bombed reading lexer state "
							 (number->string lexer-state)))) ; TODO don't assume number
			     ))
		  ))
       
       (STATE-3 (lambda (stack lex-state)
		  (let* ((args '())
			 (args (cons (sframe-val (car stack)) args))
			 (stack (cdr stack))
			 (args (cons (sframe-val (car stack)) args))
			 (stack (cdr stack)))
		    ; look at current top of stack, which is as low as we go
		    (let* ((tos (car stack))
			   (bottom (sframe-state tos)))
		      
		      ; r3 is known to produce a 'num
		      (let* ((produced (apply RULE-3 args))
			     (stack (cons (make-sframe 'num produced bottom)
					  stack))
			     (shift-to (GOTO-num bottom)))
			(shift-to stack lex-state))))))

       (STATE-4 (lambda (stack lex-state)
		  (let* ((args '()))

		    ; look at current top of stack, which is as low as we go
		    (let* ((tos (car stack))
			   (bottom (sframe-state tos)))
		      
		      ; r4 is known to produce a 'num
		      (let* ((produced (apply RULE-4 args))
			     (stack (cons (make-sframe 'num produced bottom)
					  stack))
			     (shift-to (GOTO-num bottom)))
			(shift-to stack lex-state))))))

       ; This state is more normal, having just a shift and reduce
       (STATE-5 (lambda (stack lex-state)
		  ; First, grab the lookahead
		  (receive (toktype semval lex-state) (source lex-state)
			   (case toktype
			     ; perform a shift
			     ((DIGIT)
			      (STATE-3 (cons (make-sframe toktype semval STATE-5) ; put new value and self on stack
					     stack)
				       lex-state))
			     
			     ; perform a reduce
			     ((*EOF*)
			      ; Reduce: Ternary
			      (let* ((args '())
				     (args (cons (sframe-val (car stack)) args))
				     (stack (cdr stack))
				     (args (cons (sframe-val (car stack)) args))
				     (stack (cdr stack))
				     (args (cons (sframe-val (car stack)) args))
				     (stack (cdr stack)))
				; look at current top of stack, which is as low as we go
				(let* ((tos (car stack))
				       (bottom (sframe-state tos)))
				  
				  ; r4 is known to produce a 'exp
				  (let* ((produced (apply RULE-2 args))
					 (stack (cons (make-sframe 'program produced bottom)
						      stack))
					 (shift-to (GOTO-exp bottom)))
				    (shift-to stack lex-state)))))
			     
			     ; otherwise start recovery... or whatever
			     (else (error (string-append "Parser bombed reading lexer state "
							 (number->string lexer-state)))) ; TODO don't assume number
			     ))
		  ))
 
      (STATE-6 (lambda (stack lex-state)
		  (let* ((args '()))

		    ; look at current top of stack, which is as low as we go
		    (let* ((tos (car stack))
			   (bottom (sframe-state tos)))
		      
		      ; r1 is a Boolean <=== NOTE
		      (let* ((produced RULE-1)
			     (stack (cons (make-sframe 'num produced bottom) ; TODO: how to handle RULE-1?
					  stack))
			     (shift-to (GOTO-num bottom)))
			(shift-to stack lex-state))))))
      )))

;===============================================================================
#|
;; calc-source : Number [Vector Symbol] -> (values Symbol InputVal Number)
;; Produce symbols from a hardcoded vector. Returns the token type,
;; the semantic value of the token, and the new state.
(define (calc-source state stream)
  (case state
    ((0) (values 'NUM   12   1)
     (1) (values 'PLUS  #\+  2)
     (2) (values 'NUM   33   3)
     (3) (values '*EOF* ""  -1)
     else (error "Invalid lexer state."))))


;; calc-parser : Source Number -> SemVal
;; Parse calculator input using a lexing source.
(define (calc-parser source lex-state)
  (letrec 
      ( ; State lambdas take a stack and a lex state and either shift or reduce
       (STATE-0 (lambda (stack lex-state)
		  ; this state has a reduce with no lookahead,
		  ; so we don't ask the source, don't do case, and go directly to reduce.

		  ; we would have popped and redefined stack, but this reduction is nullary
		  (let* ((tos (car stack))
			 (bottom (sframe-state tos))) ; grab the state that we reduced to
		    
		    ; r5 is known to produce an 's-list
		    (let* ((produced (RULE-5)) ; run rule against stuff from stack
			   (stack (cons (make-sframe 's-list produced bottom) ; shift the semantic value onto the stack
					stack))
			   (shift-to (GOTO-s-list bottom))) ; lookup what state follows the reduction
		      (shift-to stack lex-state))))) ; shift to next state
       
       ; This state is more normal, having just a shift and reduce
       (STATE-7 (lambda (stack lex-state)
		  ; First, grab the lookahead
		  (receive (toktype semval lex-state) (source lex-state)
			   (case toktype
			     ; perform a shift
			     ((SEMICOLON)
			      (STATE-8 (cons (make-sframe toktype semval STATE-7) ; put new value and self on stack
					     stack)
				       lex-state))
			     
			     ; perform a reduce
			     ((*EOF*)
			      ; gather arguments and walk down the stack
			      (let* ((args '())
				     (args (cons (sframe-val (car stack)) args))
				     (stack (cdr stack))
				     (args (cons (sframe-val (car stack)) args))
				     (stack (cdr stack)))
				; look at current top of stack, which is as low as we go
				(let* ((tos (car stack))
				       (bottom (sframe-state tos)))
				  
				  ; r4 is known to produce a 'program
				  (let* ((produced (apply RULE-4 args))
					 (stack (cons (make-sframe 'program produced bottom)
						      stack))
					 (shift-to (GOTO-program bottom)))
				    (shift-to stack lex-state)))))
			     
			     ; otherwise start recovery... or whatever
			     (else (error (string-append "Parser bombed reading lexer state "
							 (number->string lexer-state)))) ; TODO don't assume number
			     ))
		  ))
       
       ; A rule is pulled directly from the PDA form
       (RULE-6 (lambda (s-list statement) (cons statement s-list))) ; TODO how to place these?

       ; A goto cases against the old state to find the new state
       (GOTO-exp (lambda (old)
		   (cond ((eq? old STATE-2) STATE-4)
			 ((eq? old STATE-6) STATE-9)
			 ((eq? old STATE-11) STATE-18)
			 ((eq? old STATE-12) STATE-17)
			 ((eq? old STATE-13) STATE-16)
			 ((eq? old STATE-14) STATE-15)
			 (else (error "Not in valid state for an 'exp reduction.")))))
	   
       )))
|#