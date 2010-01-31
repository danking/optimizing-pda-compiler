;;; Sexp grammar for the PDA language:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pda           ::= (pda-clause ...)
;;; pda-clause    ::= (COMMENT whatever ...)
;;;                 | (TOKENS ident ...)
;;;                 | (ERROR ident)
;;;                 | (NO-SHIFT ident ...)
;;;                 | (STATE state-name action-clause ...)
;;;                 | (RULE rule-name non-term arity action)
;;; action-clause ::= (COMMENT whatever ...)
;;;                 | (SHIFT lookahead state-name)
;;;                 | (REDUCE lookahead rule-name)
;;;                 | (ACCEPT lookahead)
;;;                 | (GOTO non-term state-name)
;;; lookahead     ::= (ident ...)
;;; state-name    ::= ident
;;; rule-name     ::= ident
;;; token         ::= ident
;;; non-term      ::= ident


;;; AST definition:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; StateName = Symbol
;;; RuleName = Symbol
;;; Token = Symbol
;;; NonTerm = Symbol
;;; Accept = Symbol
;;; No-Shift = Symbol

;; A Shift is (make-shift [ListOf Token] StateName)
(define-record shift
  lookaheads
  next-state)

;; A Reduce is (make-reduce [ListOf Token] RuleName)
(define-record reduce
  lookaheads
  rule-name)

;; A Goto is (make-goto NonTerm StateName StateName)
;; nonterm : Non-terminal that has just been produced
;; from : Current state (after reduction)
;; next : Next state (to transition to)
(define-record goto
  nonterm
  from
  next) 

;; A State is (make-state StateName [ListOf Shift] [ListOf Reduce] [ListOf Accept])
(define-record state
  name
  shifts
  reduces
  accepts)

;; A Rule is (make-rule RuleName NonTerm Nat SemAction)
(define-record rule
  name
  nonterm
  arity
  sem-action)

;; A PDA is (make-pda [Listof State] [Listof Goto] [Listof Rule] [Listof No-Shift] StateName)
(define-record pda
  states
  gotos
  rules
  noshifts
  start-state)


;;; Equality and modification functions for AST items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shift= : Shift Shift -> Boolean
;; Compare two shift actions
(define (shift= s1 s2)
  (and (equal? (shift:lookaheads s1) (shift:lookaheads s2))
       (eq?    (shift:next-state s1) (shift:next-state s2))))

(assert (shift= (make-shift '(FOO BAR) 'S1)
		(make-shift '(FOO BAR) 'S1)))
(assert (shift= (make-shift '(FOO BAR) 'S2)
		(make-shift '(FOO BAR) 'S1))
	#f)
(assert (shift= (make-shift '(FOO BAR) 'S1)
		(make-shift '(BAR) 'S1))
	#f)

;; reduce= : Reduce Reduce -> Boolean
;; Compare two reduce actions
(define (reduce= r1 r2)
  (and (equal? (reduce:lookaheads r1) (reduce:lookaheads r2))
       (eq?    (reduce:rule-name r1)  (reduce:rule-name r2))))

;; goto= : Goto Goto -> Boolean
;; Returns true if the arguments are equal Gotos
(define (goto= g1 g2)
  (and (eq? (goto:nonterm g1) (goto:nonterm g2))
       (eq? (goto:from g1)    (goto:from g2))
       (eq? (goto:next g1)    (goto:next g2))))

;;state= : State State -> Boolean
;;Returns true if the two inputs are equal.
(define (state= s1 s2)
  (and (eq?           (state:name s1)    (state:name s2))
       (list= shift=  (state:shifts s1)  (state:shifts s2))
       (list= reduce= (state:reduces s1) (state:reduces s2))
       (list= eq?     (state:accepts s1) (state:accepts s2))))

;; rule= : Rule Rule -> Boolean
;; Returns true if the two inputs are equal Rules
;; (with the exception of semantic actions)
(define (rule= r1 r2)
  (and (eq? (rule:name r1)    (rule:name r2))
       (eq? (rule:nonterm r1) (rule:nonterm r2))
       (=   (rule:arity r1)   (rule:arity r2))))

;;pda= : PDA PDA -> Boolean
;;Returns true if the two inputs are equal PDAs
(define (pda= p1 p2)
  (and (list= state= (pda:states p1)      (pda:states p2))
       (list= goto=  (pda:gotos p1)       (pda:gotos p2))
       (list= rule=  (pda:rules p1)       (pda:rules p2))
       (equal?       (pda:noshifts p1)    (pda:noshifts p2))
       (eq?          (pda:start-state p1) (pda:start-state p2))))

;;; Adder PDA Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial, really simple example -- adding (exactly) two numbers.
;;; (define adder
;;;   (parser
;;;    ((tokens DIGIT
;;; 	    (left PLUS)
;;; 	    (error *ERROR*)
;;; 	    (eos *EOF*))
;;;     (non-term exp
;;; 	      (=> (num PLUS num)   (+ num-1 num-2)))
;;;     (non-term num
;;; 	      (=> (num DIGIT)      (+ (* num 10) DIGIT)))
;;; 	      (=> ()               0))
;;;     )))

;;; This is the PDA term produced by the LALR CFG->PDA compiler.
(define adder-PDA
  '((TOKENS DIGIT PLUS *EOF*) 
    (ERROR *ERROR*) 
    (NO-SHIFT *EOF*) 
    (RULE r1 *start 2 #f) 
    (RULE r2 exp 3 (lambda (num-1 PLUS num-2) (+ num-1 num-2))) 
    (RULE r3 num 2 (lambda (num DIGIT) (+ (* num 10) DIGIT))) 
    (RULE r4 num 0 (lambda () 0)) 
    (STATE s0
	   (REDUCE () r4) 
	   (GOTO exp s1) 
	   (GOTO num s2)) 
    (STATE s1 
	   (ACCEPT (*EOF*))) 
    (STATE s2 
	   (SHIFT (DIGIT) s3) 
	   (SHIFT (PLUS) s4)) 
    (STATE s3 
	   (REDUCE () r3)) 
    (STATE s4 
	   (REDUCE () r4) 
	   (GOTO num s5)) 
    (STATE s5 
	   (SHIFT (DIGIT) s3) 
	   (REDUCE (*EOF*) r2)) 
    (STATE s6 
	   (REDUCE () r1)))
)

;;; This is the AST form for the PDA above.
(define adder-PDA-record
  	(make-pda (list (make-state 's0
				    '()
				    (list (make-reduce '() 'r4))
				    '())
			(make-state 's1
				    '()
				    '()
				    '(*EOF*))
			(make-state 's2
				    (list (make-shift '(DIGIT) 's3)
					  (make-shift '(PLUS) 's4))
				    '()
				    '())
			(make-state 's3
				    '()
				    (list (make-reduce '() 'r3))
				    '())
			(make-state 's4
				    '()
				    (list (make-reduce '() 'r4))
				    '())
			(make-state 's5
				    (list (make-shift '(DIGIT) 's3))
				    (list (make-reduce '(*EOF*) 'r2))
				    '())
			(make-state 's6
				    '()
				    (list (make-reduce '() 'r1))
				    '()))
		  (list (make-goto 'exp 's0 's1)
			(make-goto 'num 's0 's2)
			(make-goto 'num 's4 's5))
		  (list (make-rule 'r1 '*start 2 #f)
			(make-rule 'r2 'exp 3 '(lambda (num-1 PLUS num-2)
						(+ num-1 num-2)))
			(make-rule 'r3 'num 2 '(lambda (num DIGIT)
						(+ (* num 10)
						   DIGIT)))
			
			(make-rule 'r4 'num 0 '(lambda ()
						0)))
		  '(*EOF*)
		  's0))

;;; Equality and modification functions for AST items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shift+tokens : Shift [Listof Token] -> Shift
;; Adds Tokens to a Shift's lookaheads list.
(define (shift+tokens s tokens)
  (make-shift (append tokens (shift:lookaheads s))
	      (shift:next-state s)))

(assert (shift+tokens (make-shift '(FOO BAR) 'S1) '(QUX FEEP))
	(make-shift '(QUX FEEP FOO BAR) 'S1)
	shift=)

;; reduce+tokens : Reduce [Listof Token] -> Reduce
;; Adds Tokens to a Reduce's lookaheads list.
(define (reduce+tokens r tokens)
  (make-reduce (append tokens (reduce:lookaheads r))
	       (reduce:rule-name r)))

(assert (reduce+tokens (make-reduce '(FOO BAR) 'R1) '(QUX FEEP))
	(make-reduce '(QUX FEEP FOO BAR) 'R1)
	reduce=)


;;; Sexp->AST PDA parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code converts a PDA term from its sexp form into the corresponding PDA.

;; *-Sexp is a * represented as an S-expression (ie not a record)

;; update-or-add : [Listof Any] [Any -> Boolean] [Any -> Any] Any -> [Listof Any]
;; Find the first item that matches pred?, run it through modify, and
;; replace it with the output of modify. If nothing matches, add or-add to the end.
;; Not tail-recursive.
(define (update-or-add old pred? modify or-add)
  (cond ((null? old)
	 (list or-add))
	((pred? (car old))
	 (cons (modify (car old))
	       (cdr old)))
	(else
	 (cons (car old)
	       (update-or-add (cdr old) pred? modify or-add)))))

(assert (update-or-add '(1 2 3 4 5 6 7)
		       (lambda (x) (= x 3))
		       (lambda (x) (* 2 x))
		       'f)
	'(1 2 6 4 5 6 7))
(assert (update-or-add '(1 2 3 4 5 6 7)
		       (lambda (x) (= x 12))
		       (lambda (x) (* 2 x))
		       'f)
	'(1 2 3 4 5 6 7 f))


;; build-shifts : [Listof ([Listof Token] StateName)] -> [Listof Shift]
;; Collapse a list of lookahead/destination pairs into a list of Shift.
;; In the resulting list, no two entries will have the same destination name.
(define (build-shifts pairs)
  (let loop ((remain pairs)
	     (accum '())) ; [Listof Shift]
    (if (null? remain)
	accum
	(let* ((top (car remain))
	       (todo (cdr remain))
	       (lookahead (car top))
	       (destination (cadr top)))
	  (loop todo
		(update-or-add accum
			       (lambda (cur)
				 (eq? (shift:next-state cur) destination))
			       (lambda (cur)
				 (shift+tokens cur lookahead))
			       (make-shift lookahead destination)))))))

(assert (build-shifts '(((a) z)
			((b) y)
			((c d) z)))
	(list (make-shift '(c d a) 'z)
	      (make-shift '(b) 'y))
	(lambda (sl1 sl2)
	  (list= shift= sl1 sl2)))

;; build-reduces : [Listof ([Listof Token] RuleName)] -> [Listof Reduce]
;; Collapse a list of lookahead/rulename pairs into a list of Reduce.
;; In the resulting list, no two entries will have the same rulename.
(define (build-reduces pairs)
  (let loop ((remain pairs)
	     (accum '())) ; [Listof Shift]
    (if (null? remain)
	accum
	(let* ((top (car remain))
	       (todo (cdr remain))
	       (lookahead (car top))
	       (rule-name (cadr top)))
	  (loop todo
		(update-or-add accum
			       (lambda (cur)
				 (eq? (reduce:rule-name cur) rule-name))
			       (lambda (cur)
				 (reduce+tokens cur lookahead))
			       (make-reduce lookahead rule-name)))))))

(assert (build-reduces '(((a) z)
			 ((b) y)
			 ((c d) z)))
	(list (make-reduce '(c d a) 'z)
	      (make-reduce '(b) 'y))
	(lambda (rl1 rl2)
	  (list= reduce= rl1 rl2)))

;; parse-state-sexp : State-Sexp -> (values State [Listof Goto])
;; Read a state sexp (without STATE prolog) into a State and a list of Goto.
;; A State-Sexp is a (StateName State-Clause ...) and a State-Clause is
;; a shift, goto, reduce, accept, or comment.
(define (parse-state-sexp sexp)
  (let* ((name (car sexp))
	 (clauses (cdr sexp)))
    (let gather ((remain clauses)
		 (gotos '())
		 (shift-clauses '()) ; [Listof (Token StateName)]
		 (reduce-clauses '()) ; [Listof (Token RuleName)]
		 (accepts '()))
      (if (null? remain)
	  (values (make-state name
			      (reverse (build-shifts shift-clauses))
			      (reverse (build-reduces reduce-clauses))
			      (reverse accepts))
		  gotos)
	  (let* ((top (car remain))
		 (rest (cdr remain))
		 (type (car top))
		 (data (cdr top)))
	    (case type
	      ((COMMENT)
	       (gather rest gotos shift-clauses reduce-clauses accepts))
	      ((SHIFT)
	       (gather rest gotos (cons data shift-clauses) reduce-clauses accepts))
	      ((REDUCE)
	       (gather rest gotos shift-clauses (cons data reduce-clauses) accepts))
	      ((ACCEPT)
	       (gather rest gotos shift-clauses reduce-clauses (append (car data) accepts)))
	      ((GOTO)
	       (let* ((nonterm (car data))
		      (next (cadr data))
		      (new-goto (make-goto nonterm name next)))
		 (gather rest (cons new-goto gotos) shift-clauses reduce-clauses accepts)))))))))

(receive 
 (state gs) 
 (parse-state-sexp '(S1
		     (REDUCE (BAR) R3)
		     (GOTO exp S5)
		     (ACCEPT (DIGIT))
		     (SHIFT (L-PAREN) S4)
		     (GOTO num S7)
		     (REDUCE (FOO) R3)))
 (begin
   (assert state
	   (make-state 'S1
		       (list (make-shift '(L-PAREN) 'S4))
		       (list (make-reduce '(BAR FOO) 'R3))
		       (list 'DIGIT))
	   state=)
   (assert gs
	   (list (make-goto 'num 'S1 'S7)
		 (make-goto 'exp 'S1 'S5))
	   (lambda (gl1 gl2)
	     (list= goto= gl1 gl2)))))

;; sexp->PDA : PDA-Sexp -> PDA
(define (sexp->PDA form)
  ;; gather-main : Clauses [Listof State] [Listof Goto] [Listof Rule] [Listof No-Shift] StateName
  ;;            -> (values [Listof State] [Listof Goto] [Listof Rule] [Listof No-Shift] StateName)
  (let gather-main ((remain form)
		    (states '())
		    (gotos '())
		    (rules '())
		    (noshifts '())
		    (start #f))
    (if (null? remain)
	(make-pda (reverse states) (reverse gotos) (reverse rules) (reverse noshifts) start)
	(let* ((top  (car remain))
	       (rest (cdr remain))
	       (type (car top))
	       (data (cdr top)))
	  (case type
	    ((COMMENT TOKENS ERROR)
	     (gather-main rest states gotos rules noshifts start))
	    ((NO-SHIFT)
	     (gather-main rest states gotos rules (append data noshifts) start))
	    ((STATE)
	     (let ((start (or start (car data)))) ; grab name as start symbol if not yet set
	       (receive (new-state new-gotos) (parse-state-sexp data)
			(gather-main rest
				     (cons new-state states)
				     (append new-gotos gotos)
				     rules
				     noshifts
				     start))))
	    ((RULE)
	     (gather-main rest states gotos (cons (apply make-rule data) rules) noshifts start))
	    (else (error "Unrecognized PDA clause.")))))))

(assert (sexp->PDA adder-PDA)
	adder-PDA-record
	pda=)

;;; AST->Sexp PDA unparser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code converts a PDA term from its AST form into the corresponding sexp.

;;PDA->sexp : PDA -> PDA-sexp
;;Unparses a PDA record to a PDA S-expression
(define (PDA->sexp pda)
  (append (list (cons 'NO-SHIFT (pda:noshifts pda)))
	  (map (lambda (rule)
		 `(RULE ,(rule:name rule) 
			,(rule:nonterm rule) 
			,(rule:arity rule)
			,(rule:sem-action rule)))
	       (pda:rules pda))
	  (map (lambda (state)
		 `(STATE ,(state:name state)
			 ,(append (map (lambda (shift)
					 `(SHIFT ,(shift:lookaheads shift)
						 ,(shift:next-state shift)))
				       (state:shifts state))
				   (map (lambda (reduce)
					  `(REDUCE ,(reduce:lookaheads reduce)
						   ,(reduce:rule-name reduce)))
					(state:reduces state))
			  	   (map (lambda (goto)
					  `(GOTO ,(goto:nonterm goto)
						 ,(goto:next goto)))
					(filter (lambda (goto)
						  (eq? (goto:from goto) 
						       (state:name state)))
						(pda:gotos pda)))
				   (if (pair? (state:accepts state))
				       (list `(ACCEPT ,(state:accepts state)))
				       '()))))
	       (pda:states pda))))


(assert (PDA->sexp adder-PDA-record)
	'((NO-SHIFT *EOF*)
	  (RULE r1 *start 2 #f)
	  (RULE r2 exp 3 (lambda (num-1 PLUS num-2)
			   (+ num-1 num-2)))
	  (RULE r3 num 2 (lambda (num DIGIT)
			   (+ (* num 10) 
			      DIGIT)))
	  (RULE r4 num 0 (lambda () 0))
	  (STATE s0
		 ((REDUCE () r4)
		 (GOTO exp s1)
		 (GOTO num s2)))
	  (STATE s1
		 ((ACCEPT (*EOF*))))
	  (STATE s2
		 ((SHIFT (DIGIT) s3)
		  (SHIFT (PLUS) s4)))
	  (STATE s3
		 ((REDUCE () r3)))
	  (STATE s4
		 ((REDUCE () r4)
		 (GOTO num s5)))
	  (STATE s5
		 ((SHIFT (DIGIT) s3)
		 (REDUCE (*EOF*) r2)))
	  (STATE s6
		((REDUCE () r1))))
	equal?)

;;; PDA Static Checker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code checks the validity of the PDA

;; duplicate-names : [List of X] (X -> Symbol) -> Boolean
;; Checks whether there are duplicate names in the list
(define (duplicate-names lst get-name)
  (and (pair? lst)
       (if (any (lambda (x) 
		  (eq? (get-name x) (get-name (car lst))))
		(cdr lst))
	   (error "Static Checker Error - Duplicate name " (get-name (car lst)))
	   (duplicate-names (cdr lst) get-name))))

;; valid-gotos : [Listof Goto] [Listof State] [Listof Rules] -> Boolean
;; Checks whether each Goto points to a valid state names or rule nonterminals
(define (valid-gotos gotos states rules)
  (every (lambda (goto)
	    (if (and (any (lambda (state)
			       (eq? (state:name state) (goto:from goto)))				  
			     states)
		     (any (lambda (state)
			       (eq? (state:name state) (goto:next goto)))
			     states)
		     (any (lambda (rule)
			       (eq? (goto:nonterm goto) (rule:nonterm rule)))
			     rules))
		#t
		(error "Static Checker Error - Invalid Goto" goto)))
	  gotos))

;; valid-state-actions : [Listof State] [Listof Rule] -> Boolean
;; Checks whether the shifts and reduces of a state contain valid
;; state and rule names.
(define (valid-state-actions states rules)
  (every (lambda (state)
	    (and (every (lambda (shift)
			   (if (any (lambda (state)
					(eq? (state:name state) (shift:next-state shift)))
				      states)
			       #t
			       (error "Static Checker Error - Invalid Shift" shift)))
			 (state:shifts state))
		 (every (lambda (reduce)
			   (if (any (lambda (rule)
					(eq? (rule:name rule) (reduce:rule-name reduce)))
				   rules)
			       #t
			       (error "Static Checker Error - Invalid Reduce" reduce)))
			 (state:reduces state))))
	  states))

;; pda-static-check : PDA -> boolean
;; Verifies that the PDA is valid by doing the following:
;;    -- Ensuring there are no duplicate state or rule names
;;    -- Ensuring that each state/rule pointer is valid
;;       in all Gotos, Shifts, and Reduces
(define (pda-static-check pda)
  (and (not (duplicate-names (pda:states pda) state:name))
       (not (duplicate-names (pda:rules pda) rule:name))
       (valid-gotos (pda:gotos pda) (pda:states pda) (pda:rules pda))
       (valid-state-actions (pda:states pda) (pda:rules pda))
       (if (any (lambda (state) 
		    (eq? (state:name state) (pda:start-state pda)))
		  (pda:states pda))
	   #t
	   (error "Invalid Start State" (pda:start-state pda)))))

(assert (pda-static-check adder-PDA-record))

;; compile-pda : PDA-sexp -> PDA
;; Parses the PDA S-expression and statically checks to ensure
;; the PDA is valid.
(define (compile-pda pda-sexp)
  (let ((pda (sexp->PDA pda-sexp)))
    (if (pda-static-check pda)
	pda
	(error "Compile Failed" pda-sexp))))


;;; PDA -> Scheme Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code converts the PDA AST to scheme code that can run on input. 
;;; (make-pda [Listof State] [Listof Goto] [Listof Rule] [Listof No-Shift] StateName)
;;;      -> (letrec ([LISTOF states, gotos, rules, no-shifts])
;;;            (lambda (input nxt-token) (StateName input '() nxt-token '())))
;;;
(define (pda form rename compare)
  (let* ((form (cadr form))
	 (pda (compile-pda form))
	(%letrec (rename 'letrec))
        (%lambda (rename 'lambda))
	(%error (rename 'error))
	(%quote (rename 'quote))
	(%cond (rename 'cond))
	(%zero? (rename 'zero?))
	(%cons (rename 'cons))
	(%apply (rename 'apply))
	(%else (rename 'else))
	(%cdr (rename 'cdr))
	(%car (rename 'car))
	(%- (rename '-))
	)
    `(,%letrec ,(append (map (lambda (state) (fill-in-one-state state rename)) (pda:states pda))
			(map (lambda (rule) (fill-in-one-rule rule rename)) (pda:rules pda))
			(fill-in-gotos (pda:gotos pda) rename)
			`((*start (,%lambda (input val-stack nxt-token state-stack)
					    (,%error "Invalid Expression")))
			  (no-shifts (,%quote ,(pda:noshifts pda))))
			)
	       (,%lambda (input nxt-token) (,(pda:start-state pda) input '() nxt-token '())))))

;; fill-in-one-state : State Renamer -> Sexp
;; Creates one function for the state specified
;; (make-state StateName [ListOf Shift] [ListOf Reduce] [ListOf Accept])
;;      -> (StateName (lambda (input val-stack nxt-token state-stack)
;;                      (cond [LISTOF shifts, reduces, accepts])))
(define (fill-in-one-state state rename)
  (let ((%lambda (rename 'lambda))
	(%let (rename 'let))
	(%cond (rename 'cond))
	)
    `(,(state:name state) (,%lambda (input val-stack nxt-token state-stack)
			      (,%let ((token (nxt-token input)))
				,(cons %cond (append (map (lambda (shift) (fill-in-one-shift shift (state:name state) rename)) 
							  (state:shifts state))
						     (fill-in-accepts (state:accepts state) rename)
						     (fill-in-reduces (state:reduces state) (state:name state) rename))))))))

;; fill-in-one-shift : Shift StateName Renamer -> Sexp
;; Creates the cond clause for the shift specified
;; (make-shift lookaheads StateName) 
;;      -> ((member token lookaheads) (if (member token no-shifts)
;;                                        (StateName input stack nxt-token 
;;                                                   (cons (quote StateName) goto-env))
;;                                        (StateName (cdr input) (cons (car input) stack) 
;;                                                   nxt-token 
;;                                                   (cons (quote StateName) goto-env)))))
(define (fill-in-one-shift shift state-name rename)
  (let ((%member (rename 'member))
	(%cdr (rename 'cdr))
	(%cons (rename 'cons))
	(%car (rename 'car))
	(%quote (rename 'quote))
	(%eq? (rename 'eq?))
	)
    
    `(,(if (and (pair? (shift:lookaheads shift))
		(not (pair? (cdr (shift:lookaheads shift)))))
	   `(,%eq? token (,%quote ,(car (shift:lookaheads shift))))
	   `(,%member token (,%quote ,(shift:lookaheads shift))))
      (if (,%member token no-shifts)
	  (,(shift:next-state shift) input
	   val-stack
	   nxt-token
	   (,%cons (,%quote ,state-name) state-stack))
	  (,(shift:next-state shift) (,%cdr input)
	   (,%cons (,%car input) val-stack)
	   nxt-token
	   (,%cons (,%quote ,state-name) state-stack))))))

;; fill-in-accepts : [Listof Accept] Renamer -> Sexp
;; Creates the cond clause if there is an accept case
(define (fill-in-accepts accepts-lst rename)
    (let ((%member (rename 'member))
	  (%quote (rename 'quote))
	  )
      (if (null? accepts-lst)
	  '()
	  `(((,%member token (,%quote ,accepts-lst)) val-stack)))))

;; fill-in-reduces : [Listof Reduce] StateName Renamer -> Sexp
;; Creates each cond clause for every reduce case
(define (fill-in-reduces reduces-lst state-name rename)
  (cond ((null? reduces-lst) '())
	((and (null? (reduce:lookaheads (car reduces-lst))) (not (null? (cdr reduces-lst))))
	 (fill-in-reduces (append (cdr reduces-lst) (list (car reduces-lst))) state-name rename))
	(else (cons (fill-in-one-reduce (car reduces-lst) state-name rename)
		    (fill-in-reduces (cdr reduces-lst) state-name rename)))))

;; fill-in-one-reduce : Reduce StateName Renamer -> Sexp
;; Creates a cond clause for the reduce case specified
;; (make-reduce lookaheads RuleName)
;;    IF (null? lookaheads)
;;      -> (else (RuleName input val-stack nxt-token (cons (quote StateName) state-stack)))
;;              where StateName is the state this function call is in
;;    ELSE
;;      -> ((member token lookaheads) (if (member token no-shifts)
;;                                        (RuleName input stack nxt-token 
;;                                                  (cons (quote StateName) goto-env))
;;                                        (RuleName (cdr input) (cons (car input) stack) 
;;                                                  nxt-token 
;;                                                  (cons (quote StateName) goto-env))))
;;              where StateName is the state this function call is in
(define (fill-in-one-reduce reduce state-name rename)
  (let ((%member (rename 'member))
	(%else (rename 'else))
	(%cdr (rename 'cdr))
	(%quote (rename 'quote))
	(%else (rename 'else))
	(%cons (rename 'cons))
	(%eq? (rename 'eq?))
	)
    (cond ((null? (reduce:lookaheads reduce)) `(,%else (,(reduce:rule-name reduce) input val-stack nxt-token 
					 (,%cons (,%quote ,state-name) state-stack))))
	  (else `(,(if (and (pair? (reduce:lookaheads reduce))
			    (not (pair? (cdr (reduce:lookaheads reduce)))))
		       `(,%eq? token (,%quote ,(car (reduce:lookaheads reduce))))
		       `(,%member token (,%quote ,(reduce:lookaheads reduce))))
		  (if (,%member token no-shifts)
		      (,(reduce:rule-name reduce) input val-stack nxt-token
		       (,%cons (quote ,state-name) state-stack))
		      (,(reduce:rule-name reduce) (,%cdr input) val-stack nxt-token
		       (,%cons (,%quote ,state-name) state-stack))))))))


;; fill-in-one-rule : Rule Renamer -> Sexp
;; Creates a function for the rule specified
;; (make-rule RuleName NonTerm Nat SemAction)
;;      -> (RuleName (lambda (input stack nxt-token goto-env) 
;;                      (NonTerm input (applyfcn SemAction Nat) 
;;                                nxt-token (pop goto-env Nat))))
(define (fill-in-one-rule rule rename)
  (let ((%lambda (rename 'lambda))
	)
    `(,(rule:name rule) (,%lambda (input val-stack nxt-token state-stack)
			     (,(rule:nonterm rule) input ,(if (boolean? (rule:sem-action rule))
						       'val-stack
						       (applyfcn (rule:sem-action rule) (rule:arity rule) rename))
			      nxt-token
			      ,(pop-state-stack (rule:arity rule) rename))))))

;; applyfcn : Function Nat -> Sexp
;; Removes the top n elements of the stack and applies the function on them
;; (applyfcn 2 (lambda (x y) (+ x y))) -> '(let* ((args '())
;;                                                (args (cons (car val-stack) args))
;;                                                (val-stack (cdr val-stack))
;;                                                (args (cons (car val-stack) args))
;;                                                (val-stack (cdr val-stack)))
;;                                               (cons (apply (lambda (x y) (+ x y)) args) val-stack)
(define (applyfcn function n rename)
  (let ((%let* (rename 'let*))
	(%cons (rename 'cons))
	(%apply (rename 'apply))
	(%car (rename 'car))
	(%cdr (rename 'cdr)))
    (let applyfcn ((n n) (app '()))
      (if (zero? n)
	  (cons %let* (cons (cons '(args '()) app)
			    (list `(,%cons (,%apply ,function args) val-stack))))
	  (applyfcn (- n 1) (cons `(args (,%cons (,%car val-stack) args))
				  (cons `(val-stack (,%cdr val-stack)) 
					app)))))))
			      

;; pop-state-stack : Nat -> Sexp
;; Pops the stack n times.
;; Ex: (pop-state-stack stack 3) -> '(cdr (cdr (cdr state-stack)))
;;     (pop-state-stack stack 0) -> 'state-stack
(define (pop-state-stack n rename)
  (let ((%cdr (rename 'cdr)))
    (let pop-state-stack ((n n) (stack 'state-stack))
      (if (zero? n)
	  stack
	  (pop-state-stack (- n 1) `(,%cdr ,stack))))))


;; fill-in-gotos : [Listof Goto] Renamer -> Sexp
;; Creates all of the functions associated in a goto case
(define (fill-in-gotos gotos rename)
  (let ((all-nonterms (consolidate gotos '())))
    (map (lambda (nonterm) (fill-in-nonterm nonterm rename)) all-nonterms)))

;; consolidate : [Listof Goto] [Listof NonTerm] -> [Listof Nonterm]
;; Creates a list of unique nonterms paired with all their goto mappings
;; (consolidate (list (make-goto exp s0 s1)
;;		      (make-goto num s0 s2)
;;		      (make-goto exp s4 s5)
;;                    (make-goto num s4 s2)))
;;      -> '((num ((s0 s2) (s4 s2)))
;;           (exp ((s0 s1) (s4  s5))))
(define (consolidate gotos nonterms)
  (cond ((null? gotos) nonterms)
	(else (let ((any-nonterms (filter (lambda (nonterm) (eq? (car nonterm) (goto:nonterm (car gotos)))) nonterms)))
		  (if (null? any-nonterms)
		      (consolidate (cdr gotos) (cons (list (goto:nonterm (car gotos)) (list (list (goto:from (car gotos)) 
												  (goto:next (car gotos))))) nonterms))
		      (consolidate (cdr gotos) (cons (list (caar any-nonterms) (cons (list (goto:from (car gotos)) 
											   (goto:next (car gotos)))
										     (cadar any-nonterms)))
						     (filter (lambda (nonterm) 
								    (not (eq? (car nonterm) (goto:nonterm (car gotos)))))
								  nonterms))))))))

;; fill-in-nonterm : NonTerm Renamer -> Sexp
;; Creates each nonterminals' function to handle the goto case (see inverted sided gotos)
;; '(num ((s0 s2) (s4 s2)))
;;         -> (num (lambda (input val-stack nxt-token state-stack)
;;                    (cond ((eq? (car state-stack) 's0) (s2 input val-stack nxt-token state-stack))
;;                          ((eq? (car state-stack) 's4) (s2 input val-stack nxt-token state-stack)))))
(define (fill-in-nonterm nonterm rename)
  (letrec ((%eq? (rename 'eq?))
	   (%car (rename 'car))
	   (%quote (rename 'quote))
	   (%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   
	   (fill-in-cases (lambda (nonterm-cases)
			    (cond ((null? nonterm-cases) '())
				  (else (cons `((,%eq? (,%car state-stack) (,%quote ,(caar nonterm-cases)))
						(,(cadar nonterm-cases) input val-stack nxt-token state-stack))
					      (fill-in-cases (cdr nonterm-cases))))))))
    `(,(car nonterm) (,%lambda (input val-stack nxt-token state-stack)
		       ,(cons %cond (fill-in-cases (cadr nonterm)))))))


;;; SIMPLE LEXER FOR ADDER GRAMMAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(define (next-token input)                   
  (cond ((null? input) '*EOF*)            
	((eq? (first input) '+) 'PLUS)    
	(else 'DIGIT)))


;;; New Adder PDA Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This example allows adding one or more numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (define new-adder
;;;   (compile+convert-to-pda
;;;    ((tokens DIGIT
;;; 	    (left PLUS)
;;; 	    (error *ERROR*)
;;; 	    (eos *EOF*))
;;;     (non-term exp
;;; 	      (=> (num PLUS exp)   (+ num exp))
;;; 	      (=> (num)            num))
;;;     (non-term num
;;; 	      (=> (num DIGIT)      (+ (* num 10)
;;; 				      DIGIT))
;;; 	      (=> ()               0))
;;;     )))

;;; Resulting PDA
(define new-adder-PDA '((TOKENS DIGIT PLUS *EOF*) 
			(ERROR *ERROR*) 
			(NO-SHIFT *EOF*) 
			(RULE r1 *start 2 #f) 
			(RULE r2 exp 3 (lambda (num PLUS exp) (+ num-1 num-2))) 
			(RULE r3 exp 1 (lambda (num) num)) 
			(RULE r4 num 2 (lambda (num DIGIT) (+ (* num 10) DIGIT))) 
			(RULE r5 num 0 (lambda () 0)) 
			(STATE s0 (COMMENT num "=>" ".") 
			       (COMMENT num "=>" "." num DIGIT) 
			       (COMMENT exp "=>" "." num) 
			       (COMMENT exp "=>" "." num PLUS exp) 
			       (COMMENT *start "=>" "." exp *EOF*) 
			       (REDUCE () r5) 
			       (GOTO exp s1) 
			       (GOTO num s2)) 
			(STATE s1 (COMMENT *start "=>" exp "." *EOF*) 
			       (ACCEPT (*EOF*))) 
			(STATE s2 (COMMENT num "=>" num "." DIGIT) 
			       (COMMENT exp "=>" num ".") 
			       (COMMENT exp "=>" num "." PLUS exp) 
			       (SHIFT (DIGIT) s3) 
			       (SHIFT (PLUS) s4) 
			       (REDUCE (*EOF*) r3)) 
			(STATE s3 (COMMENT num "=>" num DIGIT ".") 
			       (REDUCE () r4)) 
			(STATE s4 (COMMENT num "=>" ".") 
			       (COMMENT num "=>" "." num DIGIT) 
			       (COMMENT exp "=>" "." num) 
			       (COMMENT exp "=>" num PLUS "." exp) 
			       (COMMENT exp "=>" "." num PLUS exp) 
			       (REDUCE () r5) 
			       (GOTO exp s5) 
			       (GOTO num s2)) 
			(STATE s5 (COMMENT exp "=>" num PLUS exp ".") 
			       (REDUCE () r2)) 
			(STATE s6 (COMMENT *start "=>" exp *EOF* ".") 
			       (REDUCE () r1))))