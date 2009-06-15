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
(define-record-type shift :shift
  (make-shift lookaheads next-state) ; [ListOf Token] StateName -> Shift
  shift?
  (lookaheads shift-lookaheads) ; [ListOf Token]
  (next-state shift-next-state)) ; StateName

(define-record-discloser :shift
  (lambda (o)
    `(shift
      ,(shift-lookaheads o)
      ,(shift-next-state o))))

;; A Reduce is (make-reduce [ListOf Token] RuleName)
(define-record-type reduce :reduce
  (make-reduce lookaheads rule-name) ; [ListOf Token] RuleName -> Reduce
  reduce?
  (lookaheads reduce-lookaheads) ; [ListOf Token]
  (rule-name reduce-rule-name)) ; RuleName

(define-record-discloser :reduce
  (lambda (o)
    `(reduce
      ,(reduce-lookaheads o)
      ,(reduce-rule-name o))))

;; A Goto is (make-goto NonTerm StateName StateName)
(define-record-type goto :goto
  (make-goto nonterm from next) ; NonTerm StateName StateName -> Goto
  goto?
  (nonterm goto-nonterm) ; NonTerm : Non-terminal that has just been produced
  (from goto-from) ; StateName : Current state (after reduction)
  (next goto-next)) ; StateName : Next state (to transition to)

(define-record-discloser :goto
  (lambda (o)
    `(goto
      ,(goto-nonterm o)
      ,(goto-from o)
      ,(goto-next o))))

;; A State is (make-state StateName [ListOf Shift] [ListOf Reduce] [ListOf Accept])
(define-record-type state :state
  (make-state name shifts reduces accepts)
  state?
  (name state-name)
  (shifts state-shifts)
  (reduces state-reduces)
  (accepts state-accepts))

(define-record-discloser :state
  (lambda (o)
    `(state
      ,(state-name o)
      ,(state-shifts o)
      ,(state-reduces o)
      ,(state-accepts o))))

;; A Rule is (make-rule RuleName NonTerm Nat SemAction)
(define-record-type rule :rule
  (make-rule name nonterm arity sem-action)
  rule?
  (name rule-name)
  (nonterm rule-nonterm)
  (arity rule-arity)
  (sem-action rule-sem-action))

(define-record-discloser :rule
  (lambda (o)
    `(rule
      ,(rule-name o)
      ,(rule-nonterm o)
      ,(rule-arity o)
      ,(rule-sem-action o))))

;; A PDA is (make-pda [Listof State] [Listof Goto] [Listof Rule] [Listof No-Shift] StateName)
(define-record-type pda :pda
  (make-pda states gotos rules noshifts start-state)
  pda?
  (states pda-states)
  (gotos pda-gotos)
  (rules pda-rules)
  (noshifts pda-noshifts)
  (start-state pda-start-state))

(define-record-discloser :pda
  (lambda (o)
    `(pda
      ,(pda-states o)
      ,(pda-gotos o)
      ,(pda-rules o)
      ,(pda-noshifts o)
      ,(pda-start-state o))))


;;; Equality and modification functions for AST items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shift= : Shift Shift -> Boolean
;; Compare two shift actions
(define (shift= s1 s2)
  (and (equal? (shift-lookaheads s1) (shift-lookaheads s2))
       (eq?    (shift-next-state s1) (shift-next-state s2))))

(assert (shift= (make-shift '(FOO BAR) 'S1)
		(make-shift '(FOO BAR) 'S1)))
(assert (shift= (make-shift '(FOO BAR) 'S2)
		(make-shift '(FOO BAR) 'S1))
	#f)
(assert (shift= (make-shift '(FOO BAR) 'S1)
		(make-shift '(BAR) 'S1))
	#f)

;; shift+tokens : Shift [Listof Token] -> Shift
;; Adds Tokens to a Shift's lookaheads list.
(define (shift+tokens s lot)
  (make-shift (append lot (shift-lookaheads s))
	      (shift-next-state s)))

(assert (shift+tokens (make-shift '(FOO BAR) 'S1) '(QUX FEEP))
	(make-shift '(QUX FEEP FOO BAR) 'S1)
	shift=)

;; reduce= : Reduce Reduce -> Boolean
;; Compare two reduce actions
(define (reduce= r1 r2)
  (and (equal? (reduce-lookaheads r1) (reduce-lookaheads r2))
       (eq?    (reduce-rule-name r1)  (reduce-rule-name r2))))

;; reduce+tokens : Reduce [Listof Token] -> Reduce
;; Adds Tokens to a Reduce's lookaheads list.
(define (reduce+tokens r lot)
  (make-reduce (append lot (reduce-lookaheads r))
	       (reduce-rule-name r)))

(assert (reduce+tokens (make-reduce '(FOO BAR) 'R1) '(QUX FEEP))
	(make-reduce '(QUX FEEP FOO BAR) 'R1)
	reduce=)

;; goto= : Goto Goto -> Boolean
;; Returns true if the arguments are equal Gotos
(define (goto= g1 g2)
  (and (eq? (goto-nonterm g1) (goto-nonterm g2))
       (eq? (goto-from g1)    (goto-from g2))
       (eq? (goto-next g1)    (goto-next g2))))

;;state= : State State -> Boolean
;;Returns true if the two inputs are equal.
(define (state= s1 s2)
  (and (eq?            (state-name s1)    (state-name s2))
       (list=? shift=  (state-shifts s1)  (state-shifts s2))
       (list=? reduce= (state-reduces s1) (state-reduces s2))
       (list=? eq?     (state-accepts s1) (state-accepts s2))))

;; rule= : Rule Rule -> Boolean
;; Returns true if the two inputs are equal Rules
;; (with the exception of semantic actions)
(define (rule= r1 r2)
  (and (eq? (rule-name r1)    (rule-name r2))
       (eq? (rule-nonterm r1) (rule-nonterm r2))
       (=   (rule-arity r1)   (rule-arity r2))))


;;pda= : PDA PDA -> Boolean
;;Returns true if the two inputs are equal PDAs
(define (pda= p1 p2)
  (and (list=? state= (pda-states p1)      (pda-states p2))
       (list=? goto=  (pda-gotos p1)       (pda-gotos p2))
       (list=? rule=  (pda-rules p1)       (pda-rules p2))
       (equal?        (pda-noshifts p1)    (pda-noshifts p2))
       (eq?           (pda-start-state p1) (pda-start-state p2))))

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


;; build-shifts : [Listof (Token StateName)] -> [Listof Shift]
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
				 (eq? (shift-next-state cur) destination))
			       (lambda (cur)
				 (shift+tokens cur lookahead))
			       (make-shift lookahead destination)))))))

(assert (build-shifts '(((a) z)
			((b) y)
			((c d) z)))
	(list (make-shift '(c d a) 'z)
	      (make-shift '(b) 'y))
	(lambda (sl1 sl2)
	  (list=? shift= sl1 sl2)))

;; build-reduces : [Listof (Token RuleName)] -> [Listof Reduce]
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
				 (eq? (reduce-rule-name cur) rule-name))
			       (lambda (cur)
				 (reduce+tokens cur lookahead))
			       (make-reduce lookahead rule-name)))))))

(assert (build-reduces '(((a) z)
			 ((b) y)
			 ((c d) z)))
	(list (make-reduce '(c d a) 'z)
	      (make-reduce '(b) 'y))
	(lambda (rl1 rl2)
	  (list=? reduce= rl1 rl2)))

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
	  (begin
	    (display "Shift pairs" shift-clauses)
	    (display "Reduce pairs" reduce-clauses)
	    (values (make-state name
				(build-shifts shift-clauses)
				(build-reduces reduce-clauses)
				accepts)
		    gotos))
	  (begin
	    (debug "Remaining state clauses" remain)
	    (let* ((top (car remain))
		   (rest (cdr remain))
		   (type (car top))
		   (data (cdr top)))
	      (case type
		((COMMENT)
		 (gather rest gotos shift-clauses reduce-clauses accepts))
		((SHIFT)
		 (debug "shift clause data" data)
		 (gather rest gotos (append data shift-clauses) reduce-clauses accepts))
		((REDUCE)
		 (debug "reduce clause data" data)
		 (gather rest gotos shift-clauses (append data reduce-clauses) accepts))
		((ACCEPT)
		 (debug "accept clause data" data)
		 (gather rest gotos shift-clauses reduce-clauses (append data accepts)))
		((GOTO)
		 (debug "goto clause data" data)
		 (let* ((nonterm (car data))
			(next (cadr data))
			(new-goto (make-goto nonterm name next)))
		   (debug "goto nonterm" nonterm)
		   (debug "goto next" next)
		   (debug "goto new-goto" new-goto)
		   (gather rest (cons new-goto gotos) shift-clauses reduce-clauses accepts))))))))))

(receive (state gs) (parse-state-sexp '(S1
					(REDUCE (BAR) R3)
					(GOTO exp S5)
					(ACCEPT (DIGIT))
					(SHIFT (L-PAREN) S4)
					(GOTO num S7)
					(REDUCE (FOO) R3)))
	 (begin
	   (debug "Testing state" state)
	   (assert state
		   (make-state 'S1
			       (list (make-shift '(L-PAREN) 'S4))
			       (list (make-reduce '(BAR FOO) 'R3))
			       (list 'DIGIT))
		   state=)
	   (debug "Testing gotos" gs)
	   (assert gs
		   (list (make-goto 'num 'S1 'S7)
			 (make-goto 'exp 'S1 'S5))
		   (lambda (gl1 gl2)
		     (list=? goto= gl1 gl2)))))

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
	(make-pda states gotos rules noshifts start)
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
	     (receive (new-state new-gotos) (parse-state-sexp data)
		      (gather-main rest
				   (cons new-state states)
				   (append new-gotos gotos)
				   rules
				   noshifts
				   start)))
	    ((RULE)
	     (gather-main rest states gotos (cons (apply make-rule data) rules) noshifts start))
	    (else (error "Unrecognized PDA clause.")))))))


(assert (sexp->PDA adder-PDA)
	(make-PDA (list (make-state 's6
				    '()
				    (list (make-reduce '() 'r1))
				    '())
			(make-state 's5
				    (list (make-shift '(DIGIT) 's3))
				    (list (make-reduce '(*EOF*) 'r2))
				    '())
			(make-state 's4
				    '()
				    (list (make-reduce '() 'r4))
				    '())
			(make-state 's3
				    '()
				    (list (make-reduce '() 'r3))
				    '())
			(make-state 's2
				    (list (make-shift '(PLUS) 's4)
					  (make-shift '(DIGIT) 's3))
				    '()
				    '())
			(make-state 's1
				    '()
				    '()
				    '(*EOF*))
			(make-state 's0
				    '()
				    (list (make-reduce '() 'r4))
				    '()))
		  (list (make-goto 'num 's4 's5)
			(make-goto 'num 's0 's2)
			(make-goto 'exp 's0 's1))
		  (list (make-rule 'r4 'num 0 adder-0)
			(make-rule 'r3 'num 2 adder-num)
			(make-rule 'r2 'exp 3 adder-exp)
			(make-rule 'r1 '*start 2 #f))
		  '(*EOF*)
		  's0)
	pda=)


;;; AST->Sexp PDA unparser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code converts a PDA term from its AST form into the corresponding sexp.

;....
