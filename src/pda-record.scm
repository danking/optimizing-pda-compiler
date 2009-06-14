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

;; A Reduce is (make-reduce [ListOf Token] RuleName)
(define-record-type reduce :reduce
  (make-reduce lookaheads rule-name) ; [ListOf Token] RuleName -> Reduce
  reduce?
  (lookaheads reduce-lookaheads) ; [ListOf Token]
  (rule-name reduce-rule-name)) ; RuleName

;; A Goto is (make-goto NonTerm StateName StateName)
(define-record-type goto :goto
  (make-goto nonterm from next) ; NonTerm StateName StateName -> Goto
  goto?
  (nonterm goto-nonterm) ; NonTerm : Non-terminal that has just been produced
  (from goto-from) ; StateName : Current state (after reduction)
  (next goto-next)) ; StateName : Next state (to transition to)

;; A State is (make-state StateName [ListOf Shift] [ListOf Reduce] [ListOf Accept])
(define-record-type state :state
  (make-state name shifts reduces accepts)
  state?
  (name state-name)
  (shifts state-shifts)
  (reduces state-reduces)
  (accepts state-accepts))

;; A Rule is (make-rule RuleName NonTerm Nat SemAction)
(define-record-type rule :rule
  (make-rule name nonterm arity sem-action)
  rule?
  (name rule-name)
  (nonterm rule-nonterm)
  (arity rule-arity)
  (sem-action rule-sem-action))

;; A PDA is (make-pda [Listof State] [Listof Goto] [Listof Rule] [Listof No-Shift] StateName)
(define-record-type pda :pda
  (make-pda states gotos rules noshifts start-state)
  pda?
  (states pda-states)
  (gotos pda-gotos)
  (rules pda-rules)
  (noshifts pda-noshifts)
  (start-state pda-start-state))


;;; Equality and modification functions for AST items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shift= : Shift Shift -> Boolean
;; Compare two shift actions
(define (shift= s1 s2)
  (and (equal? (shift-lookaheads s1) (shift-lookaheads s2))
       (eq?    (shift-next-state s1) (shift-next-state s2))))

;; shift+token : Shift Token -> Shift
;; Add a Token to a Shift's lookaheads list.
(define (shift+token s t)
  (make-shift (cons t (shift-lookaheads s))
	      (shift-next-state s)))

;; reduce= : Reduce Reduce -> Boolean
;; Compare two reduce actions
(define (reduce= r1 r2)
  (and (equal? (reduce-lookaheads r1) (reduce-lookaheads r2))
       (eq?    (reduce-rule-name r1)  (reduce-rule-name r2))))

;; reduce+token : Reduce Token -> Reduce
;; Add a Token to a Reduce's lookaheads list.
(define (reduce+token r t)
  (make-reduce (cons t (reduce-lookaheads r))
	       (reduce-rule-name r)))

;; goto= : Goto Goto -> Boolean
;; Returns true if the arguments are equal Gotos
(define (goto=? g1 g2)
  (and (eq? (goto-nonterm g1) (goto-nonterm g2))
       (eq? (goto-from g1)    (goto-from g2))
       (eq? (goto-next g1)    (goto-next g2))))

;;state= : State State -> Boolean
;;Returns true if the two inputs are equal.
(define (state=? s1 s2)
  (and (eq?            (state-name s1)    (state-name s2))
       (list=? shift=  (state-shifts s1)  (state-shifts s2))
       (list=? reduce= (state-reduces s1) (state-reduces s2))
       (list=? eq?     (state-accepts s1) (state-accepts s2))))

;; rule= : Rule Rule -> Boolean
;; Returns true if the two inputs are equal Rules
;; (with the exception of semantic actions)
(define (rule=? r1 r2)
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
(define (update-or-add list pred? modify or-add)
  (if (null? list)
      (list or-add)
      (if (pred? (car list))
	  (cons (modify (car list))
		(cdr list))
	  (cons (car list)
		(update-or-add (cdr list) pred? modify)))))

(if (not (equal? (update-or-add '(1 2 3 4 5 6 7) (lambda (x) (= x 3)) (lambda (x) (* 2 x)) 'f)
		 '(1 2 6 4 5 6 7)))
    (error "update-or-add failed test 1"))
(if (not (equal? (update-or-add '(1 2 3 4 5 6 7) (lambda (x) (= x 12)) (lambda (x) (* 2 x)) 'f)
		 '(1 2 3 4 5 6 7 f)))
    (error "update-or-add failed test 2"))


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
		(update-or-add (lambda (cur)
				 (eq? (shift-next-state cur) destination))
			       (lambda (cur)
				 (shift+token cur lookahead))
			       (make-shift (list lookahead) destination)))))))

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
		(update-or-add (lambda (cur)
				 (eq? (reduce-rule-name cur) rule-name))
			       (lambda (cur)
				 (reduce+token cur lookahead))
			       (make-reduce (list lookahead) rule-name)))))))

;; parse-state-sexp : State-Sexp -> (values State [Listof Goto])
;; Read a state sexp into a State and a list of Goto.
;; A State-Sexp is a (StateName State-Clause ...) and a State-Clause is
;; a shift, goto, reduce, accept, or comment.
(define (parse-state sexp)
  (let* ((name (car sexp))
	 (clauses (cdr sexp)))
    (let gather ((remain clauses)
		 (gotos '())
		 (shift-clauses '()) ; [Listof (Token StateName)]
		 (reduce-clauses '()) ; [Listof (Token RuleName)]
		 (accepts '()))
      (if (null? remain)
	  (values (make-state name
			      (build-shifts shift-clauses)
			      (build-reduces reduce-clauses)
			      accepts)
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
	       (gather rest gotos shift-clauses reduce-clauses (cons data accepts)))
	      ((GOTO)
	       (let* ((nonterm (car data))
		      (next (cadr data))
		      (new-goto (make-goto nonterm name next)))
		 (gather rest (cons new-goto gotos) shift-clauses reduce-clauses acceps)))))))))


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
	     (receive (new-state new-gotos) (gather-state data)
		      (gather-main rest
				   (cons new-state states)
				   (append new-gotos gotos)
				   rules
				   noshifts
				   start)))
	    ((RULE)
	     (gather-main rest states gotos (cons (apply make-rule data) rules) noshifts start))
	    (else (error "Unrecognized PDA clause.")))))))


;;; AST->Sexp PDA unparser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code converts a PDA term from its AST form into the corresponding sexp.

;....
