

;;;
; "Type" Definitions
;;;

; A single instance of this record is created every time a new parser is
; constructed.  It starts out mostly empty and it is passed from function to
; function as different computations take place.  By the time every function has
; executed, a completed LALR(1) parser will be stored in this record.
(define-record lalr-constructor

  ; This is a vector containing every terminal and non-terminal symbol in the
  ; grammar.  Internally, Scheme symbols are not used.  Symbols from the grammar
  ; are assigned a number instead.  By indexing that number into this vector,
  ; you can convert that number back into the original Scheme symbol.
  ;
  ; Unless explicitly stated, any reference to a non-terminal or terminal symbol
  ; in the rest of this documentation really refers to the symbol's number.
  ;
  ; Also note that all the non-terminals are listed before any of the terminals
  ; in this vector.  See 'num-non-terminals for further explanation.
  (symbols #f)

  ; This is the number of non-terminal symbols in the grammar.  It is also the
  ; number of the first terminal symbol in 'symbols.  The test to see if a
  ; particular symbol is a non-terminal is to test if it is less than this
  ; number.
  (num-non-terminals #f)

  ; These three fields function together as a dense representation of the
  ; grammar.  All three are vectors.  'rule-lhs and 'rule-rhs are of length
  ; 'nrules and 'rule-items is of length (+ 1 ritems).  Each element in the
  ; first two (except index 0) corresponds to a rule.  'rule-lhs contains the
  ; non-terminal on the "left hand side" for that particular rule.  'rule-rhs
  ; gives the index into 'rule-items where "right hand side" of the rule starts.
  ;
  ; 'rule-items contains the "right hand side" of every rule in the grammar.
  ; The first element is the first symbol of the first rule.  A negative number
  ; appears to mark the end of a rule.  The absolute value of this negative
  ; number is the index into 'rule-lhs & 'rule-rhs for the rule just finished.
  ;
  ; As an example, the simple grammar:
  ; S -> A B
  ; A -> a b c
  ; B -> {empty}
  ; B -> ( B )
  ;
  ; Would appear as:
  ; rule-lhs = #( #f S A B B )
  ; rule-rhs = #( #f 0 3 7 8 )
  ; rule-items = #( A B -1 a b c -2 -3 ( B ) -4 )
  ;
  ; Note that symbols were used for clarity.  In reality, the symbols would be
  ; replaced by their corresponding numbers.  Also note that the first element
  ; of 'rule-lhs and 'rule-rhs is not used because 0 is not negative.
  ;
  ; This representation is used because it makes constructing the LR(0) parser
  ; fast and easy.  Items are simply indexes into 'rule-items and states are
  ; simply sets of such indexes.  "Goto" is performed by adding 1 to each item
  ; in a state.  Reductions are possible when a negative number is reached and
  ; the rule to reduce by is simply the absolute value of that number.
  (rule-lhs #f)
  (rule-rhs #f)
  (rule-items #f))

; This is the record that the parser takes as input
(define-record cfg
  ; This is a list.  The elements of the list are either Scheme symbols or
  ; precedence records.  These represent the terminal symbols in the grammar.
  ; Precedence records appearing earlier have higher precedence than those
  ; appearing later.  Scheme symbols are terminal symbols with no defined
  ; precedence.
  terminals
  ; This is a list of rule records
  rules)

; This is a record used to specify terminal symbols with defined precedence.
(define-record precedence
  ; One of (left right non).  This is the associativity of 'terminals.
  associativity
  ; A list of symbols in this precedence class.
  terminals)

; This record represents a rule in the grammar
(define-record rule
  ; This will be a Scheme symbol that represents the non-terminal symbol on the
  ; left side of the rule.
  left-side
  ; This is a list of Scheme symbols represent that symbols on the right side of
  ; the rule.
  right-side
  ; This is either a terminal symbol or #f.  If it is a terminal symbol, it is
  ; the terminal symbol to use as the precedence for this rule.  If it is #f,
  ; then the precedence for the rule will be the precedence of the last terminal
  ; symbol in the rule (if any).
  precedence
  ; This is the semantic action for the rule.  No operations are done on it and
  ; it will appear in the output unmodified.
  action)

;;;
; Code
;;;

(define (create-lalr-parser grammar)
  (rewrite-grammar grammar (make-lalr-constructor)))

;;-
; This function does several checks on 'grammar to verify that it is valid.  It
; also begins to seperate the grammar into various pieces before calling
; 'pack-grammar.
;
; Arguments:
; grammar = The grammar as it was received from the user.
; lalr-record = The lalr-constructor record that we are working from
(define (verify-grammar grammar lalr-record)
  (if (not (cfg? grmmar))
      (error "Input is not a CFG Record."))
  (if (not (list? (cfg:terminals grammar)))
      (error "cfg:terminals is not a list"))
  (if (not (list? (cfg:rules grammar)))
      (error "cfg:rules is not a list"))

;   (let* ((term-func
; 	  (lambda (element term-list)
; 	    (cond ((symbol? element)
; 		   (if (not (member element term-list))
; 		       (cons element term-list)
; 		       (error "Duplicate terminal definition:" element)))
; 		  ((precedence? element)
		   

; 		   (let* ((terminals (fold-right (lambda (element term-list)
				  

  (let loop ((terminals (cfg:terminals grammar)))
    (if (not (symbol? (car terminals)))
	(error "Invalid terminal symbol:" (car terminals)))
    (if (member (car terminals) (cdr terminals))
	(error "Repeated terminal symbol:" (car terminals)))
    (if (not (null? terminals))
	(loop (cdr terminals))))

  (let loop ((precedence (cfg:precedence grammar)))
    (if (pair? precedence)
	(begin
	  (if (not (pair? (car precedence)))
	      (error "Invalid precedence list:" (car precedence)))
	  (if (not (member (caar precedence) '(left right non)))
	      (error "Unknown associativity type:" (caar precedence)))
	  (let loop2 ((term-list (cdar precedence)))
	    (if (pair? term-list)
		(begin
		  (if (not (member (car term-list) (cfg:terminals grammar)))
		      (error
		       "Precedence terminal does not appear in terminal list:"
		       (car term-list))
		      (loop2 (cdr term-list))))))
	  (loop (cdr precedence)))))

; This functions takes a grammar in the user format and does some
; transformations on it before calling 'do-things.
(define (rewrite-grammar grammar)
  (let ( (terms (if (not (pair? grammar))
		    (error "Grammar definition must be a non-empty list")
		    (car grammar)) )
	 (rules (cdr grammar)) )
    (let term-loop ( (terms terms)
		     (rev-terms '())
		     (r-assoc '())
		     (l-assoc '())
		     (non-assoc '())
		     (prec '()) )
	(if (not (null? terms))
	    (if (not (pair? (car terms)))
		; Handle terminals with no defined associativity
		(term-loop (cdr terms)
			   (check-term (car terms) rev-terms)
			   r-assoc
			   l-assoc
			   non-assoc
			   prec)
		; Handle an associativity list
		(receive (r-terms ass p) (process-assoc (cdar terms)
							rev-terms
							'()
							'())
			 (cond ( (eq? (caar terms) 'right)
				 (term-loop (cdr terms)
					    r-terms
					    (append ass r-assoc)
					    l-assoc
					    non-assoc
					    (cons p prec)) )
			       ( (eq? (caar terms) 'left)
				 (term-loop (cdr terms)
					    r-terms
					    r-assoc
					    (append ass l-assoc)
					    non-assoc
					    (cons p prec)) )
			       ( (eq? (caar terms) 'non)
				 (term-loop (cdr terms)
					    r-terms
					    r-assoc
					    l-assoc
					    (append ass non-assoc)
					    (cons p prec)) )
			       (else (error "Associativity unknown:"
					    (caar terms))) )))
	    ; At this point the state of the variables is as follows:
	    ; - terms is null
	    ; - rev-terms is a list of all terminals (in reverse)
	    ; - r-assoc is a list of all the right associative terminals
	    ; - l-assoc is a list of all the left associative terminals
	    ; - non-assoc is a list of all the non-associative terminals
	    ; - prec is a list of lists.  Each internal list is a list of
	    ;   terminals in a particular precedence class.  The lists are in
	    ;   decreasing order of precedence.
	    (begin

	      ; Start by checking the production rules for rules that define an
	      ; invalid non-terminal or a non-terminal with the same name as a
	      ; terminal symbol
	      (any (lambda (r)
		     (if (not (pair? r))
			 (error "Rule must be a non-empty list")
			 (cond ((not (valid-nonterminal? (car r)))
				(error "Invalid nonterminal:" (car r)))
			       ((member (car r) rev-terms)
				(error "Nonterminal previously defined:"
				       (car r))) )))
		   rules)

	      (let* (
		      ; The list of terminals with EOI added
		      (terms (cons eoi (reverse rev-terms)))

		      ; The list of non-terminals with the start symbol added
		      (nonterms
		       (if (null? rules)
			   (error "Grammar must contain at least one nonterminal")
			   (cons start
				 (fold-right
				  (lambda (rule nts)
				    (if (member (car rule) nts)
					(error "Nonterminal previously defined:"
					       (car rule))
					(cons (car rule) nts)))
				  '()
				  rules))))

		      ; The result of calling 'rewrite-nonterm-def on all the
		      ; non-terminal definitions.  See that function for more
		      ; information.
		      (compiled-nonterminals
		       (map (lambda (rule)
			      (rewrite-nonterm-def rule
						   terms
						   nonterms))
			    (cons `(,start ((,(cadr nonterms) ,eoi) 
					     $1))
				  rules))) )

		; Convert the various variables (mostly 'compiled-nonterminals)
		; to the forms that 'do-things wants and then call it.
		(do-things terms
			   nonterms
			   (map (lambda (x) (cons (caar x) (map cdr x)))
				(map (lambda (x) 
				       (map prod-action:production x))
				     compiled-nonterminals))
			   (apply append 
				  (map (lambda (y)
					 (map (lambda (x) 
						(cons (prod-action:production x)
						      (prod-action:action x)))
					      y))
				       compiled-nonterminals))
			   l-assoc
			   r-assoc
			   non-assoc
			   (reverse prec)
			   (cons #f
				 (apply append
					(map (lambda (x) (map prod-action:prec
							      x))
					     compiled-nonterminals))))))) ) ) )

; Small funciton used by 'process-assoc and 'rewrite-grammar to check the
; validity of a given terminal.
;
; Arguments:
; - term = the terminal symbol in question
; - rv = a list of previous terminals
; Returns
;   If 'term is valid, then 'rv with 'term cons in front of it, otherwise it
; calls 'error
(define (check-term term rv)
  (cond ((not (valid-terminal? term)) (error "Invalid terminal:" term))
	((member term rv) (error "Terminal previously defined:" term))
	(else (cons term rv))) )

; This function is used be 'rewrite-grammar to check the validity of terminals
; that have defined associativity.
;
; Arguments
; - it = The list of terminals in the association list
; - r-terms = This list of termianls seen so far
; - ass = null (used internally)
; - prec = null (used internally)
; Returns 3 values
; 1) 'r-terms with all the newly defined terminals added to it
; 2) (reverse it)
; 3) (reverse it) -> Yes, the same as #2
(define (process-assoc it r-terms ass prec)
  (if (null? it) 
      (values r-terms ass prec)
      (process-assoc (cdr it) 
		     (check-term (car it) r-terms) 
		     (cons (car it) ass) 
		     (cons (car it) prec))) )

; Checks to see if its argument is a valid non-terminal symbol
(define valid-nonterminal? symbol?)

; Checks to see if its argument is a valid terminal symbol
(define valid-terminal? symbol?)

; This function is used by 'rewrite-grammar to convert a non-terminal definition
; into a more useable form.
;
; Arguments:
; - nonterm-def = a non-terminal definition from the grammar
; - terms       = the list of terminal symbols
; - nonterms    = the list of non-terminal symbols
; Returns
;   A list of 'prod-action records.  The fields in each record are as follows:
; - production = A list of the (non-)terminals involved in the production.
;                Rather than symbols, their numeric index into 'terms or
;                'non-terms is given (with 'terms considered to be appended to
;                'non-terms).  The first element is the non-terminal that the
;                sequence reduceds to.  The rest of the elements are the right
;                side of the production.
; - action     = The (unmodified) action for the given production
; - prec       = The terminal symbol that gives the precedence for this
;                production or #f if there is no precedence.
(define (rewrite-nonterm-def nonterm-def terms nonterms)
  (let* ( (No-NT (length nonterms))

	  ; Encode takes either a terminal or non-terminal symbol and converts
	  ; the symbol into a number.  This number is based on its position in
	  ; either 'terms or 'nonterms.
	  (encode (lambda (x)
		    (let ((PosInNT (list-index (lambda (term)
						 (equal? x term)) nonterms)))
		      (if PosInNT
			  PosInNT
			  (let ((PosInT (list-index (lambda (term)
						      (equal? x term)) terms)))
			    (if PosInT
				(+ No-NT PosInT)
				(error "undefined symbol : " x))))))) )

    (if (not (pair? (cdr nonterm-def)))
	(error "At least one production needed for nonterminal"
	       (car nonterm-def))
	(let loop ((lst (cdr nonterm-def))
		   (i 1)
		   (rev-productions-and-actions '()))
	  (if (not (pair? lst))
	      (reverse rev-productions-and-actions)
	      (let* ((prec (if (equal? (length (car lst)) 3)
			       (cadaar lst)
			       #f))
		     (rhs  (if (equal? (length (car lst)) 3)
			       (list-ref (car lst) 1)
			       (caar lst)))
		     (rest  (cdr lst))
		     (prod (map encode (cons (car nonterm-def) rhs))))
		(for-each (lambda (x)
			    (if (not (or (member x terms) (member x nonterms)))
				(error "Invalid terminal or nonterminal" x)))
			  rhs)
		(loop rest
		      (+ i 1)
		      (cons
		       (make-prod-action prod
					 (if (equal? (length (car lst)) 3)
					     (list-ref (car lst) 2)
					     (list-ref (car lst) 1))
					 prec)
		       rev-productions-and-actions))))))) )

;;-

; This functions as the high-level branching procudure.
; It sets several global variables and then calls several other procedures to
; do processing on those variables.
;
; Sample Grammar (used below)
; S -> A       : $1
;    | S A     : (+ $1 $2)
; A -> ;empty  : 1
;    | A x y   : (* $1 $2 $3)
;
; Arguments
; - terms = The list of terminal symbols
; - vars = The list of non-terminal symbols
; - gram = A list that looks like the following:
;          '( (S (A)
;                (S A))
;             (A ()
;                (A x y)) )
;          Note that, instead of symbols, the terminals and non-terminals will
;          be replaced by the corresponding numbers.
; - gram/actions = A list that looks like the following:
;          '( ((S A) . $1)
;             ((S S A) . (+ $1 $2))
;             ((A) . 1)
;             ((A x y) . (* $1 $2 $3)) )
;          Again, symbols are replaced by corresponding numbers.
; - la = The list of left-associative terminal symbols
; - ra = The list of right-associative terminal symbols
; - na = The list of non-associative terminal symbols
; - prec = A list of lists.  Each internal list is a list of terminals in a
;          particular precedence class.  The lists are in increasing order of
;          precedence.
; - rprec = A list containing either terminal symbols or #f.  Each element
;           corresponds to an element in 'gram/actions.  It gives the precedence
;           for the corresponding rule or #f if there is no precedence.
(define (do-things terms vars gram gram/actions la ra na prec rprec)
  (set! the-terminals (list->vector terms))
  (set! the-nonterminals (list->vector vars))
  (set! nterms (length terms))
  (set! nvars  (length vars))
  (set! nsyms  (+ nterms nvars))
  (set! global-terms terms)

  (let ( (rule-preced (list->vector rprec)) 
	 (no-of-rules (length gram/actions))
	 (no-of-items (let loop ((l gram/actions) (count 0))
			(if (null? l) 
			    count
			    (loop (cdr l) (+ count (length (caar l))))))) )
    (pack-grammar no-of-rules no-of-items gram)
    (calculate-precedence rule-preced)
    (set-derives)
    (set-nullable)
    (generate-states)
    (lalr)
    (build-tables prec ra na rule-preced)
    (compact-action-table) ) )
