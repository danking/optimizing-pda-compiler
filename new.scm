;;;
; Input Type Definition
;;;

; The LALR(1) Table Constructor takes one of these records as input
(define-record cfg

  ; This is a list.  The elements of the list are either Scheme symbols or
  ; precedence records.  These represent the terminal symbols in the grammar.
  ; Precedence records appearing earlier have higher precedence than those
  ; appearing later.  Scheme symbols are terminal symbols with no defined
  ; precedence.
  terminals

  ; This is a Scheme symbol which will be returned by the lexer when the end of
  ; the input has been reached.  It must not appear in either 'terminals or
  ; 'rules.
  eoi

  ; This is the non-terminal that is the start symbol
  start

  ; This is a list of rule records
  rules)

; This is a record used to specify terminal symbols with defined precedence.
(define-record precedence

  ; One of (left right non).  This is the associativity of 'terminals.
  associativity

  ; A list of terminal symbols in this precedence class.
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
; Internal Type Definitions
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
  (num-nonterminals #f)

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
  (rule-items #f)

  ; These are both vectors where each element corresponds to the same numbered
  ; element in rule-lhs & rule-rhs.  'rule-precedence gives the terminal symbol
  ; that defines the precedence for the rule or #f if no precedence is defined.
  ; 'rule-action is the semantic action for the rule.
  (rule-precedence #f)
  (rule-action #f)

  ; These is the list of precedence-records that was received as input
  (precedence #f)
  )

;;;
; Code
;;;

(define (create-lalr-parser grammar)
  (let ((lalr-record (convert-grammar grammar)))
    lalr-record))

;;-
(define (convert-grammar grammar)
  (if (not (cfg? grammar))
      (error "Input is not a CFG Record."))

  (let* ((lalr-record (make-lalr-constructor))
	 (terminals (extract-terminals (cfg:terminals grammar)))
	 (precedence (extract-precedence (cfg:terminals grammar)))
	 (nonterminals (extract-nonterminals (cfg:rules grammar)))
	 (num-nonterminals (+ 1 (length nonterminals)))
	 (symbols (create-symbol-map nonterminals num-nonterminals terminals))
	 (reverse-map (create-reverse-map symbols)))

    ; Deal with the symbol for end-of-input
    (let ((eoi (cfg:eoi grammar)))
      (if (not (symbol? eoi))
	  (error "cfg:eoi must be a symbol."))
      (if (table-ref reverse-map eoi)
	  (error "cfg:eoi cannot be a terminal or nonterminal symbol:" eoi))

      (vector-set! symbols num-nonterminals eoi)
      (table-set! reverse-map eoi num-nonterminals))

    ; Create the new start symbol (i.e. S' -> S <eoi>)
    (let ((start (cfg:start grammar)))
      (if (or (not (symbol? start))
	      (not (table-ref reverse-map start))
	      (not (< (table-ref reverse-map start) num-nonterminals)))
	  (error "cfg:start must be a nonterminal symbol"))

      (vector-set! symbols 0 (create-start-symbol start reverse-map))
      (table-set! reverse-map (vector-ref symbols 0) 0))

    (set-lalr-constructor:num-nonterminals lalr-record num-nonterminals)
    (set-lalr-constructor:symbols lalr-record symbols)
    (set-lalr-constructor:precedence lalr-record precedence)

    ; Compile the grammar
    (receive (rule-lhs rule-rhs rule-items)
	     (pack-grammar (cfg:rules grammar) reverse-map num-nonterminals)

	     (set-lalr-constructor:rule-lhs lalr-record rule-lhs)
	     (set-lalr-constructor:rule-rhs lalr-record rule-rhs)
	     (set-lalr-constructor:rule-items lalr-record rule-items))

    ; Store the precedence and actions for later use
    (set-lalr-constructor:rule-precedence lalr-record
					  (extract-rule-prec (cfg:rules grammar)
							     num-nonterminals
							     reverse-map))
    (set-lalr-constructor:rule-action lalr-record
     (list->vector (cons* '() '$1 (map rule:action (cfg:rules grammar)))))

    lalr-record))

(define (extract-terminals grammar-terminals)
  (if (not (list? grammar-terminals))
      (error "cfg:terminals is not a list"))
  (fold-right
   (lambda (element term-list)
     (cond ((symbol? element)
	    (if (member element term-list)
		(error "Duplicate terminal definition:" element)
		(cons element term-list)))
	   ((precedence? element)
 	    (if (list? (precedence:terminals element))
 		(fold-right (lambda (element term-list)
 			      (if (symbol? element)
 				  (if (member element term-list)
 				      (error "Duplicate terminal definition:"
 					     element)
 				      (cons element term-list))
 				  (error "Invalid terminal symbol:" element)))
 			    term-list (precedence:terminals element))
 		(error "precedence:terminals must be a list")))
	   (else
	    (error "Invalid terminal declaration:" element))))
   '() grammar-terminals))

(define (extract-precedence grammar-terminals)
  (fold-right
   (lambda (element prec-list)
     (if (precedence? element)
	 (if (member (precedence:associativity element) '(left right non))
	     (cons element prec-list)
	     (error "Unknown associativity:"
		    (precedence:associativity element)))
	 prec-list))
   '() grammar-terminals))

(define (extract-nonterminals grammar-rules)
  (if (not (list? grammar-rules))
      (error "cfg:rules is not a list"))
  (delete-duplicates
   (map
    (lambda (element)
      (if (rule? element)
	  (let ((x (rule:left-side element)))
	    (if (symbol? x)
		x
		(error "Invalid nonterminal name:" x)))
	  (error "cfg:rules must contain only rule records")))
    grammar-rules)))

(define (create-symbol-map nonterminals num-nonterminals terminals)
  (let ((symbols (make-vector (+ num-nonterminals (length terminals) 1))))
    (let loop ((i 1) (values nonterminals))
      (cond ((pair? values)
	     (vector-set! symbols i (car values))
	     (loop (+ i 1) (cdr values)))
	    ((= i num-nonterminals)
	     (loop (+ i 1) terminals))
	    (else
	     symbols)))))

(define (create-reverse-map symbols)
  (let ((table (make-symbol-table)))
    (let loop ((i 0))
      (if (< i (vector-length symbols))
	  (let ((symbol (vector-ref symbols i)))
	    (if (symbol? symbol)
		(table-set! table symbol i))
	    (loop (+ i 1)))))
    table))

(define (create-start-symbol grammar-start reverse-map)
  (let loop ((sym '*start))
    (if (table-ref reverse-map sym)
	(loop (string->symbol (string-append (symbol->string sym) "*")))
	sym)))

(define (pack-grammar grammar-rules reverse-map eoi-num)
  (let* ((num-rules (+ (length grammar-rules) 1))
	 (rule-lhs (make-vector num-rules #f))
	 (rule-rhs (make-vector num-rules #f))
	 (num-items (fold (lambda (rule num-items)
			    (+ num-items (length (rule:right-side rule)) 1))
			  0 grammar-rules))
	 (rule-items (make-vector (+ 1 num-items) #f)))

    (let rule-loop ((rules grammar-rules) (item-num 0) (rule-num 1))
      (if (pair? rules)
	  (begin
	    (vector-set! rule-lhs rule-num
			 (table-ref reverse-map (rule:left-side (car rules))))
	    (vector-set! rule-rhs rule-num item-num)
	    (let item-loop ((syms (rule:right-side (car rules)))
			    (item-num item-num))
	      (if (pair? syms)
		  (let ((num (table-ref reverse-map (car syms))))
		    (cond ((eq? num #f)
			   (error "Unknown symbol:" (car syms)))
			  ((= num eoi-num)
			   (error "cfg:eoi cannot be used in the grammar"))
			  (else
			   (vector-set! rule-items item-num num)
			   (item-loop (cdr syms) (+ item-num 1)))))
		  (begin
		    (vector-set! rule-items item-num (- rule-num))
		    (rule-loop (cdr rules) (+ item-num 1) (+ rule-num 1))))))))

    (values rule-lhs rule-rhs rule-items)))

(define (extract-rule-prec grammar-rules num-nonterminals reverse-map)
  (list->vector (cons* #f #f
   (map
    (lambda (rule)
      (let ((prec (rule:precedence rule)))
	(cond ((not prec) #f)
	      ((table-ref reverse-map prec) =>
	       (lambda (num)
		 (if (< num num-nonterminals)
		     (error
		      "Precedence cannot be based on a nonterminal symbol:"
		      prec)
		     num)))
	      (else
	       (error "Unknown terminal symbol:" prec)))))
    grammar-rules))))
