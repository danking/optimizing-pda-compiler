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

  ; This is a vector of length 'num-nonterminals.  Its values are lists of
  ; rule numbers (indexes in 'rule-rhs & 'rule-lhs).  The vector gives, for each
  ; non-terminal symbol, the list of rules of which it is on the left-hand-side.
  (derives #f)

  ; This is a vector of length 'num-nonterminals.  Its values are lists of rule
  ; numbers.  Its purpose is to make 'compute-closure fast.
  ;
  ; The values are the result of unioning the 'derives sets for every
  ; non-terminal in "this" non-terminal's First set.  The code comments for
  ; 'compute-firsts give the best description of what the First set is.
  (first-derives #f)

  ; This is a vector of length 'num-nonterminals.  Its values are either #t or
  ; #f.  It specifies which non-terminal symbols can derive the empty string and
  ; which cannot.
  (nullable #f)

  ; This is a vector containing the states in the LR(0) parser.  The first state
  ; is in the first slot.  See the description of the state-record for more
  ; information.
  (states #f)

  ; This vector is the same size as 'states.  Its values are #t or #f.  It marks
  ; individual states as either consistent or inconsistent.  A state is
  ; inconsistent if a reduction by more than one rule may occur in the state or
  ; if there is at least one reduction and no non-terminal transitions.
  (consistent #f)

  ; This is a vector whose size is one greater than 'states.  It maps a given
  ; state to the number of reductions that can take place in all inconsistent
  ; states numbered less than the state in question.  The last element contains
  ; the sum for all the states.
  ;
  ; The purpose of the vector is to serve as an index into 'reduction-rule-num,
  ; 'lookback, and 'LA.  If a state is inconsistent, then this vector maps the
  ; index into those vectors where the data for said state starts.
  (reduction-map #f)

  ; This vector is the same size as the last element in 'reduciton-map (which
  ; could be 0).  It contains, in order of state number, the rule numbers of
  ; reductions that take place in inconsistent states.
  (reduction-rule-num #f)

  ; This vector is the same size as 'reduction-rule-num.  It maps a reduction to
  ; the list of non-terminal transitions that "this" reduction can immediately
  ; cause.
  (lookback #f)

  ; This vector is the same size as 'reduction-rule-num.  Its values are bitsets
  ; where every bit corresponds to a terminal symbol.  It maps a reduction to
  ; the list of terminals that can appear on the front of the input-string if
  ; this reduction were to take place.  This is the end result of the LALR
  ; analysis.
  (LA #f)

  ; This is a vector of length (+ num-nonterminals 1).  Each element corresponds
  ; to a non-terminal symbol.  The value for a particular non-terminal is the
  ; sum of the number of transistions that each non-terminal before it was
  ; involved in.  The last element is the sum for the entire grammar.
  ;
  ; The purpose of the vector is to map a given non-terminal to the start of the
  ; data for it in 'from-state and 'to-state.
  (goto-map #f)

  ; These vectors are the same size as the last element in 'goto-map.  Each
  ; element corresponds to non-terminal transition.  The values are the numbers
  ; (indexes in 'states) of the starting and ending states (respectively) for
  ; the transition.
  (from-state #f)
  (to-state #f)

  ;
  (reads #f)
  (includes #f)

  ;
  (follow #f)
  )

;;; States, Shifts, and Reductions
(define-record state

  ; This is a number unique to the state.  It is the same number as its index in
  ; the 'states vector.
  number

  ; This is the symbol that was shifted just prior to arriving in this state
  access-symbol

  ; This is the (sorted) set of (kernel) items in this state.  The items are
  ; represented by indexes into 'rule-items.  The "dot" is considered to be
  ; immediately before the index given.
  items

  ; This is the list of state records that represent states that can be reached
  ; from "this" state by shifting a symbol.  The list is in descending order
  ; according to the value of the records' 'access-symbol field.  In particular,
  ; this means that non-terminal shifts (a.k.a. "gotos") come last.
  (shifts #f)

  ; This is a list of rule numbers (indexes into 'rule-lhs & 'rule-rhs).  This
  ; represents which rules can be reduced by in this state.
  (reductions #f))

;;; Sets
; Sets are represented by sorted lists.

; This function adds 'element to 'set
(define (set-insert element set)
  (if (null? set)
      (cons element '())
      (let ((x (car set)))
	(cond ((< element x) (cons element set))
	      ((> element x) (cons x (set-insert element (cdr set))))
	      (else set)))))

; This function takes two sets and computes their union.
(define (set-union set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((a (car set1)) (b (car set2)))
		(cond ((> a b) (cons b (set-union set1 (cdr set2))))
		      ((< a b) (cons a (set-union (cdr set1) set2)))
		      (else (set-union (cdr set1) set2)))))))

;;; Bit-Sets
; Bit-Sets are represented as integers because Scheme has infinite precision

; Computes the union of two bit-sets
(define bit-union bitwise-ior)

; Sets the 'i-th bit of 'bitset to 1.
(define (set-bit bitset i)
  (bit-union bitset (expt 2 i)))

;;; Vector Manipulation Functions
(define (vector-copy vector . size)
  (let* ((size (if (null? size) (vector-length vector) (car size)))
	 (result (make-vector size)))
    (let loop ((i 0))
      (if (< i size)
	  (begin
	    (vector-set! result i (vector-ref vector i))
	    (loop (+ i 1)))
	  result))))


;;;
; Code
;;;

(define (test)
  (eval '(create-lalr-parser tiger-grammar) (interaction-environment)))

; - DR = One terminal-bitset for every non-terminal transistion.  Gives the list
;   of terminal symbols that may appear immediately after a non-terminal
;   with no nullable non-terminals shifted in-between (for that, see Read).
; - reads - One list of non-terminal transisions for every non-terminal
;   transision.  For every non-terminal transision, it gives the list of
;   nullable non-terminal transisions that can be made immediately after it.
; - Read = Read(p,A) = DR(p,A) u U{Read(r,C)|(p,A) reads (r,C)}
; - includes = One list of non-terminal transisions for every non-terminal
;   transision.  Basically, if "A -> B C D", and 'D' is nullable, then the
;   Follow set of both 'C' and 'D' "includes" the follow set of 'A'.
; - Follow = Follow(p,A) = Read(p,A) u U{Follow(p',B)|(p,A) includes (p',B)}
;   This contains one terminal-bitset for every non-terminal transision.  It
;   gives every terminal symbol that can appear on the input string immediately
;   after a non-terminal transision.
; - lookback = One list of non-terminal transitions for every inconsistent
;   reduction.  This maps each reduction to the list of possible non-terminal
;   shifts that could occur immediately after the reduction takes place
; - LA = LA(q,A->w) = U{Follow(p,A)|(q,A->w) lookback (p,A)}
;   The is the goal of the LALR algorithm.  It gives the list of
;   terminal symbols that can appear on the input string after a reduction takes
;   place.  In other words, it gives the list of terminal-symbols that should
;   cause a "reduce" in the engine.
;
; - consistent = tells which states are consistent
; - reduction-map = maps a state to the start of its information in LAruleno,
;   lookback, and LA
; - reduction-rule-num = lists the rules that are reduced by in each
;   inconsistent state
(define (create-lalr-parser grammar)
  (let ((lalr-record (convert-grammar grammar)))
    (compute-precedence lalr-record)
    (compute-derives lalr-record)
    (compute-nullable lalr-record)

    (compute-first-derives lalr-record)
    (compute-LR0-states lalr-record)

    (compute-consistent-and-reduction-map lalr-record)
    (compute-reduction-rule-num lalr-record)
    (compute-goto-map lalr-record)
    (compute-DR-and-reads lalr-record)
    (compute-includes-and-lookback lalr-record)
    (compute-follow lalr-record)
    (compute-LA lalr-record)

    lalr-record))

;;-
; This function takes the grammar as it was passed in and converts into the
; representations used internal to the parser.  Along the way, it does a lot
; of sanity-checking on the grammar.
;
; This function sets the values of the following lalr-constructor fields:
; - symbols
; - num-nonterminals
; - rule-lhs
; - rule-rhs
; - rule-items
; - rule-precedence (This value will be modified by 'calculate-precedence)
; - rule-action
; - precedence
(define (convert-grammar grammar)
  (if (not (cfg? grammar))
      (error "Input is not a CFG Record."))

  (let* ((lalr-record (make-lalr-constructor))
	 (terminals (extract-terminals (cfg:terminals grammar)))
	 (precedence (extract-precedence (cfg:terminals grammar)))
	 (nonterminals (extract-nonterminals (cfg:rules grammar)))
	 (num-nonterminals (+ 1 (length nonterminals)))
	 (eoi (cfg:eoi grammar)))

    (receive
     (symbols reverse-map)
     (create-symbol-map nonterminals num-nonterminals terminals)

     ; Deal with the symbol for end-of-input
     (if (not (symbol? eoi))
	 (error "cfg:eoi must be a symbol."))
     (if (table-ref reverse-map eoi)
	 (error "cfg:eoi cannot be a terminal or nonterminal symbol:" eoi))

     (vector-set! symbols num-nonterminals eoi)
     (table-set! reverse-map eoi num-nonterminals)

     ; Create the new start symbol (S' -> S <eoi>)
     (let* ((start (cfg:start grammar))
	    (new-start (create-start-symbol start reverse-map))
	    (aug-grammar (cons (make-rule new-start (list start eoi) #f '$1)
			       (cfg:rules grammar))))

       (if (or (not (symbol? start))
	       (not (table-ref reverse-map start))
	       (not (< (table-ref reverse-map start) num-nonterminals)))
	   (error "cfg:start must be a nonterminal symbol"))

       (vector-set! symbols 0 new-start)
       (table-set! reverse-map new-start 0)

       ; Compile the grammar
       (receive
	(rule-lhs rule-rhs rule-items)
	(pack-grammar aug-grammar reverse-map num-nonterminals)

	(set-lalr-constructor:rule-lhs lalr-record rule-lhs)
	(set-lalr-constructor:rule-rhs lalr-record rule-rhs)
	(set-lalr-constructor:rule-items lalr-record rule-items))

       ; Store the precedence and actions for later use
       (set-lalr-constructor:rule-precedence lalr-record
					     (extract-rule-prec aug-grammar
								num-nonterminals
								reverse-map))
       (set-lalr-constructor:rule-action lalr-record
	(list->vector (cons '() (map rule:action aug-grammar)))))

     ; Set the last few fields and then return the record
     (set-lalr-constructor:num-nonterminals lalr-record num-nonterminals)
     (set-lalr-constructor:symbols lalr-record symbols)
     (set-lalr-constructor:precedence lalr-record precedence))

    lalr-record))

; This is a helper function used by 'convert-grammar.  It takes the list of
; terminals and precedence records in the input and returns a simple list of
; terminals.  Along the way, it checks for invalid terminals symbols and for
; duplicate declarations.
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

; This is a helper function used by 'convert-grammar.  It takes the list of
; terminals and precedence records and throws away all but the precedence
; records.  It also makes sure that the precedence records are valid.
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

; This is a helper function used by 'convert-grammar.  It takes the list of
; rules and returns a list of all the non-terminal symbols that the rules
; define.  It also checks to see that the non-terminal names are valid.
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

; This is a helper function used by 'convert-grammar.  It creates a mapping
; between the Scheme symbols used in the grammar to the numbers used internally.
; It returns two values.  The first is a vector that is stored in
; 'lalr-constructor.  It is used to map from numbers back to the symbols.  The
; section value is a hash-table that maps the symbols into the corresponding
; numbers.
(define (create-symbol-map nonterminals num-nonterminals terminals)
  (let ((symbol-map (make-vector (+ num-nonterminals (length terminals) 1)))
	(reverse-map (make-symbol-table)))
    (let loop ((i 1) (symbols nonterminals))
      (cond ((pair? symbols)
	     (vector-set! symbol-map i (car symbols))
	     (table-set! reverse-map (car symbols) i)
	     (loop (+ i 1) (cdr symbols)))
	    ((= i num-nonterminals)
	     (loop (+ i 1) terminals))
	    (else
	     (values symbol-map reverse-map))))))

; This is a helper function used by 'convert-grammar.  It creates a the new
; start symbol and insures that it does not conflict with any symbols already
; existing in the grammar.
(define (create-start-symbol grammar-start reverse-map)
  (let loop ((sym '*start))
    (if (table-ref reverse-map sym)
	(loop (string->symbol (string-append (symbol->string sym) "*")))
	sym)))

; This is a helper function used by 'convert-grammar.  It creates the 'rule-lhs,
; 'rule-rhs, and 'rule-items fields of the lalr-constructor.
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
			  ((and (= num eoi-num) (not (= rule-num 1)))
			   (error "cfg:eoi cannot be used in the grammar"))
			  (else
			   (vector-set! rule-items item-num num)
			   (item-loop (cdr syms) (+ item-num 1)))))
		  (begin
		    (vector-set! rule-items item-num (- rule-num))
		    (rule-loop (cdr rules) (+ item-num 1) (+ rule-num 1))))))))

    (values rule-lhs rule-rhs rule-items)))

; This is a helper function used by 'convert-grammar.  It takes a list of rules
; and returns a vector containing their precedence fields.  Along the the way
; it makes sure that any terminal symbols found are valid and coverts them to
; their numeric representation.  The returned vector has a dummy first element
; to correspond to the fact that 'rule-lhs and 'rule-rhs do not sure their first
; element either.
(define (extract-rule-prec grammar-rules num-nonterminals reverse-map)
  (list->vector (cons #f
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

;;-
; This function is responsible for computing the 'rule-precedence field.  The
; vector was created in 'convert-grammar where some of the values were set.
; These values correspond to rules whose precedence was manually specified and
; this function does not change them.  For the rest, the precedence is set to
; value of the last terminal-symbol in the rule or to #f if there are no
; terminal symbols.
(define (compute-precedence lalr-record)
  (let ((rule-prec (lalr-constructor:rule-precedence lalr-record))
	(rule-rhs (lalr-constructor:rule-rhs lalr-record))
	(rule-items (lalr-constructor:rule-items lalr-record))
	(num-nonterminals (lalr-constructor:num-nonterminals lalr-record)))
    (let rule-loop ((rule-num 1))
      (if (< rule-num (vector-length rule-prec))
	  (if (vector-ref rule-prec rule-num)
	      (rule-loop (+ rule-num 1))
	      (let item-loop ((item-num (vector-ref rule-rhs rule-num))
			      (cur-prec #f))
		(let ((item (vector-ref rule-items item-num)))
		  (cond ((< item -1)
			 (vector-set! rule-prec rule-num cur-prec)
			 (rule-loop (+ rule-num 1)))
			((< item num-nonterminals)
			 (item-loop (+ item-num 1) cur-prec))
			(else
			 (item-loop (+ item-num 1) item))))))))))

;;-
; This function is responsible for computing the 'derives field.  This is a
; mapping from a nonterminal symbol to the list of rule-numbers of which it is
; on the left-hand-side.
(define (compute-derives lalr-record)
  (let ((rule-lhs (lalr-constructor:rule-lhs lalr-record))
	(derives (make-vector (lalr-constructor:num-nonterminals lalr-record)
			      '())))
    (let loop ((rule-num 1))
      (if (< rule-num (vector-length rule-lhs))
	  (let ((non-term (vector-ref rule-lhs rule-num)))
	    (vector-set! derives non-term
			 (cons rule-num (vector-ref derives non-term)))
	    (loop (+ rule-num 1)))))
    (let loop ((i 0)) ;<------- Is the reverse loop necessary -------------- ???
      (if (< i (vector-length derives))
	  (begin
	    (vector-set! derives i (reverse (vector-ref derives i)))
	    (loop (+ i 1)))))
    (set-lalr-constructor:derives lalr-record derives)))

;;-
; This function is responsible for computing the 'nullable field.  This vector
; tells whether or not a given non-terminal symbol can derive the empty string.
;
; Algorithm:
; The algorithm is divided into two parts.  During the first part, the function
; loops over every rule and determines which ones are trivially nullable (they
; have nothing on the right side), which ones are trivially not nullable (they
; contain terminal symbols, which ones are uncertain (they contain only
; non-terminal symbols).  It constructs the following data structures:
; - rule-count = A vector where each element corresponds to a rule.  For the
;      uncertain rules, it contains the number of symbols on the right side.
; - rule-sets = A vector where each element corresponds to a non-terminal
;      symbol.  The values are lists of rule numbers.  The rules in question are
;      all uncertain rules where the given non-terminal could affect whether or
;      not the rule becomes nullable.
; - nullable = This is the final product.  It starts off assuming that every
;      non-terminal is not nullable and this assumption is gradually refined.
;      By the end of the first part, the non-terminals which are trivially
;      nullable have been marked as such.
; - nulled-non-terms = This is a list of which non-terminals have been marked
;      as nullable.
;
; The second part of algorithm loops over 'nulled-non-terms.  For each rule in
; 'rule-sets corresponding to the non-terminal, it reduces the rule's value in
; 'rule-count by 1.  When this value becomes 0 (meaning that every non-terminal
; on the rule's right side is now known to be nullable), the non-terminal on
; the rule's left-side is set to nullable.  It is then added to
; 'nulled-non-terms so that the loop considers the effects its new status might
; have on other rules.
(define (compute-nullable lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-lhs (lalr-constructor:rule-lhs lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (rule-count (make-vector (vector-length rule-lhs) 0))
	 (rule-sets (make-vector num-nonterminals '()))
	 (nullable (make-vector num-nonterminals #f)))
    ; Part 1
    (let rule-loop ((rule-num 1) (nulled-non-terms '()))
      (if (< rule-num (vector-length rule-lhs))
	  (let ((first-item-num (vector-ref rule-rhs rule-num)))
	    ; Check too see if the rule is trivially nullable
	    (if (< (vector-ref rule-items first-item-num) 0)
		(let ((symbol (vector-ref rule-lhs rule-num)))
		  (if (not (vector-ref nullable symbol))
		      (begin
			(vector-set! nullable symbol #t)
			(rule-loop (+ rule-num 1)
				   (cons symbol nulled-non-terms)))))
		; Check to see if the rule is trivially not-nullable
		(let term-loop ((item-num first-item-num))
		  (let ((symbol (vector-ref rule-items item-num)))
		    (cond ((>= symbol num-nonterminals)
			   (rule-loop (+ rule-num 1) nulled-non-terms))
			  ((> symbol 0)
			   (term-loop (+ item-num 1)))
			  ; otherwise, construct the nullable graph
			  (else
			   (let nonterm-loop ((item-num first-item-num))
			     (let ((symbol (vector-ref rule-items item-num)))
			       (if (> symbol 0)
				   (begin
				     (vector-set! rule-count rule-num
						  (+ 1 (vector-ref rule-count
								   rule-num)))
				     (vector-set! rule-sets symbol
						  (cons rule-num (vector-ref
								  rule-sets
								  symbol)))
				     (nonterm-loop (+ item-num 1)))
				   (rule-loop (+ rule-num 1)
					      nulled-non-terms))))))))))
	  ; Part 2
	  (let non-term-loop ((nulled-non-terms nulled-non-terms))
	    (if (pair? nulled-non-terms)
		(let rule-loop ((rule-list (vector-ref rule-sets
						       (car nulled-non-terms)))
				(nulled-non-terms (cdr nulled-non-terms)))
		  (if (pair? rule-list)
		      (let* ((rule-num (car rule-list))
			     (count (- (vector-ref rule-count rule-num) 1)))
			(vector-set! rule-count rule-num count)
			(if (= count 0)
			    (let ((symbol (vector-ref rule-lhs rule-num)))
			      (if (vector-ref nullable symbol)
				  (rule-loop (cdr rule-list) nulled-non-terms)
				  (begin
				    (vector-set! nullable symbol #t)
				    (rule-loop (cdr rule-list)
					       (cons symbol
						     nulled-non-terms)))))
			    (rule-loop (cdr rule-list) nulled-non-terms)))
		      (non-term-loop nulled-non-terms)))
		(set-lalr-constructor:nullable lalr-record nullable)))))))

;;-
; This function commputes the 'first-derives field.  This is a vector where each
; element corresponds to a non-terminal symbol.  The values are sets that are
; the union of the 'derive value for every element in the non-terminal's 'first
; set (see 'compute-firsts).
(define (compute-first-derives lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (derives (lalr-constructor:derives lalr-record))
	 (firsts (compute-firsts lalr-record))
	 (first-derives (make-vector num-nonterminals)))
    (let non-term-loop ((i 0))
      (if (< i num-nonterminals)
	  (let first-loop ((first-set (vector-ref firsts i)) (fderive '()))
	    (if (pair? first-set)
		(first-loop (cdr first-set)
			    (set-union fderive
				       (vector-ref derives (car first-set))))
		(begin
		  (vector-set! first-derives i fderive)
		  (non-term-loop (+ i 1)))))))
    (set-lalr-constructor:first-derives lalr-record first-derives)))

; This function computest the value of 'firsts.  The comments inside the
; function give the best description of what this value is.
(define (compute-firsts lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-lhs (lalr-constructor:rule-lhs lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (firsts (make-vector num-nonterminals '())))

    ; For each rule, if the right-hand-side begins with a non-terminal, then
    ; that non-terminal is in the first set for the non-terminal on the
    ; left-hand-side.
    (let loop ((i 1))
      (if (< i (vector-length rule-lhs))
	  (let ((symbol (vector-ref rule-items (vector-ref rule-rhs i))))
	    (if (< -1 symbol num-nonterminals)
		(let ((lhs (vector-ref rule-lhs i)))
		  (vector-set! firsts lhs
			       (set-insert symbol (vector-ref firsts lhs)))))
	    (loop (+ i 1)))))

    ; If non-terminal A is in the first set for non-terminal B, then the first set
    ; for non-terminal B includes the first set for non-terminal A.
    ; (transitive closure)
    (let change-loop ((continue #t))
      (if continue
	  (let non-term-loop ((i 0) (continue #f))
	    (if (>= i num-nonterminals)
		(change-loop continue)
		(let ((old-first (vector-ref firsts i)))
		  (let union-loop ((to-do old-first) (new-first old-first))
		    (if (pair? to-do)
			(union-loop (cdr to-do)
				    (set-union new-first
					       (vector-ref firsts (car to-do))))
			(if (equal? old-first new-first)
			    (non-term-loop (+ i 1) continue)
			    (begin
			      (vector-set! firsts i new-first)
			      (non-term-loop (+ i 1) #t))))))))))

    ; The first set for a non-terminal includes itself.  (reflexive closure)
    (let loop ((i 0))
      (if (< i num-nonterminals)
	  (begin
	    (vector-set! firsts i (set-insert i (vector-ref firsts i)))
	    (loop (+ i 1)))))

    ; Return the result
    firsts))

;;-
(define (compute-LR0-states lalr-record)
  (let* ((num-symbols (vector-length (lalr-constructor:symbols lalr-record)))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (state-queue (make-vector (vector-length rule-items)))
	 (item-map ((make-table-maker equal? (lambda (lst) (fold + 0 lst))))))
    (vector-set! state-queue 0 (make-state 0 #f '(0)))
    (table-set! item-map '(0) (vector-ref state-queue 0))

    (let state-loop ((cur-state 0) (last-state 1))
      (if (< cur-state last-state)
	  (let* ((state (vector-ref state-queue cur-state))
		 (items (compute-closure state lalr-record))
		 (next-states (compute-goto items num-symbols rule-items)))
	    (save-reductions state items rule-items)
	    (let next-loop ((symbol (- num-symbols 1))
			    (last-state last-state)
			    (shifts '()))
	      (if (>= symbol 0) ;<-------- Can we go the other direction --- ???
		  (let ((next-items (vector-ref next-states symbol)))
		    (cond ((null? next-items)
			   (next-loop (- symbol 1) last-state shifts))
			  ((table-ref item-map next-items) =>
			   (lambda (state)
			     (next-loop (- symbol 1) last-state
					(cons state shifts))))
			  (else
			   (let ((new-state (make-state last-state symbol
							next-items)))
			     (vector-set! state-queue last-state new-state)
			     (table-set! item-map next-items new-state)
			     (next-loop (- symbol 1) (+ last-state 1)
					(cons new-state shifts))))))
		  (begin
		    (set-state:shifts state (reverse shifts))
		    (state-loop (+ cur-state 1) last-state)))))
	  (set-lalr-constructor:states lalr-record
				       (vector-copy state-queue last-state))))))

; This function computes the "closure" operation on a set of items.
(define (compute-closure state lalr-record)
  (let ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	(rule-rhs (lalr-constructor:rule-rhs lalr-record))
	(rule-items (lalr-constructor:rule-items lalr-record))
	(first-derives (lalr-constructor:first-derives lalr-record))
	(item-set (state:items state)))
    (let item-loop ((items item-set) (closure-set item-set))
      (if (pair? items)
	  (let ((symbol (vector-ref rule-items (car items))))
	    (if (< -1 symbol num-nonterminals)
		(let first-loop ((first (vector-ref first-derives symbol))
				 (closure-set closure-set))
		  (if (pair? first)
		      (first-loop (cdr first)
				  (set-insert (vector-ref rule-rhs (car first))
					      closure-set))
		      (item-loop (cdr items) closure-set)))
		(item-loop (cdr items) closure-set)))
	  closure-set))))

; This is the "Goto" operation when constructing an LR(0) parser
(define (compute-goto items num-symbols rule-items)
  (let ((next (make-vector num-symbols '())))
    (let loop ((items items))
      (if (pair? items)
	  (let* ((item (car items))
		 (symbol (vector-ref rule-items item)))
	    (if (>= symbol 0)
		(vector-set! next symbol
			     (cons (+ item 1) (vector-ref next symbol))))
	    (loop (cdr items)))))
    (let loop ((i 0))
      (if (< i num-symbols)
	  (begin
	    (vector-set! next i (reverse (vector-ref next i)))
	    (loop (+ i 1)))))
    next))

; This function computes the 'reductions field of 'state
(define (save-reductions state items rule-items)
  (set-state:reductions state
			(let loop ((items items))
			  (if (null? items)
			      '()
			      (let ((item (vector-ref rule-items (car items))))
				(if (< item 0)
				    (cons (- item) (loop (cdr items)))
				    (loop (cdr items))))))))

;;-
; This function computes the 'consistent and 'reduction-map fields.
(define  (compute-consistent-and-reduction-map lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (num-states (vector-length states))
	 (consistent (make-vector num-states #f))
	 (reduction-map (make-vector (+ num-states 1))))
    (let loop ((count 0) (i 0))
      (if (< i num-states)
	  (let* ((state (vector-ref states i))
		 (shifts (state:shifts state))
		 (reductions (state:reductions state))
		 (num-reductions (length reductions)))
	    (vector-set! reduction-map i count)
	    (if (and (> num-reductions 0)
		     (or (> num-reductions 1)
			 (and (not (null? shifts))
			      (not (< (state:access-symbol (last shifts))
				      num-nonterminals)))))
		(loop (+ count num-reductions) (+ i 1))
		(begin
		  (vector-set! consistent i #t)
		  (loop count (+ i 1)))))
	  (vector-set! reduction-map num-states count)))
    (set-lalr-constructor:consistent lalr-record consistent)
    (set-lalr-constructor:reduction-map lalr-record reduction-map)))

;;-
; This function computes the 'reduction-rule-num field.
(define (compute-reduction-rule-num lalr-record)
  (let* ((states (lalr-constructor:states lalr-record))
	 (num-states (vector-length states))
	 (consistent (lalr-constructor:consistent lalr-record))
	 (reduction-map (lalr-constructor:reduction-map lalr-record))
	 (reduction-rule-num (make-vector
			      (vector-ref reduction-map
					  (- (vector-length reduction-map)
					     1)))))
    (let state-loop ((state-num 0) (rule-count 0))
      (if (< state-num num-states)
	  (if (vector-ref consistent state-num)
	      (state-loop (+ state-num 1) rule-count)
	      (let rule-loop ((reductions (state:reductions
					   (vector-ref states state-num)))
			      (rule-count rule-count))
		(if (null? reductions)
		    (state-loop (+ state-num 1) rule-count)
		    (begin
		      (vector-set! reduction-rule-num rule-count
				   (car reductions))
		      (rule-loop (cdr reductions) (+ rule-count 1))))))))
    (set-lalr-constructor:reduction-rule-num lalr-record reduction-rule-num)))

;;-
; This function computes the 'goto-map, 'from-state, and 'to-state fields
(define (compute-goto-map lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (num-states (vector-length states))
	 (goto-map (make-vector (+ num-nonterminals 1) 0)))

    (let state-loop ((state-num 0) (num-gotos 0))
      (if (< state-num num-states)
	  (let shift-loop ((shift-states (state:shifts (vector-ref states
								   state-num)))
			   (num-gotos num-gotos))
	    (if (null? shift-states)
		(state-loop (+ state-num 1) num-gotos)
		(let ((symbol (state:access-symbol (car shift-states))))
		  (if (< symbol num-nonterminals)
		      (begin
			(vector-set! goto-map symbol
				     (+ 1 (vector-ref goto-map symbol)))
			(shift-loop (cdr shift-states) (+ num-gotos 1)))
		      (shift-loop (cdr shift-states) num-gotos)))))))

    (let sum-loop ((i 0) (sum 0))
      (if (< i num-nonterminals)
	  (let ((x (vector-ref goto-map i)))
	    (vector-set! goto-map i sum)
	    (sum-loop (+ i 1) (+ sum x)))
	  (vector-set! goto-map num-nonterminals sum)))

    (let* ((temp-map (vector-copy goto-map))
	   (from-state (make-vector (vector-ref temp-map num-nonterminals)))
	   (to-state (make-vector (vector-length from-state))))
      (let state-loop ((state-num 0))
	(if (< state-num num-states)
	    (let shift-loop ((shift-states (state:shifts
					    (vector-ref states state-num))))
	      (if (null? shift-states)
		  (state-loop (+ state-num 1))
		  (let* ((state (car shift-states))
			 (to-state-num (state:number state))
			 (symbol (state:access-symbol state)))
		    (if (< symbol num-nonterminals)
			(let ((i (vector-ref temp-map symbol)))
			  (vector-set! temp-map symbol (+ i 1))
			  (vector-set! from-state i state-num)
			  (vector-set! to-state i to-state-num)))
		    (shift-loop (cdr shift-states)))))))
      (set-lalr-constructor:goto-map lalr-record goto-map)
      (set-lalr-constructor:from-state lalr-record from-state)
      (set-lalr-constructor:to-state lalr-record to-state))))

; This is a helper function used in the computation of 'reads and 'includes.
; When given a state and a non-terminal symbol, it returns the number of that
; particular "non-terminal transition."
(define (map-goto state-num symbol lalr-record)
  (let ((goto-map (lalr-constructor:goto-map lalr-record))
	(from-state (lalr-constructor:from-state lalr-record)))
    (let loop ((low (vector-ref goto-map symbol))
	       (high (- (vector-ref goto-map (+ symbol 1)) 1)))
      (if (> low high)
	  (error "Internal Error: map-goto can't map.")
	  (let* ((middle (quotient (+ low high) 2))
		 (middle-state-num (vector-ref from-state middle)))
	    (cond ((= middle-state-num state-num)
		   middle)
		  ((< middle-state-num state-num)
		   (loop (+ middle 1) high))
		  (else
		   (loop low (- middle 1)))))))))

; This function computes the 'reads field and the DR value.  DR is stored in the
; 'follow field where it will serve as the initial value when 'compute-follow is
; called.
(define (compute-DR-and-reads lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (nullable (lalr-constructor:nullable lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (to-state (lalr-constructor:to-state lalr-record))
	 (num-gotos (vector-length to-state))
	 (DR (make-vector num-gotos 0))
	 (reads (make-vector num-gotos)))
    (let goto-loop ((goto-num 0))
      (if (< goto-num num-gotos)
	  (let ((state-num (vector-ref to-state goto-num)))
	    (let shift-loop ((shift-states (state:shifts
					    (vector-ref states state-num)))
			     (gotos '()))
	      (if (pair? shift-states)
		  (let ((symbol (state:access-symbol (car shift-states))))
		    (if (< symbol num-nonterminals)
			(if (vector-ref nullable symbol)
			    (shift-loop (cdr shift-states)
					(cons (map-goto state-num symbol)
					      gotos))
			    (shift-loop (cdr shift-states) gotos))
			(begin
			  (vector-set! DR goto-num
				       (set-bit (vector-ref DR goto-num)
						(- symbol num-nonterminals)))
			  (shift-loop (cdr shift-states) gotos))))
		  (vector-set! reads goto-num (reverse gotos)))) ;< --- Is this necessary --- ???
	    (goto-loop (+ goto-num 1)))))
    (set-lalr-constructor:follow lalr-record DR)
    (set-lalr-constructor:reads lalr-record reads)))

;;-
(define (compute-includes-and-lookback lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (derives (lalr-constructor:derives lalr-record))
	 (nullable (lalr-constructor:nullable lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (consistent (lalr-constructor:consistent lalr-record))
	 (to-state (lalr-constructor:to-state lalr-record))
	 (from-state (lalr-constructor:from-state lalr-record))
	 (num-gotos (vector-length to-state))
	 (reduction-map (lalr-constructor:reduction-map lalr-record))
	 (reduction-rule-num (lalr-constructor:reduction-rule-num lalr-record))
	 (includes-tp (make-vector num-gotos))
	 (lookback (make-vector (vector-length reduction-rule-num) '())))
    (let goto-loop ((goto-num 0))
      (if (< goto-num num-gotos)
	  (let ((state-num1 (vector-ref from-state goto-num))
		(symbol1 (state:access-symbol (vector-ref states (vector-ref to-state goto-num)))))
	    (let rule-loop ((rules (vector-ref derives symbol1))
			    (edges '()))
	      (if (pair? rules)
		  (let forward-loop ((item-num (vector-ref rule-rhs (car rules)))
				     (state (vector-ref states state-num1))
				     (state-nums (list state-num1)))
		    (let ((symbol (vector-ref rule-items item-num)))
		      (if (> symbol 0) ;<--------------------------- the start-symbol can be 0 --- ???
			  (let state-loop ((shifts (state:shifts state)))
			    (cond ((null? shifts)
				   (error "Internal Error: Could not find shift symbol.")) ;<------------- ???
				  ((= (state:access-symbol (car shifts)) symbol)
				   (forward-loop (+ item-num 1) (car shifts) (cons (state:number (car shifts)) state-nums)))
				  (else
				   (state-loop (cdr shifts)))))
			  (begin
			    (if (not (vector-ref consistent (state:number state)))
				(let ((max (vector-ref reduction-map (+ (state:number state) 1))))
				  (let reduction-loop ((red-num (vector-ref reduction-map (state:number state))))
				    (cond ((>= red-num max)
					   (error "Internal Error: Could not find reduction number."))
					  ((= (vector-ref reduction-rule-num red-num) (car rules))
					   (vector-set! lookback red-num (cons goto-num (vector-ref lookback red-num))))
					  (else
					   (reduction-loop (+ red-num 1)))))))
			    (let reverse-loop ((done #f)
					       (state-nums (cdr state-nums))
					       (item-num (- item-num 1))
					       (edges edges))
			      (if done
				  (rule-loop (cdr rules) edges)
				  (let ((symbol (vector-ref rule-items item-num)))
				    (if (< -1 symbol num-nonterminals)
					(reverse-loop (not (vector-ref nullable symbol))
						      (cdr state-nums)
						      (- item-num 1)
						      (cons (map-goto (car state-nums) symbol lalr-record) edges))
					(reverse-loop #t state-nums item-num edges)))))))))
		  (vector-set! includes-tp goto-num edges)))
	    (goto-loop (+ goto-num 1)))))
    (set-lalr-constructor:lookback lalr-record lookback)
    (set-lalr-constructor:includes lalr-record (transpose includes-tp))))

;
(define (transpose includes-tp)
  (let* ((num-gotos (vector-length includes-tp))
	 (includes (make-vector num-gotos '())))
    (let outer-loop ((outer-goto-num 0))
      (if (< outer-goto-num num-gotos)
	  (let inner-loop ((inner-gotos (vector-ref includes-tp outer-goto-num)))
	    (if (null? inner-gotos)
		(outer-loop (+ outer-goto-num 1))
		(let ((inner-goto-num (car inner-gotos)))
		  (vector-set! includes inner-goto-num (cons outer-goto-num (vector-ref includes inner-goto-num)))
		  (inner-loop (cdr inner-gotos)))))))
    (let loop ((i 0)) ;< -------------------------- Is this necessary --- ???
      (if (< i num-gotos)
	  (begin
	    (vector-set! includes i (reverse (vector-ref includes i)))
	    (loop (+ i 1)))))
    includes))

;;-
(define (compute-follow lalr-record)
  (let ((reads (lalr-constructor:reads lalr-record))
	(includes (lalr-constructor:includes lalr-record))
	(follow (lalr-constructor:follow lalr-record)))
    (digraph (digraph follow reads) includes)))

; Note: What do we do about cycles?
(define (digraph F R)
  (let* ((size (vector-length F))
	 (N (make-vector size 0))
	 (S (make-vector size 0)))
    (let loop ((i 0))
      (if (< i size)
	  (begin
	    (if (and (= 0 (vector-ref N i))
		     (pair? (vector-ref R i)))
		(traverse i N R S 1 F))
	    (loop (+ i 1)))))
    F))

;
(define (traverse x N R S d F)
  (let ((infinity (+ (vector-length S) 1)))
    (vector-set! S d x)
    (vector-set! N x d)
    (let loop ((R-list (vector-ref R x)))
      (if (pair? R-list)
	  (let ((y (car R-list)))
	    (if (= 0 (vector-ref N y))
		(traverse y N R S (+ d 1) F))
	    (if (> (vector-ref N x) (vector-ref N y))
		(vector-set! N x (vector-ref N y)))
	    (vector-set! F x (bit-union (vector-ref F x) (vector-ref F y)))
	    (loop (cdr R-list)))))
    (if (= (vector-ref N x) d)
	(let loop ((top d))
	  (let ((t (vector-ref S top)))
	    (vector-set! N t infinity)
	    (if (not (= x t))
		(begin
		  (vector-set! F x (bit-union (vector-ref F x)
					      (vector-ref F t)))
		  (loop (- top 1)))))))))

;;-
(define (compute-LA lalr-record)
  (let* ((reduction-map (lalr-constructor:reduction-map lalr-record))
	 (lookback (lalr-constructor:lookback lalr-record))
	 (num-reductions (vector-length lookback))
	 (follow (lalr-constructor:follow lalr-record))
	 (LA (make-vector num-reductions 0)))
    (let reduction-loop ((red-num 0))
      (if (< red-num num-reductions)
	  (let lookback-loop ((lookbacks (vector-ref lookback red-num)))
	    (if (pair? lookbacks)
		(let ((cur-LA (vector-ref LA red-num))
		      (lookback-follow (vector-ref follow (car lookbacks))))
		  (vector-set! LA red-num (bit-union cur-LA lookback-follow))
		  (lookback-loop (cdr lookbacks)))
		(reduction-loop (+ red-num 1))))))
    (set-lalr-constructor:LA lalr-record LA)))
