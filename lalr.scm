;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            Input Type Definition                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The LALR(1) Table Constructor takes one of these records as input
(define-record cfg

  ; This is a list.  The elements of the list are either Scheme symbols or
  ; 'cfg-precedence records.  These represent the terminal symbols in the
  ; grammar. Precedence records appearing earlier have higher precedence than
  ; those appearing later.  Scheme symbols are terminal symbols with no defined
  ; precedence.
  terminals

  ; This is a Scheme symbol which will be returned by the lexer when the end of
  ; the input has been reached.  It must not appear in either 'terminals or
  ; 'rules.
  eoi

  ; This is the error symbol.  This symbol is shifted whenever the Push-Down
  ; Automaton encounters an error.  It may be used in 'rules but should not
  ; appear in 'terminals and should never be returned by the lexer.  If the
  ; error symbols is not used, the value may be #f.
  error

  ; This is the non-terminal that is the start symbol
  start

  ; This is a list of rule records
  rules)

; This is a record used to specify terminal symbols with defined precedence.
(define-record cfg-precedence

  ; One of (left right non).  This is the associativity of 'terminals.
  associativity

  ; The list of terminal symbols in this precedence class.
  terminals)

; This record represents a rule in the grammar
(define-record cfg-rule

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            Output Type Definition                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The LALR(1) table constructor returns this as its output
(define-record LR-program

  ; This is a list of Scheme symbols and gives the terminal symbols which are
  ; valid for the lexer to return.
  terminals

  ; This is the symbol that the lexer should return when it reaches the end of
  ; the input.
  eoi

  ; This is the symbol that is shifted when errors are encountered.  If defined,
  ; in will appear in 'rules and 'states but should never be returned by the
  ; lexer.  If not defined, this entry will be #f.
  error

  ; This is a vector of 'lalr-rule records.  Reduction actions in 'states will
  ; refer to elements of this vector by their index.
  rules

  ; This is a vector of states.  The start state is state 0.  The values are
  ; association lists.  The elements of the association lists look like:
  ; (sym action arg)
  ; - sym is a Scheme symbol that is either a terminal or non-terminal symbol
  ; - action is one of 'shift, 'goto (only if 'sym is a non-terminal), 'reduce,
  ;   or 'accept.
  ; - arg is a number.  For 'shift and 'goto, it is a state number.  For
  ;   'reduce, it is an index into 'rules.  It is not present for 'accept.
  states)

; The LR-program:rules field will contain a vector of these records.
(define-record LR-rule

  ; This is a Scheme symbol that gives the non-terminal to shift upon a
  ; reduction by this rule
  left-side

  ; This is a vector containing the terminal and non-terminal symbols on the
  ; right side of this rule.  This is primarily used for its length with is the
  ; number of elements to pop when a reduction by this rule occurs.
  right-side

  ; This is the action for this rule.  It is unmodified from what was passed
  ; the LALR(1) Table constructor.
  action)

; The LR-program:states field will contain a vector of these records.
(define-record LR-state

  ; This is an association list that contains the shift and reduce actions for
  ; this particular states.  The items of the list take the form:
  ;   (terminal action number)
  ; terminal is the terminal symbol of lookahead on which the action should
  ; take place.  action is either 'shift, 'reduce, or 'accept.  number is either
  ; a state number, a rule number, or not present, depending on action.
  ; Additionally, terminal may be #f to denote a "default" action.  This action,
  ; which should always be 'reduce if it is defined, will take place if no other
  ; terminal matches the lookahead symbol.
  (shift-reduce-table #f)

  ; This is an association list which contains the gotos for this state.  The
  ; items of this list take the form of:
  ;   (non-terminal state-number)
  ; non-terminal is the non-terminal symbol on which this goto takes place.
  ; state-number is the state-number to shift into.
  (goto-table #f)

  ; This list encodes the items which make up this state.  This list is used for
  ; debugging the parser.  The items are in the format of
  ;   (rule-number dot-index)
  ; rule-number is an index into 'rules.  dot-number is the index into
  ; LR-rule:right-side before which the "dot" occurs.  Note that rule-number may
  ; appear multiple times and dot-index will not be a valid 'right-side index
  ; if the dot appears at the end of the rule.
  (items #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Internal Types                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ; This will be either a Scheme symbol or #f, depending on whether or not an
  ; error symbol was defined for the grammar.  If one was defined, it will be
  ; the last element of 'symbols.
  (error-symbol #f)

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
  ; rule-items = #( A B -1 a b c -2 -3 ( B ) -4 #f )
  ;
  ; Note that symbols were used for clarity.  In reality, the symbols would be
  ; replaced by their corresponding numbers.  Also note that the first element
  ; of 'rule-lhs and 'rule-rhs is not used because 0 is not negative and that
  ; the last element of 'rule-items is always #f.
  ;
  ; This representation is used because it makes constructing the LR(0) parser
  ; fast and easy.  Items are simply indexes into 'rule-items and states are
  ; simply sets of such indexes.  "Goto" is performed by adding 1 to each item
  ; in a state.  Reductions are possible when a negative number is reached and
  ; the rule to reduce by is simply the absolute value of that number.
  (rule-lhs #f)
  (rule-rhs #f)
  (rule-items #f)

  ; This vector maps terminal symbols to their corresponding precedence number.
  ; Higher numbers have higher precedence.  Terminal symbols with no defined
  ; precedence are assigned the number 0.
  (precedence-map #f)

  ; This vector maps precedence numbers to the corresponding associativity for
  ; that precedence level (one of 'left, 'right, or 'non).  Precedence level 0
  ; is mapped to 'right to correspond to the default of choosing a shift over
  ; a reduce.
  (associativity-map #f)

  ; These are both vectors where each element corresponds to the same numbered
  ; element in rule-lhs & rule-rhs.  'rule-precedence gives the precedence level
  ; for the rule and 'rule-action is the semantic action for the rule.
  (rule-precedence #f)
  (rule-actions #f)

  ; This is a vector of length 'num-nonterminals.  Its values are lists of
  ; rule numbers (indexes in 'rule-rhs & 'rule-lhs).  The vector gives, for each
  ; non-terminal symbol, the list of rules of which it is on the left-hand-side.
  (derives #f)

  ; This is a vector of length 'num-nonterminals.  Its values are lists of rule
  ; numbers.  Its purpose is to make 'compute-closure fast.
  ;
  ; The values are the result of unioning the 'derives sets for every
  ; non-terminal in the given non-terminal's First set.  The code comments for
  ; 'compute-firsts give the best description of what the First set is.
  (first-derives #f)

  ; This is a vector of length 'num-nonterminals.  Its values are either #t or
  ; #f.  It specifies which non-terminal symbols can derive the empty string and
  ; which cannot.
  (nullable #f)

  ; This is a vector containing 'state records.  It represents the states in the
  ; LR(0) parser.  The starting state is in index 0.  See the description of the
  ; 'state record for more information.
  (states #f)

  ; This is the index into 'states where the accept state occurs.  The accept
  ; will occur on the EOI symbol in this state.
  (accept-state #f)

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
  ; 'lookback, and 'LA.  If a state is inconsistent, then this vector maps a
  ; a state to the index into those vectors where the data for that state
  ; starts.
  (reduction-map #f)

  ; This vector is the same size as the last element in 'reduction-map (which
  ; could be 0).  It contains, in order of state number, the rule numbers of
  ; reductions that take place in inconsistent states.
  (reduction-rule-num #f)

  ; This vector is the same size as 'reduction-rule-num.  It maps a reduction to
  ; the list of non-terminal transitions that "this" reduction can immediately
  ; cause.  See the LALR analysis description in the comments for
  ; 'construct-lalr-parser.
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

  ; Both these vectors are the same size as 'from-state & 'to-state and both map
  ; a non-terminal transition to a list of non-terminal transitions.  These
  ; vectors define the relationships of the same name that are described in the
  ; in the LALR analysis overview.  See the comments for 'construct-lalr-parser.
  (reads #f)
  (includes #f)

  ; This vector is the same size as 'from-state & 'to-state and maps
  ; non-terminal transitions to the list of terminal symbols that can be shifted
  ; after it.  See the comments for 'construct-lalr-parser.
  (follow #f)

  ; This is the action table that goes into the LR-automaton.  It is generated
  ; seperately because conflict resolution has to be dealt with when
  ; constructing it.
  (action-table #f))

; This record defines an individual state in the LR(0) state machine.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 Utility Code                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------------------------------ Sets -------------------------------------
; Sets are represented as sorted lists

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

;---------------------------------- Bit-Sets -----------------------------------
; Bit-Sets are simply integers.

; Computes the union of two bit-sets
(define bit-union bitwise-ior)

; Sets the 'i-th bit of 'bitset to 1.
(define (set-bit bitset i)
  (bit-union bitset (expt 2 i)))

;---------------------------------- Vectors ------------------------------------
(define (vector-copy vector . size)
  (let* ((size (if (null? size) (vector-length vector) (car size)))
	 (result (make-vector size)))
    (let loop ((i 0))
      (if (< i size)
	  (begin
	    (vector-set! result i (vector-ref vector i))
	    (loop (+ i 1)))
	  result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                              The Main Algorithm                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is the top-level function for the PDA generation algorithm.  It takes
; either one or two arguments.  The first argument is a 'cfg record that defines
; the Context-Free Grammar to convert.  The second, optional argument is a place
; to write the state table.  This may take many forms.  If it is a string, it
; will be interpreted as a filename and the state-table will be written to that
; file.  If it is a port, the state-table will be written to that port.  If it
; is '#t, the state-table will be written to the value of (current-output-port).
; If it is '#f or not given, the state table will not be written.
;
; It returns an 'LR-program record that represents the LALR(1) parser.
;
;
; --- WHERE TO FIND MORE INFORMATION
;
; The comments in this file assume that you are familiar with general LR
; parsing.  If not, a good introduction can be found in the "Dragon Book" here:
;
;     <citation for Dragon Book>
;
; This function generations a LALR(1) state table.  It uses an efficient
; algorithm that is described fully in:
;
;     "Efficient Computation of LALR(1) Look-Ahead Sets", F. DeRemer and
;     T. Pennello, TOPLAS, vol. 4, no. 4, october 1982.
;
;
; --- A SHORT DESCRIPTION OF THE LALR ALGORITHM
;
; Note: In the follow description, 'u' denotes "union" and 'U', when applied to
; a set of sets, denotes the result of unioning all internal sets.  'p' and 'q'
; are states in the LR(0) state machine.  'A', 'B', 'C', and 'D' are
; non-terminal symbols.  'w' represents a possibly empty string of terminal and
; non-terminal symbols.  A->w denotes a reduction.
;
; The final result of the LALR analysis is LA.  This is a list of terminals for
; every reduction in the LR(0) state machine.  This tells the table constructor
; which look-ahead tokens should have the corresponding "reduce" action.  LA is
; represented internally as a bitset with one bit for every terminal symbol.
;
; LA(q,A->w) = U( Follow(p,A) | (q,A->w) lookback (p,A) )
;
; lookback is mapping from reductions in the LR(0) state machine to a list of
; non-terminal transitions ("gotos") that could happen immediately after each
; reduction.
;
; Follow is a list of terminal symbols for every non-terminal transition in the
; LR(0) state machine.  It gives the list of terminal symbols that can appear
; on the input string immediately after each nonterminal transition in a valid
; grammar.
;
; Thus, the problem of computing LA is reduced to the problem of computing
; follow sets for the "gotos" in the grammar.
;
; Follow(p,A) = Read(p,A) u U{ Follow(p',B) | (p,A) includes (p',B) }
; Read(p,A)   = DR(p,A)   u U{ Read(r,C)    | (p,A) reads    (r,C)  }
;
; Note that these definitions are recursive and very similar.
;
; includes is a mapping from non-terminal transitions to a list of non-terminal
; transitions.  This relation takes care of the fact that if there is a rule
; A -> B C D, and 'D' is nullable, then the follow sets of both 'C' and 'D'
; "include" whatever is in the follow set of 'A'.
;
; reads is another mapping from non-terminal transitions to a list of
; non-terminal transitions.  This relation takes care of the fact that if
; "A B C D" appears on the right side of a rule, and 'B' and 'C' are nullable,
; then the follow set for 'B' includes the follow set of 'C' and the follow set
; of 'A' includes the follow set of 'B'.
;
; DR ("Direct Read") is a list of terminal symbols for every non-terminal
; trasition.  It tells which terminal symbols can be shifted immediately after
; a non-terminal transition.  This is just the list of which terminal symbols
; have a "shift" action defined in the "to state" of the transition.
;
; Now we have a way to compute Follow sets (and thus LA sets) from simple
; properties of the grammar and the LR(0) state machine.  We just need a way to
; deal with the recursive definition of Read and Follow.  The paper defines a
; fuction called "digraph" which takes care of this.  It finds
; Strongly Connected Components (a maximal set of verticies where there is a
; path from any vertex to any other vertex in the set) in the graph definef by
; the includes and reads relations and treats them as single nodes.  After that,
; the graph becomes a tree and information simply propogates up the tree.
;
; QUALIFICATION
; The paper computes LA for every reduction in the grammar.  This algorithm only
; computes LA for "inconsistent" states.  Inconsistent states are states in
; which there is more than one reduction or a single reduction and no
; non-terminal transitions.  For the other reductions, the reduction happens by
; "default" (that is, for any terminal symbol that does not have any other
; action defined).  This does not change the grammar any.  Any errors in the
; input string will be caught in the next state when the PDA finds that no shift
; is defined for the terminal symbol.
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

    (compute-action-table lalr-record)
    (construct-LR-program lalr-record)))

;-------------------------------------------------------------------------------
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
	 (terminals (extract-terminals (cfg:terminals grammar)
				       (cfg:error grammar)))
	 (num-terminals (+ 1 (length terminals)))
	 (nonterminals (extract-nonterminals (cfg:rules grammar)))
	 (num-nonterminals (+ 1 (length nonterminals)))
	 (eoi (cfg:eoi grammar)))

    (receive
     (symbols reverse-map)
     (create-symbol-map nonterminals num-nonterminals terminals num-terminals)

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
	    (aug-grammar (cons (make-cfg-rule new-start (list start eoi) #f '$1)
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

       ; Convert the precedence information to hard numbers
       (receive
	(precedence-map associativity-map)
	(create-precedence-map (cfg:terminals grammar) num-terminals
			       num-nonterminals reverse-map)

	(set-lalr-constructor:precedence-map lalr-record precedence-map)
	(set-lalr-constructor:associativity-map lalr-record associativity-map)
	(set-lalr-constructor:rule-precedence lalr-record
	 (extract-rule-prec aug-grammar num-nonterminals reverse-map
			    precedence-map)))

       ; Store the rule actions for later use
       (set-lalr-constructor:rule-actions lalr-record
	(list->vector (cons '() (map cfg-rule:action aug-grammar)))))

     ; Set the last few fields and then return the record
     (set-lalr-constructor:num-nonterminals lalr-record num-nonterminals)
     (set-lalr-constructor:symbols lalr-record symbols)
     (set-lalr-constructor:error-symbol lalr-record (cfg:error grammar)))

    lalr-record))

; This is a helper function used by 'convert-grammar.  It takes the list of
; terminals and precedence records in the input and returns a simple list of
; terminals.  Along the way, it checks for invalid terminals symbols.  Duplicate
; terminal declarations will be caught by 'create-symbol-map.
(define (extract-terminals grammar-terminals error-symbol)
  (if (not (list? grammar-terminals))
      (error "cfg:terminals is not a list"))
  (if (and error-symbol (not (symbol? error-symbol)))
      (error "cfg:error is an invalid symbol"))
  (fold-right
   (lambda (element term-list)
     (cond ((and (eq? element error-symbol) error-symbol)
	    (error "cfg:error cannot be the same as a terminal symbol"))
	   ((symbol? element)
	    (cons element term-list))
	   ((cfg-precedence? element)
 	    (if (list? (cfg-precedence:terminals element))
 		(fold-right
		 (lambda (element term-list)
		   (cond ((and (eq? element error-symbol) error-symbol)
			  (error
			   "cfg:error cannot be the same as a terminal symbol"))
			 ((symbol? element)
			  (cons element term-list))
			 (else
			  (error "Invalid terminal symbol:" element))))
		 term-list (cfg-precedence:terminals element))
 		(error "precedence:terminals must be a list")))
	   (else
	    (error "Invalid terminal declaration:" element))))
   (if error-symbol (list error-symbol) '()) grammar-terminals))

; This is a helper function used by 'convert-grammar.  It takes the list of
; rules and returns a list of all the non-terminal symbols that the rules
; define.  It also checks to see that the non-terminal names are valid.
; Nonterminal names that are the same as terminal names will be caught by
; 'create-symbol-map.
(define (extract-nonterminals grammar-rules)
  (if (not (list? grammar-rules))
      (error "cfg:rules is not a list"))
  (delete-duplicates
   (map
    (lambda (element)
      (if (cfg-rule? element)
	  (let ((x (cfg-rule:left-side element)))
	    (if (symbol? x)
		x
		(error "Invalid nonterminal name:" x)))
	  (error "cfg:rules must contain only rule records")))
    grammar-rules)))

; This is a helper function used by 'convert-grammar.  It creates a mapping
; between the Scheme symbols used in the grammar to the numbers used internally.
; It returns two values.  The first is a vector that is stored in
; 'lalr-constructor.  It is used to map from numbers back to the symbols.  The
; second value is a hash-table that maps the symbols into the corresponding
; numbers.
;
; This function also checks to make sure that there are no duplicate terminal
; symbols ('nonterminals is guaranteed to have no duplicates) and that there are
; no terminal symbols and nonterminal symbols with the same name.
(define (create-symbol-map nonterminals num-nonterminals terminals num-terms)
  (let ((symbol-map (make-vector (+ num-nonterminals num-terms)))
	(reverse-map (make-symbol-table)))
    (let loop ((i (+ num-nonterminals 1)) (terminals terminals))
      (if (pair? terminals)
	  (let ((terminal (car terminals)))
	    (if (table-ref reverse-map terminal)
		(error "Duplicate terminal definition:" terminal))
	    (vector-set! symbol-map i terminal)
	    (table-set! reverse-map terminal i)
	    (loop (+ i 1) (cdr terminals)))))
    (let loop ((i 1) (nonterminals nonterminals))
      (if (pair? nonterminals)
	  (let ((nonterminal (car nonterminals)))
	    (if (table-ref reverse-map nonterminal)
		(error "Non-terminal and terminal with same name:" nonterminal))
	    (vector-set! symbol-map i nonterminal)
	    (table-set! reverse-map nonterminal i)
	    (loop (+ i 1) (cdr nonterminals)))))
    (values symbol-map reverse-map)))

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
			    (+ num-items (length (cfg-rule:right-side rule)) 1))
			  0 grammar-rules))
	 (rule-items (make-vector (+ 1 num-items) #f)))

    (let rule-loop ((rules grammar-rules) (item-num 0) (rule-num 1))
      (if (pair? rules)
	  (begin
	    (vector-set! rule-lhs rule-num
			 (table-ref reverse-map
				    (cfg-rule:left-side (car rules))))
	    (vector-set! rule-rhs rule-num item-num)
	    (let item-loop ((syms (cfg-rule:right-side (car rules)))
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

; This is a helper function used by 'convert-grammar.  It take the list of
; terminals symbols and precedence records from the input and returns two
; values.  The first is a vector that maps terminal symbols to a number which
; represents the terminal's precedence.  Higher numbers represent higher
; precedence and zero is assigned to terminals without defined precedence.  The
; second value is a vector that maps precedence numbers to that precedence
; level's corresponding associativity.  Level 0 is assigned the value of 'right
; to correspond to choosing shifts over reductions.
(define (create-precedence-map terminals num-terms num-nonterminals reverse-map)
  (let* ((precedence-map (make-vector num-terms 0))
	 (rev-assoc-map (cdr
	  (fold-right
	   (lambda (element count-and-assoc)
	     (if (cfg-precedence? element)
		 (let ((associativity (cfg-precedence:associativity element)))
		   (for-each
		    (lambda (terminal)
		      (vector-set! precedence-map (- (table-ref reverse-map
								terminal)
						     num-nonterminals)
				   (car count-and-assoc)))
		    (cfg-precedence:terminals element))
		   (if (not (member associativity '(left right non)))
		       (error "Unknown associativity:" associativity))
		   (cons (+ 1 (car count-and-assoc))
			 (cons associativity (cdr count-and-assoc))))
		 count-and-assoc))
	   '(1 . (right)) terminals))))
    (values precedence-map (list->vector (reverse rev-assoc-map)))))

; This is a helper function used by 'convert-grammar.  It takes a list of rules
; and returns a vector containing their precedence values.  Along the way, it
; makes sure that any terminal symbols found are valid and coverts them to thier
; corresponding precedence values.  The returned vector has a dummy first
; element to correspond to the fact that 'rule-lhs & 'rule-rhs do not use their
; first element.
(define (extract-rule-prec rules num-nonterminals reverse-map precedence-map)
  (list->vector (cons #f
   (map
    (lambda (rule)
      (let ((prec (cfg-rule:precedence rule)))
	(cond ((not prec) #f)
	      ((table-ref reverse-map prec) =>
	       (lambda (num)
		 (if (< num num-nonterminals)
		     (error
		      "Precedence cannot be based on a nonterminal symbol:"
		      prec)
		     (vector-ref precedence-map (- num num-nonterminals)))))
	      (else
	       (error "Unknown terminal symbol:" prec)))))
    rules))))

;-------------------------------------------------------------------------------
; This function is responsible for computing the 'rule-precedence field.  The
; vector was created in 'convert-grammar where some of the values were set.
; These values correspond to rules whose precedence was manually specified and
; this function does not change them.  For the rest, the precedence is set to
; the precedence value of the last terminal-symbol in the rule or to 0 if there
; are no terminal symbols.
(define (compute-precedence lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (num-rules (vector-length rule-rhs))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (precedence-map (lalr-constructor:precedence-map lalr-record))
	 (rule-prec (lalr-constructor:rule-precedence lalr-record)))
    (let rule-loop ((rule-num 1))
      (if (< rule-num num-rules)
	  (if (vector-ref rule-prec rule-num)
	      (rule-loop (+ rule-num 1))
	      (let item-loop ((item-num (vector-ref rule-rhs rule-num))
			      (cur-prec 0))
		(let ((item (vector-ref rule-items item-num)))
		  (cond ((< item -1)
			 (vector-set! rule-prec rule-num cur-prec)
			 (rule-loop (+ rule-num 1)))
			((< item num-nonterminals)
			 (item-loop (+ item-num 1) cur-prec))
			(else
			 (item-loop (+ item-num 1)
				    (vector-ref precedence-map
						(- item
						   num-nonterminals))))))))))))

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
; This function builds an LR(0) state machine for the grammar.  It starts by
; putting the start state into the state queue and then it uses 'closure and
; 'goto until no more states are generated.  The result is store in the 'states
; field of 'lalr-record.
(define (compute-LR0-states lalr-record)
  (let* ((num-symbols (vector-length (lalr-constructor:symbols lalr-record)))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (state-queue (make-vector (vector-length rule-items))) ;<---- This needs to be variable length --- !!!
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

; This function takes a list of "kernel" items from 'state and returns the
; complete list of items that represent that state.  The new items will all have
; the dot at the front of the rule.  For example, if the parser can be in the
; state "A -> a B c . D" then it can also be in the state "D -> . f G".
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

; This function takes a list of items (which represent a state) and returns a
; vector.  The vector is indexed by terminal and non-terminal symbols and the
; values are new lists of items.  These represent (possibly new) states which
; can be reached by shifting the corresponding symbol.
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

; This fuction figures out which reductions are possible in the given state and
; then saves them in the state record.
(define (save-reductions state items rule-items)
  (set-state:reductions state
			(let loop ((items items))
			  (if (null? items)
			      '()
			      (let ((item (vector-ref rule-items (car items))))
				(if (< item 0)
				    (cons (- item) (loop (cdr items)))
				    (loop (cdr items))))))))

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
; This function computes the 'goto-map, 'from-state, and 'to-state fields.
; Basically, it enumerates all the non-terminal transitions ("gotos") in the
; LR(0) state machine.  'from-state & 'to-state are the numbers of the starting
; and destination state for each transition.  All the non-terminal transitions
; are listed together.  'goto-map maps a given non-terminal to the starting
; point in 'from-state & 'to-state for that non-terminal.
(define (compute-goto-map lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (num-states (vector-length states))
	 (goto-map (make-vector (+ num-nonterminals 1) 0)))

    ; Calculate the number of "gotos" each non-terminal is involved in
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

    ; Covert the individual sums into indexes into 'from-state and 'to-state
    (let sum-loop ((i 0) (sum 0))
      (if (< i num-nonterminals)
	  (let ((x (vector-ref goto-map i)))
	    (vector-set! goto-map i sum)
	    (sum-loop (+ i 1) (+ sum x)))
	  (vector-set! goto-map num-nonterminals sum)))

    ; Compute 'from-state and 'to-state
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

      ; Finish up
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
; This function finishes computing the value of the 'follow field.  This field
; was initialized by 'compute-DR-and-reads to the value of DR (see the comment
; to 'construct-lalr-parser).  This function uses the 'digraph function as
; defined in the DeRemer and Pennello paper to compute Read and then Follow.
(define (compute-follow lalr-record)
  (let ((reads (lalr-constructor:reads lalr-record))
	(includes (lalr-constructor:includes lalr-record))
	(follow (lalr-constructor:follow lalr-record)))
    (digraph (digraph follow reads) includes)))

; This is the Digraph function defined in the DeRemer and Pennello paper.  It is
; a helper function is 'compute-follow.  The variable names were taken directly
; out of the paper.  For an overview of how it works, see the intro on LALR
; lookahead computation.  For detailed information, see the paper.
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

; This is a helper function to 'digraph.
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

;--------------------------------------------------------------------------------
; This function computes the 'LA field.  This is a list of terminal symbmols,
; represented as bitsets, for every inconsistent reduction in the grammar.
; These are the terminal symbols on which the parsing engine should "reduce" by
; the given rule when in the given state.
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

;-------------------------------------------------------------------------------
; This function computes the 'action-table value.  It computes the actions one
; state at a time.  It uses the vector 'workspace to temporarily hold actions
; until the state has been analyzed.  The vector is one larger than the length
; of symbols and each element (except the last) corresponds to a symbol.  The
; values are actions and are either numbers or #f.  Negative numbers are
; reductions and are the negated value of the rule number to reduce by.  Other
; numbers are shifts (or gotos) and correspond to states.  #f means that no
; action is defined and in this case, the default action is used.  The default
; action is encoded in the last element of 'workspace.  In most cases it will be
; #f meaning the action is Error.  Sometimes, however, the default action is to
; Reduce.
;
; The values of the resulting association list will look like: (sym action arg)
; where sym is the Scheme symbol representation of a terminal or non-terminal
; symbol, action is one of '(shift goto reduce accept), and arg will be a rule
; number (for 'reduce), a state number (for 'shift or 'goto), or not present
; (for 'accept).  Additionally, the rule number will be one less than the rule
; number used internally.  This is to adjust for the fact that there is no
; internal rule 0.
(define (compute-action-table lalr-record)
  (let* ((symbols (lalr-constructor:symbols lalr-record))
	 (num-symbols (vector-length symbols))
	 (num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (num-states (vector-length states))
	 (consistent (lalr-constructor:consistent lalr-record))
	 (reduction-map (lalr-constructor:reduction-map lalr-record))
	 (reduction-rule-num (lalr-constructor:reduction-rule-num lalr-record))
	 (LA (lalr-constructor:LA lalr-record))
	 (action-table (make-vector num-states))
	 (workspace (make-vector (+ num-symbols 1))))
    (let state-loop ((state-num 0))
      (if (< state-num num-states)
	  (let* ((state (vector-ref states state-num))
		 (reductions (state:reductions state))
		 (LR-state (make-LR-state)))
	    ; Start by clearing the workspace
	    (vector-fill! workspace #f)

	    ; Add the reductions
	    (if (and (pair? reductions) (vector-ref consistent state-num))
		(vector-set! workspace num-symbols (- (car reductions)))
		(let ((max (vector-ref reduction-map (+ state-num 1))))
		  (let reduction-loop ((LA-num (vector-ref reduction-map
							   state-num)))
		    (if (< LA-num max)
			(let ((rule (vector-ref reduction-rule-num LA-num))
			      (bitset (vector-ref LA LA-num)))
			  (let term-loop ((term-num num-nonterminals)
					  (bitset bitset))
			    (if (not (= bitset 0))
				(begin
				  (if (= (modulo bitset 2) 1)
				      (vector-set! workspace term-num
						   (resolve-conflict
						    state-num
						    term-num
						    (vector-ref workspace
								term-num)
						    (- rule)
						    lalr-record)))
				  (term-loop (+ term-num 1)
					     (quotient bitset 2))))))))))

	    ; Add the shifts and gotos
	    (let shift-loop ((shifts (state:shifts state)))
	      (if (pair? shifts)
		  (let* ((to-state-num (state:number (car shifts)))
			 (symbol (state:access-symbol (car shifts))))
		    (vector-set! workspace symbol
				 (resolve-conflict state-num symbol
						   (vector-ref workspace symbol)
						   to-state-num lalr-record))
		    (shift-loop (cdr shifts)))))

	    ; Optimize the default action
	    (cond ((null? reductions)
		   #f) ; Do nothing
		  ((vector-ref workspace num-symbols)
		   ; Check to make sure that the range is not filled
		   (let loop ((symbol-num num-nonterminals))
		     (cond ((= symbol-num num-symbols)
			    ; If we get here, the range was filled.
			    (vector-set! workspace num-symbols #f))
			   ((vector-ref workspace symbol-num)
			    (loop (+ symbol-num 1))))))
		  (else ; There are multiple reductions.  Make one the default
		   (let ((red-count (make-integer-table)))
		     (let loop ((symbol-num num-nonterminals)
				(cur-max -1) (cur-rule #f))
		       (if (< symbol-num num-symbols)
			   (let ((action (vector-ref workspace symbol-num)))
			     (if (and action (< action 0))
				 (let* ((count (table-ref red-count action))
					(new-count (if count (+ count 1) 0)))
				   (table-set! red-count action new-count)
				   (if (> new-count cur-max)
				       (loop (+ symbol-num 1) new-count action)
				       (loop (+ symbol-num 1)
					     cur-max cur-rule)))
				 (loop (+ symbol-num 1) cur-max cur-rule)))
			   (begin
			     (vector-set! workspace num-symbols cur-rule)
			     (let loop ((symbol-num num-nonterminals))
			       (if (< symbol-num num-symbols)
				   (begin
				     (if (eq? (vector-ref workspace symbol-num)
					      cur-rule)
					 (vector-set! workspace symbol-num #f))
				     (loop (+ symbol-num 1)))))))))))

	    ; Add the goto actions
	    (let goto-loop ((symbol-num 0) (gotos '()))
	      (cond ((>= symbol-num num-nonterminals)
		     (set-LR-state:goto-table LR-state gotos))
		    ((vector-ref workspace symbol-num) =>
		     (lambda (state)
		       (goto-loop (+ symbol-num 1)
				  (cons (list (vector-ref symbols symbol-num)
					      'goto state)
					gotos))))
		    (else
		     (goto-loop (+ symbol-num 1) gotos))))

	    ; Add the shift/reduce actions
	    (let action-loop ((symbol-num num-nonterminals)
			      (actions (if (vector-ref workspace num-symbols)
					   (list #f 'reduce
						 (- (+ (vector-ref workspace
								   num-symbols)
						       1)))
					   '())))
	      (cond ((>= symbol-num num-symbols)
		     (set-LR-state:shift-reduce-table LR-state actions))
		    ((vector-ref workspace symbol-num) =>
		     (lambda (action)
		       (let* ((sym (vector-ref symbols symbol-num))
			      (action
			       (cond ((< action 0)
				      (list sym 'reduce (- (+ action 1))))
				     ((= symbol-num num-nonterminals)
				      (list sym 'accept))
				     (else
				      (list sym 'shift action)))))
			 (action-loop (+ symbol-num 1)
				      (cons action actions)))))
		    (else
		     (action-loop (+ symbol-num 1) actions))))

	    ; Add the list of items
	    (let item-loop ((our-items (compute-closure state lalr-record))
			    (output-items '()))
	      (if (pair? our-items)
		  (let ((item (car our-items)))
		    (let search-loop ((cur-item item))
		      (if (>= (vector-ref rule-items cur-item) 0)
			  (search-loop (+ cur-item 1))
			  (let* ((rule (- (vector-ref rule-items cur-item)))
				 (first-item (vector-ref rule-rhs rule)))
			    (item-loop (cdr our-items)
				       (cons (list (- rule 1)
						   (- item first-item))
					     output-items))))))
		  (set-LR-state:items LR-state output-items)))

	    ; Finish up this state and to to the next
	    (vector-set! action-table state-num LR-state)
	    (state-loop (+ state-num 1)))))

    (set-lalr-constructor:action-table lalr-record action-table)))

; This is a helper function used by 'compute-action-table.  It is used to
; resolve conflicts that are encountered when building the action table.  It
; takes two actions are returns the one that should be used.  In the special
; case of two actions with equal precedence and an associativity of 'non, it
; returns #f to indicate the action of Error.
;
; Arguments:
; - state = The state number.  It is used in warning messages.
; - terminal = The terminal symbol on which the conflict is occuring.
; - cur-action = The current action in the action table for 'terminal.  In most
;      cases, this will be #f.  In this case, there is no conflict and we just
;      return the new action.  In all other cases, it will be a negative number
;      indicating a reduction.  Due to properties of the LR(0) parser, there
;      will never be a Shift/Shift conflict.
; - new-action = This is the new candidate action
; - lalr-record = The lalr-record which contains the precedence maps
(define (resolve-conflict state-num terminal cur-action new-action lalr-record)
  (cond ((eq? cur-action #f)
	 new-action)
	((and (< cur-action 0) (< new-action 0))
	 (display "Reduce/Reduce conflict in state: ")
	 (display state-num)
	 (newline)
	 (max cur-action new-action))
	(else
	 (let* ((symbols (lalr-constructor:symbols lalr-record))
		(term-base (lalr-constructor:num-nonterminals lalr-record))
		(precedence-map (lalr-constructor:precedence-map lalr-record))
		(assoc-map (lalr-constructor:associativity-map lalr-record))
		(rule-precedence (lalr-constructor:rule-precedence lalr-record))
		(term-prec (vector-ref precedence-map (- terminal term-base)))
		(rule-prec (vector-ref rule-precedence (- cur-action)))
		(associativity (vector-ref assoc-map term-prec)))
	   (cond ((> term-prec rule-prec)
		  new-action)
		 ((< term-prec rule-prec)
		  cur-action)
		 ; At this point, we know the two are equal
		 ((= term-prec 0)
		  (display "Shift/Reduce conflict in state: ")
		  (display state-num) (newline)
		  (display "   Shift: ")
		  (display (vector-ref symbols terminal)) (newline)
		  (display "   Reduce: ")
		  (display (- (+ cur-action 1))) (newline)
		  new-action)
		 ((eq? associativity 'left)
		  cur-action)
		 ((eq? associativity 'right)
		  new-action)
		 (else ; non-associative
		  #f))))))

;-------------------------------------------------------------------------------
; This is the function that is called after all the major computation has been
; done.  It converts everything into a 'LR-program record and then returns it.
(define (construct-LR-program lalr-record)
  (let* ((symbols (lalr-constructor:symbols lalr-record))
	 (num-symbols (vector-length symbols))
	 (num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-lhs (lalr-constructor:rule-lhs lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (rule-actions (lalr-constructor:rule-actions lalr-record))

	 (terminals (let loop ((i (- num-symbols 1)) (list '()))
		      (if (> i num-nonterminals)
			  (loop (- i 1) (cons (vector-ref symbols i) list))
			  list)))
	 (eoi (vector-ref symbols num-nonterminals))
	 (error (lalr-constructor:error-symbol lalr-record))
	 (rules (make-vector (- (vector-length rule-rhs) 1)))
	 (states (lalr-constructor:action-table lalr-record)))

    (let rule-loop ((top-item (- (vector-length rule-items) 2)))
      (if (> top-item 0)
	  (let* ((rule-num (- (vector-ref rule-items top-item)))
		 (bottom-item (vector-ref rule-rhs rule-num))
		 (rule-length (- top-item bottom-item))
		 (right-side (make-vector rule-length)))
	    (let item-loop ((i 0))
	      (if (< i rule-length)
		  (begin
		    (vector-set! right-side i
				 (vector-ref symbols
					     (vector-ref rule-items
							 (+ i bottom-item))))
		    (item-loop (+ i 1)))))
	    (vector-set! rules (- rule-num 1)
			 (make-LR-rule (vector-ref symbols
						   (vector-ref rule-lhs
							       rule-num))
				       right-side
				       (vector-ref rule-actions rule-num)))
	    (rule-loop (- bottom-item 1)))))
    (make-LR-program terminals eoi error rules states)))
