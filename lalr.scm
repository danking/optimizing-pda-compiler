;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Internal Types                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A single instance of this record is created every time a new parser is
;;; constructed.  It starts out mostly empty and it is passed from function to
;;; function as different computations take place.  By the time every function
;;; has executed, a completed LALR(1) parser will be stored in this record.
(define-record lalr-constructor

  ;; This is a vector containing every terminal and non-terminal symbol in the
  ;; grammar.  Internally, Scheme symbols are not used.  Symbols from the
  ;; grammar are assigned a number instead.  By indexing that number into this
  ;; vector, you can convert that number back into the original Scheme symbol.
  ;;
  ;; Unless explicitly stated, any reference to a non-terminal or terminal
  ;; symbol in the rest of this documentation really refers to the symbol's
  ;; number.
  ;;
  ;; Also note that all the non-terminals are listed before any of the terminals
  ;; in this vector.  See NUM-NONTERMINALS for further explanation.
  (symbols #f)

  ;; This is the number of non-terminal symbols in the grammar.  It is also the
  ;; number of the first terminal symbol in SYMBOLS.  The test to see if a
  ;; particular symbol is a non-terminal is to test if it is less than this
  ;; number.
  (num-nonterminals #f)

  ;; This is the start symbol named by the user.  The *real* start symbol that
  ;; was created by this program is always at index 0 in SYMBOLS.
  (user-start-symbol #f)

  ;; This will be either a Scheme symbol or #f, depending on whether or not an
  ;; error symbol was defined for the grammar.  If one was defined, it will be
  ;; the last element of SYMBOLS.
  (error-symbol #f)

  ;; This is the list of symbols that cannot be shifted.  This does not mean
  ;; much to the code in this file but it affects error handling in the PDA so
  ;; the information is passed on through.
  (no-shift #f)

  ;; This is the list of symbols that can guard the accept action.  
  (end-of-parse #f)

  ;; These three fields function together as a dense representation of the
  ;; grammar.  All three are vectors.  Each element RULE-LHS and RULE-RHS
  ;; (except index 0) corresponds to a rule.  RULE-LHS contains the non-terminal
  ;; on the "left hand side" for that particular rule.  RULE-RHS gives the index
  ;; into RULE-ITEMS where "right hand side" of the rule starts.
  ;;
  ;; RULE-ITEMS contains the "right hand side" of every rule in the grammar.
  ;; The first element is the first symbol of the first rule.  A negative number
  ;; appears to mark the end of a rule.  The absolute value of this negative
  ;; number is the index into RULE-LHS & RULE-RHS for the rule just finished.
  ;;
  ;; As an example, the simple grammar:
  ;; S -> A B
  ;; A -> a b c
  ;; B -> {empty}
  ;; B -> ( B )
  ;
  ;; Would appear as:
  ;; rule-lhs = #( #f S A B B )
  ;; rule-rhs = #( #f 0 3 7 8 )
  ;; rule-items = #( A B -1 a b c -2 -3 ( B ) -4 #f )
  ;
  ;; Note that symbols were used for clarity.  In reality, the symbols would be
  ;; replaced by their corresponding numbers.  Also note that the first element
  ;; of RULE-LHS and RULE-RHS is not used because 0 is not negative and that the
  ;; last element of RULE-ITEMS is always #f.
  ;;
  ;; This representation is used because it makes constructing the LR(0) parser
  ;; fast and easy.  Items are simply indexes into RULE-ITEMS and states are
  ;; simply sets of such indexes.  "Goto" is performed by adding 1 to each item
  ;; in a state.  Reductions are possible when a negative number is reached and
  ;; the rule to reduce by is simply the absolute value of that number.
  (rule-lhs #f)
  (rule-rhs #f)
  (rule-items #f)

  ;; This vector maps terminal symbols to their corresponding precedence number.
  ;; Higher numbers have higher precedence.  Terminal symbols with no defined
  ;; precedence are assigned the number 0.
  (precedence-map #f)

  ;; This vector maps precedence numbers to the corresponding associativity for
  ;; that precedence level (one of 'left, 'right, or 'non).  Precedence level 0
  ;; is mapped to 'right to correspond to the default of choosing a shift over a
  ;; reduce.
  (associativity-map #f)

  ;; These are both vectors where each element corresponds to the same numbered
  ;; element in rule-lhs & rule-rhs.  RULE-PRECEDENCE gives the precedence level
  ;; for the rule and RULE-ACTION is the semantic action for the rule.
  (rule-precedence #f)
  (rule-actions #f)

  ;; This is a vector of length NUM-NONTERMINALS.  Its values are lists of rule
  ;; numbers (indexes in RULE-RHS & RULE-LHS).  The vector gives, for each
  ;; non-terminal symbol, the list of rules of which it is on the
  ;; left-hand-side.
  (derives #f)

  ;; This is a vector of length NUM-NONTERMINALS.  Its values are lists of rule
  ;; numbers.  Its purpose is to make COMPUTE-CLOSURE fast.
  ;;
  ;; The values are the result of unioning the DERIVES sets for every
  ;; non-terminal in the given non-terminal's First set.  The code comments for
  ;; COMPUTE-FIRSTS give the best description of what the First set is.
  (first-derives #f)

  ;; This is a vector of length NUM-NONTERMINALS.  Its values are either #t or
  ;; #f.  It specifies which non-terminal symbols can derive the empty string
  ;; and which cannot.
  (nullable #f)

  ;; This is a vector containing STATE records.  It represents the states in the
  ;; LR(0) parser.  The starting state is in index 0.  See the description of
  ;; the STATE record for more information.
  (states #f)

  ;; This is the index into STATES where the accept state occurs.  The accept
  ;; will occur on the EOI symbol in this state.
  (accept-state #f)

  ;; This vector is the same size as STATES.  Its values are #t or #f.  It marks
  ;; individual states as either consistent or inconsistent.  A state is
  ;; inconsistent if a reduction by more than one rule may occur in the state or
  ;; if there is at least one reduction and no non-terminal transitions.
  (consistent #f)

  ;; This is a vector whose size is one greater than STATES.  It maps a given
  ;; state to the number of reductions that can take place in all inconsistent
  ;; states numbered less than the state in question.  The last element contains
  ;; the sum for all the states.
  ;;
  ;; The purpose of the vector is to serve as an index into REDUCTION-RULE-NUM,
  ;; LOOKBACK, and LA.  If a state is inconsistent, then this vector maps a a
  ;; state to the index into those vectors where the data for that state starts.
  (reduction-map #f)

  ;; This vector is the same size as the last element in REDUCTION-MAP (which
  ;; could be 0).  It contains, in order of state number, the rule numbers of
  ;; reductions that take place in inconsistent states.
  (reduction-rule-num #f)

  ;; This vector is the same size as REDUCTION-RULE-NUM.  It maps a reduction to
  ;; the list of non-terminal transitions that "this" reduction can immediately
  ;; cause.  See the LALR analysis description in the comments for
  ;; CONSTRUCT-LALR-PARSER.
  (lookback #f)

  ;; This vector is the same size as REDUCTION-RULE-NUM.  Its values are bitsets
  ;; where every bit corresponds to a terminal symbol.  It maps a reduction to
  ;; the list of terminals that can appear on the front of the input-string if
  ;; this reduction were to take place.  This is the end result of the LALR
  ;; analysis.
  (LA #f)

  ;; This is a vector of length (+ num-nonterminals 1).  Each element
  ;; corresponds to a non-terminal symbol.  The value for a particular
  ;; non-terminal is the sum of the number of transistions that each
  ;; non-terminal before it was involved in.  The last element is the sum for
  ;; the entire grammar.
  ;;
  ;; The purpose of the vector is to map a given non-terminal to the start of
  ;; the data for it in FROM-STATE and TO-STATE.
  (goto-map #f)

  ;; These vectors are the same size as the last element in GOTO-MAP.  Each
  ;; element corresponds to non-terminal transition.  The values are the numbers
  ;; (indexes in STATES) of the starting and ending states (respectively) for
  ;; the transition.
  (from-state #f)
  (to-state #f)

  ;; Both these vectors are the same size as FROM-STATE & TO-STATE and both map
  ;; a non-terminal transition to a list of non-terminal transitions.  These
  ;; vectors define the relationships of the same name that are described in the
  ;; in the LALR analysis overview.  See the comments for CONSTRUCT-LALR-PARSER.
  (reads #f)
  (includes #f)

  ;; This vector is the same size as FROM-STATE & TO-STATE and maps non-terminal
  ;; transitions to the list of terminal symbols that can be shifted after it.
  ;; See the comments for CONSTRUCT-LALR-PARSER.
  (follow #f)

  ;; This is the action table that goes into the LR-automaton.  It is generated
  ;; seperately because conflict resolution has to be dealt with when
  ;; constructing it.
  (state-table #f)

  ;; This contains the list of global comments to place in the output
  (comments '()))

;;; This record defines an individual state in the LR(0) state machine.
(define-record state

  ;; This is a number unique to the state.  It is the same number as its index
  ;; in the LALR-CONSTRUCTOR:STATES vector.
  number

  ;; This is the symbol that was shifted just prior to arriving in this state
  access-symbol

  ;; This is the (sorted) set of (kernel) items in this state.  The items are
  ;; represented by indexes into RULE-ITEMS.  The "dot" is considered to be
  ;; immediately before the index given.
  items

  ;; This is the list of state records that represent states that can be reached
  ;; from "this" state by shifting a symbol.  The list is in descending order
  ;; according to the value of the records' ACCESS-SYMBOL field.  In particular,
  ;; this means that non-terminal shifts (a.k.a. "gotos") come last.
  (shifts #f)

  ;; This is a list of rule numbers (indexes into RULE-LHS & RULE-RHS).  This
  ;; represents which rules can be reduced by in this state.
  (reductions #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 Utility Code                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; --------------------------------- Sets --------------------------------- ;;;
;;; Sets are represented as sorted lists

;;; This function adds ELEMENT to SET
(define (set-insert element set)
  (if (null? set)
      (cons element '())
      (let ((x (car set)))
	(cond ((< element x) (cons element set))
	      ((> element x) (cons x (set-insert element (cdr set))))
	      (else set)))))

;;; This function takes two sets and computes their union.
(define (set-union set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((a (car set1)) (b (car set2)))
		(cond ((> a b) (cons b (set-union set1 (cdr set2))))
		      ((< a b) (cons a (set-union (cdr set1) set2)))
		      (else (set-union (cdr set1) set2)))))))

;;; ------------------------------- Bit-Sets ------------------------------- ;;;
;;; Bit-Sets are simply integers.

;;; Computes the union of two bit-sets
(define bit-union bitwise-ior)

;;; Sets the 'i-th bit of BITSET to 1.
(define (set-bit bitset i)
  (bit-union bitset (expt 2 i)))

;;; ------------------------------- Vectors -------------------------------- ;;;
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

;;; This is the top-level function for the PDA generation algorithm.  It takes a
;;; CFG record (which represents a Context-Free Grammar) as input and returns a
;;; LR-PROGRAM record (which contains the PDA state-table) as output.
;;;
;;;
;;; --- WHERE TO FIND MORE INFORMATION
;;;
;;; The comments in this file assume that you are familiar with general LR
;;; parsing.  If not, a good introduction can be found in the "Dragon Book"
;;; here:
;;;
;;;     <citation for Dragon Book>
;;;
;;; This function generations a LALR(1) state table.  It uses an efficient
;;; algorithm that is described fully in:
;;;
;;;     "Efficient Computation of LALR(1) Look-Ahead Sets", F. DeRemer and
;;;     T. Pennello, TOPLAS, vol. 4, no. 4, october 1982.
;;;
;;;
;;; --- A SHORT DESCRIPTION OF THE LALR ALGORITHM
;;;
;;; Note: In the follow description, 'u' denotes "union" and 'U', when applied
;;; to a set of sets, denotes the result of unioning all internal sets.  'p' and
;;; 'q' are states in the LR(0) state machine.  'A', 'B', 'C', and 'D' are
;;; non-terminal symbols.  'w' represents a possibly empty string of terminal
;;; and non-terminal symbols.  A->w denotes a reduction.
;;;
;;; The final result of the LALR analysis is LA.  This is a list of terminals
;;; for every reduction in the LR(0) state machine.  This tells the table
;;; constructor which look-ahead tokens should have the corresponding "reduce"
;;; action.  LA is represented internally as a bitset with one bit for every
;;; terminal symbol.
;;;
;;; LA(q,A->w) = U( Follow(p,A) | (q,A->w) lookback (p,A) )
;;;
;;; lookback is mapping from reductions in the LR(0) state machine to a list of
;;; non-terminal transitions ("gotos") that could happen immediately after each
;;; reduction.
;;;
;;; Follow is a list of terminal symbols for every non-terminal transition in
;;; the LR(0) state machine.  It gives the list of terminal symbols that can
;;; appear on the input string immediately after each nonterminal transition in
;;; a valid grammar.
;;;
;;; Thus, the problem of computing LA is reduced to the problem of computing
;;; follow sets for the "gotos" in the grammar.
;;;
;;; Follow(p,A) = Read(p,A) u U{ Follow(p',B) | (p,A) includes (p',B) }
;;; Read(p,A)   = DR(p,A)   u U{ Read(r,C)    | (p,A) reads    (r,C)  }
;;;
;;; Note that these definitions are recursive and very similar.
;;;
;;; includes is a mapping from non-terminal transitions to a list of
;;; non-terminal transitions.  This relation takes care of the fact that if
;;; there is a rule A -> B C D, and 'D' is nullable, then the follow sets of
;;; both 'C' and 'D' "include" whatever is in the follow set of 'A'.
;;;
;;; reads is another mapping from non-terminal transitions to a list of
;;; non-terminal transitions.  This relation takes care of the fact that if "A B
;;; C D" appears on the right side of a rule, and 'B' and 'C' are nullable, then
;;; the follow set for 'B' includes the follow set of 'C' and the follow set of
;;; 'A' includes the follow set of 'B'.
;;;
;;; DR ("Direct Read") is a list of terminal symbols for every non-terminal
;;; trasition.  It tells which terminal symbols can be shifted immediately after
;;; a non-terminal transition.  This is just the list of which terminal symbols
;;; have a "shift" action defined in the "to state" of the transition.
;;;
;;; Now we have a way to compute Follow sets (and thus LA sets) from simple
;;; properties of the grammar and the LR(0) state machine.  We just need a way
;;; to deal with the recursive definition of Read and Follow.  The paper defines
;;; a fuction called "digraph" which takes care of this.  It finds Strongly
;;; Connected Components (a maximal set of verticies where there is a path from
;;; any vertex to any other vertex in the set) in the graph definef by the
;;; includes and reads relations and treats them as single nodes.  After that,
;;; the graph becomes a tree and information simply propogates up the tree.
;;;
;;; QUALIFICATION
;;;
;;; The paper computes LA for every reduction in the grammar.  This algorithm
;;; only computes LA for "inconsistent" states.  Inconsistent states are states
;;; in which there is more than one reduction or a single reduction and no
;;; non-terminal transitions.  For the other reductions, the reduction happens
;;; by "default" (that is, for any terminal symbol that does not have any other
;;; action defined).  This does not change the grammar any.  Any errors in the
;;; input string will be caught in the next state when the PDA finds that no
;;; shift is defined for the terminal symbol.

;;; ----------------------------------------------------------------------------
;;; Arguments:
;;; - TERMS = The list of terminal symbols used in the grammar.  This should
;;;      contain all those that appear in PREC, ERR, and EOS as well.
;;; - PREC = The precedence levels used by the grammar.  These should look like
;;;      (prec term ...) where PREC is one of '(left right non)
;;; - ERR = The name of the error symbol used by the grammar or #f.
;;; - EOS = The name of the token that will mark the end-of-stream or #f.
;;; - EOP = A list of tokens that are valid for guarding the accept action.
;;;      This should probably include EOS but that is not required.
;;; - NO-SHIFT = A list of tokens that should never be shifted.  This should
;;;      probably include EOS but that is not required.
;;; - START = The name of the start non-terminal
;;; - RULES = The list of non-terminal declarations.  These should look like:
;;;      (non-term ([prec] (sym ...) action) ...)
(define (create-lalr-parser terms prec err eos eop no-shift start rules)
  (let ((lalr-record (convert-grammar terms prec err eos eop no-shift start
				      rules)))
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

    (compute-state-table lalr-record)
    (construct-PDA lalr-record)))

;;;-----------------------------------------------------------------------------
;;; This function takes the grammar as it was passed in and converts into the
;;; representations used internal to the parser.  Along the way, it does a lot
;;; of sanity-checking on the grammar.
;;;
;;; This function sets the values of the following LALR-CONSTRUCTOR fields:
;;; - SYMBOLS
;;; - NUM-NONTERMINALS
;;; - USER-START-SYMBOL
;;; - ERROR-SYMBOL
;;; - NO-SHIFT
;;; - END-OF-PARSE
;;; - RULE-LHS
;;; - RULE-RHS
;;; - RULE-ITEMS
;;; - RULE-PRECEDENCE (This value will be modified by CALCULATE-PRECEDENCE)
;;; - RULE-ACTION
;;; - PRECEDENCE
(define (convert-grammar terminals prec err eos eop no-shift start rules)
  (any (lambda (term)
	 (if (not (symbol? term))
	     (error "Invalid terminal symbol:" term)))
       terminals)
  (let* ((lalr-record (make-lalr-constructor))
	 (num-terminals (length terminals))
	 (nonterminals (extract-nonterminals rules))
	 (num-nonterminals (+ 1 (length nonterminals)))
	 (eop (if (and (null? eop) eos) (list eos) eop)))
    (receive (symbols reverse-map)
	     (create-symbol-map nonterminals num-nonterminals
				terminals num-terminals)

       ;; Create the new start symbol (S' -> S <eop1> | S <eop2> | ...)
       (let* ((new-start (create-start-symbol start reverse-map))
	      (aug-grammar (cons (cons new-start
				       (map (lambda (eop)
					      (list (list start eop) #f))
					    eop)) rules)))
	 (if (or (not (table-ref reverse-map start))
		 (not (< (table-ref reverse-map start) num-nonterminals)))
	     (error "start must be a nonterminal symbol"))

	 (vector-set! symbols 0 new-start)
	 (table-set! reverse-map new-start 0)

	 ;; Convert the precedence information to numbers
	 (receive (precedence-map associativity-map)
		  (create-precedence-map prec num-terminals num-nonterminals
					 reverse-map)

	   ;; Compile the grammar
	   (receive (rule-lhs rule-rhs rule-items rule-precedence rule-actions)
		    (pack-grammar aug-grammar reverse-map precedence-map
				  new-start no-shift)

	     (set-lalr-constructor:precedence-map lalr-record precedence-map)
	     (set-lalr-constructor:associativity-map lalr-record
						     associativity-map)	
	     (set-lalr-constructor:rule-lhs lalr-record rule-lhs)
	     (set-lalr-constructor:rule-rhs lalr-record rule-rhs)
	     (set-lalr-constructor:rule-items lalr-record rule-items)
	     (set-lalr-constructor:rule-precedence lalr-record rule-precedence)
	     (set-lalr-constructor:rule-actions lalr-record rule-actions))))

       ;;Set the last few fields then return the record
       (set-lalr-constructor:num-nonterminals lalr-record num-nonterminals)
       (set-lalr-constructor:symbols lalr-record symbols)
       (set-lalr-constructor:user-start-symbol lalr-record
					       (table-ref reverse-map start))
       (set-lalr-constructor:error-symbol lalr-record err)
       (set-lalr-constructor:no-shift lalr-record no-shift)
       (set-lalr-constructor:end-of-parse lalr-record
	  (map (lambda (token) (table-ref reverse-map token)) eop)))

    lalr-record))



;;; This is a helper function used by CONVERT-GRAMMAR.  It takes the list of
;;; rules and returns a list of all the non-terminal symbols that the rules
;;; define.  It also checks to see that the non-terminal names are valid.
;;; Nonterminal names that are the same as terminal names will be caught by
;;; CREATE-SYMBOL-MAP.
(define (extract-nonterminals rules)
  (fold-right
   (lambda (rule non-terms)
     (let ((nt (car rule)))
       (if (not (symbol? nt))
	   (error "Invalid non-terminal name:" nt))
       (if (member nt non-terms)
	   (error "Duplicate non-terminal declaration"))
       (cons nt non-terms)))
   '() rules))

;;; This is a helper function used by CONVERT-GRAMMAR.  It creates a mapping
;;; between the Scheme symbols used in the grammar to the numbers used
;;; internally.  It returns two values.  The first is a vector that is stored in
;;; LALR-CONSTRUCTOR.  It is used to map from numbers back to the symbols.  The
;;; second value is a hash-table that maps the symbols into the corresponding
;;; numbers.
;;;
;;; This function also checks to make sure that there are no duplicate terminal
;;; symbols (NONTERMINALS is guaranteed to have no duplicates) and that there
;;; are no terminal symbols and nonterminal symbols with the same name.
(define (create-symbol-map nonterminals num-nonterminals terminals num-terms)
  (let ((symbol-map (make-vector (+ num-nonterminals num-terms)))
	(reverse-map (make-symbol-table)))
    (let loop ((i num-nonterminals) (terminals terminals))
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

;;; This is a helper function used by CONVERT-GRAMMAR.  It creates a the new
;;; start symbol and insures that it does not conflict with any symbols already
;;; existing in the grammar.
(define (create-start-symbol start reverse-map)
  (if (not (symbol? start))
      (error "Invalid start symbol:" start))
  (let loop ((sym '*start))
    (if (table-ref reverse-map sym)
	(loop (string->symbol (string-append (symbol->string sym) "*")))
	sym)))

;;; This is a helper function used by CONVERT-GRAMMAR.  It takes the precedence
;;; declarations from the input and returns two records.  The first is a vector
;;; that maps terminals symbols to a number which represents the terminal's
;;; precedence.  Higher numbers represent higher precedence and zero is assigned
;;; to terminals without defined precedence.  The second value is a vector that
;;; maps precedence numbers to that precedence level's corresponding
;;; associativity.  Level 0 is assigned the value of 'right to correspond to
;;; choosing shifts over reductions.
(define (create-precedence-map prec num-terms num-nonterminals reverse-map)
  (let* ((precedence-map (make-vector num-terms 0))
	 (rev-assoc-map (cdr
	  (fold-right
	   (lambda (prec-decl count-dot-assoc)
	     (for-each
	      (lambda (terminal)
		(vector-set! precedence-map (- (table-ref reverse-map terminal)
					       num-nonterminals)
			     (car count-dot-assoc)))
	      (cdr prec-decl))
	     (if (not (member (car prec-decl) '(left right non)))
		 (error "Unknown associativity:" (car prec-decl)))
	     (cons (+ 1 (car count-dot-assoc))
		   (cons (car prec-decl) (cdr count-dot-assoc))))
	   '(1 . (right)) prec))))
    (values precedence-map (list->vector (reverse rev-assoc-map)))))

;;; This is a helper function used by CONVERT-GRAMMAR.  Its primary purpose is
;;; to iterate over all the rules and change them to the internal format defined
;;; by RULE-LHS, RULE-RHS, and RULE-ITEMS.  Along the way, it also extracts
;;; information about rule precedence and actions.
(define (pack-grammar nonterm-decls reverse-map prec-map new-start no-shift)
  (receive (rule-lhs rule-rhs rule-items rule-precedence rule-actions)
	   (allocate-rule-vectors nonterm-decls)
    (let nt-loop ((nonterm-decls nonterm-decls) (item-num 0) (rule-num 1))
      (if (pair? nonterm-decls)
	  (let rule-loop ((rules (cdar nonterm-decls))
			  (item-num item-num) (rule-num rule-num))
	    (if (pair? rules)
		(let* ((has-prec (= (length (car rules)) 3))
		       (syms (if has-prec (cadar rules) (caar rules)))
		       (action (if has-prec (caddr rules) (cadar rules))))
		  (vector-set! rule-lhs rule-num
			       (table-ref reverse-map (caar nonterm-decls)))
		  (vector-set! rule-rhs rule-num item-num)
		  (vector-set! rule-actions rule-num action)
		  (if has-prec
		      (begin
			(if (not (symbol? (caar rules)))
			    (error "Rule precedence must be a symbol:"
				   (caar rules)))
			(let ((num (table-ref reverse-map (caar rules))))
			  (if num
			      (vector-set! rule-precedence rule-num num)
			      (error
			       "Rule precedence must be a terminal symbol:"
			       (caar rules))))))
		  (let item-loop ((syms syms) (item-num item-num))
		    (if (pair? syms)
			(let ((num (table-ref reverse-map (car syms))))
			  (cond ((eq? num #f)
				 (error "Unknown symbol:" (car syms)))
				((and (member (car syms) no-shift)
				      (not (eq? (caar nonterm-decls)
						new-start)))
				 (error "No-Shift tokens cannot appear in the grammar:" (car syms)))
				(else
				 (vector-set! rule-items item-num num)
				 (item-loop (cdr syms) (+ item-num 1)))))
			(begin
			  (vector-set! rule-items item-num (- rule-num))
			  (rule-loop (cdr rules) (+ item-num 1)
				     (+ rule-num 1))))))
		(nt-loop (cdr nonterm-decls) item-num rule-num)))))

    (values rule-lhs rule-rhs rule-items rule-precedence rule-actions)))

;;; This is a helper function used by PACK-GRAMMAR.  It does a quick iteration
;;; over all the rules to find out what size vectors are needed.
(define (allocate-rule-vectors rules)
  (let rule-loop ((num-rules 1) (num-items 1) (rules rules))
    (if (pair? rules)
	(let item-loop ((num-items num-items) (num-rules num-rules)
			(right-sides (cdar rules)))
	  (cond ((null? right-sides)
		 (rule-loop num-rules num-items (cdr rules)))
		((= (length (car right-sides)) 2)
		 (item-loop (+ (length (caar right-sides)) 1 num-items)
			    (+ 1 num-rules) (cdr right-sides)))
		((= (length (car right-sides)) 3)
		 (item-loop (+ (length (cadar right-sides)) 1 num-items)
			    (+ 1 num-rules) (cdr right-sides)))
		(else
		 (error "Malformed right-side for non-terminal:" (car rules)))))
	(values (make-vector num-rules #f)     ;rule-lhs
		(make-vector num-rules #f)     ;rule-rhs
		(make-vector num-items #f)     ;rule-items
		(make-vector num-rules #f)     ;rule-precedence
		(make-vector num-rules #f))))) ;rule-actions

;;;-----------------------------------------------------------------------------
;;; This function is responsible for computing the RULE-PRECEDENCE field.  The
;;; vector was created in CONVERT-GRAMMAR where some of the values were set.
;;; These values correspond to rules whose precedence was manually specified and
;;; this function does not change them.  For the rest, the precedence is set to
;;; the precedence value of the last terminal-symbol in the rule or to 0 if
;;; there are no terminal symbols.
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

;;;-----------------------------------------------------------------------------
;;; This function is responsible for computing the DERIVES field.  This is a
;;; mapping from a nonterminal symbol to the list of rule-numbers of which it is
;;; on the left-hand-side.
(define (compute-derives lalr-record)
  (let ((rule-lhs (lalr-constructor:rule-lhs lalr-record))
	(derives (make-vector (lalr-constructor:num-nonterminals lalr-record)
			      '())))
    (let loop ((rule-num (- (vector-length rule-lhs) 1)))
      (if (> rule-num 0)
	  (let ((non-term (vector-ref rule-lhs rule-num)))
	    (vector-set! derives non-term
			 (cons rule-num (vector-ref derives non-term)))
	    (loop (- rule-num 1)))))
    (set-lalr-constructor:derives lalr-record derives)))

;;;-----------------------------------------------------------------------------
;;; This function is responsible for computing the NULLABLE field.  This vector
;;; tells whether or not a given non-terminal symbol can derive the empty
;;; string.
;;;
;;; Algorithm:
;;; The algorithm is divided into two parts.  During the first part, the
;;; function loops over every rule and determines which ones are trivially
;;; nullable (they have nothing on the right side), which ones are trivially not
;;; nullable (they contain terminal symbols, which ones are uncertain (they
;;; contain only non-terminal symbols).  It constructs the following data
;;; structures:
;;; - RULE-COUNT = A vector where each element corresponds to a rule.  For the
;;;      uncertain rules, it contains the number of symbols on the right side.
;;; - RULE-SETS = A vector where each element corresponds to a non-terminal
;;;      symbol.  The values are lists of rule numbers.  The rules in question
;;;      are all uncertain rules where the given non-terminal could affect
;;;      whether or not the rule becomes nullable.
;;; - NULLABLE = This is the final product.  It starts off assuming that every
;;;      non-terminal is not nullable and this assumption is gradually refined.
;;;      By the end of the first part, the non-terminals which are trivially
;;;      nullable have been marked as such.
;;; - NULLED-NON-TERMS = This is a list of which non-terminals have been marked
;;;      as nullable.
;;;
;;; The second part of algorithm loops over NULLED-NON-TERMS.  For each rule in
;;; RULE-SETS corresponding to the non-terminal, it reduces the rule's value in
;;; RULE-COUNT by 1.  When this value becomes 0 (meaning that every non-terminal
;;; on the rule's right side is now known to be nullable), the non-terminal on
;;; the rule's left-side is set to nullable.  It is then added to
;;; NULLED-NON-TERMS so that the loop considers the effects its new status might
;;; have on other rules.
(define (compute-nullable lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-lhs (lalr-constructor:rule-lhs lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (rule-count (make-vector (vector-length rule-lhs) 0))
	 (rule-sets (make-vector num-nonterminals '()))
	 (nullable (make-vector num-nonterminals #f)))
    ;; Part 1
    (let rule-loop ((rule-num 1) (nulled-non-terms '()))
      (if (< rule-num (vector-length rule-lhs))
	  (let ((first-item-num (vector-ref rule-rhs rule-num)))
	    ;; Check too see if the rule is trivially nullable
	    (if (< (vector-ref rule-items first-item-num) 0)
		(let ((symbol (vector-ref rule-lhs rule-num)))
		  (if (not (vector-ref nullable symbol))
		      (begin
			(vector-set! nullable symbol #t)
			(rule-loop (+ rule-num 1)
				   (cons symbol nulled-non-terms)))))
		;; Check to see if the rule is trivially not-nullable
		(let term-loop ((item-num first-item-num))
		  (let ((symbol (vector-ref rule-items item-num)))
		    (cond ((>= symbol num-nonterminals)
			   (rule-loop (+ rule-num 1) nulled-non-terms))
			  ((> symbol 0)
			   (term-loop (+ item-num 1)))
			  ;; otherwise, construct the nullable graph
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
	  ;; Part 2
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

;;;-----------------------------------------------------------------------------
;;; This function commputes the FIRST-DERIVES field.  This is a vector where
;;; each element corresponds to a non-terminal symbol.  The values are sets that
;;; are the union of the DERIVE value for every element in the non-terminal's
;;; 'first set (see COMPUTE-FIRSTS).
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

;;; This function computest the value of FIRSTS.  The comments inside the
;;; function give the best description of what this value is.
(define (compute-firsts lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-lhs (lalr-constructor:rule-lhs lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (firsts (make-vector num-nonterminals '())))

    ;; For each rule, if the right-hand-side begins with a non-terminal, then
    ;; that non-terminal is in the first set for the non-terminal on the
    ;; left-hand-side.
    (let loop ((i 1))
      (if (< i (vector-length rule-lhs))
	  (let ((symbol (vector-ref rule-items (vector-ref rule-rhs i))))
	    (if (< -1 symbol num-nonterminals)
		(let ((lhs (vector-ref rule-lhs i)))
		  (vector-set! firsts lhs
			       (set-insert symbol (vector-ref firsts lhs)))))
	    (loop (+ i 1)))))

    ;; If non-terminal A is in the first set for non-terminal B, then the first
    ;; set for non-terminal B includes the first set for non-terminal A.
    ;; (transitive closure)
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

    ;; The first set for a non-terminal includes itself.  (reflexive closure)
    (let loop ((i 0))
      (if (< i num-nonterminals)
	  (begin
	    (vector-set! firsts i (set-insert i (vector-ref firsts i)))
	    (loop (+ i 1)))))

    ;; Return the result
    firsts))

;;;-----------------------------------------------------------------------------
;;; This function builds an LR(0) state machine for the grammar.  It starts by
;;; putting the start state into the state queue and then it uses
;;; COMPUTE-CLOSURE and COMPUTE-GOTO until no more states are generated.  The
;;; result is store in the STATES field of LALR-RECORD.
(define (compute-LR0-states lalr-record)
  (let* ((num-symbols (vector-length (lalr-constructor:symbols lalr-record)))
	 (num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (user-start-symbol (lalr-constructor:user-start-symbol lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (derives (lalr-constructor:derives lalr-record))
	 (init-items (map (lambda (i) (vector-ref rule-rhs i))
			  (vector-ref derives 0)))
	 (item-map ((make-table-maker equal? (lambda (lst) (fold + 0 lst)))))
	 (start-state (make-state 0 #f init-items)))
    (table-set! item-map init-items start-state)

    (let state-loop ((state-stack (list start-state))
		     (state-list (list start-state)))
      (if (pair? state-stack)
	  (let* ((state (car state-stack))
		 (items (compute-closure state lalr-record))
		 (next-states (compute-goto items num-symbols rule-items)))
	    (save-reductions state items rule-items)
	    (let next-loop ((symbol 0) (shifts '()) (state-list state-list)
			    (state-stack (cdr state-stack)))
	      (if (< symbol num-symbols)
		  (let ((next-items (vector-ref next-states symbol)))
		    (cond ((null? next-items)
			   (next-loop (+ symbol 1) shifts
				      state-list state-stack))
			  ((table-ref item-map next-items) =>
			   (lambda (state)
			     (next-loop (+ symbol 1) (cons state shifts)
					state-list state-stack)))
			  (else
			   (let* ((new-state
				  (make-state (+ (state:number (car state-list))
						 1)
					      symbol next-items)))
			     (if (and (= (state:number state) 0)
				      (= symbol user-start-symbol))
				 (set-lalr-constructor:accept-state lalr-record
				   (state:number new-state)))
			     (table-set! item-map next-items new-state)
			     (next-loop (+ symbol 1) (cons new-state shifts)
					(cons new-state state-list)
					(cons new-state state-stack))))))
		  (begin
		    (set-state:shifts state (reverse shifts))
		    (state-loop state-stack state-list)))))
	  (set-lalr-constructor:states lalr-record
				       (list->vector (reverse state-list)))))))

;;; This function takes a list of "kernel" items from STATE and returns the
;;; complete list of items that represent that state.  The new items will all
;;; have the dot at the front of the rule.  For example, if the parser can be in
;;; the state "A -> a B c . D" then it can also be in the state "D -> . f G".
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

;;; This function takes a list of items (which represent a state) and returns a
;;; vector.  The vector is indexed by terminal and non-terminal symbols and the
;;; values are new lists of items.  These represent (possibly new) states which
;;; can be reached by shifting the corresponding symbol.
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

;;; This fuction figures out which reductions are possible in the given state
;;; and then saves them in the state record.
(define (save-reductions state items rule-items)
  (set-state:reductions state
			(let loop ((items items))
			  (if (null? items)
			      '()
			      (let ((item (vector-ref rule-items (car items))))
				(if (< item 0)
				    (cons (- item) (loop (cdr items)))
				    (loop (cdr items))))))))

;;;-----------------------------------------------------------------------------
;;; This function computes the CONSISTENT and REDUCTION-MAP fields.
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

;;;-----------------------------------------------------------------------------
;;; This function computes the REDUCTION-RULE-NUM field.
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

;;;-----------------------------------------------------------------------------
;;; This function computes the GOTO-MAP, FROM-STATE, and TO-STATE fields.
;;; Basically, it enumerates all the non-terminal transitions ("gotos") in the
;;; LR(0) state machine.  FROM-STATE & TO-STATE are the numbers of the starting
;;; and destination state for each transition.  All the non-terminal transitions
;;; are listed together.  GOTO-MAP maps a given non-terminal to the starting
;;; point in FROM-STATE & TO-STATE for that non-terminal.
(define (compute-goto-map lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (num-states (vector-length states))
	 (goto-map (make-vector (+ num-nonterminals 1) 0)))

    ;; Calculate the number of "gotos" each non-terminal is involved in
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

    ;; Covert the individual sums into indexes into FROM-STATE and TO-STATE
    (let sum-loop ((i 0) (sum 0))
      (if (< i num-nonterminals)
	  (let ((x (vector-ref goto-map i)))
	    (vector-set! goto-map i sum)
	    (sum-loop (+ i 1) (+ sum x)))
	  (vector-set! goto-map num-nonterminals sum)))

    ;; Compute FROM-STATE and TO-STATE
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

      ;; Finish up
      (set-lalr-constructor:goto-map lalr-record goto-map)
      (set-lalr-constructor:from-state lalr-record from-state)
      (set-lalr-constructor:to-state lalr-record to-state))))

;;; This is a helper function used in the computation of READS and INCLUDES.
;;; When given a state and a non-terminal symbol, it returns the number of that
;;; particular "non-terminal transition."
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

;;;-----------------------------------------------------------------------------
;;; This function computes the READS field and the DR value.  DR is stored in
;;; the FOLLOW field where it will serve as the initial value when
;;; COMPUTE-FOLLOW is called.
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
		  (vector-set! reads goto-num gotos)))
	    (goto-loop (+ goto-num 1)))))
    (set-lalr-constructor:follow lalr-record DR)
    (set-lalr-constructor:reads lalr-record reads)))

;;;-----------------------------------------------------------------------------
;;; This function computes the INCLUDES and LOOKBACK fields.  It does this by
;;; looping over every non-terminal transition and over every rule that the
;;; given non-terminal is involved in.  LOOKBACK is computed by iterating to the
;;; end of the rule and making a connection between the state at the rule's end
;;; and the state at the beginning.  INCLUDES is computed by working backward
;;; from the end of each rule and making a link between the original goto and
;;; the rule's symbols as long the symbols remain nullable.
;;;
;;; Note: The definition of INCLUDES is that if there are rules:
;;; A -> ... B
;;; A -> ... C
;;; then the relation should map B -> A and C -> A.  This algorithm computing
;;; this backward.  It maps A -> (B C).  When the mapping is completed, the
;;; function TRANSPOSE is called to put things in the correct order.
(define (compute-includes-and-lookback lalr-record)
  (let* ((derives (lalr-constructor:derives lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (to-state (lalr-constructor:to-state lalr-record))
	 (num-gotos (vector-length to-state))
	 (reduction-rule-num (lalr-constructor:reduction-rule-num lalr-record))
	 (includes-tp (make-vector num-gotos))
	 (lookback (make-vector (vector-length reduction-rule-num) '())))
    (let goto-loop ((goto-num 0))
      (if (< goto-num num-gotos)
	  (let rule-loop ((rules
			   (vector-ref derives
				       (state:access-symbol
					(vector-ref states
						    (vector-ref to-state
								goto-num)))))
			  (edges '()))
	    (if (pair? rules)
		(rule-loop (cdr rules)
			   (analyze-rule (car rules) goto-num edges
					 lookback lalr-record))
		(begin
		  (vector-set! includes-tp goto-num edges)
		  (goto-loop (+ goto-num 1)))))))
    (set-lalr-constructor:lookback lalr-record lookback)
    (set-lalr-constructor:includes lalr-record (transpose includes-tp))))

;;; This is a helper function used by COMPUTE-INCLUDES-AND-LOOKBACK.  It works
;;; on an individual rule.  It does two things.  The first is that it adds
;;; entries to LOOKBACK as necessary.  The second is that it computes what edges
;;; need to be added to the includes relation for the given GOTO-NUM and
;;; RULE-NUM.
(define (analyze-rule rule-num goto-num edges lookback lalr-record)
  (let* ((num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (nullable (lalr-constructor:nullable lalr-record))
	 (consistent (lalr-constructor:consistent lalr-record))
	 (reduction-map (lalr-constructor:reduction-map lalr-record))
	 (reduction-rule-num (lalr-constructor:reduction-rule-num lalr-record))
	 (from-state (lalr-constructor:from-state lalr-record))
	 (state-num1 (vector-ref from-state goto-num)))
    (let forward-loop ((item-num (vector-ref rule-rhs rule-num))
		       (state (vector-ref states state-num1))
		       (state-nums (list state-num1)))
      (let ((symbol (vector-ref rule-items item-num)))
	(if (>= symbol 0)
	    (let state-loop ((shifts (state:shifts state)))
	      (cond ((null? shifts)
		     (error "Internal Error: Could not find shift symbol."))
		    ((= (state:access-symbol (car shifts)) symbol)
		     (forward-loop (+ item-num 1) (car shifts)
				   (cons (state:number (car shifts))
					 state-nums)))
		    (else
		     (state-loop (cdr shifts)))))
	    (begin
	      (if (not (vector-ref consistent (state:number state)))
		  (let ((max (vector-ref reduction-map
					 (+ (state:number state) 1))))
		    (let red-loop ((red-num (vector-ref reduction-map
							(state:number state))))
		      (cond ((>= red-num max) (error
			    "Internal Error: Could not find reduction number."))
			    ((= (vector-ref reduction-rule-num red-num)
				rule-num)
			     (vector-set! lookback red-num
					  (cons goto-num (vector-ref lookback
								     red-num))))
			    (else
			     (red-loop (+ red-num 1)))))))
	      (let reverse-loop ((done #f) (state-nums (cdr state-nums))
				 (item-num (- item-num 1)) (edges edges))
		(if done
		    edges
		    (let ((symbol (vector-ref rule-items item-num)))
		      (if (< -1 symbol num-nonterminals)
			  (reverse-loop (not (vector-ref nullable symbol))
					(cdr state-nums)
					(- item-num 1)
					(cons (map-goto (car state-nums) symbol
							lalr-record)
					      edges))
			  (reverse-loop #t state-nums item-num edges)))))))))))

;;; This is a helper function used by COMPUTE-INCLUDES-AND-LOOKBACK.  It
;;; reverses the mapping of INCLUDES-TP as described in the comments for that
;;; function.
(define (transpose includes-tp)
  (let* ((num-gotos (vector-length includes-tp))
	 (includes (make-vector num-gotos '())))
    (let outer-loop ((outer-goto-num 0))
      (if (< outer-goto-num num-gotos)
	  (let inner-loop ((inner-gotos (vector-ref includes-tp outer-goto-num)))
	    (if (null? inner-gotos)
		(outer-loop (+ outer-goto-num 1))
		(let ((inner-goto-num (car inner-gotos)))
		  (vector-set! includes inner-goto-num
			       (cons outer-goto-num (vector-ref includes
								inner-goto-num)))
		  (inner-loop (cdr inner-gotos)))))))
    includes))

;;;-----------------------------------------------------------------------------
;;; This function finishes computing the value of the FOLLOW field.  This field
;;; was initialized by COMPUTE-DR-AND-READS to the value of DR (see the comment
;;; to CONSTRUCT-LALR-PARSER).  This function uses the DIGRAPH function as
;;; defined in the DeRemer and Pennello paper to compute Read and then Follow.
(define (compute-follow lalr-record)
  (let ((reads (lalr-constructor:reads lalr-record))
	(includes (lalr-constructor:includes lalr-record))
	(follow (lalr-constructor:follow lalr-record)))
    (digraph (digraph follow reads) includes)))

;;; This is the Digraph function defined in the DeRemer and Pennello paper.  It
;;; is a helper function is COMPUTE-FOLLOW.  The variable names were taken
;;; directly out of the paper.  For an overview of how it works, see the intro
;;; on LALR lookahead computation.  For detailed information, see the paper.
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

;;; This is a helper function to DIGRAPH.
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

;;;------------------------------------------------------------------------------
;;; This function computes the LA field.  This is a list of terminal symbmols,
;;; represented as bitsets, for every inconsistent reduction in the grammar.
;;; These are the terminal symbols on which the parsing engine should "reduce"
;;; by the given rule when in the given state.
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

;;;-----------------------------------------------------------------------------
;;; This function computes the STATE-TABLE field.  It computes the actions one
;;; state at a time.  It uses the vector WORKSPACE to temporarily hold actions
;;; until the state has been analyzed.  The vector is one larger than the length
;;; of symbols and each element (except the last) corresponds to a symbol.  The
;;; values are lists of numbers which represent actions.  Negative numbers are
;;; reductions and are the negated value of the rule number to reduce by.  Other
;;; numbers are shifts (or gotos) and correspond to states.  If there is more
;;; than one number, that symbol has a conflict and the action on top is the one
;;; that will appear in the generated PDA.  The default action is encoded in the
;;; last element of WORKSPACE.  Usually this will be empty but sometimes it will
;;; contain a reduce.
(define (compute-state-table lalr-record)
  (let* ((symbols (lalr-constructor:symbols lalr-record))
	 (num-symbols (vector-length symbols))
	 (num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (end-of-parse (lalr-constructor:end-of-parse lalr-record))
	 (rule-lhs (lalr-constructor:rule-lhs lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (states (lalr-constructor:states lalr-record))
	 (accept-state (lalr-constructor:accept-state lalr-record))
	 (num-states (vector-length states))
	 (consistent (lalr-constructor:consistent lalr-record))
	 (reduction-map (lalr-constructor:reduction-map lalr-record))
	 (reduction-rule-num (lalr-constructor:reduction-rule-num lalr-record))
	 (LA (lalr-constructor:LA lalr-record))
	 (state-table (make-vector num-states '()))
	 (workspace (make-vector (+ num-symbols 1))))
    (let state-loop ((state-num 0))
      (if (< state-num num-states)
	  (let* ((state (vector-ref states state-num))
		 (reductions (state:reductions state)))
	    ;; Start by clearing the workspace
	    (vector-fill! workspace '())

	    ;; Add the reductions
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
			    (if (= bitset 0)
				(reduction-loop (+ LA-num 1))
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

	    ;; Add the shifts and gotos
	    (let shift-loop ((shifts (state:shifts state)))
	      (if (pair? shifts)
		  (let* ((to-state-num (state:number (car shifts)))
			 (symbol (state:access-symbol (car shifts))))
		    (vector-set! workspace symbol
				 (resolve-conflict state-num symbol
						   (vector-ref workspace symbol)
						   to-state-num lalr-record))
		    (shift-loop (cdr shifts)))))

	    ;; Optimize the default action
	    (cond ((null? reductions)
		   #f) ; Do nothing
		  ((vector-ref workspace num-symbols)
		   ;; Check to make sure that the range is not filled
		   (let loop ((symbol-num num-nonterminals))
		     (cond ((= symbol-num num-symbols)
			    ;; If we get here, the range was filled.
			    (vector-set! workspace num-symbols '()))
			   ((not (null? (vector-ref workspace symbol-num)))
			    (loop (+ symbol-num 1))))))
		  (else ; There are multiple reductions.  Make one the default
		   (let ((red-count (make-integer-table)))
		     (let loop ((symbol-num num-nonterminals)
				(cur-max -1) (cur-rule #f))
		       (if (< symbol-num num-symbols)
			   (let ((actions (vector-ref workspace symbol-num)))
			     (if (and (pair? actions) (< (car actions) 0))
				 (let* ((action (car actions))
					(count (table-ref red-count action))
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
				     (if (equal? (vector-ref workspace
							     symbol-num)
						 (list cur-rule))
					 (vector-set! workspace symbol-num '()))
				     (loop (+ symbol-num 1)))))))))))

	    ;; Add the goto actions
	    (let goto-loop ((symbol-num (- num-nonterminals 1)) (gotos '()))
	      (cond ((< symbol-num 0)
		     (vector-set! state-table state-num gotos))
		    ((null? (vector-ref workspace symbol-num))
		     (goto-loop (- symbol-num 1) gotos))
		    (else
		     (goto-loop
		      (- symbol-num 1)
		      (cons (list 'GOTO (vector-ref symbols symbol-num)
				  (state->symbol (car (vector-ref workspace
								  symbol-num))))
			    gotos)))))

	    ;; Add the default action, if any
	    (let ((default (vector-ref workspace num-symbols)))
	      (if (not (null? default))
		  (vector-set! state-table state-num
			       (cons (list 'REDUCE '() (rule->symbol default))
				     (vector-ref state-table state-num)))))

	    ;; Add the shift/reduce actions
	    (let action-loop ((symbol-num (- num-symbols 1)) (comment #f)
			      (result (vector-ref state-table state-num)))
	      (cond ((< symbol-num num-nonterminals)
		     (vector-set! state-table state-num result))
		    ((null? (vector-ref workspace symbol-num))
		     (action-loop (- symbol-num 1) #f result))
		    (else
		     (let* ((actions (vector-ref workspace symbol-num))
			    (action (car actions))
			    (lookahead (list (vector-ref symbols symbol-num)))
			    (encoding*
			     (cond ((< action 0)
				    (list 'REDUCE lookahead
					  (rule->symbol action)))
				   ((and (= state-num accept-state)
					 (member symbol-num end-of-parse))
				    (list 'ACCEPT lookahead))
				   (else
				    (list 'SHIFT lookahead
					  (state->symbol action)))))
			    (encoding (if comment
					  (list 'COMMENT encoding*)
					  encoding*)))
		       (vector-set! workspace symbol-num (cdr actions))
		       (action-loop symbol-num #t (cons encoding result))))))

	    ;; Add the list of items
	    (let item-loop ((items (compute-closure state lalr-record))
			    (result (vector-ref state-table state-num)))
	      (if (pair? items)
		  (let search-loop ((last-item (car items)))
		    (if (>= (vector-ref rule-items last-item) 0)
			(search-loop (+ last-item 1))
			(let cons-loop ((item (- last-item 1))
					(dot-item (car items)) (rule '()))
			  (cond ((and dot-item (< item dot-item))
				 (cons-loop item #f (cons "." rule)))
				((or (< item 0)
				     (< (vector-ref rule-items item) 0))
				 (item-loop (cdr items)
				  (cons (cons*
					 'COMMENT
					 (vector-ref symbols
					  (vector-ref rule-lhs
					   (- (vector-ref rule-items
							  last-item))))
					 "=>" rule) result)))
				(else
				 (cons-loop (- item 1) dot-item
					    (cons (vector-ref symbols
						   (vector-ref rule-items item))
						  rule)))))))
		  (vector-set! state-table state-num result)))

	    ;; Finish up this state and to to the next
	    (vector-set! state-table state-num
			 (cons* 'STATE (state->symbol state-num)
				(vector-ref state-table state-num)))
	    (state-loop (+ state-num 1)))))

    (set-lalr-constructor:state-table lalr-record state-table)))

;;; This is a helper function used by COMPUTE-STATE-TABLE.  It is used to
;;; resolve conflicts that are encountered when building the action table.  It
;;; takes two actions are returns the one that should be used.  In the special
;;; case of two actions with equal precedence and an associativity of 'non, it
;;; returns '() to indicate the action of Error.
;;;
;;; Arguments:
;;; - STATE = The state number.  It is used in warning messages.
;;; - TERMINAL = The terminal symbol on which the conflict is occuring.
;;; - CUR-ACTION = The current action in the action table for TERMINAL.  It will
;;;      be a list containting the possible actions that could happen on
;;;      TERMINAL.  In most cases, this will be '() and there is no conflict.
;;;      In other cases, the CAR of the list will contain the conflicting
;;;      action.  This will always be a negative number indicating a reduction.
;;;      Due to properties of the LR(0) parser, there will never be a
;;;      Shift/Shift conflict.
;;; - NEW-ACTION = This is the new candidate action
;;; - LALR-RECORD = The LALR-CONSTRUCTOR which contains the precedence maps
(define (resolve-conflict state-num terminal cur-actions new-action lalr-record)
  (cond ((null? cur-actions)
	 (list new-action))
	((and (< (car cur-actions) 0) (< new-action 0))
	 (let ((message (string-append "Reduce/Reduce conflict in state: s"
				       (number->string state-num)))
	       (comments (lalr-constructor:comments lalr-record)))
	   (display message) (newline)
	   (set-lalr-constructor:comments lalr-record (cons (list message)
							    comments))
	   (if (< (car cur-actions) new-action)
	       (cons new-action cur-actions)
	       (cons* (car cur-actions) new-action (cdr cur-actions)))))
	(else
	 (let* ((symbols (lalr-constructor:symbols lalr-record))
		(term-base (lalr-constructor:num-nonterminals lalr-record))
		(precedence-map (lalr-constructor:precedence-map lalr-record))
		(assoc-map (lalr-constructor:associativity-map lalr-record))
		(rule-precedence (lalr-constructor:rule-precedence lalr-record))
		(term-prec (vector-ref precedence-map (- terminal term-base)))
		(rule-prec (vector-ref rule-precedence (- (car cur-actions))))
		(associativity (vector-ref assoc-map term-prec))
		(comments (lalr-constructor:comments lalr-record)))
	   (cond ((> term-prec rule-prec)
		  (cons new-action (cdr cur-actions)))
		 ((< term-prec rule-prec)
		  cur-actions)
		 ;; At this point, we know the two are equal
		 ((= term-prec 0)
		  (let ((message (string-append
				  "Shift/Reduce conflict in state: s"
				  (number->string state-num))))
		    (display message) (newline)
		    (set-lalr-constructor:comments lalr-record
						   (cons (list message)
							 comments)))
		  (cons new-action cur-actions))
		 ((eq? associativity 'left)
		  cur-actions)
		 ((eq? associativity 'right)
		  (cons new-action (cdr cur-actions)))
		 (else ; non-associative
		  '()))))))

(define (state->symbol state-num)
  (string->symbol (string-append "s" (number->string state-num))))

(define (rule->symbol rule-num)
  (string->symbol (string-append "r" (number->string (- rule-num)))))

;;;-----------------------------------------------------------------------------
;;; This is the function that is called after all the major computation has been
;;; done.  It generates the "rest" of the PDA (after COMPUTE-STATE-TABLE did the
;;; hard part.
(define (construct-PDA lalr-record)
  (let* ((symbols (lalr-constructor:symbols lalr-record))
	 (num-symbols (vector-length symbols))
	 (num-nonterminals (lalr-constructor:num-nonterminals lalr-record))
	 (rule-lhs (lalr-constructor:rule-lhs lalr-record))
	 (rule-rhs (lalr-constructor:rule-rhs lalr-record))
	 (rule-items (lalr-constructor:rule-items lalr-record))
	 (rule-actions (lalr-constructor:rule-actions lalr-record))
	 (error-symbol (lalr-constructor:error-symbol lalr-record))
	 (no-shift (lalr-constructor:no-shift lalr-record))
	 (comments (lalr-constructor:comments lalr-record))

	 (tokens (let loop ((i (- num-symbols 1)) (result '()))
		   (if (< i num-nonterminals)
		       result
		       (let ((symbol (vector-ref symbols i)))
			 (if (eq? symbol error-symbol)
			     (loop (- i 1) result)
			     (loop (- i 1) (cons symbol result)))))))
	 (rules (make-vector (vector-length rule-rhs)))
	 (state-table (lalr-constructor:state-table lalr-record)))

    (let rule-loop ((rule-num 1))
      (if (< rule-num (vector-length rules))
	  (let item-loop ((item (vector-ref rule-rhs rule-num)) (count 0))
	    (if (< (vector-ref rule-items item) 0)
		(begin
		  (vector-set! rules rule-num
			       (list 'RULE (rule->symbol (- rule-num))
				     (vector-ref symbols (vector-ref rule-lhs
								     rule-num))
				     count (vector-ref rule-actions rule-num)))
		  (rule-loop (+ rule-num 1)))
		(item-loop (+ item 1) (+ count 1))))))

    (append (map (lambda (x) (cons 'COMMENT x)) comments)
	    (list (cons 'TOKENS tokens))
	    (if error-symbol (list (list 'ERROR error-symbol)) '())
	    (if (null? no-shift) '() (list (cons 'NO-SHIFT no-shift)))
	    (cdr (vector->list rules))
	    (vector->list state-table))))
