; The LALR(1) Table Constructor takes one of these records as input
(define-record cfg

  ; This is a list containing either symbols or 'cfg-precedence records.  These
  ; represent the terminal symbols in the grammar.  Precedence records appearing
  ; earlier in the list have higher precedence than those appearing later.
  ; Symbols represent terminal symbols with no defined precedence.
  terminals

  ; This is the symbol which will be returned by the lexer when the end of the
  ; input has been reached.  It must not appear in either 'terminals or 'rules.
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

  ; This is the symbol on the left side of the rule.
  left-side

  ; This is the list of symbols on the right side of the rule.
  right-side

  ; This is either a terminal symbol or #f.  If it is a terminal symbol, it is
  ; the terminal symbol to use as the precedence for this rule.  If it is #f,
  ; then the precedence for the rule will be the precedence of the last terminal
  ; symbol in the rule (if any).
  precedence

  ; This is the semantic action for the rule.  No operations are done on it and
  ; it will appear in the output unmodified.
  action)

;-------------------------------------------------------------------------------

; The LALR(1) table constructor returns this as its output
(define-record LR-program

  ; This is the list of terminal symbols.  Put another way, the elements in this
  ; list in addition to the symbol in 'eoi are the only symbols that are valid
  ; for the lexer to return.
  terminals

  ; This is the symbol that the lexer should return when it reaches the end of
  ; the input.
  eoi

  ; This is the symbol that is shifted when errors are encountered.  If defined,
  ; in will appear in 'rules and 'states but should never be returned by the
  ; lexer.  If not defined, this entry will be #f.
  error

  ; This is a vector of 'LR-rule records.  Reduction actions in 'states will
  ; refer to elements of this vector by their index.
  rules

  ; This is a vector of 'LR-state records.  Each element represents a state in
  ; PDA.  The start state is at index 0.
  states)

; The LR-program:rules field will contain a vector of these records.
(define-record LR-rule

  ; This is the symbol to shift when reducing by this rule.
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
  ; this particular state.  The items of the list will be one of:
  ;   (terminal 'shift state-num)
  ;   (terminal 'reduce rule-num)
  ;   (terminal 'accept)
  ; 'terminal is usually a symbol which triggers the action.  It may also be #t
  ; in which case it defines the "default" action.  The default action will be
  ; taken if no other rule matches.  The default action is always a reduce.
  ; If the action is 'shift, the 'state-num gives the index of the state to
  ; shift into.  If the action is 'reduce, 'rule-num gives the index of the rule
  ; to reduce by.  The 'accept action terminates the PDA.
  shift-reduce-table

  ; This is an association list which contains the gotos for this state.  The
  ; items of this list take the form of:
  ;   (non-terminal state-number)
  ; non-terminal is the non-terminal symbol on which this goto takes place.
  ; state-number is the state-number to shift into.
  goto-table

  ; This list encodes the items which make up this state.  This list is used
  ; primarily for debugging and is not required.  As such, this will either be
  ; a list of pairs or #f.
  ;
  ; If the list is present, then each pair represents an item.  The car of the
  ; pair is an index into 'LR-program:rules and the cdr is an index into
  ; 'LR-rule:right-side.  The "dot" comes before the corresponding right-side
  ; index.  Note that if the "dot" is at the end of the rule, the index will not
  ; be valid.
  items)
