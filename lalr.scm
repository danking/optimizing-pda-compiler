;; ---------------------------------------------------------------------- ;;
;; FICHIER               : lalr.scm                                       ;;
;; DATE DE CREATION      : Mon Jan 22 15:42:32 1996                       ;;
;; DERNIERE MODIFICATION : Mon Jun  3 10:24:43 1996                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.          ;;
;;   (for the Bison source code translated in Scheme)                     ;;
;; Copyright (C) 1996 Dominique Boucher                                   ;;
;;   (for the translation in Scheme)                                      ;;
;; ---------------------------------------------------------------------- ;;
;; An efficient Scheme LALR(1) Parser Generator  -- lalr.scm              ;;
;; ---------------------------------------------------------------------- ;;
;; This file contains yet another LALR(1) parser generator written in     ;;
;; Scheme. In contrast to other such parser generators, this one          ;;
;; implements a more efficient algorithm for computing the lookahead sets.;;
;; The algorithm is the same as used in Bison (GNU yacc) and is described ;;
;; in the following paper:                                                ;;
;;                                                                        ;;
;; "Efficient Computation of LALR(1) Look-Ahead Set", F. DeRemer and      ;;
;; T. Pennello, TOPLAS, vol. 4, no. 4, october 1982.                      ;;
;;                                                                        ;;
;; As a consequence, it is not written in a fully functional style.       ;;
;; The program has been successfully tested on several Scheme             ;;
;; interpreters and compilers, including scm4d3, Gambit v2.2, and         ;;
;; MIT-Scheme 7.2.0 (microcode 11.127, runtime 14.160).                   ;;
;; ---------------------------------------------------------------------- ;;
;; HOW TO USE THE PROGRAM                                                 ;;
;;                                                                        ;;
;; To generate a parser for a given grammar, the latter must be first     ;;
;; written down in scheme. The next section will describe the syntax      ;;
;; of the grammar. Now suppose your grammar is defined like this:         ;;
;;                                                                        ;;
;;    (define my-grammar { grammar })                                     ;;
;;                                                                        ;;
;; All you need to do is evaluate the expression:                         ;;
;;                                                                        ;;
;;    (gen-lalr1 my-grammar "file" [prefix])                              ;;
;;                                                                        ;;
;; where "file" is the name of the file (a string) that will contain the  ;;
;; tables for LR-parsing. The last argument must be supplied if you want  ;;
;; multiple parsers coexist in the same application. It must be a symbol, ;;
;; otherwise it will be ignored.                                          ;;
;;                                                                        ;;
;; To run the parser, you must first load the LR parsing driver(also part ;;
;; of this distribution):                                                 ;;
;;                                                                        ;;
;;      (load "lr-dvr.scm")                                               ;;
;;                                                                        ;;
;; The interface to the generated parser will be the function             ;;
;;                                                                        ;;
;;     ([prefix-]parse lexer errorp)                                      ;;
;;                                                                        ;;
;; where lexer is the name of the scanner feeding the parser with pairs   ;;
;; (token . lval) and errorp is the name of a user-defined error          ;;
;; function (the standard error function can be used as well).            ;;
;;                                                                        ;;
;;                                                                        ;;
;; Here are some notes about the lexer and the error function:            ;;
;;                                                                        ;;
;;   - the tokens (which are the first components of the pairs returned   ;;
;;     by the lexer) must agree with the tokens defined in the grammar.   ;;
;;                                                                        ;;
;;   - when the lexer wants to signal the end of the input, it must       ;;
;;     return the pair '(0) each time it's invoked.                       ;;
;;                                                                        ;;
;;   - the error function must accept two parameters (the standard error  ;;
;;     function accepts a variable number of parameters, so it accepts    ;;
;;     two).                                                              ;;
;;                                                                        ;;
;; ---------------------------------------------------------------------- ;;
;; THE GRAMMAR FORMAT                                                     ;;
;;                                                                        ;;
;; The grammar is specified by first giving the list of terminals and the ;;
;; list of non-terminal definitions. Each non-terminal definition         ;;
;; is a list where the first element is the non-terminal and the other    ;;
;; elements are the right-hand sides (lists of grammar symbols). In       ;;
;; addition to this, each rhs can be followed by a semantic action.       ;;
;; By convention, use strings for tokens and atoms for non-terminals.     ;;
;;                                                                        ;;
;; For example, consider the following (yacc) grammar:                    ;;
;;                                                                        ;;
;;   e : e '+' t                                                          ;;
;;     | t                                                                ;;
;;     ;                                                                  ;;
;;                                                                        ;;
;;   t : t '*' f                                                          ;;
;;     | f                                                                ;;
;;     ;                                                                  ;;
;;                                                                        ;;
;;   f : ID                                                               ;;
;;     ;                                                                  ;;
;;                                                                        ;;
;; The same grammar, written for the scheme parser generator, would look  ;;
;; like this (with semantic actions)                                      ;;
;;                                                                        ;;
;; (define my-grammar                                                     ;;
;;   '(                                                                   ;;
;;     ; Terminal symbols                                                 ;;
;;     ID ADD MULT                                                        ;;
;;     ; Productions                                                      ;;
;;     (e (e ADD t)  : (+ $1 $3)                                          ;;
;;        (t)        : $1                                                 ;;
;;        )                                                               ;;
;;     (t (t MULT f) : (* $1 $3)                                          ;;
;;        (f)        : $1                                                 ;;
;;        )                                                               ;;
;;     (f (ID)       : $1)                                                ;;
;;    ))                                                                  ;;
;;                                                                        ;;
;; In semantic actions, the symbol $<n> refers to the synthesized         ;;
;; attribute value of the nth symbol in the production. The value         ;;
;; associated with the non-terminal on the left is the result of          ;;
;; evaluating the semantic action (it defaults to #f).                    ;;
;;                                                                        ;;
;; If you evaluate                                                        ;;
;;                                                                        ;;
;;    (gen-lalr1 my-grammar "foo.scm" 'my)                                ;;
;;                                                                        ;;
;; then the generated parser will be named 'my-parser'.                   ;;
;;                                                                        ;;
;; NOTE ON CONFLICT RESOLUTION                                            ;;
;;                                                                        ;;
;; Conflicts in the grammar are handled in a conventional way.            ;;
;; Shift/Reduce conflicts are resolved by shifting, and Reduce/Reduce     ;;
;; conflicts are resolved by choosing the rule listed first in the        ;;
;; grammar definition.                                                    ;;
;;                                                                        ;;
;; You can print the states of the generated parser by evaluating         ;;
;; `(print-states)'. The format of the output is similar to the one       ;;
;; produced by bison when given the -v command-line option.               ;;
;; ---------------------------------------------------------------------- ;;
;; lalr.scm is free software; you can redistribute it and/or modify       ;;
;; it under the terms of the GNU General Public License as published by   ;;
;; the Free Software Foundation; either version 2, or (at your option)    ;;
;; any later version.                                                     ;;
;;                                                                        ;;
;; lalr.scm is distributed in the hope that it will be useful,            ;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of         ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          ;;
;; GNU General Public License for more details.                           ;;
;;                                                                        ;;
;; You should have received a copy of the GNU General Public License      ;;
;; along with lalr.scm; see the file COPYING.  If not, write to           ;;
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  ;;
;;                                                                        ;;
;; Dominique Boucher -- Universite de Montreal                            ;;
;;                                                                        ;;
;; Send questions, comments or suggestions to boucherd@iro.umontreal.ca   ;;
;; ---------------------------------------------------------------------- ;;

(define-syntax def-macro 
  (syntax-rules ()
    ((def-macro (name form ...) body ...)
     (define-syntax name 
       (syntax-rules ()
	 ((name form ...) (let () body ...)))))))

(def-macro (BITS-PER-WORD) 28)
(def-macro (logical-or x  y ...) (bitwise-ior x y ...))

;; ILYA: This function toggles the bth bit of first 
;; element of vector v.
(def-macro (set-bit v b)
  (let ((x (quotient b (BITS-PER-WORD)))
	(y (expt 2 (remainder b (BITS-PER-WORD)))))
     (vector-set! v x (logical-or (vector-ref v x) y))))

;; ILYA: This function sets the nth element of vector v1
;; to the union of nth element of vector v1 & nth element 
;; of vector v2.
(def-macro (bit-union v1 v2 n)
  (do ((i 0 (+ i 1)))
       ((= i n))
     (vector-set! v1 i (logical-or (vector-ref v1 i) 
				    (vector-ref v2 i)))))

;; - Macro pour les structures de donnees

;; ILYA: Macro's that do stuff

; This "record" represents a state in the parser.  It contains 4 fields:
; - number = A unique number assigned to each record.
; - acc-sym = The symbol that was shifted to cause a tranfer to this state. <--- ???
; - nitems = (length items)
; - items = The list of items in this state.  Each item is represented by an
;           index into 'ritems.  The "dot" is considered to be before the each
;           index in its corresponding rule.
(def-macro (new-core)              (make-vector 4 0))
(def-macro (set-core-number! c n)  (vector-set! c 0 n))
(def-macro (set-core-acc-sym! c s) (vector-set! c 1 s))
(def-macro (set-core-nitems! c n)  (vector-set! c 2 n))
(def-macro (set-core-items! c i)   (vector-set! c 3 i))
(def-macro (core-number c)         (vector-ref c 0))
(def-macro (core-acc-sym c)        (vector-ref c 1))
(def-macro (core-nitems c)         (vector-ref c 2))
(def-macro (core-items c)          (vector-ref c 3))

; This "record" represents the shifts that can take place in the paser.  It has
; 3 fields:
; - number = This is the same number as the "number" field in its corresponding
;            "core" record.
; - nshifts = (length shifts)
; - shifts = A list of "core numbers" representing states that can be reached
;            from "this" state by shifting some symbol.  These will be ordered
;            in descending order according to the corresponding "core" record's
;            "acc-sym" (i.e. non-terminal shifts, "goto"s, will come last).
(def-macro (new-shift)              (make-vector 3 0))
(def-macro (set-shift-number! c x)  (vector-set! c 0 x))
(def-macro (set-shift-nshifts! c x) (vector-set! c 1 x))
(def-macro (set-shift-shifts! c x)  (vector-set! c 2 x))
(def-macro (shift-number s)         (vector-ref s 0))
(def-macro (shift-nshifts s)        (vector-ref s 1))
(def-macro (shift-shifts s)         (vector-ref s 2))

; This "record" represents the reductions that can take place in a given state
; in the LR(0) parser.  It contains 3 fields:
; - number = This is the same number as its "number" field in its corresponding
;            "core".
; - nreds = (length rules)
; - rules = A list of rules (indexes into 'rrhs & 'rlhs) that can be reduced
(def-macro (new-red)                (make-vector 3 0))
(def-macro (set-red-number! c x)    (vector-set! c 0 x))
(def-macro (set-red-nreds! c x)     (vector-set! c 1 x))
(def-macro (set-red-rules! c x)     (vector-set! c 2 x))
(def-macro (red-number c)           (vector-ref c 0))
(def-macro (red-nreds c)            (vector-ref c 1))
(def-macro (red-rules c)            (vector-ref c 2))

;; ILYA: make a vector of nelem elements and initialize all 
;; elements to 0
(def-macro (new-set nelem)
  (make-vector nelem 0))

(define-record prod-action
  production
  action
  prec)

;; - Constantes
(define STATE-TABLE-SIZE 1009)

; This is the name of the start symbol used internally by the program.  This
; should not appear in any grammar
(define start '*start)

; This is the name of the terminal symbol that the lexer sends which it reaches
; this end of the input stream.
(define eoi '*EOI*)

;; - Tableaux 
;; ILYA: for each global variable I provide a set of functions where
;; it's being referenced (human-driven dataflow analysis)

; This is a vector of length 'nrules.  Each spot (except 0) corresponds to a
; rule and contains the index into 'ritem where the items for the right-side
; of this rule start.
(define rrhs         #f) ;; initialize-all, pack-grammar, set-firsts, closure, 
                         ;; build-relations, print-item, calculate-precedence

; This is a vector of length 'nrules.  Each spot (except 0) corresponds to a
; rule and contains the non-terminal on the left-side of the rule.
(define rlhs         #f) ;; initialize-all, pack-grammar, set-derives,
                         ;; set-nullable, print-item

; This is a vector of length 'nitems + 1.  It contains of the items on the
; right-side of all of rules.  'rrhs contains the index of where to start for a
; particular rule.  The end of a rule is marked by a negative number.  The
; negative number is the index into 'rrhs and 'rlhs for the rule in question
; times -1.
(define ritem        #f) ;; initialize-all, pack-grammar, set-nullable, 
                         ;; set-firsts, closure, new-itemsets, 
                         ;; save-reductions, set-max-rhs, build-relations, 
                         ;; print-item, calculate-precedence

; Each element in this vector corresponds to an element in 'the-nonterminals.
; The values are either #t or #f depending on whether the given non-terminal
; symbol can derive the empty string or not.
(define nullable     #f) ;; initialize-all, set-nullable, initialize-F, 
                         ;; build-relations

; The is a vector of lists.  Each element corresponds to an element in
; 'the-nonterminals.  The values are lists of indexes into 'rlhs & 'rrhs.  This
; means that the non-terminal in question directly "derives" the corresponding
; right-hand side.
(define derives      #f) ;; initialize-all, set-derives, set-firsts, 
	                 ;; set-fderives, build-relations

; This is similar to 'derives except that the lists in this vector contain all
; possible rules that can be derived from the corresponding non-terminal no matter
; how many intermediate derivations are in-between.  For example, the start
; symbol's list will contain every rule.  See 'set-fderives for more info. <-- ???
(define fderives     #f) ;; initialize-all, set-fderives, closure

; This is a vector of size 'nvars with each element corresponding to an element
; in 'the-nonterminals.  The values are sets of non-terminals where each
; non-terminal in the set is capable of appearing at the begining of a
; derivation from the non-terminal in question. <--------------------------- ???
(define firsts       #f) ;; initialize-all, set-firsts, set-fderives

; This is a vector of size 'nsyms.  It is used in constructing the LR(0) parser.
; If a shift is possible from the current state by some symbol, then the element
; in this vector corresponding to that symbol will contain the list of "kernel"
; items that are in the next state.  If a shift by a symbol is undefined in the
; current state, then so is the value in this vector (i.e. it could be
; anything).  This list of symbols which are defined is stored in 'shift-symbol.
(define kernel-base  #f) ;; initialize-all, allocate-item-sets, new-itemsets, 
                         ;; get-state, new-state

; This vector corresponds to 'kernel-base.  For elements of 'kernel-base which
; are defined, the corresponding element of this vector will point to the last
; element of the list in 'kernel-base.
(define kernel-end   #f) ;; initialize-all, allocate-item-sets, new-itemsets

; This is a list of symbols that is used while constructing the LR(0) parser.
; The symbols in this list are valid terminals and non-terminals to look for in
; the current state.  The state to shift into when the given symbol is seen is
; found by consulting 'kernel-base.
(define shift-symbol #f) ;; initialize-all, new-itemsets, append-states

; This is a list of "core" records that is used while constructing the LR(0)
; parser.  These core records represent the states that can be gotten to from
; the current state by shifting some symbol.
(define shift-set    #f) ;; initialize-all, append-states, save-shifts

; A vector of size 'STATE-TABLE-SIZE.  This functions as a hash-table to lookup
; a "core" record given a list of items.  The hashing function is the sumation
; of the list (This assumes that the list consits of indexes into the 'ritems
; vector).
(define state-table  #f) ;; initialize-all, get-state

; This is a vector of size 'nstates.  It maps a "core-number" to the
; "core-acc" (or accessing symbol) for each core.
(define acces-symbol #f) ;; initialize-all, set-accessing-symbol, 
                         ;; initialize-LA, set-goto-map, initialize-F, 
                         ;; build-relations

; This is a vector of size 'nstates.  It maps a given "red number" to the "red"
; record for that number.  If there is no "red" record for a particular state,
; then the value #f is mapped instead.
(define reduction-table #f) ;; initialize-all, set-reduction-table, 
                            ;; initialize-LA, build-tables, 
                            ;; compact-action-table

; This is just like 'reduction-table except it deals with "shift" records.
(define shift-table  #f) ;; initialize-all, set-shift-table, initialize-LA, 
                         ;; initialize-F, build-relations, build-tables

; This is a vector of size 'nstates.  It maps a given state number to either #t
; or #f.  A state is considered inconsistent if a reduction by more than one
; rule my occur in the state or if a there is a reduction and no no non-terminal
; shifts (gotos).
(define consistent   #f) ;; initialize-all, initialize-LA, build-relations, 
	                 ;; build-tables

; This is a vector of size 'nstates + 1.  It maps a given state number to a
; non-negative integer.  This integer is the sum of the number of reductions
; that take place in all the inconsistent states (see 'consistent) numbered less
; than the one in question.  Thus, the first element is always 0.  The last
; element, which does not correspond to a state, contains the sum for the whole
; grammar.
(define lookaheads   #f) ;; initialize-all, initialize-LA, add-lookahead-edge, 
                         ;; compute-lookaheads, build-tables

; This is a vector the is the same length as the value of the last element of
; 'lookaheads (but at least 1).  Each element is a bit-set.
(define LA           #f) ;; initialize-all, initialize-LA, compute-lookaheads, 
	                 ;; build-tables

; This is a vector that is the same size as 'LA.  In contains, in order, the
; rule numbers (indexes in 'rlhs & 'rrhs) of the rules involved in the
; reductions in inconsistent (see 'consistent) states.
(define LAruleno     #f) ;; initialize-all, initialize-LA. add-lookahead-edge, 
	                 ;; build-tables

; This is a vector that is the same size as 'LA.
(define lookback     #f) ;; initialize-all, initialize-LA, add-lookback-edge, 
	                 ;; compute-lookaheads, 

; This is a vector of size 'nvars + 1.  Each element corresponds to a
; non-terminal and the value is the sum of the number of shifts that each
; non-terminal before it was involved in.  The last element is the total
; number of non-terminal shifts in the LR(0) parser (the same value as in
; 'ngotos).
;
; I believe that these numbers function as starting indexes for looking into
; 'from-state & 'to-state when analyzing goto transisions.
(define goto-map     #f) ;; initialize-all, set-goto-map, map-goto

; This is a vector of size 'ngotos.  Each element corresponds to a particular
; "goto" transision.  The elements are state numbers.  This vector gives the
; starting state for the corresponding "goto" transision.
(define from-state   #f) ;; initialize-all, set-goto-map, map-goto, 
                         ;; build-relations

; This vector is just like 'from-state except that it gives the destination
; state.
(define to-state     #f) ;; initialize-all, set-goto-map, initialize-F, 
                         ;; build-relations

; This is a vector of size 'ngotos.
(define includes     #f) ;; initialize-all, build-relations, digraph,

; This is a vector of size 'ngotos.  Each element is a bit-set.
(define F            #f) ;; initialize-all, initialize-F, compute-lookaheads, 
                         ;; traverse
(define action-table #f) ;; initialize-all, build-tables,
                         ;; compact-action-table, print-states 

;; - Variables
(define nitems          #f) ;The number of items (see pack-grammar)
(define nrules          #f) ;The number of rules + 1
(define nvars           #f) ;The number of non-terminal symbols
(define nterms          #f) ;The number of terminal symbols
(define nsyms           #f) ;'nterms + 'nvars
(define nstates         #f) ;The number of states in the parser.  This next (unique) "core" number is the current value of this number.
(define first-state     #f) ;A list of "core"s that represent the states in the LR(0) parser.
(define last-state      #f) ;A reference to the last element of 'first-state
(define final-state     #f) ;The number of the "core" that is the acceptance state
(define first-shift     #f) ;The list of "shift" records.
(define last-shift      #f) ;A reference to the last element of the "shift" list.
(define first-reduction #f) ;A list of "red"s that represent reductions in the LR(0) parser.
(define last-reduction  #f) ;A reference to the last element of 'last-reduction
(define nshifts         #f) ;A variable that is used in constructing the LR(0) parser.  It represents the number of shifts possible in the current state.
(define maxrhs          #f) ;This is the number of symbols on the ride-hand-side of the longest rule in the grammar.
(define ngotos          #f) ;The total number of "goto" transisions in the grammar.
(define token-set-size  #f) ;Set to (+ 1 (quotient nterms (BITS-PER-WORD)))
(define grammar #f)
(define global-terms #f) ;The list of terminal symbols

; This is the start function.  'rewrite-grammar is where the real work starts.
; TODO: List of the arguments to this function
; TODO: Think of a beter name for this function.
(define (gen-lalr1 gram output-file . opt)
  (initialize-all)
  (rewrite-grammar gram)
  (output-to-file output-file opt) )  ;; see utils-io.scm

; Initializes all the global variables to their starting values
(define (initialize-all)
  (set! rrhs         #f)
  (set! rlhs         #f)
  (set! ritem        #f)
  (set! nullable     #f)
  (set! derives      #f)
  (set! fderives     #f)
  (set! firsts       #f)
  (set! kernel-base  #f)
  (set! kernel-end   #f)
  (set! shift-symbol #f)
  (set! shift-set    #f)
  (set! state-table  (make-vector STATE-TABLE-SIZE '()))
  (set! acces-symbol #f)
  (set! reduction-table #f)
  (set! shift-table  #f)
  (set! consistent   #f)
  (set! lookaheads   #f)
  (set! LA           #f)
  (set! LAruleno     #f)
  (set! lookback     #f)
  (set! goto-map     #f)
  (set! from-state   #f)
  (set! to-state     #f)
  (set! includes     #f)
  (set! F            #f)
  (set! action-table #f)
  (set! nstates         #f)
  (set! first-state     #f)
  (set! last-state      #f)
  (set! final-state     #f)
  (set! first-shift     #f)
  (set! last-shift      #f)
  (set! first-reduction #f)
  (set! last-reduction  #f)
  (set! nshifts         #f)
  (set! maxrhs          #f)
  (set! ngotos          #f)
  (set! token-set-size  #f))

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

; The purpose of this function is to compute the values for 'nrules, 'nitems,
; 'rlhs, 'rrhs, and 'ritem.  See the descriptions of those variables for more
; information.
;
; Arguments:
; - no-of-rules = The number of rules in the grammar (each non-terminal has at
;                 least one rule)
; - no-of-items = The number of symbols in the grammar.  Duplicates count as
;                 many times as they appear.  The grammar below has 10 items.
; - gram = The grammar in the format of
;          '( (S (A)
;                (S A))
;             (A ()
;                (A x y)) )
; Returns:
;   nothing (it produces side effects)
(define (pack-grammar no-of-rules no-of-items gram)
  (set! nrules (+  no-of-rules 1))
  (set! nitems no-of-items)
  (set! rlhs (make-vector nrules #f))
  (set! rrhs (make-vector nrules #f))
  (set! ritem (make-vector (+ 1 nitems) #f))

  (let loop ((p gram) (item-no 0) (rule-no 1))
    (if (not (null? p))
	(let ((nt (caar p)))
	  (let loop2 ((prods (cdar p)) (it-no2 item-no) (rl-no2 rule-no))
	    (if (null? prods)
		(loop (cdr p) it-no2 rl-no2)
		(begin
		  (vector-set! rlhs rl-no2 nt)
		  (vector-set! rrhs rl-no2 it-no2)
		  (let loop3 ((rhs (car prods)) (it-no3 it-no2))
		    (if (null? rhs)
			(begin
			  (vector-set! ritem it-no3 (- rl-no2))
			  (loop2 (cdr prods) (+ it-no3 1) (+ rl-no2 1)))
			(begin
			  (vector-set! ritem it-no3 (car rhs))
			  (loop3 (cdr rhs) (+ it-no3 1))))))))))) )

; This function computes the value of 'derives.
(define (set-derives)
  (let ( (delts (make-vector (+ nrules 1) 0))
	 (dset  (make-vector nvars -1)) )
    (let loop ((i 1) (j 0))		; i = 0
      (if (< i nrules)
	  (let ((lhs (vector-ref rlhs i)))
	    (if (>= lhs 0)
		(begin
		  (vector-set! delts j (cons i (vector-ref dset lhs)))
		  (vector-set! dset lhs j)
		  (loop (+ i 1) (+ j 1)))
		(loop (+ i 1) j)))))   ;<----------------------------------- ???
    (set! derives (make-vector nvars 0))
    (let loop ((i 0))
      (if (< i nvars)
	  (let ((q (let loop2 ((j (vector-ref dset i)) (s '()))
		     (if (< j 0)
			 s
			 (let ((x (vector-ref delts j)))
			   (loop2 (cdr x) (cons (car x) s)))))))
	    (vector-set! derives i q)
	    (loop (+ i 1)))))) )

; This function computes the value of 'nullable
(define (set-nullable)
  (set! nullable (make-vector nvars #f))
  (let ((squeue (make-vector nvars #f))
	(rcount (make-vector (+ nrules 1) 0))
	(rsets  (make-vector nvars #f))
	(relts  (make-vector (+ nitems nvars 1) #f)))
    (let loop ((r 0) (s2 0) (p 0))
      (let ((*r (vector-ref ritem r)))
	(if *r
	    (if (< *r 0)
		(let ((symbol (vector-ref rlhs (- *r))))
		  (if (and (>= symbol 0)
			   (not (vector-ref nullable symbol)))
		      (begin
			(vector-set! nullable symbol #t)
			(vector-set! squeue s2 symbol)
			(loop (+ r 1) (+ s2 1) p))))
		(let loop2 ((r1 r) (any-tokens #f))
		  (let* ((symbol (vector-ref ritem r1)))
		    (if (> symbol 0)
			(loop2 (+ r1 1) (or any-tokens (>= symbol nvars)))
			(if (not any-tokens)
			    (let ((ruleno (- symbol)))
			      (let loop3 ((r2 r) (p2 p))
				(let ((symbol (vector-ref ritem r2)))
				  (if (> symbol 0)
				      (begin
					(vector-set! rcount ruleno
						     (+ (vector-ref rcount ruleno) 1))
					(vector-set! relts p2
						     (cons (vector-ref rsets symbol)
							   ruleno))
					(vector-set! rsets symbol p2)
					(loop3 (+ r2 1) (+ p2 1)))
				      (loop (+ r2 1) s2 p2)))))
			    (loop (+ r1 1) s2 p))))))
	    (let loop ((s1 0) (s3 s2))
	      (if (< s1 s3)
		  (let loop2 ((p (vector-ref rsets (vector-ref squeue s1))) (s4 s3))
		    (if p 
			(let* ((x (vector-ref relts p))
			       (ruleno (cdr x))
			       (y (- (vector-ref rcount ruleno) 1)))
			  (vector-set! rcount ruleno y)
			  (if (= y 0)
			      (let ((symbol (vector-ref rlhs ruleno)))
				(if (and (>= symbol 0)
					 (not (vector-ref nullable symbol)))
				    (begin
				      (vector-set! nullable symbol #t)
				      (vector-set! squeue s4 symbol)
				      (loop2 (car x) (+ s4 1)))
				    (loop2 (car x) s4)))
			      (loop2 (car x) s4))))
		    (loop (+ s1 1) s4)))))))))
		  
; Computes the value of 'firsts.
(define (set-firsts)
  (set! firsts (make-vector nvars '()))

  ; For each non-terminal, and for each rule, if the rule begins with a
  ; non-terminal, then that non-terminal is in the first set for the left-hand
  ; side non-terminal.
  (let loop ((i 0))
    (if (< i nvars)
	(let loop2 ((sp (vector-ref derives i)))
	  (if (null? sp)
	      (loop (+ i 1))
	      (let ((sym (vector-ref ritem (vector-ref rrhs (car sp)))))
		(if (< -1 sym nvars)
		    (vector-set! firsts i (sinsert sym (vector-ref firsts i))))
		(loop2 (cdr sp)))))))

  ; If non-terminal A is in the first set for non-terminal B, then the first set
  ; for non-terminal B includes the first set for non-terminal A.
  ; (transitive closure)
  (let loop ((continue #t))
    (if continue
	(let loop2 ((i 0) (cont #f))
	  (if (>= i nvars)
	      (loop cont)
	      (let* ((x (vector-ref firsts i))
		     (y (let loop3 ((l x) (z x))
			  (if (null? l)
			      z
			      (loop3 (cdr l)
				     (sunion (vector-ref firsts (car l)) z))))))
		(if (equal? x y)
		    (loop2 (+ i 1) cont)
		    (begin
		      (vector-set! firsts i y)
		      (loop2 (+ i 1) #t))))))))

  ; The first set for a given non-terminal includes itself.  (reflexive closure)
  (let loop ((i 0))
    (if (< i nvars)
	(begin
	  (vector-set! firsts i (sinsert i (vector-ref firsts i)))
	  (loop (+ i 1))))))

; Fonction set-fderives qui calcule un tableau de taille
; nvars et qui donne, pour chaque non-terminal, une liste des regles pouvant
; etre derivees a partir de ce non-terminal. (se sert de firsts)
;
; Google Translation:
; Function set-fderives which calculates a table of size
; nvars and which gives, for each not-terminal, a list of the rules being able
; to be derivees from this not-terminal.  (is useful itself of firsts)

; This function calculates the value of 'fderives.
; The value of each element is the union of all the 'derives sets for every
; non-terminal in the corresponding element of 'firsts.
(define (set-fderives)
  (set! fderives (make-vector nvars #f))

  (set-firsts)
  (let loop ((i 0))
    (if (< i nvars)
	(let ((x (let loop2 ((l (vector-ref firsts i)) (fd '()))
		   (if (null? l)
		       fd
		       (loop2 (cdr l)
			      (sunion (vector-ref derives (car l)) fd))))))
	  (vector-set! fderives i x)
	  (loop (+ i 1))))))

; Fonction calculant la fermeture d'un ensemble d'items LR0
; ou core est une liste d'items

;
; Arguments:
; - core = The result of calling 'core-items on a core.  This is a list of
;          "kernel" items (see the Dragon Book) that compose an individual state
;          in the parser.
; Returns (I think)
;   A complete list of items that are in this state.  Items are represented by
; indexes into 'ritem.  The dot is considered to be before the index given.
(define (closure core)
  ;; Initialization
  (let ( (ruleset (make-vector nrules #f)) )
    (let loop ((csp core))
      (if (not (null? csp))
	  (let ((sym (vector-ref ritem (car csp))))
	    (if (< -1 sym nvars)
		(let loop2 ((dsp (vector-ref fderives sym)))
		  (if (not (null? dsp))
		      (begin
			(vector-set! ruleset (car dsp) #t)
			(loop2 (cdr dsp))))))
	    (loop (cdr csp)))))

    (let loop ((ruleno 1) (csp core) (itemsetv '())) ; ruleno = 0
      (if (< ruleno nrules)
	  (if (vector-ref ruleset ruleno)
	      (let ((itemno (vector-ref rrhs ruleno)))
		(let loop2 ((c csp) (itemsetv2 itemsetv))
		  (if (and (pair? c)
			   (< (car c) itemno)) ;<--------------------------- ???
		      (loop2 (cdr c) (cons (car c) itemsetv2))
		      (loop (+ ruleno 1) c (cons itemno itemsetv2)))))
	      (loop (+ ruleno 1) csp itemsetv))
	  (let loop2 ((c csp) (itemsetv2 itemsetv))
	    (if (pair? c)
		(loop2 (cdr c) (cons (car c) itemsetv2))
		(reverse itemsetv2)))))) )

; This function is called by 'generate-states.
(define (allocate-storage)
  (set! kernel-base (make-vector nsyms 0))
  (set! kernel-end  (make-vector nsyms #f)))

; This function is called by 'generate-states to do some initial setup.
(define (initialize-states)
  (let ((p (new-core)))
    (set-core-number! p 0)
    (set-core-acc-sym! p #f)
    (set-core-nitems! p 1)
    (set-core-items! p '(0))

    (set! first-state (list p))
    (set! last-state first-state)
    (set! nstates 1)))

; This function constructs the LR(0) parser.
(define (generate-states)
  (allocate-storage)
  (set-fderives)
  (initialize-states)
  (let loop ((this-state first-state))
    (if (pair? this-state)
	(let* ((x (car this-state))
	       (is (closure (core-items x))))

	  (save-reductions x is)
	  (new-itemsets is)
	  (append-states)
	  (if (> nshifts 0)
	      (save-shifts x))
	  (loop (cdr this-state))))))

; This function is roughly the "goto" operation from the Dragon Book's
; description of how to construct an LR(0) parser.  It takes a list of items as
; input and generates new states based on those items.
;
; Arguments
; - itemset = The list of items that represent a given state in the LR(0)
;             parser.  The elements are indexes into 'ritem with the "dot"
;             occuring right before the index in question.
; Returns:
; Nothing directly.  It has the following side-effects:
; - Sets 'shift-symbol to be the list of symbols that will cause a shift in the
;   given state.
; - Sets 'nshifts to be (length shift-symbol)
; - Modifies 'kernel-base so that the indexes corresponding to the elements of
;   'shift-symbol contain lists of indexes into 'ritem.  These lists represent
;   the "kernel" (see the Dragon Book) items of the state gotten to be shifting
;   the given symbol in this state.  The elements of 'kernel-base that do not
;   correspond to items in 'shift-symbol are modified.
; - Modifies 'kernel-end to continue its relationship with 'kernel-base.
(define (new-itemsets itemset)
  ;; - Initialization
  (set! shift-symbol '())
  (let loop ((i 0))
    (if (< i nsyms)
	(begin
	  (vector-set! kernel-end i '())
	  (loop (+ i 1)))))

  (let loop ((isp itemset))
    (if (pair? isp)
	(let* ((i (car isp))
	       (sym (vector-ref ritem i)))
	  (if (>= sym 0)
	      (begin
		(set! shift-symbol (sinsert sym shift-symbol))
		(let ((x (vector-ref kernel-end sym)))
		  (if (null? x)
		      (begin
			(vector-set! kernel-base sym (cons (+ i 1) x))
			(vector-set! kernel-end sym (vector-ref kernel-base sym)))
		      (begin
			(set-cdr! x (list (+ i 1)))
			(vector-set! kernel-end sym (cdr x)))))))
	  (loop (cdr isp)))))

  (set! nshifts (length shift-symbol)))

; This function takes a list of items and returns the number of the
; corresponding "core".  If the "core" that corresponds to the list does not
; exist, then it is created.
;
; Arguments:
; - sym = An index into 'kernel-base.  (vector-ref kernel-base sym) is the list
;         of items.
; Returns:
;   The "core number" corresponding to the given state.
(define (get-state sym)
  (let* ((isp  (vector-ref kernel-base sym))
	 (n    (length isp))
	 (key  (let loop ((isp1 isp) (k 0))
		 (if (null? isp1)
		     (modulo k STATE-TABLE-SIZE)
		     (loop (cdr isp1) (+ k (car isp1))))))
	 (sp   (vector-ref state-table key)))
    (if (null? sp)
	(let ((x (new-state sym)))
	  (vector-set! state-table key (list x))
	  (core-number x))
	(let loop ((sp1 sp))
	  (if (and (= n (core-nitems (car sp1)))
		   (let loop2 ((i1 isp) (t (core-items (car sp1)))) 
		     (if (and (pair? i1) 
			      (= (car i1)
				 (car t)))
			 (loop2 (cdr i1) (cdr t))
			 (null? i1))))
	      (core-number (car sp1))
	      (if (null? (cdr sp1))
		  (let ((x (new-state sym)))
		    (set-cdr! sp1 (list x))
		    (core-number x))
		  (loop (cdr sp1))))))))

; This function creates a new "core" symbol (and thus a new state) to represent
; the given list of items.
;
; Arguments:
; - sym = An index into 'kernel-base.  The list of items is taken from there.
; Returns:
;   The new "core" record.  It also modifies the values of 'last-state (and thus
; 'first-state), 'nstates, and possibly 'final-state.
(define (new-state sym)
  (let* ((isp  (vector-ref kernel-base sym))
	 (n    (length isp))
	 (p    (new-core)))
    (set-core-number! p nstates)
    (set-core-acc-sym! p sym)
    (if (= sym nvars) (set! final-state nstates))
    (set-core-nitems! p n)
    (set-core-items! p isp)
    (set-cdr! last-state (list p))
    (set! last-state (cdr last-state))
    (set! nstates (+ nstates 1))
    p))

; Uses the list in 'shift-symbol to construct a list in 'shift-set that contains
; the new "core" records (and thus states) that were represented by
; 'shift-symbol.
(define (append-states)
  (set! shift-set
	(let loop ((l (reverse shift-symbol)))
	  (if (null? l)
	      '()
	      (cons (get-state (car l)) (loop (cdr l)))))))

; This function takes a "core" record as an argument and creates a new "shift"
; record for it.  This function uses the values of 'nshifts and 'shift-set and
; it assumes that 'nshifts is greater than zero.  It adds the newly created
; "shift" record to the shift list ('first-shift & 'last-shift).
(define (save-shifts core)
  (let ((p (new-shift)))
	(set-shift-number! p (core-number core))
	(set-shift-nshifts! p nshifts)
	(set-shift-shifts! p shift-set)
	(if last-shift
	    (begin
	      (set-cdr! last-shift (list p))
	      (set! last-shift (cdr last-shift)))
	    (begin
	      (set! first-shift (list p))
	      (set! last-shift first-shift)))))

; This function takes a state and create a new "red" record for it if needed.
;
; Arguments
; - core = The "core" record for the state in question.
; - itemset = The (complete) list of items in the state in question.  This is
;             needed because some "nonkernel items" (see the Dragon Book) might
;             make reductions on the empty string possible.
; Returns:
; Nothing - It adds the new record to the the reduction-list ('first-reduction &
; 'last-reduction).
(define (save-reductions core itemset)
  (let ((rs (let loop ((l itemset))
	      (if (null? l)
		  '()
		  (let ((item (vector-ref ritem (car l))))
		    (if (< item 0)
			(cons (- item) (loop (cdr l)))
			(loop (cdr l))))))))
    (if (pair? rs)
	(let ((p (new-red)))
	  (set-red-number! p (core-number core))
	  (set-red-nreds!  p (length rs))
	  (set-red-rules!  p rs)
	  (if last-reduction
	      (begin
		(set-cdr! last-reduction (list p))
		(set! last-reduction (cdr last-reduction)))
	      (begin
		(set! first-reduction (list p))
		(set! last-reduction first-reduction)))))))


;; --

; This function computes the LALR Look-Ahead Sets as described in the DeRemer
; and Pennello paper.
(define (lalr)
  (set! token-set-size (+ 1 (quotient nterms (BITS-PER-WORD))))
  (set-accessing-symbol)
  (set-shift-table)
  (set-reduction-table)
  (set-max-rhs)
  (initialize-LA)
  (set-goto-map)
  (initialize-F)
  (build-relations)
  (digraph includes)
  (compute-lookaheads))

; This function computes the value of 'acces-symbol
(define (set-accessing-symbol)
  (set! acces-symbol (make-vector nstates #f))
  (let loop ((l first-state))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! acces-symbol (core-number x) (core-acc-sym x))
	  (loop (cdr l))))))

; This function computes the value of 'shift-table.
(define (set-shift-table)
  (set! shift-table (make-vector nstates #f))
  (let loop ((l first-shift))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! shift-table (shift-number x) x)
	  (loop (cdr l))))))

; This function computes the value of 'reduction-table.
(define (set-reduction-table)
  (set! reduction-table (make-vector nstates #f))
  (let loop ((l first-reduction))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! reduction-table (red-number x) x)
	  (loop (cdr l))))))

; This function computes the value of 'maxrhs.
(define (set-max-rhs)
  (let loop ((p 0) (curmax 0) (length 0))
    (let ((x (vector-ref ritem p)))
      (if x
	  (if (>= x 0)
	      (loop (+ p 1) curmax (+ length 1))
	      (loop (+ p 1) (max curmax length) 0))
	  (set! maxrhs curmax)))))

; This function computes the values of 'consistent, 'lookaheads, and 'LAruleno.
; It also allocates the initial values of 'LA and 'lookback.
(define (initialize-LA)
  (set! consistent (make-vector nstates #f))
  (set! lookaheads (make-vector (+ nstates 1) #f))

  (let loop ((count 0) (i 0))
    (if (< i nstates)
	(begin
	  (vector-set! lookaheads i count)
	  (let ((rp (vector-ref reduction-table i))
		(sp (vector-ref shift-table i)))
	    (if (and rp
		     (or (> (red-nreds rp) 1)
			 (and sp
			      (not
			       (< (vector-ref acces-symbol
					      (last (shift-shifts sp)))
				  nvars)))))
		(loop (+ count (red-nreds rp)) (+ i 1))
		(begin
		  (vector-set! consistent i #t)
		  (loop count (+ i 1))))))

	(begin
	  (vector-set! lookaheads nstates count)
	  (let ((c (max count 1)))
	    (set! LA (make-vector c #f))
	    (do ((j 0 (+ j 1))) ((= j c)) (vector-set! LA j (new-set token-set-size)))
	    (set! LAruleno (make-vector c -1))
	    (set! lookback (make-vector c #f)))
	  (let loop ((i 0) (np 0))
	    (if (< i nstates)
		(if (vector-ref consistent i)
		    (loop (+ i 1) np)
		    (let ((rp (vector-ref reduction-table i)))
		      (if rp
			  (let loop2 ((j (red-rules rp)) (np2 np))
			    (if (null? j)
				(loop (+ i 1) np2)
				(begin
				  (vector-set! LAruleno np2 (car j))
				  (loop2 (cdr j) (+ np2 1)))))
			  (loop (+ i 1) np))))))))))

; This function constructs the value of 'goto-map, 'from-state, and 'to-state.
(define (set-goto-map)
  (set! goto-map (make-vector (+ nvars 1) 0))
  (let ((temp-map (make-vector (+ nvars 1) 0)))
    (let loop ((ng 0) (sp first-shift))
      (if (pair? sp)
	  (let loop2 ((i (reverse (shift-shifts (car sp)))) (ng2 ng))
	    (if (pair? i)
		(let ((symbol (vector-ref acces-symbol (car i))))
		  (if (< symbol nvars)
		      (begin
			(vector-set! goto-map symbol 
				     (+ 1 (vector-ref goto-map symbol)))
			(loop2 (cdr i) (+ ng2 1)))
		      (loop2 (cdr i) ng2)))
		(loop ng2 (cdr sp))))

	  (let loop ((k 0) (i 0))
	    (if (< i nvars)
		(begin
		  (vector-set! temp-map i k)
		  (loop (+ k (vector-ref goto-map i)) (+ i 1)))

		(begin
		  (do ((i 0 (+ i 1)))
		      ((>= i nvars))
		    (vector-set! goto-map i (vector-ref temp-map i)))

		  (set! ngotos ng)
		  (vector-set! goto-map nvars ngotos)
		  (vector-set! temp-map nvars ngotos)
		  (set! from-state (make-vector ngotos #f))
		  (set! to-state (make-vector ngotos #f))
		  
		  (do ((sp first-shift (cdr sp)))
		      ((null? sp))
		    (let* ((x (car sp))
			   (state1 (shift-number x)))
		      (do ((i (shift-shifts x) (cdr i)))
			  ((null? i))
			(let* ((state2 (car i))
			       (symbol (vector-ref acces-symbol state2)))
			  (if (< symbol nvars)
			      (let ((k (vector-ref temp-map symbol)))
				(vector-set! temp-map symbol (+ k 1))
				(vector-set! from-state k state1)
				(vector-set! to-state k state2))))))))))))))


(define (map-goto state symbol)
  (let loop ((low (vector-ref goto-map symbol))
	     (high (- (vector-ref goto-map (+ symbol 1)) 1)))
    (if (> low high)
	(begin
	  (display (list "Error in map-goto" state symbol)) (newline)
	  0)
	(let* ((middle (quotient (+ low high) 2))
	       (s (vector-ref from-state middle)))
	  (cond
	   ((= s state)
	    middle)
	   ((< s state)
	    (loop (+ middle 1) high))
	   (else
	    (loop low (- middle 1))))))))

; This function implements parts B - D of the algorithm under section 6 of the
; DeRemer and Pennello paper.
;
; As a side effect, it computes the value of 'F.
(define (initialize-F)
  (set! F (make-vector ngotos #f))
  (do ((i 0 (+ i 1))) ((= i ngotos)) (vector-set! F i (new-set token-set-size)))

  (let ((reads (make-vector ngotos #f)))

    (let loop ((i 0) (rowp 0))
      (if (< i ngotos)
	  (let* ((rowf (vector-ref F rowp))
		 (stateno (vector-ref to-state i))
		 (sp (vector-ref shift-table stateno)))
	    (if sp
		(let loop2 ((j (shift-shifts sp)) (edges '()))
		  (if (pair? j)
		      (let ((symbol (vector-ref acces-symbol (car j))))
			(if (< symbol nvars)
			    (if (vector-ref nullable symbol)
				(loop2 (cdr j) (cons (map-goto stateno symbol) 
						     edges))
				(loop2 (cdr j) edges))
			    (begin
			      (set-bit rowf (- symbol nvars))
			      (loop2 (cdr j) edges))))
		      (if (pair? edges)
			  (vector-set! reads i (reverse edges))))))
	      (loop (+ i 1) (+ rowp 1)))))
    (digraph reads)))

; Helper function for 'build-relations.
(define (add-lookback-edge stateno ruleno gotono)
  (let ((k (vector-ref lookaheads (+ stateno 1))))
    (let loop ((found #f) (i (vector-ref lookaheads stateno)))
      (if (and (not found) (< i k))
	  (if (= (vector-ref LAruleno i) ruleno)
	      (loop #t i)
	      (loop found (+ i 1)))

	  (if (not found)
	      (begin (display "Error in add-lookback-edge : ")
		     (display (list stateno ruleno gotono)) (newline))
	      (vector-set! lookback i
			   (cons gotono (vector-ref lookback i))))))))

; Helper function for 'build-relations.
(define (transpose r-arg n)
  (let ((new-end (make-vector n #f))
	(new-R  (make-vector n #f)))
    (do ((i 0 (+ i 1))) 
	((= i n))
      (let ((x (list 'bidon)))
	(vector-set! new-R i x)
	(vector-set! new-end i x)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (let ((sp (vector-ref r-arg i)))
	(if (pair? sp)
	    (let loop ((sp2 sp))
	      (if (pair? sp2)
		  (let* ((x (car sp2))
			 (y (vector-ref new-end x)))
		    (set-cdr! y (cons i (cdr y)))
		    (vector-set! new-end x (cdr y))
		    (loop (cdr sp2))))))))
    (do ((i 0 (+ i 1)))
	((= i n))
      (vector-set! new-R i (cdr (vector-ref new-R i))))
    
    new-R))


; This function does part E.
;
; It computes the value of 'includes and 'lookback.
(define (build-relations)
  (let ( (get-state (lambda (stateno symbol)
		      (let loop ((j (shift-shifts (vector-ref shift-table stateno)))
				 (stno stateno))
			(if (null? j)
			    stno
			    (let ((st2 (car j)))
			      (if (= (vector-ref acces-symbol st2) symbol)
				  st2
				  (loop (cdr j) st2))))))) )
    (set! includes (make-vector ngotos #f))
    (do ((i 0 (+ i 1)))
	((= i ngotos))
      (let ((state1 (vector-ref from-state i))
	    (symbol1 (vector-ref acces-symbol (vector-ref to-state i))))
	(let loop ((rulep (vector-ref derives symbol1))
		   (edges '()))
	  (if (pair? rulep)
	      (let ((*rulep (car rulep)))
		(let loop2 ((rp (vector-ref rrhs *rulep))
			    (stateno state1)
			    (states (list state1)))
		  (let ((*rp (vector-ref ritem rp)))
		    (if (> *rp 0)
			(let ((st (get-state stateno *rp)))
			  (loop2 (+ rp 1) st (cons st states)))
			(begin
			  
			  (if (not (vector-ref consistent stateno))
			      (add-lookback-edge stateno *rulep i))
			  
			  (let loop2 ((done #f) 
				      (stp (cdr states))
				      (rp2 (- rp 1))
				      (edgp edges))
			    (if (not done)
				(let ((*rp (vector-ref ritem rp2)))
				  (if (< -1 *rp nvars)
				      (loop2 (not (vector-ref nullable *rp))
					     (cdr stp)
					     (- rp2 1)
					     (cons (map-goto (car stp) *rp) edgp))
				      (loop2 #t stp rp2 edgp)))
				
				(loop (cdr rulep) edgp))))))))
	      (vector-set! includes i edges)))))
    (set! includes (transpose includes ngotos))) )
			
(define (compute-lookaheads)
  (let ((n (vector-ref lookaheads nstates)))
    (let loop ((i 0))
      (if (< i n)
	  (let loop2 ((sp (vector-ref lookback i)))
	    (if (pair? sp)
		(let ((LA-i (vector-ref LA i))
		      (F-j  (vector-ref F (car sp))))
		  (bit-union LA-i F-j token-set-size)
		  (loop2 (cdr sp)))
		(loop (+ i 1))))))) )

; This is a helper function for 'digraph.
(define (traverse i INDEX R VERTICES top)
  (let ( (infinity (+ ngotos 2)) )	  
    (set! top (+ 1 top))
    (vector-set! VERTICES top i)
    (let ((height top))
	(vector-set! INDEX i height)
	(let ((rp (vector-ref R i)))
	  (if (pair? rp)
	      (let loop ((rp2 rp))
		(if (pair? rp2)
		    (let ((j (car rp2)))
		      (if (= 0 (vector-ref INDEX j))
			  (traverse j INDEX R VERTICES top))
		      (if (> (vector-ref INDEX i) 
			     (vector-ref INDEX j))
			  (vector-set! INDEX i (vector-ref INDEX j)))
		      (let ((F-i (vector-ref F i))
			    (F-j (vector-ref F j)))
			(bit-union F-i F-j token-set-size))
		      (loop (cdr rp2))))))
	  (if (= (vector-ref INDEX i) height)
	      (let loop ()
		(let ((j (vector-ref VERTICES top)))
		  (set! top (- top 1))
		  (vector-set! INDEX j infinity)
		  (if (not (= i j))
		      (begin
			(bit-union (vector-ref F i) 
				   (vector-ref F j)
				   token-set-size)
			(loop)))))))))) 

; This is the Digraph function from section 4 of the DeRemer and Pennello paper.
(define (digraph relation)
  (let ( (INDEX (make-vector (+ ngotos 1) 0))
	  (R relation) 
	  (VERTICES (make-vector (+ ngotos 1) 0))
	  (top 0) )
    (let loop ((i 0))
      (if (< i ngotos)
	  (begin
	    (if (and (= 0 (vector-ref INDEX i))
		     (pair? (vector-ref R i)))
		(traverse i INDEX R VERTICES top))
	    (loop (+ i 1)))))) )

;; --
(define (build-tables prec right-assoc non-assoc rule-preced)
  (define (add-action St Sym Act)
    (let* ((x (vector-ref action-table St))
	   (y (assv Sym x)))
     
      (if y
	  (if (not (= Act (cdr y)))
	      ;; -- there is a conflict 
	      (begin
		(if (and (<= (cdr y) 0)
			 (<= Act 0))
		    (begin
		      (display "%% Reduce/Reduce conflict ")
		      (display "(reduce ") (display (- Act))
		      (display ", reduce ") (display (- (cdr y)))
		      (display ") on ") (print-symbol (+ Sym nvars))
		      (display " in state ") (display St)
		      (newline)
		      (set-cdr! y (max (cdr y) Act)))
		    (begin
		      (let* ((state (list-ref first-state St))
			     (token-sym (vector-ref the-terminals
						    Sym))
			     (token-prec (lookup-precedence prec token-sym))
			     (rule-prec (lookup-precedence prec 
							   (vector-ref rule-preced
								       (- (cdr y))))))
						    
			(cond ((and (not rule-prec)
				    (not token-prec))
			       (begin
				 (display "%% Shift/Reduce conflict ")
				 (display "(shift ") (display Act)
				 (display ", reduce ") (display 
							(- (cdr y)))
				 (display ") on ") (print-symbol 
						    (+ Sym nvars))
				 (display " in state ") (display St)
				 (newline)
				      (set-cdr! y Act)))
			      ((not rule-prec)
			       (set-cdr! y Act))
			      ((not token-prec)
			       #f)
			      ((> token-prec rule-prec)
			       (set-cdr! y Act))
			      ((eq? token-prec rule-prec)
			       (cond ((member token-sym right-assoc)
					   (set-cdr! y Act))
				     ((member token-sym non-assoc)
				      (set-cdr! y '*error*))))))))))
	  (vector-set! action-table St
		       (cons (cons Sym Act) x)))))
  
  (set! action-table (make-vector nstates '()))

  (do ((i 0 (+ i 1)))  ; i = state
      ((= i nstates))
    (let ((red (vector-ref reduction-table i)))
      (if (and red (>= (red-nreds red) 1))
	  (if (and (= (red-nreds red) 1) (vector-ref consistent i))
	      (add-action i 'default (- (car (red-rules red))))
	      (let ((k (vector-ref lookaheads (+ i 1))))
		(let loop ((j (vector-ref lookaheads i)))
		  (if (< j k)
		      (let ((rule (- (vector-ref LAruleno j)))
			    (lav  (vector-ref LA j)))
			(let loop2 ((token 0) (x (vector-ref lav 0)) (y 1) (z 0))
			  (if (< token nterms)
			      (begin
				(let ((in-la-set? (modulo x 2)))
				  (if (= in-la-set? 1)
				      (add-action i token rule)))
				(if (= y (BITS-PER-WORD))
				    (loop2 (+ token 1) 
					   (vector-ref lav (+ z 1))
					   1
					   (+ z 1))
				    (loop2 (+ token 1) (quotient x 2) (+ y 1) z)))))
			(loop (+ j 1)))))))))

    (let ((shiftp (vector-ref shift-table i)))
      (if shiftp
	  (let loop ((k (shift-shifts shiftp)))
	    (if (pair? k)
		(let* ((state (car k))
		       (symbol (vector-ref acces-symbol state)))
		  (if (>= symbol nvars)
		      (add-action i (- symbol nvars) state))
		  (loop (cdr k))))))))

  (add-action final-state 0 'accept))

(define (compact-action-table)
  (define (most-common-action acts)
    (let ((accums '()))
      (let loop ((l acts))
	(if (pair? l)
	    (let* ((x (cdar l))
		   (y (assv x accums)))
	      (if (and (number? x) (< x 0))
		  (if y
		      (set-cdr! y (+ 1 (cdr y)))
		      (set! accums (cons `(,x . 1) accums))))
	      (loop (cdr l)))))

      (let loop ((l accums) (max 0) (sym #f))
	(if (null? l)
	    sym
	    (let ((x (car l)))
	      (if (> (cdr x) max)
		  (loop (cdr l) (cdr x) (car x))
		  (loop (cdr l) max sym)))))))

  (do ((i 0 (+ i 1)))
      ((= i nstates))
    (let ((acts (vector-ref action-table i)))
      (if (vector? (vector-ref reduction-table i))
	  (let ((act (most-common-action acts)))
	    (vector-set! action-table i
			 (cons `(default . ,(if act act 'error))
			       (filter (lambda (x) 
					 (not (eq? (cdr x) act)))
				       acts))))
	  (vector-set! action-table i 
		       (cons `(default . *error*) acts))))))

;; --

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

; Checks to see if its argument is a valid non-terminal symbol
(define (valid-nonterminal? x)
  (symbol? x))

; Checks to see if its argument is a valid terminal symbol
(define (valid-terminal? x)
  (symbol? x))

;; ---------------------------------------------------------------------- ;;
;; Miscellaneous                                                          ;;
;; ---------------------------------------------------------------------- ;;

; Takes two sets (represented as sorted lists) and computes their union.
(define (sunion lst1 lst2)
  (let loop ((L1 lst1)
	     (L2 lst2))
    (cond ((null? L1)    L2)
	  ((null? L2)    L1)
	  (else
	   (let ((x (car L1)) (y (car L2)))
	     (cond
	      ((> x y)
	       (cons y (loop L1 (cdr L2))))
	      ((< x y)
	       (cons x (loop (cdr L1) L2)))
	      (else
	       (loop (cdr L1) L2))
	      ))))))

; Takes a set (represented as a sorted list) and adds 'elem to the set.
(define (sinsert elem lst)
  (let loop ((l1 lst))
    (if (null? l1)
	(cons elem l1)
	(let ((x (car l1)))
	  (cond ((< elem x)
		 (cons elem l1))
		((> elem x)
		 (cons x (loop (cdr l1))))
		(else 
		 l1))))))

;; ---------------------------------------------------------------------- ;;
;; Debugging tools ...                                                    ;;
;; ---------------------------------------------------------------------- ;;
(define the-terminals #f)
(define the-nonterminals #f)

(define (print-item item-no)
  (let loop ((i item-no))
    (let ((v (vector-ref ritem i)))
      (if (>= v 0)
	  (loop (+ i 1))
	  (let* ((rlno    (- v))
		 (nt      (vector-ref rlhs rlno)))
	    (display (vector-ref the-nonterminals nt)) (display " --> ")
	    (let loop ((i (vector-ref rrhs rlno)))
	      (let ((v (vector-ref ritem i)))
		(if (= i item-no)
		    (display ". "))
		(if (>= v 0)
		    (begin
		      (print-symbol v)
		      (display " ")
		      (loop (+ i 1)))
		    (begin 
		      (display "   (rule ")
		      (display (- v))
		      (display ")")
		      (newline))))))))))
  
(define (print-symbol n)
  (display (if (>= n nvars)
	       (vector-ref the-terminals (- n nvars))
	       (vector-ref the-nonterminals n))))
  
(define (print-action act)
  (cond
   ((eq? act '*error*)
    (display " : Error"))
   ((eq? act 'accept)
    (display " : Accept input"))
   ((< act 0)
    (display " : reduce using rule ")
    (display (- act)))
   (else
    (display " : shift and goto state ")
    (display act)))
  (newline)
  #t)

(define (print-actions acts)
    (let loop ((l acts))
      (if (null? l)
	  #t
	  (let ((sym (caar l))
		(act (cdar l)))
	    (display "   ")
	    (cond
	     ((eq? sym 'default)
	      (display "default action"))
	     (else
	      (print-symbol (+ sym nvars))))
	    (print-action act)
	    (loop (cdr l))))))

(define (print-states)
  
  (if (not action-table)
      (begin
	(display "No generated parser available!")
	(newline)
	#f)
      (begin
	(display "State table") (newline)
	(display "-----------") (newline) (newline)
  
	(let loop ((l first-state))
	  (if (null? l)
	      #t
	      (let* ((core  (car l))
		     (i     (core-number core))
		     (items (core-items core))
		     (actions (vector-ref action-table i)))
		(display "state ") (display i) (newline)
		(newline)
		(for-each (lambda (x) (display "   ") (print-item x))
			  items)
		(newline)
		(print-actions actions)
		(newline)
		(loop (cdr l))))))))

(define (lookup-precedence prec x)
  (let loop ((i 0))
    (if (>= i (length prec))
	#f
	(if (member x (list-ref prec i))
	    i
	    (loop (+ 1 i))))))

; This function calculates figures out the precedence for each rule that did not
; have a precedence symbol specified manually.  It does this by going through
; the items on the right hand side of the rule and finding the last terminal
; symbol.  The precedence of that symbol, if it has one, then becomes the
; precedence for the rule.
;
; Arguments:
; - rule-preced = A vector containing the manually specified precedence symbol for
;                 the corresponding rule in 'rrhs & 'rlhs or #f if it was not
;                 specified.
; Rules:
;   nothing (it modifies the value of 'rule-preced)
(define (calculate-precedence rule-preced)
  (let loop ((x 1))
    (if (< x nrules)
	(if (vector-ref rule-preced x)
	    (loop (+ x 1))
	    (begin
	      (vector-set! rule-preced
			   x
			   (let iloop ((i (vector-ref rrhs x))
				       (psf #f))
			     (let ((t (vector-ref ritem i)))
			       (cond ((< t -1)
				      psf)
				     ((< t nvars)    ; a non-terminal
				      (iloop (+ 1 i) psf))
				     (else
				      (iloop (+ 1 i)
					     (vector-ref
					      the-terminals
					      (- t nvars))))))))
	      (loop (+ x 1)))))))
