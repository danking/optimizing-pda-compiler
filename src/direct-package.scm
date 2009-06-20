;;; Directly executable parser in Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main file.


(define-structure direct-parser (export )
  (open pp define-record-types srfi-8 srfi-23 ascii lalr scheme)

  (begin
  
;;; Loading instructions:
;;; ,open pp ; pretty-printing
;;; ,open define-record-types ; records
;;; ,open srfi-8 ; receive
;;; ,open srfi-23 ; error
;;; ,open ascii ; char->ascii


;;; There's just helper functions for now, and test data


;; Turn assertions on or off
(define assertion-mode #t)

(define-syntax assert
  (syntax-rules ()
    ;; assert : X X [X X -> Boolean] -> Boolean
    ;; Given a value and a predicate, tests if the predicate returns true for the value.
    ;; If assertions are off, or they are on and the assertion is valid, just return #t.
    ;; If assertions fails, print the actual and expected values for debugging.
    ((assert actual expected comparator)
     (if assertion-mode
	 (let ((act-val actual)
	       (exp-val expected)
	       (comp comparator))
	   (or (comp act-val exp-val)
	       (with-current-output-port (current-error-port)
		 (display "Assertion failed. Expected:") (newline)
		 (p exp-val)
		 (display "Actual: ")
		 (p act-val)
		 #f))
	 #t)))
    
    ;; assert : X X -> Boolean
    ;; Assertion using default comparator `equal?`.
    ((assert one two)
     (assert one two equal?))
    
    ;; assert : Boolean -> Boolean
    ;; Assertion on a single boolean value.
    ((assert bool) 
     (assert bool #t))
))


;; Turn debugging statements on or off.
(define debug-mode #t)

(define-syntax debug
  (syntax-rules ()
    ;; debug : String Any -> Void
    ;; Pretty-print value for debugging, if debug flag is set.
    ((debug msg val)
     (if debug-mode
	 (begin
	   (display msg)
	   (display ": ")
	   (p val))))))


;; define-record-simple : 'Name ('Field ...) -> Void
;; Define a record-type and record-discloser for the given name and fields.
;; Example: (define-record-simple foo (bar qux)) gives structure foo with fields bar and qux.
(define-syntax define-record-simple
  (lambda (e r c)
    (let* ((symbol+ (lambda syms
		      (string->symbol (apply string-append (map symbol->string syms)))))
	   (params (cdr e))
	   (name (car params))		      ; 'sframe
	   (fields (cadr params))	      ; '(type val state)
	   (tag (symbol+ ': name))	      ; ':sframe
	   (pred (symbol+ name '?))	      ; 'sframe?
	   (make (symbol+ 'make- name))	      ; 'make-sframe
	 
	   (%lambda (r 'lambda))
	   (%define-record-type (r 'define-record-type))
	   (%define-record-discloser (r 'define-record-discloser))
	   (%begin (r 'begin))
	   (%cons (r 'cons))
	   (%list (r 'list))
	 
	   (constructor (cons make fields)) ; '(make-sframe type val state)
	   (accessors (map (lambda (field)
			     (list field
				   (symbol+ name '- field)))
			   fields)) ; '((type sframe-type) (val sframe-val) (state sframe-state))
	   (disclosers (map (lambda (acc)
			      `(,(cadr acc) o))
			    accessors)) ; '((sframe-type o) (sframe-val o) (sframe-state o))
	   (definer (append (list %define-record-type name tag
				  constructor
				  pred)
			    accessors))
	   (discloser `(,%define-record-discloser
			,tag
			(,%lambda (o)
				  ,(cons 'list (cons `(quote ,make) disclosers))))))
      `(,%begin ,definer ,discloser))))



; Basic adder CFG as PDA (see CFG in playground file.)
(define (adder-exp num-1 PLUS num-2)
  (+ num-1 num-2))
(define (adder-num num DIGIT)
  (+ (* num 10) (- (char->ascii DIGIT) (char->ascii #\0))))
(define (adder-0)
  0)
(define adder-PDA
  `((TOKENS DIGIT PLUS *EOF*)
    (ERROR *ERROR*)
    (NO-SHIFT *EOF*)
    (RULE r1 *start 2 #f)
    (RULE r2 exp 3 (lambda (num1 PLUS num2) (+ num1 num2)))
    (RULE r3 num 2 adder-num)
    (RULE r4 num 0 adder-0)
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
	   (COMMENT num "=>" num DIGIT ".")
	   (REDUCE () r3))
    (STATE s4
	   (REDUCE () r4)
	   (GOTO num s5))
    (STATE s5
	   (SHIFT (DIGIT) s3)
	   (REDUCE (*EOF*) r2))
    (STATE s6
	   (REDUCE () r1))))

))
