;;; Directly executable parser in Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main file.


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
	   (if (comp act-val exp-val) #t
	       (begin (display "Assertion failed. Expected:") (newline)
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
	   (p val))))))


;; list=? : [X X -> Boolean] [Listof X] [Listof X] -> Boolean
;; to compare the contents of two lists using the given comparator
(define (list=? comp l1 l2)
  (cond
   ((null? l1) (null? l2))
   ((null? l2) #f)
   (else (and (comp (car l1) (car l2))
	      (list=? comp (cdr l1) (cdr l2))))))

(assert (list=? = '() '()))
(assert (list=? = '(1 2 3) '(1 2 3)))
(assert (list=? = '(1 2 3) '(1 2 4))
	#f)
(assert (list=? = '(1 2 3) '(1 2 3 4))
	#f)
(assert (list=? = '(1 2 3 4) '(1 2 3))
	#f)

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
    (RULE r2 exp 3 ,adder-exp)
    (RULE r3 num 2 ,adder-num)
    (RULE r4 num 0 ,adder-0)
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