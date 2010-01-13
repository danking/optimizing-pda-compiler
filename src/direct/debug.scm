;;; A set of helper functions for debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	 (with-current-output-port (current-error-port)
	   (display msg)
	   (display ": ")
	   (p val))))))

(define adder-PDA
  '((TOKENS DIGIT PLUS *EOF*) 
    (ERROR *ERROR*) 
    (NO-SHIFT *EOF*) 
    (RULE r1 *start 2 #f) 
    (RULE r2 exp 3 (lambda (num-1 PLUS num-2) (+ num-1 num-2))) 
    (RULE r3 num 2 (lambda (num DIGIT) (+ (* num 10) DIGIT))) 
    (RULE r4 num 0 (lambda () 0)) 
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
	   (REDUCE () r3)) 
    (STATE s4 
	   (REDUCE () r4) 
	   (GOTO num s5)) 
    (STATE s5 
	   (SHIFT (DIGIT) s3) 
	   (REDUCE (*EOF*) r2)) 
    (STATE s6 
	   (REDUCE () r1)))
)