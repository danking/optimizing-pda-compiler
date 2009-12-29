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
  

(define test-PDA
  '((TOKENS NUM L-PAREN R-PAREN SEMICOLON TIMES DIVIDE PLUS MINUS *EOF*) 
    (ERROR *ERROR*) 
    (NO-SHIFT *EOF*) 
    (RULE r1 *start 2 #f) 
    (RULE r2 program 1 (lambda (s-list) s-list)) 
    (RULE r3 program 2 (lambda (s-list exp) 
			 (begin (display exp) 
				(newline) 
				(cons exp s-list)))) 
    (RULE r4 program 2 (lambda (s-list *ERROR*) 
			 (cons (if #f #f) s-list))) 
    (RULE r5 s-list 0 (lambda () '())) (RULE r6 s-list 2 (lambda (s-list statement) (cons statement s-list))) (RULE r7 statement 2 (lambda (exp SEMICOLON) (begin (display exp) (newline) exp))) (RULE r8 statement 2 (lambda (*ERROR* SEMICOLON) (if #f #f))) (RULE r9 exp 1 (lambda (NUM) NUM)) (RULE r10 exp 3 (lambda (exp-1 PLUS exp-2) (+ exp-1 exp-2))) (RULE r11 exp 3 (lambda (exp-1 MINUS exp-2) (- exp-1 exp-2))) (RULE r12 exp 3 (lambda (expA TIMES expB) (* expA expB))) (RULE r13 exp 3 (lambda (expA DIVIDE exp) (quotient expA exp))) (RULE r14 exp 3 (lambda (L-PAREN exp R-PAREN) exp)) (STATE s0 (COMMENT s-list "=>" "." s-list statement) (COMMENT s-list "=>" ".") (COMMENT program "=>" "." s-list *ERROR*) (COMMENT program "=>" "." s-list exp) (COMMENT program "=>" "." s-list) (COMMENT *start "=>" "." program *EOF*) (REDUCE () r5) (GOTO program s1) (GOTO s-list s2)) (STATE s1 (COMMENT *start "=>" program "." *EOF*) (ACCEPT (*EOF*))) (STATE s2 (COMMENT exp "=>" "." L-PAREN exp R-PAREN) (COMMENT exp "=>" "." exp DIVIDE exp) (COMMENT exp "=>" "." exp TIMES exp) (COMMENT exp "=>" "." exp MINUS exp) (COMMENT exp "=>" "." exp PLUS exp) (COMMENT exp "=>" "." NUM) (COMMENT statement "=>" "." *ERROR* SEMICOLON) (COMMENT statement "=>" "." exp SEMICOLON) (COMMENT s-list "=>" s-list "." statement) (COMMENT program "=>" s-list "." *ERROR*) (COMMENT program "=>" s-list "." exp) (COMMENT program "=>" s-list ".") (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (SHIFT (*ERROR*) s7) (REDUCE (*EOF*) r2) (GOTO statement s3) (GOTO exp s4)) (STATE s3 (COMMENT s-list "=>" s-list statement ".") (REDUCE () r6)) (STATE s4 (COMMENT exp "=>" exp "." DIVIDE exp) (COMMENT exp "=>" exp "." TIMES exp) (COMMENT exp "=>" exp "." MINUS exp) (COMMENT exp "=>" exp "." PLUS exp) (COMMENT statement "=>" exp "." SEMICOLON) (COMMENT program "=>" s-list exp ".") (SHIFT (SEMICOLON) s19) (SHIFT (TIMES) s11) (SHIFT (DIVIDE) s12) (SHIFT (PLUS) s13) (SHIFT (MINUS) s14) (REDUCE (*EOF*) r3)) (STATE s5 (COMMENT exp "=>" NUM ".") (REDUCE () r9)) (STATE s6 (COMMENT exp "=>" L-PAREN "." exp R-PAREN) (COMMENT exp "=>" "." L-PAREN exp R-PAREN) (COMMENT exp "=>" "." exp DIVIDE exp) (COMMENT exp "=>" "." exp TIMES exp) (COMMENT exp "=>" "." exp MINUS exp) (COMMENT exp "=>" "." exp PLUS exp) (COMMENT exp "=>" "." NUM) (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s9)) (STATE s7 (COMMENT statement "=>" *ERROR* "." SEMICOLON) (COMMENT program "=>" s-list *ERROR* ".") (SHIFT (SEMICOLON) s8) (REDUCE (*EOF*) r4)) (STATE s8 (COMMENT statement "=>" *ERROR* SEMICOLON ".") (REDUCE () r8)) (STATE s9 (COMMENT exp "=>" L-PAREN exp "." R-PAREN) (COMMENT exp "=>" exp "." DIVIDE exp) (COMMENT exp "=>" exp "." TIMES exp) (COMMENT exp "=>" exp "." MINUS exp) (COMMENT exp "=>" exp "." PLUS exp) (SHIFT (R-PAREN) s10) (SHIFT (TIMES) s11) (SHIFT (DIVIDE) s12) (SHIFT (PLUS) s13) (SHIFT (MINUS) s14)) (STATE s10 (COMMENT exp "=>" L-PAREN exp R-PAREN ".") (REDUCE () r14)) (STATE s11 (COMMENT exp "=>" "." L-PAREN exp R-PAREN) (COMMENT exp "=>" "." exp DIVIDE exp) (COMMENT exp "=>" exp TIMES "." exp) (COMMENT exp "=>" "." exp TIMES exp) (COMMENT exp "=>" "." exp MINUS exp) (COMMENT exp "=>" "." exp PLUS exp) (COMMENT exp "=>" "." NUM) (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s18)) (STATE s12 (COMMENT exp "=>" "." L-PAREN exp R-PAREN) (COMMENT exp "=>" exp DIVIDE "." exp) (COMMENT exp "=>" "." exp DIVIDE exp) (COMMENT exp "=>" "." exp TIMES exp) (COMMENT exp "=>" "." exp MINUS exp) (COMMENT exp "=>" "." exp PLUS exp) (COMMENT exp "=>" "." NUM) (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s17)) (STATE s13 (COMMENT exp "=>" "." L-PAREN exp R-PAREN) (COMMENT exp "=>" "." exp DIVIDE exp) (COMMENT exp "=>" "." exp TIMES exp) (COMMENT exp "=>" "." exp MINUS exp) (COMMENT exp "=>" exp PLUS "." exp) (COMMENT exp "=>" "." exp PLUS exp) (COMMENT exp "=>" "." NUM) (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s16)) (STATE s14 (COMMENT exp "=>" "." L-PAREN exp R-PAREN) (COMMENT exp "=>" "." exp DIVIDE exp) (COMMENT exp "=>" "." exp TIMES exp) (COMMENT exp "=>" exp MINUS "." exp) (COMMENT exp "=>" "." exp MINUS exp) (COMMENT exp "=>" "." exp PLUS exp) (COMMENT exp "=>" "." NUM) (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s15)) (STATE s15 (COMMENT exp "=>" exp "." DIVIDE exp) (COMMENT exp "=>" exp "." TIMES exp) (COMMENT exp "=>" exp MINUS exp ".") (COMMENT exp "=>" exp "." MINUS exp) (COMMENT exp "=>" exp "." PLUS exp) (REDUCE (R-PAREN) r11) (REDUCE (SEMICOLON) r11) (SHIFT (TIMES) s11) (SHIFT (DIVIDE) s12) (REDUCE (PLUS) r11) (REDUCE (MINUS) r11) (REDUCE (*EOF*) r11)) (STATE s16 (COMMENT exp "=>" exp "." DIVIDE exp) (COMMENT exp "=>" exp "." TIMES exp) (COMMENT exp "=>" exp "." MINUS exp) (COMMENT exp "=>" exp PLUS exp ".") (COMMENT exp "=>" exp "." PLUS exp) (REDUCE (R-PAREN) r10) (REDUCE (SEMICOLON) r10) (SHIFT (TIMES) s11) (SHIFT (DIVIDE) s12) (REDUCE (PLUS) r10) (REDUCE (MINUS) r10) (REDUCE (*EOF*) r10)) (STATE s17 (COMMENT exp "=>" exp DIVIDE exp ".") (COMMENT exp "=>" exp "." DIVIDE exp) (COMMENT exp "=>" exp "." TIMES exp) (COMMENT exp "=>" exp "." MINUS exp) (COMMENT exp "=>" exp "." PLUS exp) (REDUCE (R-PAREN) r13) (REDUCE (SEMICOLON) r13) (REDUCE (TIMES) r13) (REDUCE (DIVIDE) r13) (REDUCE (PLUS) r13) (REDUCE (MINUS) r13) (REDUCE (*EOF*) r13)) (STATE s18 (COMMENT exp "=>" exp "." DIVIDE exp) (COMMENT exp "=>" exp TIMES exp ".") (COMMENT exp "=>" exp "." TIMES exp) (COMMENT exp "=>" exp "." MINUS exp) (COMMENT exp "=>" exp "." PLUS exp) (REDUCE (R-PAREN) r12) (REDUCE (SEMICOLON) r12) (REDUCE (TIMES) r12) (REDUCE (DIVIDE) r12) (REDUCE (PLUS) r12) (REDUCE (MINUS) r12) (REDUCE (*EOF*) r12)) (STATE s19 (COMMENT statement "=>" exp SEMICOLON ".") (REDUCE () r7)) (STATE s20 (COMMENT *start "=>" program *EOF* ".") (REDUCE () r1))))