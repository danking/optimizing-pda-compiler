(define fullcalc
  (compile+convert-to-pda
   ((tokens NUM L-PAREN R-PAREN SEMICOLON
	    (left TIMES DIVIDE)
	    (left PLUS MINUS)
	    (error *ERROR*)
	    (eos *EOF*))
    
    (non-term program
	      (=> (s-list)				s-list)
	      (=> (s-list exp)				(begin (display exp) (newline) (cons exp s-list)))
	      (=> (s-list *ERROR*)			(cons (if #f #f) s-list)))
    (non-term s-list
	      (=> ()					'())
	      (=> (s-list statement)			(cons statement s-list)))
    (non-term statement
	      (=> (exp SEMICOLON)			(begin (display exp) (newline) exp))
	      (=> (*ERROR* SEMICOLON)			(if #f #f)))
    (non-term exp
              (=> (NUM)					NUM)
	      (=> (exp PLUS exp)			(+ exp-1 exp-2))
	      (=> (exp MINUS exp)			(- exp-1 exp-2))
	      (=> ((expA exp) TIMES (expB exp))		(* expA expB))
	      (=> ((expA exp) DIVIDE exp)		(quotient expA exp))
	      (=> (L-PAREN exp R-PAREN)			exp))
    )))

(define adder
  (compile+convert-to-pda
   ((tokens DIGIT
	    (left PLUS)
	    (error *ERROR*)
	    (eos *EOF*))
    (non-term exp
	      (=> (num PLUS num)   (+ num-1 num-2)))
    (non-term num
	      (=> (num DIGIT)      (+ (* num 10)
				      (- (char->ascii DIGIT)
					 (char->ascii #\0))))
	      (=> ()               0))
    )))

(define new-adder
  (compile+convert-to-pda
   ((tokens DIGIT
	    (left PLUS)
	    (error *ERROR*)
	    (eos *EOF*))
    (non-term exp
	      (=> (num PLUS exp)   (+ num exp))
	      (=> (num)            num))
    (non-term num
	      (=> (num DIGIT)      (+ (* num 10)
				      (- (char->ascii DIGIT)
					 (char->ascii #\0))))
	      (=> ()               0))
    )))

; produces...

(define adder-PDA
  ((TOKENS DIGIT PLUS *EOF*)
   (ERROR *ERROR*)
   (NO-SHIFT *EOF*)
   (RULE r1 *start 2 #f)
   (RULE r2 exp 3 (lambda (num-1 PLUS num-2)
		    (+ num-1 num-2)))
   (RULE r3 num 2 (lambda (num DIGIT)
		    (+ (* num 10) (- (char->ascii DIGIT) (char->ascii #\0)))))
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