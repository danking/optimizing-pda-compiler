
(load "lalr.scm")
(load "lr-dvr.scm")
(define-record production
  non-terminal
  rhs)

(define (reverse-table tbl v state) 
  (if (not (null? tbl))
      (if (not (car tbl))
	  (reverse-table (cdr tbl) v (+ state 1))
	  (begin
	    (for-each 
	     (lambda (shift)
	       (if (and (number? (cdr shift))
			(not (negative? 
			      (cdr shift))))
		   (vector-set! v
				(cdr shift)
				(cons state
				      (vector-ref
				       v
				       (cdr shift))))))
	     (car tbl))
	    (reverse-table (cdr tbl) v  (+ state 1))))))


(define (calculate-num-retpt grammar)
  (define (sft-stats sft)
    (let loop ((i 0)
	       (x 0))
      (if (< i (vector-length sft))
	  (loop (+ i 1)
		(if (eq? (length (vector-ref sft i)) 1)
		    (+ x 1)
		    x))
	  (begin
	    (display x)
	    (display " States are called from only one other state")
	    (newline)))))
  (let ((num-list (make-vector
		   (length tmp:action-table)
		   '()))
	(sft 
	 (let* ((ret-val (make-vector 
			 (length tmp:action-table)
			 '())))
	   (reverse-table tmp:action-table ret-val 0)
	   (reverse-table tmp:goto-table ret-val 0)
	   ret-val)))
			
    (sft-stats sft)
    
    (let init ((at tmp:action-table)
	       (state-num 0))
      (if (not (null? at))
	  (let ((state (car at))
		(ret-pts '()))
	    (for-each 
	     (lambda (x)
	      
	       (cond   ((eq? 'accept (cdr x))
			(set! ret-pts (cons 'accept ret-pts)))
		       ((not (number? (cdr x)))
			#f)
		       ((< (cdr x) -1)
			   (begin (display (cdr x))
			     (let ((size 
			       (length (production:rhs
					(list-ref grammar
						  ( - (* -1
							(cdr x))
						      1))))))
			  (if (not (zero? size))
			      (if (not (member size ret-pts))
				  (set! ret-pts (cons size ret-pts)))))))
		       ((eq? (cdr x) -1)
			(if (not (member 1 ret-pts))
			    (set! ret-pts (cons 1 ret-pts))))))
			 
	     state)
	    (vector-set! num-list state-num ret-pts)
	    (init (cdr at) (+ state-num 1)))))
;    (display num-list)
    ;; a lower bound for return pts have been set for all states
    (letrec ((propagate (lambda (ret-pt state)
			(if (or 
			     (not (number? ret-pt))
			     (not (zero? ret-pt)))
			    (let ((rpfs (vector-ref num-list state)))
			      (if (not (member ret-pt rpfs))
				  (vector-set! num-list
					       state
					       (cons ret-pt
						     rpfs)))
			      (for-each 
			       (lambda (prev-state)
				 (propagate 
				  (if (number? ret-pt)
				      (- ret-pt 1)
				      ret-pt)
				  prev-state))
			       (vector-ref sft state)))))))
      (let loop ((state 0))
	(if (< state (length tmp:action-table))
	    (begin
	      (for-each
	       (lambda (ret-pt)
		 (propagate ret-pt state))
	       (vector-ref num-list state))
	      (loop (+ state 1)))))
      num-list)))
      
(define (my-parse-gram my-grammar) 
;  (display (equal? my-grammar tiger-grammar))
 ; (display (cdr my-grammar))
  (let loop ((ret (cons 
		   (make-production
		    0
		    '(1)) '()))
	     (nt 1)
	     (gram (cdr my-grammar))) 
   
    (if (null? gram)
	(reverse ret)
	(loop 
	       (let iloop ((rules (cdar gram))
			   (lsf ret))
		 
		 (if (null? rules)
		     lsf
		     (begin ;(display (car rules)) (newline)
		     (if (or (null? (caar rules))
			     (not (eq? (caaar rules) 'prec)))
			  (iloop (cdr rules)
				(cons (make-production nt (caar rules))
				      lsf))
			  (iloop (cdr rules)
				 (cons (make-production nt 
							(nth (car rules) 1))
				       lsf))))))		    
	
	       (+ nt 1)
	       (cdr gram)))))
  
(define (create-declarations prefix)
  (display "type stack = array of int")
  (newline)
  (display "type tokenStream = array of int")
  (newline)
  (newline)
  (display "var MAX_SIZE := 4096")
  (newline)
  (newline)
  (display "var ")
  (display prefix)
  (display "Stack := stack[ MAX_SIZE ] of 0")
  (newline)
  (display "var x := 0")
  (newline)
  (display "var ")
  (display prefix)
  (display "TS := tokenStream[ MAX_SIZE ] of 0")
  (newline)
  (display "var ")
  (display prefix)
  (display "StackPtr := -1")
  (newline)
  (display "var ")
  (display prefix)
  (display "TsPtr := 0")
  (newline)
  (newline))
  
(define (gen-state-code state prefix ret-pts-list g)
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
  (let* ((at-entry (reverse (list-ref tmp:action-table state)))
	(goto-entry (reverse (list-ref tmp:goto-table state)))
	(num-retpts (length (list-ref ret-pts-list state)))
	(calculate-ret-pt (lambda (l r)
			    (+ 1 (pos-in-list r l))))
	(output-goto (lambda   ( toState )
		       (display "<| ")
		       (display prefix)
		       (display "State")
		       (display toState)
		       (display "(s, sp+1, ts, tsp) | ")
		       (let loop ((ret-pts (list-ref ret-pts-list toState)))
			 (if (not (null? ret-pts))
			     (let ((ret-pt (car ret-pts)))
			       (if (eq? ret-pt 1) 
				   (begin
				     (display prefix)
				     (display "TsPtr => ")
				     (display " <| goto (")
				     (display prefix)
				     (display "TsPtr")
				     (display ") | ")
				     (let iloop ((x 1))
				       (if (<= x num-retpts)
					   (begin 
					     (display " #")
					     (display x)
					     (if (< x num-retpts)
						 (display ", ")
						 (display " "))
					     (iloop (+ x 1)))))
				     (display " |>  "))
				   (begin
				     (display "#")
		     ;;; Hairy mapping calculations
				     (display (calculate-ret-pt 
					       (list-ref ret-pts-list state)
					 	(if (integer? ret-pt)
						    (- ret-pt 1)
						    ret-pt)))))
			       
			       (display " ")
			       (if (> (length ret-pts) 1)
				   (display ", "))
			       (loop (cdr ret-pts)))))
			(display " |>")
			(newline)))
	(output-shift (lambda (state)
			(display "(sp := sp + 1;")
			(newline)
			(display prefix)
			(display "Stack[sp] := ")
			(display prefix)
			(display "TS[tsp]; ")
			(newline)
			(display "tsp := tsp + 1; ")
			(newline)
			(output-goto state)
			(display ")"))))
    
    (display "/* ")
    (let* ((s (nth first-state state))
	   (actions (vector-ref action-table state))
	   (items (core-items s)))
	 (display "state ") 
	 (display state)
	 (newline)
	 (newline)
	 (for-each (lambda (x) (display "   ") (print-item x))
		   items)
	 (newline)
	 (print-actions actions)
	 (newline))
   ; (display "Number of Return Pts: ")
   ; (display (length (nth ret-pts-list state)))
    (newline)
    (display "*/")
    (newline)
    (display "function ")
    (display prefix)
    (display "State")
    (display state)
    (display "(s:stack, sp:int, ts:tokenStream, tsp:int): int")
    (let tiny-loop ((l (- (length (list-ref ret-pts-list state)) 1)))
      (if (> l 0)
	  (begin
	    (display ", int")
	    (tiny-loop (- l 1)))
	  (display " = ")))


    (newline)
    (display "let ")
    (newline)

    ;;; generate function for goto-table
    (display "function goto(tsp:int): int ")
    (let tiny-loop ((l (- (length (list-ref ret-pts-list state)) 1)))
      (if (> l 0)
	  (begin
	    (display ", int")
	    (tiny-loop (- l 1)))
	  (display " = ")))
    (newline)
    (for-each 
     (lambda (goto)
       (display "if s[sp] = ")
       (display (car goto))
       (display " then ")
       (newline)
       (output-goto (cdr goto))
       (newline)
       (display "else "))
     goto-entry)
    (display "(print(\" Error unknown\"); exit(0); 0)")
    
    (newline)
    (display " in ")
    (for-each 
     (lambda (action)
       (let ((output-reduce (lambda () 
			      (let ((p (list-ref g
				     (- (* -1
					   (cdr action))
					1))))
		    (if (zero? (length (production:rhs p)))
			(begin
			  (display "( sp := sp + 1; ")
			  (newline)
			  (display prefix)
			  (display "Stack[ sp ] = ")
			  (display (production:non-terminal p))
			  (display " ;")
			  (display "<| goto (tsp) | ")
			  (let iloop ((x 1))
			    (if (<= x num-retpts)
				(begin 
				  (display " #")
				  (display x)
				  (display " ")
				  (if (not (zero? (- num-retpts x)))
				      (display ", "))
				  (iloop (+ x 1)))))
			  (display " |>  "))
			(let ((n (length (production:rhs p))))
			  (display "( sp := sp - ")
			  (display (- n 1))
			  (display " ; ")
			  (newline)
			  (display prefix)
			  (display "Stack[ sp ] = ")
			  (display (production:non-terminal p))
			  (display "; <| tsp | #")
			  (display (calculate-ret-pt 
				    (list-ref ret-pts-list state)
				    n))
			  (display " |> ) ")
			  (newline)))))))
			  
       (if (number? (car action))
	   (begin
	     (display "if ")
	     (display prefix)
	     (display "TS[ tsp ] = ")
	     (display (car action))
	     (display " then ")
	     (newline)))
       (cond ((eq? (cdr action)
		   '*error*)
	      (begin
		(display "(print(\"Error Unknown \"); exit(0); 0)")
	
		(newline)))
	     ((eq? (cdr action)
		   'accept)
	      (begin
		(display "<| 1 | #1 |>")
		(newline)))
	     ((negative? (cdr action))
	      (output-reduce))
	     (else
	      (output-shift (cdr action)))))
       (if (number? (car action))
	   (display " else ")
	   (newline)))
     at-entry)
    (newline)
    (display "end")
    (newline)))
		      

(define (sort l) 
  (if (< (length l) 2)
      l
      (letrec ((x (car l))
	       (insert 
		(lambda (x l lsf)
		  (cond 
		   ((null? l)
		    (reverse (cons x lsf)))
		   ((< x (car l))
		    (append (reverse (cons x lsf))
			    l))
		   (else
		    (insert x (cdr l) (cons (car l)
					    lsf)))))))
	(insert x (sort (cdr l)) '()))))
	
(define (stats rpl)
  (let loop ((hist'())
	     (i 0))
    (if ( < i (vector-length rpl))
	(let* ((num (length (vector-ref rpl i)))
	       (y (assoc num hist)))
	  (if y
	      (begin
		(set-cdr! y (+ (cdr y) 1))
		(loop hist (+ i 1)))
	      (loop (cons (cons num 1)
			  hist)
		    (+ i 1))))
	(begin 
	  (display i)
	  (newline)
	  hist))))

(define tmp #f)     
(define (mr-gen-lalr1 my-grammar file prefix)
  (gen-lalr1 my-grammar file 'tmp)
  (load file)
  
  (let* ((grammar (my-parse-gram my-grammar)) 
	 (ret-pts-list (vector->list (calculate-num-retpt grammar))))
		  
    
    (set! tmp ret-pts-list)
    (display (stats (list->vector ret-pts-list)))
    (newline)
    (display ret-pts-list)
    (newline)
    (with-output-to-file file
      (lambda ()
	(display "let ")
	(newline)
	(create-declarations prefix)
	(let iloop ((state 0))
	      (if (< state (length ret-pts-list))
		  (begin
		    (gen-state-code state
				    prefix
				    ret-pts-list
				    grammar)
		    (iloop (+ state 1)))))
	(newline)
	(display " in <|tigState0(tigStack, tigStackPtr, tigTS, tigTsPtr) | x => print(\"Finished \"); |> end ")))))
      
    
