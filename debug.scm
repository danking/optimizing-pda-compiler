(define (border-string)
  (display '(list the-terminals the-nonterminals rlhs rrhs ritem action-table acces-symbol shift-table))
  (newline)
  '())

(define (old->new old)
  (let* ((the-terminals (first old))
	 (the-nonterminals (second old))
	 (rule-lhs (third old))
	 (rule-rhs (fourth old))
	 (rule-items (fifth old))
	 (action-table (sixth old))
	 (access-table (seventh old))
	 (shift-table (eighth old))

	 (num-nonterminals (vector-length the-nonterminals))
	 (terminals (vector->list the-terminals))
	 (symbol-map (list->vector (append (vector->list the-nonterminals) terminals)))
	 (rules (make-vector (- (vector-length rule-lhs) 1)))
	 (states (make-vector (vector-length action-table))))

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
				 (vector-ref symbol-map
					     (vector-ref rule-items
							 (+ i bottom-item))))
		    (item-loop (+ i 1)))))
	    (vector-set! rules (- rule-num 1)
			 (make-LR-rule (vector-ref symbol-map
						   (vector-ref rule-lhs
							       rule-num))
				       right-side
				       #f))
	    (rule-loop (- bottom-item 1)))))

    (let state-loop ((state-num 0))
      (if (< state-num (vector-length states))
	  (let ((state (make-LR-state #f #f #f)))
	    (set-LR-state:shift-reduce-table
	     state
	     (reverse (map (lambda (action)
		    (let ((sym (if (eq? (car action) 'default)
				   #t
				   (vector-ref symbol-map (+ (car action) num-nonterminals))))
			  (action (cond ((eq? (cdr action) 'accept)
					 (list 'accept))
					((< (cdr action) 0)
					 (list 'reduce (- (+ (cdr action) 1))))
					(else
					 (list 'shift (cdr action))))))
		      (cons sym action)))
		  (remove (lambda (action) (eq? (cdr action) '*error*))
			  (vector-ref action-table state-num)))))

	    (let goto-loop ((shifts (if (vector-ref shift-table state-num) (vector-ref (vector-ref shift-table state-num) 2) '()))
			    (out-gotos '()))
	      (cond ((null? shifts)
		     (set-LR-state:goto-table state (reverse out-gotos)))
		    ((< (vector-ref access-table (car shifts)) num-nonterminals)
		     (goto-loop (cdr shifts)
				(cons (list (vector-ref symbol-map (vector-ref access-table (car shifts)))
					    'goto (car shifts))
				      out-gotos)))
		    (else
		     (goto-loop (cdr shifts) out-gotos))))
	    
	    (vector-set! states state-num state)
	    (state-loop (+ state-num 1)))))

    (make-LR-program (cdr terminals) '*EOI* #f rules states)))

(define (compare-programs A B)
  (let* ((A-rules (LR-program:rules A))
	 (B-rules (LR-program:rules B))
	 (A-states (LR-program:states A))
	 (B-states (LR-program:states B))
	 (A->B-state (make-vector (vector-length A-states) #f))
	 (B->A-state (make-vector (vector-length B-states) #f))
	 (dual (lambda (A-state) (cons A-state (let ((x (vector-ref A->B-state A-state))) (if x x '?))))))

    ; Compare the terminals
    (let loop ((A (LR-program:terminals A)) (B (LR-program:terminals B)))
      (cond ((null? A)
	     (if (null? B)
		 #f ; continue
		 (error "The terminal lists are not equal.")))
	    ((member (car A) B)
	     (loop (cdr A) (delete (car A) B)))
	    (else
	     (error "The terminal lists are not equal."))))

    ; Compare the EOI symbols
    (if (not (eq? (LR-program:eoi A) (LR-program:eoi B)))
	(error "The EOI symbols are not equal."))

    ; State mapping
    (vector-set! A->B-state 0 0)
    (vector-set! B->A-state 0 0)
    (let state-loop ((state-stack '(0)))
      (if (pair? state-stack)
	  (let* ((state-num (car state-stack))
		 (A-state (vector-ref A-states state-num))
		 (B-state (vector-ref B-states (vector-ref A->B-state state-num))))
	    (let sr-loop ((state-stack (cdr state-stack))
			  (A-actions (append (LR-state:shift-reduce-table A-state) (LR-state:goto-table A-state)))
			  (B-actions (append (LR-state:shift-reduce-table B-state) (LR-state:goto-table B-state))))
	      (if (null? A-actions)
		  (let extra-loop ((B-actions B-actions))
		    (if (pair? B-actions)
			(begin
			  (difference (dual state-num) (caar B-actions) "nothing" (cdar B-actions))
			  (extra-loop (cdr B-actions)))
			(state-loop state-stack)))
		  (let* ((A-action (car A-actions))
			 (B-action (assoc (car A-action) B-actions))
			 (new-states (cond ((eq? B-action #f)
					    (difference (dual state-num) (car A-action) (cdr A-action) "nothing")
					    '())
					   ((not (eq? (cadr A-action) (cadr B-action)))
					    (difference (dual state-num) (car A-action) (cdr A-action) (cdr B-action))
					    '())
					   ((eq? (cadr A-action) 'accept)
					    '())
					   ((eq? (cadr A-action) 'reduce)
					    (if (not (= (caddr A-action) (caddr B-action)))
						(difference (dual state-num) (car A-action) (cdr A-action) (cdr B-action)))
					    '())
					   ((eq? (vector-ref A->B-state (caddr A-action)) #f)
					    (vector-set! A->B-state (caddr A-action) (caddr B-action))
					    (vector-set! B->A-state (caddr B-action) (caddr A-action))
					    (list (caddr A-action)))
					   ((= (vector-ref A->B-state (caddr A-action)) (caddr B-action))
					    '())
					   (else
					    (difference (dual state-num) (car A-action) (list 'shift (dual (caddr A-action)))
							(list 'shift (dual (vector-ref B->A-state (caddr B-action)))))
					    '()))))
		    (sr-loop (append new-states state-stack) (cdr A-actions) (delete B-action B-actions))))))))))

(define (difference state symbol A B)
  (display "State: ") (display state)
  (display "  Sym: ") (display symbol)
  (display "  A: ") (display A)
  (display "  B: ") (display B)
  (newline))