(define cfg->pda
  (lambda (form rename compare)
    (if (not (null? (cdr form)))
	(error "cfg->pda only accepts one argument"))
    (let loop ((terms '()) (prec '()) (err #f) (eos #f) (eop #f) (no-shift #f)
	       (start #f) (nonterms '()) (form (car form)))
      (cond ((null? form)
	     (create-lalr-parser (reverse terms) (reverse prec) err eos
				 (cond (eop eop) (eos (list eos)) (else '()))
				 (cond (no-shift no-shift) (eos (list eos))
				       (else '()))
				 start (reverse nonterms)))
	    ((not (pair? form))
	     (error "CFG form must be a list."))
	    ((not (list? (car form)))
	     (error "CFG elements must be keyword-delimited lists."))
	    ((eq? (caar form) 'comment)
	     (loop terms prec err eos eop no-shift start nonterms (cdr form)))
	    ((eq? (caar form) 'no-shift)
	     (loop terms prec err eos eop (if no-shift
					      (append no-shift (cdar form))
					      (cdar form))
		   start nonterms (cdr form)))
	    ((eq? (caar form) 'end-of-parse)
	     (loop terms prec err eos (if eop
					  (append eop (cdar form))
					  (cdar form))
		   no-shift start nonterms (cdr form)))
	    ((eq? (caar form) 'tokens)
	     (let token-loop ((terms terms) (prec prec) (err err) (eos eos)
			      (token-decs (cdar form)))
	       (cond ((null? token-decs)
		      (loop terms prec err eos eop no-shift start nonterms
			    (cdr form)))
		     ((not (list? (car token-decs)))
		      (token-loop (cons (car token-decs) terms) prec err eos
				  (cdr token-decs)))
		     ((eq? (caar token-decs) 'comment)
		      (token-loop terms prec err eos (cdr token-decs)))
		     ((eq? (caar token-decs) 'error)
		      (cond ((not (= (length (car token-decs)) 2))
			     (error "Keyword error takes 1 argument"))
			    (err
			     (error "Error keyword redeclared"))
			    (else
			     (token-loop (cons (cadar token-decs) terms) prec
					 (cadar token-decs) eos
					 (cdr token-decs)))))
		     ((eq? (caar token-decs) 'eos)
		      (cond ((not (= (length (car token-decs)) 2))
			     (error "Keyword eos takes 1 argument"))
			    (eos
			     (error "EOS token redeclared"))
			    (else
			     (token-loop (cons (cadar token-decs) terms) prec
					 err (cadar token-decs)
					 (cdr token-decs)))))
		     ((member (caar token-decs) '(left right non))
		      (if (= (length (car token-decs)) 1)
			  (token-loop terms prec err eos (cdr token-decs))
			  (token-loop (append (reverse (cdar token-decs)) terms)
				      (cons (car token-decs) prec) err eos
				      (cdr token-decs))))
		     (else
		      (error "Unknown keyword:" (caar token-decs))))))
	    ((eq? (caar form) 'non-term)
	     (if (< (length (car form)) 3)
		 (error "Non-term declaration must include at least one rule."))
	     (if (not (symbol? (cadar form)))
		 (error "Non-terminal name must by a symbol."))
	     (loop terms prec err eos eop no-shift (if start start (cadar form))
		   (cons (cons (cadar form)
			       (fold-right
				(lambda (element result)
				  (cond ((not (list? element))
					 (error
					  "non-term clauses must be lists"))
					((eq? (car element) '=>)
					 (cons (cdr element) result))
					((eq? (car element) 'comment)
					 result)
					(else
					 (error "Unknown keyword:"
						(caar element)))))
				'() (cddar form)))
			 nonterms)
		   (cdr form)))
	    (else
	     (error "Unknown keyword:" (caar form)))))))

(define (temp-func terms prec err eos eop no-shift start nonterms)
  (display "Terms: ") (display terms) (newline)
  (display "Prec: ") (display prec) (newline)
  (display "Err: ") (display err) (newline)
  (display "EOS: ") (display eos) (newline)
  (display "EOP: ") (display eop) (newline)
  (display "No-Shift: ") (display no-shift) (newline)
  (display "Start: ") (display start) (newline)
  (display "NonTerms: ") (display nonterms) (newline))