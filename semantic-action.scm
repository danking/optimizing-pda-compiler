(define (null-action-evaluator action item-values)
  #t)

(define (scheme-action-evaluator action item-values)
  (apply (compile-action action (length item-values)) item-values))

(define (compile-action action num-args)
  (let loop ((n num-args) (param-list '()))
    (if (> n 0)
	(loop (- n 1) (cons (create-action-symbol n) param-list))
	(eval (list 'lambda param-list action)
	      (scheme-report-environment 5)))))

(define (create-action-symbol number)
  (let loop ((number number) (chars '()))
    (if (> number 0)
	(loop (quotient number 10)
	      (cons (vector-ref '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
				(modulo number 10))
		    chars))
	(string->symbol (list->string (cons #\$ chars))))))

(define (compile-actions program)
  (let* ((rules (LR-program:rules program))
	 (num-rules (vector-length rules)))
    (let loop ((i 0))
      (if (< i num-rules)
	  (let ((rule (vector-ref rules i)))
	    (set-LR-rule:action
	     rule
	     (compile-action (LR-rule:action rule)
			     (vector-length (LR-rule:right-side rule)))))))))
