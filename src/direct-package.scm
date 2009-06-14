;;; Directly executable parser in Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main file.



;;; There's just helper functions for now


;; Turn assertions on or off
(define assertions #t)

(define-syntax assert
  (syntax-rules ()
    ;; assert : X X [X X -> Boolean] -> Boolean
    ;; Given a value and a predicate, tests if the predicate returns true for the value.
    ;; If assertions are off, or they are on and the assertion is valid, just return #t.
    ;; If assertions fails, print the actual and expected values for debugging.
    ((assert actual expected comparator)
     (if assertions
	 (let ((act-val actual)
	       (exp-val expected)
	       (comp comparator))
	   (if (comp act-val exp-val) #t
	       (begin (display "Assertion failed. Expected:") (newline)
		      (display (quote expected)) (newline)
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


(define-structure direct-parse
  (export)
  (open lalr)
  (files lalr-package pda-record make-parser))