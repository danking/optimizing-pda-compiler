(define-structure lalr (export cfg create-lalr-parser print-LR-program tiger-grammar)
 (open list-lib
       string-lib
       tables
       scheme-with-scsh
       defrec-package)
 (files cfg lalr utilities examples))
