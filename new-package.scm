(define-structure new (export cfg create-lalr-parser tiger-grammar)
 (open list-lib
       string-lib
       scheme-with-scsh
       defrec-package)
 (files cfg tiger-cfg new))
