(define-structure new (export cfg create-lalr-parser tiger-grammar)
 (open list-lib
       string-lib
       tables
       scheme-with-scsh
       defrec-package)
 (files cfg new tiger-cfg))
