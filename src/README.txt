This directory contains a LALR(1) parser written in Scheme.  The following files
are relevant:

- lalr-package.scm
  Contains the package declarations for everything.  Look here if you want to
  see all the exported macros.
- lalr.scm
  This file contains the LALR(1) parser generator algorithm.  This is where
  all of the complex code is.
- cfg.scm
  This file defines the CFG->PDA macros.  It is basically a front-end to
  lalr.scm.  At one point it converted an sexp into a record-based AST format
  but that is now gone.  However, the separations of the files remains.
- semantic-action.scm
  This file contains code to covert a CFG with embeded-Scheme semantic actions
  to semantic actions that are wrapped in LAMBDAs.  It defines the
  COMPILE-ACTIONS macros.
- engine.scm
  This file defines the PDA to Scheme compiler in the form of the PARSE/PDA
  macro.
- macro-glue.scm
  This file defines a few high-level macros that are very useful.  In fact,
  these are the ones called in practice.  They combine several steps at once.

examples.scm contains a simple calculator that illustrates some of the features
of this program.  What follows are the steps required to load it:

  Welcome to scsh 0.6.4 (Olin Shivers)
  Type ,? for help.
  > ,config ,load lalr-package.scm
  lalr-package.scm
  > ,open lalr
  Load structure lalr (y/n)? y
  [scheme-with-scsh]
  [lalr-temp[lalr-macro engine.scm cfg.scm lalr.scm semantic-action.scm]
  [((for-syntax #f))]
  ]
  [lalr macro-glue.scm]
  > ,load examples.scm
  examples.scm

  ,config ,load lalr-package.scm
  ,open lalr
  ,load examples.scm

In examples.scm, the variable CALCULATOR holds the parser program.  Some sample
uses of this are:

  > (calculator '(3 + 5 - 2 * 3))
  2
  ; 3 values
  #t
  '(2)
  '()
  > (calculator '(#\( 2 + 7 #\) * #\( #\( 11 - 1 #\) #\) #\; + 5 5 #\; 1234567890 ))
  90
  Parse error in state: s2 on token: +
  1234567890
  ; 3 values
  #f
  '(1234567890 #{Unspecific} 90)
  '()

Note the generated parser returns 3 values.  The first is a boolean indicating
whether or not the parser was successful.  The second is the return value of the
last semantic action.  The final value is the left-over stream (always null in
this case).

For more information on how this whole thing works, see the paper.
