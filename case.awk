#!/bin/awk -f 

/^function goto.*$/ { $0 = $0 "case (s[sp]) of" }
/^\(print\(" Error unknown"\); exit\(0\); 0\)$/ { $0 = "  default (print(\" Error unknown\"); exit(0); 0) end" }
/^if s\[sp\] = .* then $/ { $0 = "  " $4 " (" }
/^else if s\[sp\] = .* then $/ { $0 = ")  " $5 " (" }
/^else \(print\(" Error unknown"\); exit\(0\); 0\)$/ { $0 = ")  default (print(\" Error unknown\"); exit(0); 0) end" }

/^ in if tigerTS\[ tsp \] = .* then $/ { $0 = "in case (tigerTS[ tsp ]) of " $7 " (" }
/^\) else if tigerTS\[ tsp \] = .* then $/ { $0 = "))  " $8 " (" }
/^ else if tigerTS\[ tsp \] = .* then $/ { $0 = ")  " $7 " (" }
/^\) else \(print\("Error Unknown "\); exit\(0\); 0\)$/ { $0 = "))  default (print(\"Error Unknown \"); exit(0); 0) end" }
/^\) else \( sp := sp - .* ; $/ {$2 = ")default"}
/^tigerStack\[ sp \] := .*; <\| tsp \| \#.* \|> \)$/ {$0 = $0 " end"} 
/^ in \( sp := sp . .* ; *$/ {$0 = "in case (tigerTS[ tsp ]) of default ( sp := sp " $6 $7 ";"}
/^ else \( sp := sp . .* *; $/ {$1 = ")default" }
/^tigerStack\[ sp \] := .* ;<\| goto \(tsp\) \|  \#.*  \|>\)   else \( sp := sp . .*; $/ { $12 = ")default" }
/^tigerStack\[ sp \] := .* ;<\| goto \(tsp\) \|  \#.*  \|>\) *$/ { $0 = $0 " end" }
/^tigerStack\[ sp \] := .*; <\| tsp \| \#.* \|> \) *$/ { $0 = $0 " end" }
  {print $0}
