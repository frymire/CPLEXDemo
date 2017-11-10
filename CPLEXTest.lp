\ENCODING=ISO-8859-1
\Problem name: ilog.cplex

Maximize
 obj: x0 + 2 x1 + 3 x2 + x3 - 7 y
Subject To
 c1: - x0 + x1 + x2 + 10 x3 <= 20
 c2: x0 - 3 x1 + x2 <= 30
 c3: x1 - 3.5 x3  = 0
Bounds
 0 <= x0 <= 40
 2 <= x3 <= 3
 0 <= y <= 1
Binaries
 y 
Generals
 x3 
End
