FUNCTION_BLOCK DERIVATIVE
   VAR_INPUT
     RUN : BOOL ;
     XIN : REAL ;
     CYCLE : REAL ;
   END_VAR
   VAR_OUTPUT
     XOUT : REAL ;
   END_VAR
   VAR X1, X2, X3 : REAL ; END_VAR
   IF RUN THEN
      XOUT := (3.0 * (XIN - X3) + X1 - X2)
              / (10.0 * CYCLE) ;
      X3 := X2 ; X2 := X1 ; X1 := XIN ;
   ELSE XOUT := 0.0; X1 := XIN ; X2 := XIN ; X3 := XIN ;
   END_IF ;
 END_FUNCTION_BLOCK