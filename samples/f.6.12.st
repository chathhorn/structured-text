 FUNCTION_BLOCK RAMP
   VAR_INPUT
     RUN : BOOL ;
     X0,X1 : REAL ;
     TR : REAL ;
     CYCLE : REAL ;
   END_VAR
   VAR_OUTPUT
     BUSY : BOOL ;
     XOUT : REAL := 0.0 ;
   END_VAR
   VAR XI : REAL ;
       T : REAL := 0.0;
   END_VAR
   BUSY := RUN ;
   IF RUN THEN
      IF T >= TR THEN BUSY := 0 ; XOUT := X1 ;
      ELSE XOUT := XI + (X1-XI) * T
                                / TR ;
           T := T + CYCLE ;
      END_IF ;
   ELSE XOUT := X0 ; XI := X0 ; T := 0.0 ;
   END_IF ;
 END_FUNCTION_BLOCK