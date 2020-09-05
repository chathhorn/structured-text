PROGRAM temp
  VAR
    I1,I2,I3,I4:INT;
    B1,B2,B3:BOOL;
    R1,R2,R3,R4:REAL;
  END_VAR
  RAMP(RUN:=B1,X0:=R1,X1:=R2,TR:=R3,CYCLE:=R4);

END_PROGRAM

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