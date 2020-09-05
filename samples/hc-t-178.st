PROGRAM temp
  VAR
    I1,I2,I3,I4:INT;
    B1,B2,B3:BOOL;
    R0,R2,R3,R4:REAL;
  END_VAR

  STACK_INT(PUSH:=B1,POP:=B2,R1:=B3,IN:=I1,N:=I2);


END_PROGRAM

FUNCTION_BLOCK STACK_INT
  VAR_INPUT PUSH, POP: BOOL R_EDGE;
            R1 : BOOL ;
            IN : INT ;
            N  : INT ;
  END_VAR
  VAR_OUTPUT EMPTY : BOOL := 1 ;
             OFLO  : BOOL := 0 ;
             OUT   : INT  := 0 ;
  END_VAR
  VAR STK : ARRAY[0..127] OF INT;
      NI : INT :=128  ;
      PTR : INT := -1 ;
  END_VAR

    IF R1 THEN
       OFLO := 0; EMPTY := 1; PTR := -1;
       NI := LIMIT (MN:=1,IN:=N,MX:=128); OUT := 0;
    ELSIF POP & NOT EMPTY THEN
       OFLO := 0; PTR := PTR-1; EMPTY := PTR < 0;
       IF EMPTY THEN OUT := 0;
       ELSE OUT := STK[PTR];
       END_IF ;
    ELSIF PUSH & NOT OFLO THEN
       EMPTY := 0; PTR := PTR+1; OFLO := (PTR = NI);
       IF NOT OFLO THEN OUT := IN ; STK[PTR] := IN;
       ELSE OUT := 0;
       END_IF ;
    END_IF ;
END_FUNCTION_BLOCK