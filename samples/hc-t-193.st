PROGRAM temp
  VAR
    P1,P2:INT;
    P3:REAL;
    P4:BOOL;
  END_VAR

  DIFFEQ(RUN:=P4,XIN:=P3);

END_PROGRAM

FUNCTION_BLOCK DIFFEQ
   VAR_INPUT
     RUN : BOOL ;
     XIN : REAL ;
     A : ARRAY[1..127] OF REAL ;
     M : INT ;
     B : ARRAY[0..127] OF REAL ;
     N : INT ;
   END_VAR
   VAR_OUTPUT XOUT : REAL := 0.0 ; END_VAR
   VAR
     XI : ARRAY [0..127] OF REAL ;
     XO : ARRAY [0..127] OF REAL ;
     I : INT ;
   END_VAR
   XO[0] := XOUT ; XI[0] := XIN ;
   XOUT := B[0] * XIN ;
   IF RUN THEN
      FOR I := M TO 1 BY -1 DO
          XOUT := XOUT + A[I] * XO[I] ; XO[I] := XO[I-1];
      END_FOR;
      FOR I := N TO 1 BY -1 DO
          XOUT := XOUT + B[I] * XI[I] ; XI[I] := XI[I-1];
      END_FOR;
   ELSE
      FOR I := 1 TO M DO XO[I] := 0.0; END_FOR;
      FOR I := 1 TO N DO XI[I] := 0.0; END_FOR;
   END_IF ;
 END_FUNCTION_BLOCK