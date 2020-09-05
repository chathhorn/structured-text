PROGRAM temp
  VAR
    I1,I2,I3,I4:INT;
    B1,B2,B3:BOOL;
    R1,R2,R3,R4:REAL;
  END_VAR

  I3:=WEIGH(weigh_command:=B1,gross_weight:=I1,tare_weight:=I2);


END_PROGRAM

FUNCTION WEIGH : INT
  VAR_INPUT
    weigh_command : BOOL;
    gross_weight : INT ;
    tare_weight : INT ;
  END_VAR

IF weigh_command THEN
  WEIGH := gross_weight - tare_weight;
END_IF ;
END_FUNCTION