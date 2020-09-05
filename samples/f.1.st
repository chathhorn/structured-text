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