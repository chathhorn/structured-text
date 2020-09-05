PROGRAM Fibonacci
  VAR_INPUT
    N : INT := 1;
  END_VAR
  VAR_OUTPUT
	O : INT := 1;
  END_VAR
  VAR
    F1, F2, CNT : INT;
  END_VAR

  F1 := 1;
  F2 := 1;
  CNT := 2;

  IF N <= CNT THEN
    O := 1;
  ELSE
    WHILE CNT < N DO
      O := AddTwo(In0:=F1, In1:=F2);
      F1 := F2;
      F2 := O;
      CNT := CNT + 1;
    END_WHILE;
  END_IF;

END_PROGRAM

FUNCTION AddTwo : INT
  VAR_INPUT
    In0, In1 : INT;
  END_VAR

  AddTwo := In0 + In1;
END_FUNCTION