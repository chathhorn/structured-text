// LTL: [] ( {not is_breaker_open and meter_va > thresh} -> () {threshold})
FUNCTION THRESHOLD : BOOL
      VAR_INPUT
            meter_va : INT;
            is_breaker_open : BOOL;
            thresh : INT;
      END_VAR

      IF not is_breaker_open and meter_va > thresh THEN
            THRESHOLD := TRUE;
      ELSE
            THRESHOLD := FALSE;
      END_IF;
END_FUNCTION

