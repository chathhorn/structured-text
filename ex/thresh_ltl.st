VAR_GLOBAL
      meter_va     : INT;
      breaker_open : BOOL;
      threshold    : INT;
END_VAR

// LTL thresh: [] ( {breaker_open OR meter_va > threshold} -> () {breaker_open})
PROGRAM
      IF breaker_open OR meter_va > threshold THEN
            breaker_open := TRUE;
      ELSE
            breaker_open := FALSE;
      END_IF;
END_PROGRAM
