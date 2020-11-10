VAR_GLOBAL
      PumpState : Int;
      StateDescription : String;
END_VAR

// Duplicated for including example of VAR_ACCESS syntax.
VAR_ACCESS
      PumpState_A : Int;
      StateDescription_A : String;
END_VAR

PROGRAM pumps
      (* Translate the pump state into a text string *)
      CASE PumpState OF
      0:
            StateDescription := "Stopped";
            PumpState := 1;
      1:
            StateDescription := "Running";
            PumpState := 2;
      2:
            StateDescription := "Failed";
            PumpState := 3;
      3..9:
            StateDescription := "Another case";
      ELSE
            StateDescription := "Invalid";
      END_CASE;
END_PROGRAM
