VAR_GLOBAL
      PumpState : Int;
      StateDescription : String;
END_VAR

PROGRAM pumps
      (* Translate the pump state into a text string *)
      CASE PumpState OF
      0:
            StateDescription := "Stopped";
      1:
            StateDescription := "Running";
      2:
            StateDescription := "Failed";
      ELSE
            StateDescription := L"Invalid";
      END_CASE;
END_PROGRAM
