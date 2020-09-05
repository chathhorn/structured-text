FUNCTION_BLOCK CMD_MONITOR
 VAR_INPUT AUTO_CMD : BOOL ;
          AUTO_MODE : BOOL ;
            MAN_CMD : BOOL ;
        MAN_CMD_CHK : BOOL ;
          T_CMD_MAX : REAL ;
               FDBK : BOOL ;
                ACK : BOOL ;
 END_VAR
 VAR_OUTPUT CMD : BOOL ;
           ALRM : BOOL ;
 END_VAR
 VAR CMD_TMR : TON ;
     ALRM_FF : SR ;
 END_VAR


 CMD := AUTO_CMD & AUTO_MODE
          OR MAN_CMD & NOT MAN_CMD_CHK & NOT AUTO_MODE ;
    CMD_TMR (IN := CMD, PT := T_CMD_MAX);
    ALRM_FF (S1 := CMD_TMR.Q & NOT FDBK, R := ACK);
    ALRM := ALRM_FF.Q1;
    
END_FUNCTION_BLOCK