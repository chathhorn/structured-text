FUNCTION_BLOCK FWD_REV_MON
VAR_INPUT AUTO : BOOL ;
  ACK : BOOL ;        
  AUTO_FWD : BOOL ;    
  MAN_FWD : BOOL ;     
  MAN_FWD_CHK : BOOL ;
  T_FWD_MAX : TIME ;  
  FWD_FDBK : BOOL ;   
                       
  AUTO_REV : BOOL ;   
  MAN_REV : BOOL ;     
  MAN_REV_CHK : BOOL ; 
  T_REV_MAX : TIME ;  
  REV_FDBK : BOOL ;    
END_VAR                
VAR_OUTPUT KLAXON : BOOL ;  
  FWD_REV_ALRM : BOOL;
  FWD_CMD : BOOL ;     
  FWD_ALRM : BOOL ;    
  REV_CMD : BOOL ;    
  REV_ALRM : BOOL ;   
END_VAR
VAR FWD_MON : CMD_MONITOR; 
  REV_MON : CMD_MONITOR;   
  FWD_REV_FF : SR ;      
END_VAR

  FWD_MON    (AUTO_MODE   := AUTO,
              ACK         := ACK,
              AUTO_CMD    := AUTO_FWD,
              MAN_CMD     := MAN_FWD,
              MAN_CMD_CHK := MAN_FWD_CHK,
              T_CMD_MAX   := T_FWD_MAX,
              FDBK        := FWD_FDBK);
  REV_MON    (AUTO_MODE   := AUTO,
              ACK         := ACK,
              AUTO_CMD    := AUTO_REV,
              MAN_CMD     := MAN_REV,
              MAN_CMD_CHK := MAN_REV_CHK,
              T_CMD_MAX   := T_REV_MAX,
              FDBK        := REV_FDBK);
  FWD_REV_FF (S1 := FWD_MON.CMD & REV_MON.CMD, R := ACK);

  FWD_REV_ALRM := FWD_REV_FF.Q1;
  FWD_CMD := FWD_MON.CMD & NOT FWD_REV_ALRM;
  FWD_ALRM := FWD_MON.ALRM;
  REV_CMD := REV_MON.CMD & NOT FWD_REV_ALRM;
  REV_ALRM := REV_MON.ALRM;
  KLAXON := FWD_ALRM OR REV_ALRM OR FWD_REV_ALRM;
END_FUNCTION_BLOCK