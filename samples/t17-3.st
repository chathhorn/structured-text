PROGRAM Test
	VAR_GLOBAL
 		LIM_SW_S5 AT %IX27 : BOOL;
 		CONV_START AT %QX25 : BOOL;
 		TEMPERATURE AT %IW28: INT;
 		C2 AT %Q* : BYTE ;
	END_VAR
	VAR_ACCESS
 		LIM_SW_S5_A : BOOL;
 		CONV_START_A : BOOL;
 		TEMPERATURE_A : INT;
	END_VAR
END_PROGRAM
