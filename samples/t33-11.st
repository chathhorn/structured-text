FUNCTION_BLOCK AND_EDGE
	VAR_INPUT X : BOOL; Y : BOOL; END_VAR
    VAR X_TRIG : R_TRIG ; Y_TRIG : F_TRIG ; END_VAR
    
    X_TRIG(CLK := X) ;
	Y_TRIG(CLK := Y) ;
    Z := X_TRIG.Q AND Y_TRIG.Q;
END_FUNCTION_BLOCK
