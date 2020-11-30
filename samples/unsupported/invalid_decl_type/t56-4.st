FUNCTION Test : BOOL
	VAR_INPUT
		A, B, C : REAL;
	END_VAR
	VAR_OUTPUT
		D, X1, X2 : REAL;
		NROOTS : INT;
	END_VAR

	D := B*B - 4*A*C ;
	IF D < 0.0 THEN NROOTS := 0 ;
	ELSIF D = 0.0 THEN
		NROOTS := 1 ;
		X1 := - B/(2.0*A) ;
	ELSE
		NROOTS := 2 ;
		X1 := (- B + SQRT(D))/(2.0*A) ;
		X2 := (- B - SQRT(D))/(2.0*A) ;
	END_IF ;
END_FUNCTION