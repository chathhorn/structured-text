FUNCTION_BLOCK F_TRIG
	VAR_INPUT
		CU, CD, R, LD: BOOL;
		PV : INT;
	END_VAR
	VAR_OUT
		CV : INT;
		QU, QD : BOOL;
	END_VAR;
	VAR
		PVmax, PVmin : INT;
	END_VAR
	IF R THEN CV := 0 ;
	ELSIF LD THEN CV := PV ;
	ELSE
		IF NOT (CU AND CD) THEN
		    IF CU AND (CV < PVmax)
		    THEN CV := CV+1;
		    ELSIF CD AND (CV > PVmin)
		    THEN CV := CV-1;
		    END_IF;
		END_IF;
	END_IF ;
	QU := (CV >= PV) ;
	QD := (CV <= 0) ;
END_FUNCTION_BLOCK