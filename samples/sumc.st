PROGRAM Sumc
	VAR
		n, i, s : INT;
	END_VAR

	n := 100;
	WHILE i < n DO
		i := i + 1;
		s := s + i;
		// @ltlmc : L
	END_WHILE;
END_PROGRAM

