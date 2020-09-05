PROGRAM Counter
	VAR
		A : INT;
		Counter : INT := 5;
	END_VAR
	A := 0;
	A := Inc(In:=Counter, Out=>Counter) + Dec(In:=Counter, Out=>Counter);
	@ltlmc : L

END_PROGRAM

FUNCTION Inc : INT
	VAR_INPUT
		In : INT;
	END_VAR
	VAR_OUTPUT
		Out : INT;
	END_VAR

	Out := In + 1;
	Inc := In;

END_FUNCTION

FUNCTION Dec : INT
	VAR_INPUT
		In : INT;
	END_VAR
	VAR_OUTPUT
		Out : INT;
	END_VAR

	Out := In - 1;
	Dec := In;

END_FUNCTION

// krun -d build --ltlmc "[]Ltl (<>Ltl eqTo(Counter:::A, 7))" tests\counter.st
// krun -d build --ltlmc "[]Ltl (<>Ltl eqTo(Counter:::A, 11))" tests\counter.st

