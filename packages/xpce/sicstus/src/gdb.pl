:- require([ pid/1,
	     shell/1,
	     format_to_chars/3
	   ]).

gdb :-
	pid(Pid),
	format_to_chars('xgdb /usr/local/lib/sicstus-3.7.1/bin/sp.exe ~w &',
			[Pid], CmdChars),
	atom_chars(Cmd, CmdChars),
	shell(Cmd).
