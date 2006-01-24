:- module(cpp_parms,
	  [ compile/1,			% +CPP source
	    port/1			% Port to use for the server
	  ]).

:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(foreign, '..')).

compile(CPP) :-
	file_name_extension(Exe, _, CPP),
	compile(CPP, Exe).

compile(CPP, Exe) :-
	port(Port),
	current_prolog_flag(c_cc, gcc), !,
	sformat(Cmd, 'g++ -DPORT=~w -I.. -I. -o ~w ~w', [Port, Exe, CPP]),
	shell(Cmd).
	
port(5000).
