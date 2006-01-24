:- module(cpp_parms,
	  [ compile/1,			% +CPP source
	    port/1			% Port to use for the server
	  ]).

:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(foreign, '..')).

compile(CPP) :-
	file_name_extension(Exe, _, CPP),
	compile(CPP, Exe).

%	compile(+C++-Source, -Executable)
%	
%	Call the C++ compiler.  The   path  -I../../../../include  finds
%	SWI-proxy.cpp and SWI-proxy.h in the   distribution.  -I.. finds
%	them in the package source.

compile(CPP, Exe) :-
	port(Port),
	current_prolog_flag(c_cc, gcc), !,
	sformat(Cmd,
		'g++ -DPORT=~w -I. -I.. -I../../../../include -o ~w ~w',
		[Port, Exe, CPP]),
	shell(Cmd).
compile(CPP, Exe) :-
	port(Port),
	current_prolog_flag(c_cc, cl), !, % Windows MSVC
	sformat(Cmd,
		'cl.exe /nologo /DPORT=~w /DWIN32 /GX \
		 /I. /I.. /I../../../include /I../../../../include \
		 -o ~w ~w wsock32.lib',
		[Port, Exe, CPP]),
%	writeln(Cmd),
	shell(Cmd).
	
port(5000).
