#!@prefix@/bin/@PL@ -f none -g main -s

:- doc_collect(true).
:- use_module(library(pldoc/doc_library)).
:- use_module(library(lists)).

:- doc_load_library.

%%	main
%
%	Start the documentation server and wait. Does not provide access
%	to the toplevel, so it can be run as a background process.
%	
%	Start as
%	
%	    ==
%	    ./man_server.pl [--daemon] [--workers=N] [--port=Port] [--root=Path]
%	    ==

main :-
	current_prolog_flag(argv, Argv),
	append(_, [--|AV], Argv), !,
	start_server(AV),
	wait(AV).

start_server(Argv) :-
	av_option(port(Port), Argv, 8008),
	av_option(workers(Workers), Argv, 4),
	av_option(root(Root), Argv, '/pldoc'),
	assert(http:location(pldoc, Root, [priority(10)])),
	doc_server(Port,
		   [ workers(Workers)
		   ]).

wait(Argv) :-
	memberchk('--daemon', Argv), !,
	thread_get_message(_),
	halt.
wait(_).
	
av_option(Option, Argv, Default) :-
	Option =.. [Name,Value],
	(   format(atom(Prefix), '--~w=', [Name]),
	    member(Av, Argv),
	    atom_concat(Prefix, ValueAtom, Av),
	    atom_codes(ValueAtom, Codes),
	    name(Value0, Codes)
	->  Value = Value0
	;   Value = Default
	).
