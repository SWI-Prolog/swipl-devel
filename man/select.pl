:- use_module(library(dcg/basics)).

:- initialization
	(   catch(main, E,
		  ( print_message(error, E),
		    fail
		  ))
	->  halt
	;   halt(1)
	).

main :-
	current_prolog_flag(argv, [Def,File]),
	setup_call_cleanup(
	    open(File, read, In),
	    (	read_line_to_codes(In, Line),
		process(Line, In, Def, [])
	    ),
	    close(In)).

process(end_of_file, _, _, State) :- !,
	assertion(State == []).
process(Line, In, DefA, State) :-
	phrase(("#ifdef", whites, string(Def), whites), Line), !,
	(   atom_codes(DefA, Def)
	->  Cond = true
	;   Cond = false
	),
	read_line_to_codes(In, Line2),
	process(Line2, In, DefA, [Cond|State]).
process(Line, In, DefA, State) :-
	phrase(("#ifndef", whites, string(Def), whites), Line), !,
	(   atom_codes(DefA, Def)
	->  Cond = false
	;   Cond = true
	),
	read_line_to_codes(In, Line2),
	process(Line2, In, DefA, [Cond|State]).
process(Line, In, Def, State) :-
	phrase(("#else", whites), Line), !,
	read_line_to_codes(In, Line2),
	State = [Old|State2],
	(   Old == true
	->  New = false
	;   New = true
	),
	process(Line2, In, Def, [New|State2]).
process(Line, In, Def, State) :-
	phrase(("#endif", whites), Line), !,
	read_line_to_codes(In, Line2),
	State = [_|State2],
	process(Line2, In, Def, State2).
process(_Line, In, Def, State) :-
	State = [false|_], !,
	read_line_to_codes(In, Line2),
	process(Line2, In, Def, State).
process(Line, In, Def, State) :-
	format('~s~n', [Line]),
	read_line_to_codes(In, Line2),
	process(Line2, In, Def, State).

