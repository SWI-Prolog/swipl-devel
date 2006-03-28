:- module(unicode_derived_core_properties,
	  [ unicode_derived_core_property/2
	  ]).

:- dynamic
	derived_property/2,
	loaded/0.

%	unicode_derived_core_property(?Code, ?Prop)
%	
%	

unicode_derived_core_property(Code, Prop) :-
	loaded, !,
	derived_property(Code, Prop).
unicode_derived_core_property(Code, Prop) :-
	retractall(derived_property(_,_)),
	process_file,
	assert(loaded),
	derived_property(Code, Prop).

process_file :-
	process_file('DerivedCoreProperties.txt').

process_file(File) :-
	open(File, read, In),
	call_cleanup(process_stream(In), close(In)).

process_stream(In) :-
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  true
	;   process_line(Line),
	    process_stream(In)
	).

process_line(Line) :-
	debug(unicode_data, 'Line "~s"', [Line]),
	phrase(line(Codes, Class), Line),
	forall(member(C, Codes),
	       assert(derived_property(C, Class))).
	


line([], -) -->
	ws, "#", skip_rest, !.
line([], -) -->
	ws, !.
line(Codes, Class) -->
	ucc(First),
	(   ".."
	->  ucc(Last),
	    { numlist(First, Last, Codes) }
	;   { Codes = [First] }
	),
	ws, ";", ws, 
	class(Class),
	ws,
	"#",
	skip_rest.
	      
class(Class) -->
	identifier(Id),
	{ downcase_atom(Id, Class) }.

identifier(Word) -->
	[C0], { code_type(C0, csymf) },
	csyms(Cs),
	{ atom_codes(Word, [C0|Cs]) }.

csyms([H|T]) -->
	[H], { code_type(H, csym) }, !,
	csyms(T).
csyms([]) -->
	[].

ucc(Val) -->
	hex_digit(D0),
	hex_digit(D1),
	hex_digit(D2),
	hex_digit(D3),
	{ Val0 is D0<<12 + D1<<8 + D2<<4 + D3 },
	xucc(Val0, Val).

xucc(Val0, Val) -->
	hex_digit(D), !,
	{ Val1 is Val0<<4 + D },
	xucc(Val1, Val).
xucc(Val, Val) -->
	[].

hex_digit(D) -->
	[C],
	{ code_type(C, xdigit(D)) }.

w -->
	[C],
	{ code_type(C, white) }.

ws -->
	w, !,
	ws.
ws -->
	[].

skip_rest(_, []).
eol -->
	[10].
