/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Give online help
*/

:- module(online_help,
	[ help/1
	, help/0
	, apropos/1
	]).

:- use_module(library(help_index)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module  defines the  online  help  facility of   SWI-Prolog.   It
assumes  (Prolog) index file  at library(help_index)   and  the actual
manual  at library(online_manual).   Output  is piped through  a  user
defined pager, which defaults to `more'.

BUGS:
If the pager  is quit prematurely Prolog will  abort  on  noticing the
broken pipe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	help/0

help :-
	help(help/1).

%	help(+Subject)
%	Display online help on specified subject.

help(What) :-
	give_help(What).

%	apropos(Pattern)
%	Give a list of subjects that might be appropriate.

apropos(What) :-
	give_apropos(What).

give_help(Name/Arity) :- !,
	predicate(Name, Arity, _, From, To), !,
	show_help([From-To]).
give_help(Section) :-
	user_index(Index, Section), !,
	section(Index, _, From, To),
	show_help([From-To]).
give_help(Function) :-
	atom(Function),
	concat('PL_', _, Function),
	function(Function, From, To), !,
	show_help([From-To]).
give_help(Name) :-
	findall(From-To, predicate(Name, _, _, From, To), Ranges),
	Ranges \== [], !,
	show_help(Ranges).
give_help(What) :-
	format('No help available for ~w~n', What).

%	show_help(+ListOfRanges)
%	Pipe specified ranges of the manual through the user defined pager

:- dynamic asserted_help_tmp_file/1.

help_tmp_file(X) :-
	asserted_help_tmp_file(X), !.
help_tmp_file(X) :-
	tmp_file(manual, X),
	asserta(asserted_help_tmp_file(X)).

show_help(Ranges) :-
	current_predicate(_, running_under_emacs_interface),
	running_under_emacs_interface, !,
	online_manual_stream(Manual),
	help_tmp_file(Outfile),
	open(Outfile, write, Output),
	show_ranges(Ranges, Manual, Output),
	close(Manual),
	close(Output),
	call_emacs('(view-file-other-window "~w")', [Outfile]).
show_help(Ranges) :-
	'$arch'('PC', _), !,		% Windows or DOS; no pipes
	online_manual_stream(Manual),
	show_ranges(Ranges, Manual, user_output).
show_help([Start-End]) :-
	End - Start > 4000, !,
	find_manual(Manual),
	find_pager(Pager),
	sformat(Cmd, 'pl-bite ~d:~d ~a | ~a', [Start, End, Manual, Pager]),
	shell(Cmd).	
show_help(Ranges) :-
	online_manual_stream(Manual),
	pager_stream(Pager),
	show_ranges(Ranges, Manual, Pager),
	close(Manual),
	close(Pager).


show_ranges([], _, _) :- !.
show_ranges([From-To|Rest], Manual, Pager) :-
	stream_position(Manual, _, '$stream_position'(From, 0, 0)),
	Range is To - From,
	copy_chars(Range, Manual, Pager),
	nl(Pager),
	show_ranges(Rest, Manual, Pager).

copy_chars(0, _, _) :- !.
copy_chars(N, _, To) :-
	0 =:= N mod 4096,
	flush_output(To),
	fail.
copy_chars(N, From, To) :-
	get0(From, C),
	put_printable(To, C),
	NN is N - 1,
	copy_chars(NN, From, To).

put_printable(_, 12) :- !.
put_printable(_, -1) :- !.
put_printable(To, C) :-
	put(To, C).

online_manual_stream(Stream) :-
	find_manual(Manual),
	open(Manual, read, Stream).

pager_stream(Stream) :-
	find_pager(Pager),
	open(pipe(Pager), write, Stream).

find_manual(Path) :-
	'$chk_file'(library('MANUAL'), Path, [''], ['']).

find_pager(Pager) :-
	getenv('PAGER', Pager), !.
find_pager(more).

%	APROPOS

give_apropos(Atom) :-
	ignore(predicate_apropos(Atom)),
	ignore(function_apropos(Atom)),
	ignore(section_apropos(Atom)).

apropos_predicate(Pattern, Name, Arity, Summary) :-
	predicate(Name, Arity, Summary, _, _),
	(   apropos_match(Pattern, Name)
	->  true
	;   apropos_match(Pattern, Summary)
	).

predicate_apropos(Pattern) :-
	findall(Name-Arity-Summary,
		apropos_predicate(Pattern, Name, Arity, Summary),
		Names),
	Names \== [],
	forall(member(Name-Arity-Summary, Names),
			format('~w/~w~t~22|~w~n', [Name, Arity, Summary])).

function_apropos(Pattern) :-
	findall(Name, (function(Name, _, _),
		       apropos_match(Pattern, Name)), Names),
	Names \== [],
	forall(member(Name, Names),
			format('Interface Function~t~22|~w()~n', Name)).

section_apropos(Pattern) :-
	findall(Index-Name, (section(Index, Name, _, _),
		       apropos_match(Pattern, Name)), Names),
	Names \== [],
	forall(member(Index-Name, Names),
			(user_index(Index, UserIndex),
			format('Section ~w~t~22|"~w"~n', [UserIndex, Name]))).

apropos_match(A, B) :-
	'$apropos_match'(A, B).			% C defined for performance

user_index(List, Index) :-
	is_list(List), !,
	to_user_index(List, S),
	name(Index, S).
user_index(List, Index) :-
	to_system_index(Index, List).

to_user_index([], "").
to_user_index([A], S) :- !,
	name(A, S).
to_user_index([A|B], S) :-
	name(A, S0),
	append(S0, "-", S1),
	append(S1, Rest, S),
	to_user_index(B, Rest).

to_system_index(A-B, I) :- !,
	to_system_index(A, C),
	integer(B),
	append(C, [B], I).
to_system_index(A, [A]) :-
	integer(A).
