/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(pce_qux, [
	  atom_to_term/2,
	  atom_to_term/3,
	  term_to_atom/2,
	  source_location/2,
	  pce_error/1,
	  pce_warn/1,
	  pce_info/1,
	  ignore/1,
	  auto_call/1,
	  concat/3,
	  free_variables/2,
	  atom_length/2,
	  time_file/2,
	  strip_module/3,
	  pce_arg/3,
	  sublist/3,
	  exists_file/1,
	  make/0,
	  source_warning/2,
	  sformat/3,
	  is_absolute_file_name/1,
	  file_directory_name/2,
	  file_base_name/2,
	  file_name_extension/3,
	  expand_file_name/2,
	  callable_predicate/1,
	  modified_since_last_loaded/1,
	  xpce_loop/0
	]).

:- meta_predicate
	ignore(:),
	auto_call(:),
	defined_predicate(:),
	sublist(:, +, ?).

:- op(100, fx, @).

:- use_module(library(charsio), [chars_to_stream/2,
				 with_input_from_chars/2,
				 with_output_to_chars/2]).
:- use_module(library(between), [between/3]).
:- use_module(library(files), [file_exists/1]).
:- use_module(library(strings), [concat_atom/2, string_append/3]).
:- use_module(library(freevars), [free_variables/4]).
:- use_module(library(arg), [genarg/3]).
:- use_module(library(call), [call/2]).
:- use_module(library(basics), [member/2]).

% strip_module(+RawTerm, -Term, -Module).
%
% If a term is of the form Module:Term, strip of the module part,
% return the plain term in Term and the Module in Module.

strip_module(RT, M, T) :-
	strip_module(RT, T, M, user).

strip_module(Module:RT2, T, M, _) :-
	atom(Module), !,
	strip_module(RT2, T, M, Module).
strip_module(T, T, M, M).

% source_location(-Path, -LineNo)
%
% Unify Path and LineNo with the filename and line number of the
% location where the last term has been read.  Used inside
% term_expansion.

source_location(File, Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position,
		'$stream_position'(_, Line, _, _, _)).

%	is_absolute_file_name(+Atom)
%	Succeeds if Atom describes an absolute filename

is_absolute_file_name(Atom) :-
	atom_chars(Atom, [0'/|_]).

%	file_directory_name(+Path, +Dir)
%	Finds directory-name from path-name

file_directory_name(Path, Dir) :-
	atom_chars(Path, PathChars),
	(   append(DirChars, BaseChars, PathChars),
	    \+ member(0'/, BaseChars),
	    BaseChars \== []
	->  atom_chars(Dir, DirChars)
	;   PathChars = [0'/|_]
	->  Dir = /
	;   Dir = ''
	).

file_base_name(Path, Base) :-
	atom_chars(Path, PathChars),
	(   append(_, BaseChars, PathChars),
	    \+ member(0'/, BaseChars),
	    BaseChars \== []
	->  atom_chars(Base, BaseChars)
	;   Base = Path
	).

%	file_name_extension(?Base, ?Ext, ?Path).
%
%	BUG: should take care of case-insensitive and other OS
%	dependencies.  This port of the SWI-Prolog predicate only
%	works on Unix.

file_name_extension(Base, DotExt, Path) :-
	nonvar(DotExt),
	atom_chars(DotExt, [0'.|EC]), !,
	atom_chars(Ext, EC),
	file_name_extension2(Base, Ext, Path).
file_name_extension(Base, Ext, Path) :-
	file_name_extension2(Base, Ext, Path).
	
file_name_extension2(Base, Ext, Path) :-	% -, -, +
	nonvar(Path), !,
	(   atom_chars(Path, PC),
	    append(BC, [0'.|EC], PC),
	    \+ member(0'/, EC),
	    \+ member(0'., EC)
	->  atom_chars(Base, BC),
	    atom_chars(Ext, EC)
	;   Ext = '',
	    Base = Path
	).
file_name_extension2(Base, Ext, Path) :- % +, +, -
	nonvar(Base),
	nonvar(Ext), !,
	atom_chars(Base, BC),
	atom_chars(Ext, EC),
	(   append(_, [0'.|EC], BC)
	->  Path = Base
	;   append(BC, [0'.|EC], PC),
	    atom_chars(Path, PC)
	).


% term_to_atom(+Term, ?Atom)
% term_to_atom(-Term, +Atom)
%
% If Term may be a variable, if Atom is an atom.
 
term_to_atom(Term, Atom) :-
        ( var(Term) ->
                atom_to_term(Atom, Term)
        ; copy_term(Term, TempTerm),
          numbervars(TempTerm, 0, _),
          with_output_to_chars(print(TempTerm), Chars),
          atom_chars(Atom, Chars)
        ).

% atom_to_term(+Atom, -Term)
% atom_to_term(+Atom, -Term, -Bindings)
%
% Convert an atom to a term, possibly saving original variable names.
% Fails if 1st argument is not an atom.
 
atom_to_term(Atom, Term) :-
        atom_to_term(Atom, Term, _).
 
atom_to_term(Atom, Term, Bindings) :-
        ( Atom == '' ->
                Term = Atom,
                Bindings = []
        ; atom(Atom) ->
                atom_to_term_1(Atom, Term, Bindings)
        ).
 
atom_to_term_1(Atom, Term, Bindings) :-
        atom_chars(Atom, Chars0),
	(   append(Base, [0'.|Spaces], Chars0),
	    all_spaces(Spaces)
	->  append(Base, " .", Chars)
	;   append(Chars0, " .", Chars)
	),
        length(Chars, Length),
        with_input_from_chars(
                read_term_with_count(Term0, Bindings0, Count),
                Chars),
        Count == Length,
	Term = Term0,
	Bindings = Bindings0.

all_spaces([]).
all_spaces([H|T]) :-
	space(H),
	all_spaces(T).

space(0' ).
space(9).
space(10).
space(13).

read_term_with_count(Term, Bindings, Count) :-
        current_input(Stream),
        ( read_term([syntax_errors(quiet), variable_names(Bindings)], Term) ->
                character_count(Stream, Count)
        ; Count = 0,
          Bindings = []
        ).

% time_file(File, Time)

time_file(File, Time):-
	pce:get(file(File), time, Time).

% concat/3

concat(A,B,C) :-
	nonvar(B), !,
	strings:concat(A, B, C).
concat(A,B,C) :-
	string_append(A,B,C).

% print_source_location

print_source_location:-
	prolog_load_context(file, File),
	prolog_load_context(term_position,
		'$stream_position'(_, Line, _, _, _)),
	write('**** File '),
	write(File),
	write(' Line '),
	write(Line),
	nl.

% pce_error(Error)
% pce_warn(Warning)
% pce_info(Info)
%
% Provide (prolog-part) PCE interface messages

pce_error(Error) :-
	print_message(error, Error).

pce_warn(Warning) :-
	print_message(warning, Warning).

pce_info(Info) :-
	print_message(informational, Info).

:- extern(pce_error(+term)).

% ignore(+Goal)
%
% call goal once, succeed always

ignore(Goal) :-
	Goal, !.
ignore(_).

%	auto_call(+Goal)
%	As goal, but invokes require/1 to define a predicate if it is
%	not defined already.

auto_call(Goal) :-
	strip_module(Goal, Module, Predicate),
	functor(Predicate, Name, Arity),
	require:require(Module, [Name/Arity]),
	Goal.

% atom_length(+Atom, -Length)

atom_length(Atom, Length) :-
	name(Atom, List),
	length(List, Length).

% free_variables(+Term, -ListOfUnBound)
%
% ListOfUnbound is a list of unbound variables in the term
 
free_variables(Term, ListOfUnbound) :-
        free_variables(Term, [], [], ListOfUnbound).
 
% pce_arg(?N, +Term, ?Arg)
%
% uses genarg/3 from library(arg), can solve for N
 
pce_arg(N, Term, Arg) :-
	genarg(N, Term, Arg).

% sublist/3

sublist(_, [], []) :- !.
sublist(G, [H|T], [H|R]) :-
	call(G, H),
	!,
	sublist(G, T, R).
sublist(G, [_|T], L) :-
	sublist(G, T, L).

% exists_file(+FileName)
%
% uses file_exists/1 from library(files)

exists_file(File):-
        file_exists(File).

% make
%
% make/0 is supposed to reload all Prolog sourcefiles that have changed since
% they were last loaded.  Don't know how to implement that in Quintus.

make :-
	pce_error(no_pw3_predicate(make/0)),
	fail.

% source_warning(+Fmt, +Args)

source_warning(Fmt, Args):-
	pce_warn(preformatted(Fmt, Args)).

% sformat(-Chars, +Format, +Args)
%
% output Format with Args to Chars

sformat(Chars, Format, Args):-
	with_output_to_chars(format(Format, Args), Chars).

%	defined_predicate(:Head)

callable_predicate(Spec) :-
      strip_module(Spec, Module, Head),
      (   predicate_property(Head, built_in)
      ->  true
      ;	  current_predicate(_, Module:Head)
      ).

%	succeeds if source file is modified since it was loaded the last
%	time.

modified_since_last_loaded(_File) :-
      fail.

%	xpce_loop

xpce_loop :-
	repeat,
	    pce_principal:send(@display, dispatch),
	fail.

% following was added for QP3.2

:- use_module(library(directory)).

expand_file_name(Spec, Files) :-
	findall(File,
		directory:file_member_of_directory('.', Spec, File, _),
		Files).
