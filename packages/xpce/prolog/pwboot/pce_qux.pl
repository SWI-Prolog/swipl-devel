:- module(pce_qux, [
	  atom_to_term/2,
	  atom_to_term/3,
	  term_to_atom/2,
	  source_location/2,
	  pce_error/1,
	  pce_warn/1,
	  pce_info/1,
	  ignore/1,
	  concat/3,
	  free_variables/2,
	  atom_length/2,
	  time_file/2,
	  strip_module/3,
	  pce_arg/3,
	  pce_sublist/3,
	  exists_file/1,
	  make/0,
	  source_warning/2,
	  sformat/3,
	  call_emacs/2,
	  expand_file_name/2	% added for QP3.2
	]).

:- meta_predicate
	ignore(:),
	pce_sublist(:, +, ?).

:- use_module(library(charsio), [chars_to_stream/2,
				 with_input_from_chars/2,
				 with_output_to_chars/2]).
:- use_module(library(between), [between/3]).
:- use_module(library(files), [file_exists/1]).
:- use_module(library(strings), [concat_atom/2, string_append/3]).
:- use_module(library(freevars), [free_variables/4]).
:- use_module(library(arg), [genarg/3]).
:- use_module(library(call), [call/2]).

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
        append(Chars0, ".", Chars),
        length(Chars, Length),
        with_input_from_chars(
                read_term_with_count(Term0, Bindings0, Count),
                Chars),
        ( Count = Length ->
                Term = Term0,
                Bindings = Bindings0
        ; Term = Atom,
          Bindings = []
        ).

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

% ignore(+Goal)
%
% call goal once, succeed always

ignore(Goal) :-
	Goal, !.
ignore(_).

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

% pce_sublist/3

pce_sublist(_, [], []) :- !.
pce_sublist(M:G, [H|T], [H|R]) :-
	call(M:G, H),
	!,
	pce_sublist(M:G, T, R).
pce_sublist(M:G, [_|T], L) :-
	pce_sublist(M:G, T, L).

% exists_file(+FileName)
%
% uses file_exists/1 from library(files)

exists_file(File):-
        file_exists(File).

% make
%
% sends 'make' command to UNIX

make:-
	unix(shell(make)).

% source_warning(+Fmt, +Args)

source_warning(Fmt, []):-
	format(user_error, Fmt, []),
	!.
source_warning(Fmt, Args):-
	format(user_error, 'source_warning: Can not deal with Args ~w', [Args]),
	format(user_error, Fmt, []).

% sformat(-Chars, +Format, +Args)
%
% output Format with Args to Chars

sformat(Chars, Format, Args):-
	with_output_to_chars(format(Format, Args), Chars).

% call_emacs(Fmt)
% call_emacs(Fmt, Args)

call_emacs(Fmt) :-
        call_emacs(Fmt, []).
call_emacs(Fmt, Args) :-
        concat_atom(['', Fmt, ''], F1),
        format(F1, Args),
	current_output(Stream),
        flush_output(Stream).

% following was added for QP3.2

:- use_module(library(directory)).

expand_file_name(Spec, Files) :-
	findall(File,
		directory:file_member_of_directory('.', Spec, File, _),
		Files).
