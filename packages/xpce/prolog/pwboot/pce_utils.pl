/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


:- module(pce_utils, [
	  pce_error/1,
	  pce_warn/1,
	  pce_info/1,
	  ignore/1,
	  strip_module/3,
	  source_location/2,
	  term_to_atom/2
	]).

:- meta_predicate ignore(:).

:- use_module(library(charsio), [chars_to_stream/2, with_output_to_chars/2]).
:- use_module(library(fromonto), [onto_chars/2]).

%       pce_error(Error)
%       pce_warn(Warning)
%       pce_info(Info)
%       Provide (prolog-part) PCE interface messages

pce_error(Error) :-
	print_message(error, Error).

pce_warn(Warning) :-
	print_message(warning, Warning).

pce_info(Info) :-
	print_message(silent, Info).


%       ignore(+Goal)
%       Call goal once, succeed always

ignore(Goal) :-
	Goal, !.
ignore(_).


%       strip_module(+RawTerm, -Term, -Module).
%       If a term is of the form Module:Term, strip of the module part,
%       return the plain term in Term and the Module in Module.

strip_module(RT, M, T) :-
	strip_module(RT, T, M, user).

strip_module(Module:RT2, T, M, _) :- !,
	strip_module(RT2, T, M, Module).
strip_module(T, T, M, M).


%       source_location(-Path, -LineNo)
%       Unify Path and LineNo with the filename and line number of the
%       location where the last term has been read.  Used inside
%       term_expansion.

source_location(File, Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position,
		'$stream_position'(_, Line, _, _, _)).


%	term_to_atom(+Term, -Atom)
%	convert a term to a single atom by characters

term_to_atom(Term, Atom) :-    % convert term to single atom by characters
	copy_term(Term, TempTerm),
	numbervars(TempTerm, 0, _),
	onto_chars(print(TempTerm), Chars),
	atom_chars(Atom, Chars).
