/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(xpce_report,
	  [ pce_error/1,
	    pce_info/1,
	    pce_warn/1
	  ]).
:- require([ concat_atom/2
	   , append/3
	   , concat/3
	   , sformat/3
	   ]).


		 /*******************************
		 *	   INFO/WARNINGS	*
		 *******************************/

pce_error(Term) :-
	print_message(error, Term).

pce_warn(Term) :-
	print_message(warning, Term).

pce_info(Term) :-
	print_message(informational, Term).

:- use_module(library('english/pce_messages')).

message_to_string(Term, Str) :-
        pce_message(Term, Actions, []), !,
        actions_to_format(Actions, Fmt, Args),
        sformat(Str, Fmt, Args).

actions_to_format([], '', []) :- !.
actions_to_format([nl], '', []) :- !.
actions_to_format([Fmt-Args,nl], Fmt, Args) :- !.
actions_to_format([Fmt0-Args0,nl|Tail], Fmt, Args) :- !,
        actions_to_format(Tail, Fmt1, Args1),
        concat_atom([Fmt0, '~n', Fmt1], Fmt),
        append(Args0, Args1, Args).
actions_to_format([Fmt0-Args0|Tail], Fmt, Args) :- !,
        actions_to_format(Tail, Fmt1, Args1),
        concat(Fmt0, Fmt1, Fmt),
        append(Args0, Args1, Args).
actions_to_format([Term|Tail], Fmt, Args) :-
        actions_to_format(Tail, Fmt1, Args1),
        concat('~w', Fmt1, Fmt),
        append([Term], Args1, Args).


		 /*******************************
		 *     HOOK IN PRINT_MESSAGE	*
		 *******************************/

:- multifile
	user:portray_message/2.

user:portray_message(Severity, Term) :-
	message_to_string(Term, Str),
	print_message(force(Severity), Str).

