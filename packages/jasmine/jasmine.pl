/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(jasmine,
	  [ odb_ses_start/5,		% -SH, +DB, +User, +Passwd, +EnvFile
	    odb_ses_end/1,		% +SH
	    odb_exec_odql/2,		% +SH, +Command
	    odb_get_var/3,		% +SH, +Var, -Value
	    odb_set_var/3,		% +SH, +Var, +Value
	    odb_collection_to_list/3,	% +SH, +VarOrOID, -List

	    odb_exec_odql/3,		% +SH, +Format, +Args
	    odql/3			% :SH, +Vars, +Statements
	  ]).
:- use_module(library(quintus)).	% meta_predicate/1

:- meta_predicate
	odql(:, +, +).

:- load_foreign_library(foreign(jasmine)).

%	odb_exec_odql(+SH, +Format, +Args)
%
%	Provide formatted ODQL commands.

odb_exec_odql(SH, Fmt, Args) :-
	sformat(Command, Fmt, Args),
	odb_exec_odql(SH, Command).

%	odql(+SH, +[Var:Type, ...], +[Cmd, ...])

odql(SH, Vars, Lines) :-
	strip_module(SH, Module, H),
	odb_declare_vars(Vars, H),
	statements(Lines, H, Module).

statements([], _, _).
statements([H|T], SH, Module) :-
	statement(H, SH, Module),
	statements(T, SH, Module).

statement(Fmt-Args, SH, _) :- !,
	odb_exec_odql(SH, Fmt, Args).
statement({Command}, _SH, Module) :- !,
	Module:Command.
statement(get(Var, Value), SH, _) :- !,
	odb_get_var(SH, Var, Value).
statement(set(Var, Value), SH, _) :- !,
	odb_set_var(SH, Var, Value).
statement(get_list(Colection, List), SH, _) :- !,
	odb_collection_to_list(SH, Colection, List).
statement(Cmd, SH, Module) :-
	odb_exec_odql(SH, Cmd, Module).

odb_declare_vars([], _).
odb_declare_vars([H|T], SH) :-
	odb_declare_var(H, SH),
	odb_declare_vars(T, SH).

odb_declare_var(Name:Type, SH) :-
	catch(odb_exec_odql(SH, 'undefVar ~w;', [Name]), _, true),
	odb_exec_odql(SH, '~w ~w;', [Type, Name]).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile prolog:message/3.

prolog:message(error(package(jasmine, Id), context(Msg, _))) -->
	[ 'Jasmine [ID=~d]: ~w'-[Id, Msg] ].
