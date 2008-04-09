/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_settings,
	  [ test_settings/0
	  ]).


:- use_module(library(plunit)).
:- use_module(library(settings)).
:- use_module(library(readutil)).

test_settings :-
	run_tests([ settings
		  ]).

:- begin_tests(settings).

:- setting(test, atom, hello, 'Test setting').

%%	reset
%
%	Hack to cleanup setting defaults

reset :-
	retractall(settings:default(_,_,_)),
	retractall(settings:value(_,_,_)),
	retractall(settings:local_file(_)).

%================================================================

test(get, X == hello) :-
	setting(test, X).
test(ch_default, [X == hello_world, cleanup(reset)]) :-
	set_setting_default(test, hello_world),
	setting(test, X).
test(save_default, [ [X,Terms] == [hello_world,[]],
		     cleanup((reset, delete_file(DB)))
		   ]) :-
	tmp_file(settings, DB),
	set_setting_default(test, hello_world),
	save_settings(DB),
	read_file_to_terms(DB, Terms, []),
	load_settings(DB),
	setting(test, X).
test(restore_default, [ [X] == [hello_world],
			cleanup(reset)
		      ]) :-
	set_setting_default(test, hello_world),
	set_setting(test, hello_universe),
	restore_setting(test),
	setting(test, X).

:- end_tests(settings).

