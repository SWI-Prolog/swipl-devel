/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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


:- module(pce_host,
	  [ '$load_pce'/0
	  , (meta_predicate)/1
	  ]).


:- module_transparent
	'$load_pce'/0.

:- use_module(library(quintus), [(meta_predicate)/1]).

		 /*******************************
		 *	    EXPANSION		*
		 *******************************/

user:term_expansion((:- require(_)), []).


		 /*******************************
		 *	    PROPERTIES		*
		 *******************************/

property(prolog(swi)).			% this is SWI-Prolog
property(file_extensions([pl])).	% list of file extensions
property(use_predicate_references).	% use direct predicate refs in methods
property(register_source_locations).	% register the source locations
property(string).			% Supports string datatype
property(runtime) :-
	get(@(pce), is_runtime_system, @(on)).
	

		 /*******************************
		 *	     ERRORS		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(T) -->
	{ '$c_current_predicate'(_, pce_messages:pce_message(_))
	}, % avoid problem while booting
	pce_messages:pce_message(T).

		/********************************
		*             ENTRY		*
		********************************/

pce_home(PceHome) :-
	absolute_file_name(pce('.'),
			   [ file_type(directory),
			     file_errors(fail)
			   ], PceHome),
	exists_directory(PceHome), !.
pce_home(PceHome) :-
	getenv('XPCEHOME', PceHome),
	exists_directory(PceHome), !.
pce_home(PceHome) :-
	(   current_prolog_flag(xpce_version, Version),
	    atom_concat('/xpce-', Version, Suffix)
	;   Suffix = '/xpce'
	),
	absolute_file_name(swi(Suffix),
			   [ file_type(directory),
			     file_errors(fail)
			   ], PceHome),
	exists_directory(PceHome), !.
pce_home(PceHome) :-
	current_prolog_flag(saved_program, true), !,
	(   current_prolog_flag(home, PceHome)
	->  true
	;   current_prolog_flag(symbol_file, Exe)
	->  file_directory_name(Exe, PceHome)
	;   PceHome = '.'
	).
pce_home(_) :-
	print_message(error, format('Cannot find XPCE home directory', [])),
	halt(1).

'$load_pce' :-
	'$c_current_predicate'('$pce_init', user:'$pce_init'(_)), !,
	init_pce.
'$load_pce' :-
	current_prolog_flag(open_shared_object, true),
	(   load_foreign_library(pce_principal:foreign(pl2xpce))
	->  true
	;   print_message(error,
			  format('Failed to load XPCE foreign library', [])),
	    halt(1)
	),
	init_pce.

init_pce :-
	(   pce_home(PceHome),
	    pce_principal:'$pce_init'(PceHome)
	->  set_prolog_flag(xpce, true)
	;   print_message(error,
			  format('Failed to initialise XPCE', [])),
	    abort
	).

%	We must declare this here as boot/english/pce_messages.pl is
%	not yet loaded.
%	
%	Right now the message is not printed from here but directly from
%	pl/src/interface.c.

:- multifile
	prolog:message/3.

prolog:message(pce(no_threads)) -->
        [ 'This version of XPCE does not support multi-threading'
        ].


		 /*******************************
		 *	       ABORT		*
		 *******************************/

:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.

user:message_hook('$aborted', _Kind, _Lines) :-
	current_prolog_flag(xpce, true),
	send(@display, reset),
	fail.
