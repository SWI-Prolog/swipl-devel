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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(win_menu,
	  [ init_win_menus/0
	  ]).
:- set_prolog_flag(generate_debug_info, false).
:- op(200, fy, @).
:- op(990, xfx, :=).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library sets up the menu of PLWIN.EXE. It is called from the system
initialisation file plwin.rc, predicate gui_setup_/0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

menu('&File',
     [ '&Consult ...' = action(user:consult(+file(open,
						  'Load file into Prolog'))),
       '&Edit ...'    = action(user:edit(+file(open,
					       'Edit existing file'))),
       '&New ...'     = action(edit_new(+file(save,
					      'Create new Prolog source'))),
       --,
       '&Reload modified files' = user:make,
       --,
       '&Navigator ...' = prolog_ide(open_navigator),
       --
     ],
     [ before_item('&Exit')
     ]).
menu('&Settings',
     [ --,
       '&User init file ...'  = prolog_edit_preferences(prolog),
       '&GUI preferences ...' = prolog_edit_preferences(xpce)
     ],
     []).
menu('&Debug',
     [ %'&Trace'	     = trace,
       %'&Debug mode'	     = debug,
       %'&No debug mode'     = nodebug,
       '&Edit spy points ...' = user:prolog_ide(open_debug_status),
       '&Edit exceptions ...' = user:prolog_ide(open_exceptions(@on)),
       '&Threads monitor ...' = user:prolog_ide(thread_monitor),
       'Debug &messages ...'  = user:prolog_ide(debug_monitor),
       'Cross &referencer ...'= user:prolog_ide(xref),
       --,
       '&Graphical debugger' = user:guitracer
     ],
     [ before_menu(-)
     ]).
menu('&Help',
     [ '&About ...'				= about,
       '&Help ...'				= help,
       'Browse &PlDoc ...'			= doc_browser,
       --,
       'SWI-Prolog website ...'			= www_open(swipl),
       '  &Manual ...'				= www_open(swipl_man),
       '  &FAQ ...'				= www_open(swipl_faq),
       '  &Quick Start ...'			= www_open(swipl_quick),
       '  Mailing &List ...'			= www_open(swipl_mail),
       '  &Download ...'			= www_open(swipl_download),
       '  &Extension packs ...'			= www_open(swipl_pack),
       --,
       '&XPCE (GUI) Manual ...'			= manpce,
       --,
       'Submit &Bug report ...'			= www_open(swipl_bugs)
     ],
     [ before_menu(-)
     ]).


init_win_menus :-
	(   menu(Menu, Items, Options),
	    (	memberchk(before_item(Before), Options)
	    ->	true
	    ;	Before = (-)
	    ),
	    (	memberchk(before_menu(BM), Options)
	    ->	win_insert_menu(Menu, BM)
	    ;	true
	    ),
	    (   '$member'(Item, Items),
		(   Item = (Label = Action)
		->  true
		;   Item == --
		->  Label = --
		),
		win_insert_menu_item(Menu, Label, Before, Action),
		fail
	    ;	true
	    ),
	    fail
	;   insert_associated_file
	).

insert_associated_file :-
	current_prolog_flag(associated_file, File),
	file_base_name(File, Base),
	atom_concat('Edit &', Base, Label),
	win_insert_menu_item('&File', Label, '&New ...', edit(file(File))).
insert_associated_file.


:- initialization
   (   win_has_menu
   ->  init_win_menus
   ;   true
   ).

		 /*******************************
		 *	      ACTIONS		*
		 *******************************/

edit_new(File) :-
	call(edit(file(File))).		% avoid autoloading

www_open(Id) :-
	Spec =.. [Id, '.'],
	call(expand_url_path(Spec, URL)),
	print_message(informational, opening_url(URL)),
	call(www_open_url(URL)),	% avoid autoloading
	print_message(informational, opened_url(URL)).

html_open(Spec) :-
	absolute_file_name(Spec, [access(read)], Path),
	call(win_shell(open, Path)).

about :-
	print_message(informational, about).


		 /*******************************
		 *	 HANDLE CALLBACK	*
		 *******************************/

action(Action) :-
	strip_module(Action, Module, Plain),
	Plain =.. [Name|Args],
	gather_args(Args, Values),
	Goal =.. [Name|Values],
	Module:Goal.

gather_args([], []).
gather_args([+H0|T0], [H|T]) :- !,
	gather_arg(H0, H),
	gather_args(T0, T).
gather_args([H|T0], [H|T]) :-
	gather_args(T0, T).

gather_arg(file(Mode, Title), File) :-
	findall(tuple('Prolog Source', Pattern),
		prolog_file_pattern(Pattern),
		Tuples),
	'$append'(Tuples, [tuple('All files', '*.*')], AllTuples),
	Filter =.. [chain|AllTuples],
	current_prolog_flag(hwnd, HWND),
	working_directory(CWD, CWD),
	call(get(@display, win_file_name,	% avoid autoloading
		 Mode, Filter, Title,
		 directory := CWD,
		 owner := HWND,
		 File)).

prolog_file_pattern(Pattern) :-
	user:prolog_file_type(Ext, prolog),
	atom_concat('*.', Ext, Pattern).


		 /*******************************
		 *	    APPLICATION		*
		 *******************************/

%%	init_win_app
%
%	If Prolog is started using --win_app, try to change directory
%	to <My Documents>\Prolog.

init_win_app :-
	current_prolog_flag(associated_file, _), !.
init_win_app :-
	current_prolog_flag(argv, Argv),
	'$append'(Pre, ['--win_app'|_Post], Argv),
	\+ '$member'(--, Pre), !,
	catch(my_prolog, E, print_message(warning, E)).
init_win_app.

my_prolog :-
	win_folder(personal, MyDocs),
	atom_concat(MyDocs, '/Prolog', PrologDir),
	(   ensure_dir(PrologDir)
	->  working_directory(_, PrologDir)
	;   working_directory(_, MyDocs)
	).


ensure_dir(Dir) :-
	exists_directory(Dir), !.
ensure_dir(Dir) :-
	catch(make_directory(Dir), E, (print_message(warning, E), fail)).


:- initialization
   init_win_app.


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(opening_url(Url)) -->
	[ 'Opening ~w ... '-[Url], flush ].
prolog:message(opened_url(_Url)) -->
	[ at_same_line, 'ok' ].
