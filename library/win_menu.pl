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

:- module(win_menu,
	  [ init_win_menus/0
	  ]).
:- style_check(+dollar).
:- op(100, fx, @).

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
       '&User init file ...' = prolog_edit_preferences(prolog),
       '&Stack sizes ...'    = prolog_edit_preferences(stack_sizes)
     ],
     []).
menu('&Debug',
     [ %'&Trace'	     = trace,
       %'&Debug mode'	     = debug,
       %'&No debug mode'     = nodebug,
       '&Edit spy points ...' = user:prolog_ide(open_debug_status),
       '&Threads monitor ...' = user:prolog_ide(thread_monitor),
       --,
       '&Graphical debugger' = user:guitracer
     ],
     [ before_menu(-)
     ]).
menu('&Help',
     [ '&About ...' 				= about,
       '&Online manual ...' 			= help,
       --,
       'SWI-Prolog &WWW home (on www) ...'	= www_open(pl),
       'SWI-Prolog &FAQ (on www) ...'		= www_open(pl_faq),
       'SWI-Prolog &Quick Start (on www) ...'	= www_open(pl_quick),
       'SWI-Prolog &Manual (on www) ...'	= www_open(pl_man),
       'SWI-Prolog Mailing &List (on www) ...'	= www_open(pl_mail),
       'SWI-Prolog &Download (on www) ...'	= www_open(pl_download),
       --,
       '&XPCE (GUI) Manual ...' 		= manpce,
       'XPCE &User Guide (on www) ...'   	= www_open(xpce_man),
       --,
       'Submit &Bug report (on www) ...'  	= www_open(pl_bugs),
       --,
       '&Donate to the SWI-Prolog project (on www) ...' = www_open(pl_donate)
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
	
about :-
	print_message(informational, about).


		 /*******************************
		 *	 HANDLE CALLBACK	*
		 *******************************/

action(Action) :-
	'$strip_module'(Action, Module, Plain),
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
	(   current_prolog_flag(associate, Ext),
	    Ext \== pl
	->  atom_concat('*.', Ext, AltPattern),
	    Filter = chain(tuple('Prolog Source',	   '*.pl'),
			   tuple('Alternate Prolog Source', AltPattern),
			   tuple('All files',   	   '*.*'))
	;   Filter = chain(tuple('Prolog Source',	   '*.pl'),
			   tuple('All files',   	   '*.*'))
	),
	current_prolog_flag(hwnd, HWND),
	call(get(@display, win_file_name, 	% avoid autoloading
		 Mode, Filter, Title,
		 owner := HWND,
		 File)).

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(opening_url(Url)) -->
	[ 'Opening ~w ... '-[Url], flush ].
prolog:message(opened_url(_Url)) -->
	[ at_same_line, 'ok' ].
