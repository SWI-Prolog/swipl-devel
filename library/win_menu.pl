/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

:- module(win_menu,
	  [ init_win_menus/0
	  ]).
:- system_module.
:- op(100, fx, @).

menu('&File',
     [ '&Consult ...' = action(consult(+file(open, 'Load file into Prolog'))),
       '&Edit ...'    = action(edit(+file(open, 'Edit existing file'))),
       '&New ...'     = action(edit_new(+file(save, 'Create new Prolog source'))),
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
       --,
       '&Graphical debugger' = user:guitracer
     ],
     [ before_menu(-)
     ]).
menu('&Help',
     [ '&About ...' 				= about,
       '&Online manual ...' 			= help,
       --,
       '&SWI-Prolog WWW home (on www) ...'	= www_open(pl),
       '&SWI-Prolog FAQ (on www) ...'		= www_open(pl_faq),
       '&SWI-Prolog Quick Start (on www) ...'	= www_open(pl_quick),
       '&SWI-Prolog Manual (on www) ...'	= www_open(pl_man),
       '&SWI-Prolog Mailing list (on www) ...'	= www_open(pl_mail),
       '&SWI-Prolog Download (on www) ...'	= www_open(pl_download),
       --,
       '&XPCE (GUI) Manual ...' 		= manpce,
       '&XPCE User Guide (on www) ...'   	= www_open(pce_man),
       --,
       'Submit but report (on www) ...'  	= www_open(pl_bugs)
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
	    (   member(Item, Items),
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
	;   true
	).
		   
:- initialization init_win_menus.

		 /*******************************
		 *	      ACTIONS		*
		 *******************************/

edit_new(File) :-
	edit(file(File)).
	
www_open(Id) :-
	print_message(informational, opening_url(URL)),
	Spec =.. [Id, '.'],
	www_open_url(Spec),
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
	get(@display, win_file_name,
	    Mode, Filter, Title,
	    owner := HWND,
	    File).

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(opening_url(Url)) -->
	[ 'Opening ~w ...'-[Url], flush ].
prolog:message(opened_url(_Url)) -->
	[ 'ok' ].
