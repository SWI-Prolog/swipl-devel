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


:- module(pce_draw,
	  [ pcedraw/0,			% start pcedraw
	    pcedraw/1,			% and load a file into it
	    save_pcedraw/1		% save to a file
	  ]).

:- use_module(library(pce)).
:- require([ file_name_extension/3
	   , is_list/1
	   , member/2
	   ]).

:- consult(library('draw/draw')).

pcedraw :-
	draw.

pcedraw([]) :- !,
	draw.
pcedraw(Files) :-
	is_list(Files), !,
	(   member(File, Files),
	    draw(File),
	    fail
	;   true
	).
pcedraw(File) :-
	draw(File).

pce_ifhostproperty(prolog(swi),
(   save_pcedraw(File) :-
	(   feature(windows, true)
	->  file_name_extension(File, exe, Exe)
	;   Exe = File
	),
	pce_autoload_all,
	pce_autoload_all,
	qsave_program(Exe,
		      [ goal=pce_main_loop(pcedraw),
			stand_alone=true
		      ])),
(   save_pcedraw(File) :-
	format(user_error, 'SWI-Prolog only~n', []))).
