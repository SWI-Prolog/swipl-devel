/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_demo,
	  [ pcedemo/0
	  ]).


:- use_module(library(pce)).
:- use_module(contrib(contrib)).
:- require([ emacs/1
	   , forall/2
	   , member/2
	   , term_to_atom/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines a demo-starting tool.  The  demo's themselves should
be  in the library  files 'demo/<demo-file>.pl'.  At   the end of this
file is a list of available demo's.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pcedemo :-
	new(B, browser('PCE Demo''s', size(60,10))),
	send(B, confirm_done, @off),
	send(B, tab_stops, vector(150)),
	fill_browser(B),

	new(D, dialog),
	send(D, below, B),
	send(D, append, new(O, button(open, message(@prolog, open_demo, B)))),
	send(D, append, button(source, message(@prolog, view_source, B))),
	send(D, append, button(quit, message(D?frame, free))),
	send(D, default_button, open),

	send(B, open_message, message(O, execute)),
	send(B, style, title, style(font := font(helvetica, bold, 14))),
	
	send(B, open).


fill_browser(B) :-
	forall(demo(Name, Summary, _File, _Predicate),
	       send(B, append, dict_item(Name,
					 string('%s	%s', Name, Summary)))),
	send(B, append,
	     dict_item('======Contributions====================',
		       style := title)),
	forall(contribution(Name, Summary, _Author, _File, _Predicate),
	       send(B, append, dict_item(Name,
					 string('%s	%s', Name, Summary)))).


open_demo(Browser) :-
	get(Browser, selection, DictItem),
	(   (	DictItem == @nil
	    ;	get(DictItem, style, title)
	    )
	->  send(@display, inform, 'First select a demo')
	;   get(DictItem, key, Name),
	    (	(   demo(Name, _Summary, File, Predicate)
		;   contribution(Name, _Summary, _Author, File, Predicate)
		)
	    ->  (   use_module(File)
		->  (   Predicate
		    ->  true
		    ;   send(@pce, inform, 'Failed to start %s demo', Name)
		    )
		;   send(@pce, inform, 'Can''t find demo sourcefile')
		)
	    ;   send(Browser, report, error, 'No such demo: %s', Name)
	    )
	).

view_source(Browser) :-
	get(Browser, selection, DictItem),
	(   DictItem == @nil
	->  send(@display, inform, 'First select a demo')
	;   get(DictItem, key, Name),
	    (	demo(Name, _Summary, File, _Predicate)
	    ;	contribution(Name, _Summary, _Author, File, _Predicate)
	    ),
	    (	locate_file(File, Path)
	    ->	emacs(Path)
	    ;	term_to_atom(File, FileAtom),
	        send(Browser, report, error,
		     'Can''t find source from %s', FileAtom)
	    )
	).

locate_file(Base, File) :-
	absolute_file_name(Base,
			   [ file_type(prolog),
			     access(read)
			   ], File).


		/********************************
		*             DEMO'S		*
		********************************/

demo('PceDraw',				% Name              
     'Drawing tool',			% Summary           
     library(pcedraw),			% Sources           
     pcedraw).				% Toplevel predicate

demo('Ispell',
     'Graphical interface to ispell (requires ispell 3)',
     demo(ispell),
     ispell) :-
	send(@pce, has_feature, process).

demo('Emacs',
     'Emacs (Epoch) look-alike editor',
     library(pce_emacs),
     emacs).

demo('FontViewer',
     'Examine PCE predefined fonts',
     demo(fontviewer),
     fontviewer).

demo('Cursors',
     'Displays browser of available cursors',
     demo(cursor),
     cursor_demo).

demo('Colours',
     'Displays browser of named colours',
     demo(colour),
     colour_browser).

demo('ImageViewer',
     'Examine image files in a directory',
     demo(imageviewer),
     image_viewer).

demo('Events',
     'Display hierarchy of event-types',
     demo(event_hierarchy),
     event_hierarchy).

demo('GraphViewer',
     'Visualise a graph represented as Prolog facts',
     demo(graph),
     graph_viewer).

demo('FtpLog',
     'Examine /usr/adm/xferlog (ftp log file)',
     demo(ftplog),
     ftplog('/usr/adm/xferlog')) :-
	send(@pce, has_feature, process),
	send(file('/usr/adm/xferlog'), access, read).


demo('ChessTool',
     'Simple frontend for /usr/games/chess',
     demo(chess),
     chess) :-
	send(@pce, has_feature, process),
	send(file('/usr/games/chess'), access, execute).

demo('Constraints',
     'Using constraints and relations',
     demo(constraint),
     constraint_demo).

demo('Kangaroo',
     'Jumping kangaroos demo',
     demo(kangaroo),
     kangaroo).

demo('Juggler',					  
     'Annimation of a juggling creature',	  
     demo(juggler),			  
     juggle_demo).				  

