/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(doc_objects, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  module  defines  the  reusable  objects   used  by  the  rendering
primitives. Please note that, as these are declaraced using pce_global/2
directive, it is possible to define any  of these objects prior to using
the document rendering primitives to overrule any of these settings.

For example:

	:- initialization
	   new(@h1_above, new(hbox(0, 30))).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	 GLOBAL OBJECTS		*
		 *******************************/

:- pce_global(@browser, 	new(@event?receiver?window)).

:- pce_global(@space_rubber,	new(rubber(1, 100, 1, allow))).
:- pce_global(@section_skip,	new(hbox(0, 20, 10))).
:- pce_global(@subsection_skip,	new(hbox(0, 20, 10))).
:- pce_global(@br,		new(hbox(rubber := rubber(linebreak := force)))).
:- pce_global(@hfill_rubber,	new(rubber(3, 100))).
:- pce_global(@hfill,		new(hbox(rubber := @hfill_rubber))).
:- pce_global(@quote_margin,	new(hbox(0, 0, 0, rubber(3, 100, 0)))).
:- pce_global(@quote_rubber,	new(rubber(3, 800, 0))).	  
:- pce_global(@nbsp,		new(tbox(' '))).
:- pce_global(@symbol_style,	new(style(font := symbol))).

:- pce_global(@table_rubber,	new(rubber(3, 100, 100))).

:- pce_global(@h1_above,	new(hbox(0, 20))).
:- pce_global(@h1_below,	new(hbox(0, 5))).
:- pce_global(@h2_above,	new(hbox(0, 20))).
:- pce_global(@h2_below,	new(hbox(0, 5))).
:- pce_global(@h3_above,	new(hbox(0, 20))).
:- pce_global(@h3_below,	new(hbox(0, 5))).
:- pce_global(@h4_above,	new(hbox(0, 20))).
:- pce_global(@h4_below,	new(hbox(0, 5))).

