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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This application provides an overview of   the ftp deamon log-file which
is (SunOs 4.1) located at /usr/adm/xferlog.  The main tool consists of a
list in which the ftp log records may   be combined by file, site, user,
etc.  The list indicates the  number   of  ftp  transfers.  Double click
opens a window with the individual transfers.

Most of the demo's in  this   directory  use user-defined classes.  This
demo illustrates writing XPCE  applications   without  creating  new PCE
classes.  This demo illustrates the capabilities of PCE code objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	      HEADER		*
		 *******************************/


:- module(ftplog,
	  [ ftplog/0,
	    ftplog/1
	  ]).

:- use_module(library(pce)).
:- require([ send_list/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Make the file-finder available for this application.  The pce_autoload/2
directive tells PCE that the class finder is defined on the library file
find_file.  The pce_global/2 directive tells PCE   to create an instance
of class finder referenced as @finder if @finder is referred to.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).


		 /*******************************
		 *	   PRESENTATION		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This section builds the ftp log tool.  It   consists of a frame with two
windows: a dialog window which  contains   a  menu_bar  (row of pulldown
menus) and a browser to show the list of ftp transfers.

The code below creates instances  of  various   of  the  PCE  UI related
classes, specified options and relates these objects.

First, the two windows are created  and   related.   The `Name := Value'
construct  specifies  parameters  by  name    rather   than  the  normal
specification by value.  This  way  of   specification  is  adviced if a
method accepts various arguments that  default   and  their order is not
obvious.  The browser could also have been created using

	...,
	new(B, browser(@default, size(60, 15))),
	...,

Next the frame is requested.  Any window   or set of combined windows is
contained in a frame object.  The frame   defines methods to request the
references of the windows and visa versa and thus is the ideal object to
represent the tool as a  whole.   The   frame  object  is used to attach
information  such  as  the   current    ftp   database.   The  predicate
load_ftp_logfile/2 (defined below parses  the   ftp  logfile.  After the
entire tool has been initialised  it  is   opened  on  the display using
->open.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ftplog :-
	ftplog('/usr/adm/xferlog').

ftplog(File) :-
	new(D, dialog('FTP transfer log')),
	new(B, browser(size := size(60, 15))),
	send(B, below, D),
	get(D, frame, Frame),
	send(B, open_message, message(@prolog, open, Frame, @arg1)),
	load_ftp_logfile(Frame, File),
	fill_dialog(D),
	view_by(Frame, file),
	send(D, open).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Create the menu_bar (row of  pulldown   menus).   A  menu_bar conists of
popup objects (in the context of   menu_bar pulldown menus).  Each popup
menu contains a set of menu_item  objects   that  map command names onto
commands.  The commands are represented by   PCE code objects.  The most
prototypical PCE code object is the message.    When a message object is
executed it will start a send operation.  Arguments are passed using the
special `var' objects @arg1, @arg2, ...    All  the following statements
are equivalent:

	1 ?- format('Hello World~n').
	2 ?- send(@prolog, format, 'Hello World~n').
	3 ?- send(message(@prolog, format, 'Hello World~n'), forward).
	4 ?- send(message(@prolog, format, @arg1), forward, 'Hello World~n').

A message may be attached to the menu   as a whole or to each individual
menu_item.  Using the first option, PCE will   forward  the value of the
menu_item  using  @arg1.   This  facility  is  chosen  for  the  view_by
pulldown.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_dialog(D) :-
	get(D, frame, F),

	send(D, append, new(MB, menu_bar)),
	send(MB, append, new(File, popup(file))),
	send(MB, append,
	     new(ViewBy, popup(view_by, message(@prolog, view_by, F, @arg1)))),
	send(MB, append, new(SortBy, popup(sort_by))),

	send_list(File, append,
		  [ menu_item(load_ftp_logfile,
			      message(@prolog, load_ftp_logfile, F),
			      end_group := @on),	% separate from next
		    menu_item(quit,
			      message(F, destroy))
		  ]),

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the view-by menu with the  attributes   of  the  first transfer log
description.  Exclude the `date' field.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	(   get(F?xferlog, head, Sheet)
	->  get(Sheet, attribute_names, Names),
	    send(Names, for_all,
		 if(@arg1 \== date,
		    message(ViewBy, append, @arg1)))
	;   true
	),

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Code to sort the browser either by  name   or  by the number of matching
transfers.  In this example we have chosen to rely completely on the use
of PCE code objects rather than   using  Prolog predicates.  Roughly the
following designs are possible:

	1) The message of the menu item  invokes a Prolog predicate that
	   requests the browser, reads the contents of the browser, sort
	   the objects in  Prolog  and  rebuild   the  contents  of  the
	   browser.

	   This approach avoids the need to   learn about PCE executable
	   code objects.  However, it  is  slow   due  to  the interface
	   overhead and long due to  all   conversions  that  have to be
	   made.

	2) The message of the menu item  invokes a Prolog predicate that
	   requests the browser and invokes `browser->sort'.

	3) Do the entire action using   code  objects.  This approach is
	   chosen below.  Code objects, when  executed, execute all code
	   objects that appear as arguments of them.  This implies that,
	   when the outer message object  is   executed  it  will try to
	   evaluate the compare function  if  it   would  not  have been
	   protected using the quote_function object.

	   A quote_function object delegates to  its associated function
	   and class function's convert  method converts quote_functions
	   into the real function.  This way  quote_function may be used
	   to avoid expansion of functions where this is not wanted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	new(SortByName, message(?(F, member, browser), sort,
				quote_function(?(@arg1?key, compare,
						 @arg2?key)))),
	new(SortByTimes, message(?(F, member, browser), sort,
				 quote_function(?(@arg1?object?size, compare,
						  @arg2?object?size)),
				 reverse := @on)),

	send(SortBy, append, menu_item(name, SortByName)),
	send(SortBy, append, menu_item(times, SortByTimes)).
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Rebuild the browser's contents collecting all   transfers  with the same
value for `Field'.  Each  browser  entry   consists  of  two fields: the
number of matching transfers and the field   value.  Hence a tab-stop is
placed  for  proper  allignment   of    the   fields.   See  `text_image
->tab_stops'.  Finally the transfer logs are appended to the browser and
the browser is sorted alpabetically  on   the  field  value (file, site,
...).  By default, `browser->sort' sorts  the   labels  rather  than the
keys.  As the number of occurences is   placed  in the first column this
would sort on the number (still alphabetically) rather than the name.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

view_by(Frame, Field) :-
	get(Frame, member, browser, B),
	send(B, clear),
	send(B, tab_stops, vector(50)),
	send(Frame?xferlog, for_all, message(@prolog, update, B, Field, @arg1)),
	send(B, sort, ?(@arg1?key, compare, @arg2?key)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Append a ftp tranfer log to the browser, which is organised on the named
field (file, user, ...).  The browser's items have a label that consists
of  the  number  of  transfers  and  the  field-value.   The  `dict_item
<-object' attribute is used to store  the transfers associated with this
entry.  This allows for easy expansion (see open/2).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

update(Browser, Field, Sheet) :-
	get(Sheet, Field, Value),
	(   get(Browser, member, Value, DI)
	->  send(DI?object, append, Sheet)
	;   send(Browser, append,
		 new(DI, dict_item(Value, @default, chain(Sheet))))
	),
	send(DI, label, string('%d\t%s', DI?object?size, Value)).
			
		 /*******************************
		 *	    SHOW RECORD		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Show all the individual transfers in a separate browser.  If the browser
is still present it will be reused.  There  are three ways to keep track
of the browser's reference:

	1) Give the browser a global   reference and use pce_global/2 to
	   create it.  If the user than  deletes   the  frame it will be
	   recreated automatically.

	2) Maintain a registration using  object-level attributes.  This
	   mechanism  requires  significant   administration    as   the
	   attributes should be updated when either   of  the windows is
	   destroyed.

	3) Use `hyper-links'.  A hyper link  is a bidirectional relation
	   between two objects.  Its destruction  is automatically taken
	   care of by `object ->unlink'.    The  hyper-link mechanism is
	   used in the code below.

Both 1) and 3) are easy-to-use and   safe mechanisms.  The difference is
that, when multiple tools are running, they will share this window using
1) and use separate windows using 3).    Using 2) is feasible when using
user-defined classes as  this  provides   for  redefining  the  instance
destruction.

The browser is filled with a description for  each sheet in the chain of
matching ftp transfers using the  `chain ->for_all' iteration mechanism.
The argument code object is called for  each element of the chain.  Note
that the message does not read

	message(Browser, append, string('%s ...

But

	message(Browser, append, create(string, '%s ...

In the first example one string object  would be created from the Prolog
interface and on each invokation of the message this (same!) object will
be appended to the browser.  The create object in the second form is not
expanded during creation of the message object but on each invokation of
the message.  This will have the desired effect of appending a formatted
string for each element of the chain.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

open(Frame, DI) :-
	(   get(Frame, hypered, open_browser, Browser)
	->  send(Browser, clear)
	;   new(Browser, browser(string('Overview for %s', DI?key),
				 size(80, 15))),
	    send(Browser, tab_stops, vector(350)),
	    send(Browser, confirm_done, @off),
	    new(_, hyper(Frame, Browser, open_browser))
	),
	send(DI?object, for_all,
	     message(Browser, append,
		     create(string,
			    '%s %s %02s:%02s %s\t%s',
			    ?(@arg1?date, month_name, short := @on),
			    @arg1?date?day,
			    @arg1?date?hour,
			    @arg1?date?minute,
			    @arg1?file,
			    @arg1?user))),
	send(Browser, open).
		     

		 /*******************************
		 *	       LOADING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load an ftp logfile.  The version  with   one  argument uses the library
defined file finder to request the logfile to load_ftp_logfile.

The parsed database and the current file  are registered as attribute of
the frame object.  All UI components can easily locate their frame using
<-frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

load_ftp_logfile(Frame) :-
	get(@finder, file, @on, File),
	load_ftp_logfile(Frame, File).

load_ftp_logfile(Frame, File) :-
	send(?(Frame, member, browser), clear),
	parse(File, Chain),
	send(Frame, attribute, attribute(xferlog, Chain)),
	send(Frame, attribute, attribute(file, File)),
	send(Frame, label, string('FTP log from %s', File)).


		 /*******************************
		 *	      PARSING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Parse the file.  This uses `char_array  <-scan' which provides access to
the C library function sscanf.  You might   have  to redefine the format
specification if you run a different ftp deamon.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

parse(File, Chain) :-
	new(Chain, chain),
	new(F, file(File)),
	send(F, open, read),

	new(Msg, while(message(@ftplog_create_sheet_message, forward,
			       Chain,
			       ?(F?read_line, scan,
				 '%s%s%d%d:%d:%d%d%d%s%d%s%s%s%s%s%s%s%d%s')),
		       new(and))),

	send(Msg, forward),
	send(F, close).

:- pce_global(@ftplog_create_sheet_message, make_create_sheet_message).

make_create_sheet_message(M) :-
	Chain = @arg1,
        Vector = @arg2,

%	DayName = ?(Vector, element, 1),
	Month   = ?(Vector, element, 2),
	Day	= ?(Vector, element, 3),
	Hour    = ?(Vector, element, 4),
	Minute	= ?(Vector, element, 5),
	Second	= ?(Vector, element, 6),
	Year    = ?(Vector, element, 7),
%	_       = ?(Vector, element, 8),
	Site    = ?(Vector, element, 9),
%	Size    = ?(Vector, element, 10),
	File    = ?(Vector, element, 11),
%	Type    = ?(Vector, element, 12),
%	_       = ?(Vector, element, 13),
%	_       = ?(Vector, element, 14),
%	_       = ?(Vector, element, 15),
	Passwd  = ?(Vector, element, 16),
	FtpUser = ?(Vector, element, 17),
%	_       = ?(Vector, element, 18),
%	_       = ?(Vector, element, 19),

	new(Pswd, var),
	User    = when(message(Pswd, suffix, @),
		       ?(Pswd, append, Site),
		       when(?(Pswd, index, @),
			    Pswd,
			    create(string, '%s@%s', Pswd, Site))),

	new(M, and(assign(new(Date, var), create(date)),
		   assign(Pswd, Passwd),
		   message(Date, convert, create(string,
						 '%s %s %d:%d:%d %d',
						 Month, Day,
						 Hour, Minute, Second,
						 Year)),
		   message(Chain, append,
			   create(sheet,
				  create(attribute, file,     File),
				  create(attribute, site,     Site),
				  create(attribute, user,     User),
				  create(attribute, ftp_user, FtpUser),
				  create(attribute, date,     Date))))).
