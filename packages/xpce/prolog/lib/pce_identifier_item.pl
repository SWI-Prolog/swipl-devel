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
The class identifier_item is  a  subclass   of  text_item  for  entering
identifiers. Its main task is not to allow for empty identifiers, handle
different case and white-space regimes.

Case regimes supported are:

	sensitive*		Identifiers are casesensitive
	upper			Identifiers are mapped to upper-case
	lower			Identifiers are mapped to lower-case
	  
white-space regimes are:

	accept			Don't change
	stripped		Delete leading and trailing white space
	canonise*		As stripped and make all internal white
				space exactly one space-character
	<a character>		As canonise, but pass spaces as this
				character.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- module(pce_identifier_item, []).
:- use_module(library(pce)).

:- pce_begin_class(identifier_item, text_item,
		   "Item for non-empty, canonised word").

variable(case,  {sensitive,upper,lower} := sensitive,
	 both, "Case mapping").
variable(blank, '{accept,stripped,canonise}|char' := canonise,
	 both, "How to handle blank space").

selection(II, Selection:name) :<-
	"Get selection and canonise"::
	get(II, get_super, selection, Name0),
	get(II, case, CaseMap),
	canonise_case(CaseMap, Name0, Name1),
	get(II, blank, BlankMap),
	canonise_blanks(BlankMap, Name1, Selection),
	(   Selection == ''
	->  get(II?name, label_name, Label),
	    send(II, error, item_not_filled, Label),
	    fail
	;   true
	).

canonise_case(upper, Name, Upper) :- !,
	get(Name, upcase, Upper).
canonise_case(lower, Name, Upper) :- !,
	get(Name, downcase, Upper).
canonise_case(_, Name, Name).

canonise_blanks(canonise, Name0, Name) :- !,
	get(Name0, strip, canonise, Name).
canonise_blanks(stripped, Name0, Name) :- !,
	get(Name0, strip, both, Name).
canonise_blanks(Mapped, Name0, Name) :-
	integer(Mapped), !,
	get(Name0, strip, canonise, Name1),
	new(S, string('%s', Name1)),
	send(S, translate, ' ', Mapped),
	get(S, value, Name),
	free(S).
canonise_blanks(_, Name, Name).

%typed(II, Key:event_id) :->
%	"Properly handle completion"::
%	(   Key == 32			% Space: completion
%	->  true
%	;   send(II, send_super, typed, Key)
%	).

:- pce_end_class(identifier_item).


:- initialization
   new(_, error(item_not_filled, '%I%s: No value', error, report)).
