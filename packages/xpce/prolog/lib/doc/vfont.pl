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

:- module(vfont, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class vfont defines a `virtual' font:   a  collection of font attributes
that can be manipulated without having to  worry about the physical font
that is available for satisfying the   specification.  The method <-font
extracts the best match physical font.

The initial font-map is rather limited, but may be expanded by the user
using the multifile predicate

	vfont:font_map(Family,		% an atom
		       Encoding,	% an atom
		       Slant,		% {r,i}
		       Weight,		% {medium, bold}
		       Fixed,		% @on, @off
		       size,		% 1..7
		       Font).		% Resulting font
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	   CLASS VFONT		*
		 *******************************/

:- pce_begin_class(vfont, object, "Virtual font").

variable(family,   name,	  both, "Basic font family").
variable(encoding, name,	  both, "Character-set encoding used").
variable(slant,	   {r,i},	  both, "Slanted?").
variable(weight,   {medium,bold}, both, "Weight of the font").
variable(fixed,	   bool,	  both, "@off: proportional").
variable(size,	   '1..7',	  both, "Size index").

initialise(VF) :->
	"Create with default attributes"::
	send(VF, family,   helvetica),
	send(VF, encoding, latin1),
	send(VF, slant,	   r),
	send(VF, weight,   medium),
	send(VF, fixed,    @off),
	send(VF, size,     3).

font(VF, Font:font) :<-
	"Get closest font"::
	get(VF, family, Fam),
	get(VF, encoding, Enc),
	get(VF, slant,	Slant),
	get(VF, weight, W),
	get(VF, fixed, Fixed),
	get(VF, size, Size),
	font(Fam, Enc, Slant, W, Fixed, Size, Font0),
	catch(get(@pce, convert, Font0, font, Font), _, fail), !.

font_size(VF, Diff:int) :->
	"Change relative size of font"::
	get(VF, size, Size0),
	Size is max(1, min(7, Size0+Diff)),
	send(VF, size, Size).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Font table.  Currently very incomplete.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	font_map/7.

font(F, E, S, W,    Fix,  Size,  Font) :-
	font_map(F, E, S, W, Fix, Size, Font), !.

font(_,	_,	_, _,	 @on,  _, fixed).
font(_,	_,	_, bold, @off, S, boldhuge)		:- S >= 6.
font(_,	_,	_, bold, @off, S, boldlarge)		:- S >= 4.
font(_,	_,	i, _,	 _,    _, italic).
font(_,	_,	_, bold, @on,  _, boldtt).
font(_,	_,	_, bold, @off, _, bold).
font(_,	_,	r, _,	 _,    S, small) 		:- S =< 2.
font(_,	_,	r, _,	 _,    4, large).
font(_,	_,	r, _,	 _,    5, large).
font(_,	_,	r, _,	 _,    S, huge)  		:- S >= 6.
font(_,	symbol,	_, _,	 _,    _, symbol).
font(_,	_,	_, _,	 _,    _, normal).

:- pce_end_class.





