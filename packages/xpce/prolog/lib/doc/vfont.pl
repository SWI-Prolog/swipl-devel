/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
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





