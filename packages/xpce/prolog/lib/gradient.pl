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

:- module(gradient, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class gradient is a subclass of image representing a gradient 1xN or Nx1
image that can be used as fill-pattern. The idea is to define a gradient
as  a  smooth  transition  from  one  colour  to  another.  The  current
implementation is more a prototype, lacking the following optimizations:

	* Don't use to many colours on 256 colour displays
	* Avoid creating a colour for each pixel when making long gradients
	* Allow for different algorithms

The current implementation uses a linear   transition in HSV-space. This
is probably one of  the  sensible   algorithms,  but  possibly we should
provide alternatives.

To create a vertical gradient in (X,Y,W,H), do:

gradient(Device, ColourTop, ColourBottom, X, Y, W, H) :-
	send(Device, display, new(B, box(W,H)), point(X,Y)),
	send(B, pen, 0),
	send(B, fill_offset, point(0,0)),
	send(B, fill_pattern,
	     gradient(@nil, ColourTop, ColourBottom, H, vertical)).

NOTE: DOS-7 based Windows (Windows 95/98/ME)   has severe limitations on
handling fill-patterns. XPCE has hacks to   avoid  these for rectangles,
making the above code only work for   boxes with <-radius: 0, no shadow,
and <-fill_offset: point(0,0). On X11 and NT   gradients  can be used on
all objects accepting  an  image  as   `colour'  for  its  interior. For
details, see also `box->fill_pattern'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(gradient, image,
		   "Create a gradient-image for filling").

variable(from,	      colour,		     get, "Starting colour").
variable(to,	      colour,		     get, "ending colour").
variable(orientation, {horizontal,vertical}, get, "Direction").


initialise(G,
	   Name:name=[name]*,
	   From:from=colour, To:to=colour,
	   Length:length='0..',
	   Orientation:orientation={horizontal,vertical}) :->
	(   Orientation == vertical
	->  W = 1, H = Length
	;   H = 1, W = Length
	),
	send_super(G, initialise, Name, W, H, pixmap),
	send(G, slot, from, From),
	send(G, slot, to, To),
	send(G, slot, orientation, Orientation),
	send(G, update).

:- pce_group(dimension).

length(G, Len:'0..') :<-
	"Get current length"::
	get(G, size, size(W,H)),
	(   get(G, orientation, vertical)
	->  Len = H
	;   Len = W
	).
length(G, Len:'0..') :->
	"Change the length"::
	(   get(G, length, Len)
	->  true
	;   (   get(G, orientation, horizontal)
	    ->	send(G, resize, Len, 1)
	    ;	send(G, resize, 1, Len)
	    ),
	    send(G, update)
	).

orientation(G, O:orientation) :->
	"Change the orientation, keeping the other parameters"::
	(   get(G, orientation, O)
	->  true
	;   get(G, length, Len),
	    (	O == horizontal
	    ->	send(G, resize, Len, 1)
	    ;	send(G, resize, 1, Len)
	    ),
	    send(G, slot, orientation, O),
	    send(G, update)
	).


horizontal(G, G2:gradient) :<-
	"Get horizontal version or <-self"::
	(   get(G, orientation, horizontal)
	->  true
	;   get(G, clone, G2),
	    send(G, orientation, horizontal)
	).

vertical(G, G2:gradient) :<-
	"Get vertical version or <-self"::
	(   get(G, orientation, vertical)
	->  true
	;   get(G, clone, G2),
	    send(G, orientation, vertical)
	).


:- pce_group(fill).

update(G) :->
	"Actually fill using the gradient"::
	get(G, from, From),
	get(G, to, To),
	hsv(From, F),
	hsv(To,   T),
	get(G, length, Len),
	get(G, orientation, Orientation),
	fill_gradient(0, Len, Orientation, G, F, T).
	
hsv(Colour, hsv(H, S, V)) :-
	get(Colour, hue, H),
	get(Colour, saturnation, S),
	get(Colour, value, V).

fill_gradient(Y, Height, _, _, _, _) :-
	Y >= Height, !.
fill_gradient(Y, Height, Orientation, Image, F, T) :-
	F = hsv(HF, SF, VF),
	T = hsv(HT, ST, VT),
	H is round(HF + (HT-HF) * Y/Height),
	S is round(SF + (ST-SF) * Y/Height),
	V is round(VF + (VT-VF) * Y/Height),
	(   Orientation == vertical
	->  send(Image, pixel, 0, Y, colour(@default, H, S, V, hsv))
	;   send(Image, pixel, Y, 0, colour(@default, H, S, V, hsv))
	),
	Y2 is Y + 1,
	fill_gradient(Y2, Height, Orientation, Image, F, T).
	
:- pce_end_class(gradient).
