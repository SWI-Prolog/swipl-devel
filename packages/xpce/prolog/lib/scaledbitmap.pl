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

:- module(scaled_bitmap, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide support for resizeable bitmap. The   bitmap can be resized using
->scale or using one of the  default XPCE graphical geometry-controlling
methods (->size, ->width, ->set, ->area, etc).

If ->keep_aspect is @on, the image is   not  distorted using vertical or
horizontal stretching.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(scaled_bitmap, bitmap,
		   "Bitmap that scales its image").

variable(original,    image,	     get, "Preserved original image").
variable(scale,	      [real|size],   get, "Scale-factor/area").
variable(keep_aspect, bool := @on,   get, "Keep aspect ratio").  

initialise(BM, Img:image, Spec:[real|size]) :->
	"Create from image and scaling factor"::
	send(BM, slot, original, Img),
	send(BM, slot, scale, Spec?clone),
	send_super(BM, initialise),
	send(BM, request_compute).


image(BM, Img:image) :->
	"Replace the image"::
	send(BM, slot, original, Img),
	send(BM, apply_scale).
image(BM, Img:image) :<-
	get(BM, original, Img).
	


apply_scale(BM) :->
	"Apply the requested scaling"::
	get(BM, scale, Scale),
	get(BM, original, Original),
	(   Scale == @default
	->  send_super(BM, image, Original)
	;   get(Original, size, size(W0, H0)),
	    (   float(Scale),
	        W is round(W0*Scale),
		H is round(H0*Scale)
	    ;   object(Scale, size(WM, HM)),
		(   get(BM, keep_aspect, @on)
		->  SF is min(WM/W0, HM/H0),
		    W is round(W0*SF),
		    H is round(H0*SF)
		;   W = WM,
		    H = HM
		)
	    ),
	    (	get_super(BM, image, Current),
		get(Current, size, size(W,H))
	    ->	true
	    ;   get(Original, scale, size(W, H), Scaled),
		send_super(BM, image, Scaled)
	    )
	).


compute(BM) :->
	"Delayed update"::
	send(BM, apply_scale),
	send(BM, request_compute, @nil).


geometry(BM, X:[int], Y:[int], W0:[int], H0:[int]) :->
	(   new_size(BM, W0, H0, W, H)
	->  send(BM, scale, size(W, H)),
	    send_super(BM, geometry, X, Y)
	;   send_super(BM, geometry, X, Y)
	).


new_size(Gr, Ws, Hs, W, H) :-
	get(Gr, width, W0),
	get(Gr, height, H0),
	default(Ws, W0, W),
	default(Hs, H0, H),
	(   W \== W0			% something changed
	;   H \== H0
	), !.


scale(BM, Scale:[real|size]) :->
	"Change the scale"::
	get(BM, scale, Old),
	(   send(Scale, same_class, Old),
	    send(Scale, equal, Old)
	->  true
	;   send(BM, slot, scale, Scale?clone),
	    send(BM, request_compute)
	).

keep_aspect(BM, Keep:bool) :->
	send(BM, slot, keep_aspect, Keep),
	send(BM, request_compute).

:- pce_end_class(scaled_bitmap).
