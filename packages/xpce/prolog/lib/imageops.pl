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

:- module(pce_image_ops, []).
:- use_module(library(pce)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  module  provides  `image<-active:   bool    -->   Image',   return
activated/inactive version of a colour-image. These methods are designed
to manipulate images on buttons.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_extend_class(image).

active(Img, Active:bool, Img2:image) :<-
	"Return image with proper activation"::
	(   Active == @off
	->  (   get(Img, hypered, inactive, Img2)
	    ->	true
	    ;	get(Img, hypered, active, _)
	    ->	Img2 = Img
	    ;	get(Img, greyed, Img2)
	    )
	;   (   get(Img, hypered, active, Img2)
	    ->	true
	    ;	Img2 = Img
	    )
	).
	    

greyed(Img, Grey:image) :<-
	"Created a greyed version of a colour image"::
	(   get(Img, hypered, inactive, Grey)
	->  true
	;   get(Img, size, size(W, H)),
	    new(Grey, image(@nil, W, H, pixmap)),
	    (   get(Img, mask, Mask),
		send(Mask, instance_of, image)
	    ->  send(Grey, mask, new(M, image(@nil, W, H, bitmap))),
		new(MB, bitmap(Mask)),
		send(MB, transparent, @on),
		send(M, draw_in, MB),
		send(M, draw_in, MB, point(1,1))
	    ;   true
	    ),
	    get(Img, monochrome, I2),
	    send(Grey, background, black),
	    new(B2, bitmap(I2)),
	    send(B2, transparent, @on),
	    send(B2, colour, white),
	    send(Grey, draw_in, B2, point(1,1)),
	    get(class(menu), class_variable, inactive_colour, ClassVar),
	    get(ClassVar, value, GreyColour),
	    send(B2, colour, GreyColour),
	    send(Grey, draw_in, B2),
	    new(_, hyper(Img, Grey, inactive, active))
	).

:- pce_end_class.
