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

:- module(area,
	[ side_pattern/3	% Side x Side x Pattern
	]).

%   side_pattern(+SideA, +SideB, -Pattern)
%
%   Pattern is the bit if SideA on area A corresponds to SideB on area B.  

side_pattern(top,    top,    2'1).
side_pattern(top,    center, 2'10).
side_pattern(top,    bottom, 2'100).
side_pattern(center, top,    2'1000).
side_pattern(center, center, 2'10000).
side_pattern(center, bottom, 2'100000).
side_pattern(bottom, top,    2'1000000).
side_pattern(bottom, center, 2'10000000).
side_pattern(bottom, bottom, 2'100000000).
side_pattern(left,   left,   2'1000000000).
side_pattern(left,   middle, 2'10000000000).
side_pattern(left,   right,  2'100000000000).
side_pattern(middle, left,   2'1000000000000).
side_pattern(middle, middle, 2'10000000000000).
side_pattern(middle, right,  2'100000000000000).
side_pattern(right,  left,   2'1000000000000000).
side_pattern(right,  middle, 2'10000000000000000).
side_pattern(right,  right,  2'100000000000000000).
