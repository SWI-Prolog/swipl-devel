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
Temporary hack to allow storing  the   documentation-files  onto the 8+3
limited DOS filesystem.  These names will   eventually  be stored in the
class-structure itself for easier maintenance.  Grrrrrrr #$%!$@%##%!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(pce_class_map, [mapped_class_name/2]).

mapped_class_name(*,                       times).
mapped_class_name(+,                       plus).
mapped_class_name(-,                       minus).
mapped_class_name(/,                       divide).
mapped_class_name(:=,                      binding).
mapped_class_name(<,                       less).
mapped_class_name(=,                       eq).
mapped_class_name(=<,                      lesseq).
mapped_class_name(==,                      equal).
mapped_class_name(>,                       greater).
mapped_class_name(>=,                      greateq).
mapped_class_name(?,                       obtain).
mapped_class_name(\==,                     noteq).
mapped_class_name(@=,			   nameref).
