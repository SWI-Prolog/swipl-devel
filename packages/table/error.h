/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef _ERROR_H_INCLUDED
#define _ERROR_H_INCLUDED

#define ERR_INSTANTIATION 1		/* general badly typed argument */
#define ERR_FORMAT 	  2		/* bad format in table */
#define ERR_IO	          3		/* general IO error */

#define error(a,b,c,d) error_func(a,b,c,(long)(d))

extern int	error_func(int type, const char *pred, int argi, long argl);

#endif /*_ERROR_H_INCLUDED*/

