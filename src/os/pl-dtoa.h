/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PL_DTOA_H_INCLUDED
#define PL_DTOA_H_INCLUDED

#define dtoa   PL_dtoa			/* avoid library conflicts */
#define strtod PL_strtod		/* avoid library conflicts */

COMMON(char *)	dtoa(double dd, int mode, int ndigits,
		     int *decpt, int *sign, char **rve);
COMMON(void)	freedtoa(char *s);
double		strtod(const char *in, char **end);

#endif /*PL_DTOA_H_INCLUDED*/
