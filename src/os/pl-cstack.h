/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam

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

#ifndef PL_CSTACK_H_INCLUDED
#define PL_CSTACK_H_INCLUDED

COMMON(void)	save_backtrace(const char *why);
COMMON(void)	btrace_destroy(struct btrace *bt);
COMMON(void)	print_backtrace(int last);		/* 1..SAVE_TRACES */
COMMON(void)	print_backtrace_named(const char *why);
COMMON(void)	initBackTrace(void);

#endif /*PL_CSTACK_H_INCLUDED*/
