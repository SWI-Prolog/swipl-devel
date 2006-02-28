/*  $Id$

    Part of the SWI-Prolog Semweb package

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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
*/

#ifndef DEBUG_H_INCLUDED
#define DEBUG_H_INCLUDED

#define O_DEBUG 1

#ifdef O_DEBUG
int		rdf_debuglevel();
foreign_t	rdf_debug(term_t level);

#define DEBUG(n, g) if ( rdf_debuglevel() >= (n) ) { g; }
#else
#define DEBUG(n, g) ((void)0);
#endif

#ifdef O_SECURE
#define SECURE(g) g
#else
#define SECURE(g) (void)0
#endif

#endif /*DEBUG_H_INCLUDED*/
