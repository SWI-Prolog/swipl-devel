/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <h/kernel.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actually this isn't part of the window   system, but OS binding. For the
moment we will ignore this fact. The corresponding windows functions are
in ../msw/mswin.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
char *ws_user(void)
    Get the identifier name of the currently logged in user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
ws_user()
{ char *name;
#ifdef HAVE_GETLOGIN

  if ( (name = getlogin()) )
    return name;

#else

  if ( (name = getenv("USER")) )
    return name;

#endif

  return NULL;
}
