/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef MSWIN_H_INCLUDED
#define MSWIN_H_INCLUDED

typedef enum
{ WINUNKNOWN,  
  WIN32S,
  WIN95,
  WIN98,
  WINME,
  NT
} os_platform;

os_platform	ws_platform(void);	/* yield windows platform */
char *		ws_os(void);		/* platform by name */

#endif /*MSWIN_H_INCLUDED*/
