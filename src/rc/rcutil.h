/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef RCUTIL_H_INCLUDED
#define RCUTIL_H_INCLUDED

#include <stdarg.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

RcMember	rc_register_member(RcArchive archive, RcMember member);
RcMember	rc_find_member(RcArchive rca, const char *name, const char *cl);

#if !defined(strncasecmp) && !defined(HAVE_STRCASECMP) && defined(HAVE_STRICMP) 
#define strcasecmp stricmp
#define strncasecmp strnicmp
#endif

#endif /*RCUTIL_H_INCLUDED*/
