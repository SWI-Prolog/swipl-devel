/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
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

#endif /*RCUTIL_H_INCLUDED*/
