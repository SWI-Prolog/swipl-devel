/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#define GLOBAL
#if defined(WIN32) || defined(__WIN32__)
#define PUBLIC_GLOBAL _declspec(dllexport)
#else
#define PUBLIC_GLOBAL
#endif
#include <h/kernel.h>
#include <h/trace.h>
#include <h/interface.h>
#include <h/arith.h>
#include <h/dialog.h>
#include <h/lang.h>
#include <ker/alloc.h>
