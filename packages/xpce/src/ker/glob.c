/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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
