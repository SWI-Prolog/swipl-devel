/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

/*
#define NOGDI
#define NOATOM
#define NOGDICAPMASKS
#define NOMETAFILE
#define NOMINMAX
#define NOMSG
#define NOOPENFILE
#define NORASTEROPS
#define NOSCROLL
#define NOSOUND
#define NOSYSMETRICS
#define NOTEXTMETRIC
#define NOWH
#define NOCOMM
#define NOKANJI
*/

#define RedrawWindow WinRedrawWindow

#define Arc WinArc
#define Ellipse WinEllipse

#include <winsock.h>

#undef Ellipse
#undef Arc

#undef RedrawWindow
#undef hyper				/* don't need this */
#undef islower				/* we have these ourselves */
#undef isupper
#undef isdigit
#undef isalnum
#undef isalpha
#undef iscntrl
#undef isprint
#undef isspace
#undef ispunct
#undef isxdigit
