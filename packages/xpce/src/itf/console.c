/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/


#include <stdio.h>
#include <stdarg.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE Console IO is only used  for   debugging  purposes. On Unix systems
this IO will normally be bound to  Unix stdout/stdin. On strictly window
based systems you may redefine these functions to use a window.

    void
    vCprintf(const char *fmt, va_list args)
	Behaves like: vprintf(fmt, args); fflush(stdout);

    int
    Cputchar(int chr)
	Behaves like: putchar(chr);

    char *
    Cgetline(char *buf, int size)
	Behaves like: fgets(buf, size, stdin); 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
Stub__vCprintf(const char *fmt, va_list args)
{ vprintf(fmt, args);
  fflush(stdout);
}


int
Stub__Cputchar(int chr)
{ return putchar(chr);
}


char *
Stub__Cgetline(char *line, int size)
{ return fgets(line, size, stdin);
}
