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


#include <stdio.h>
#include <stdarg.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE Console IO is only used  for   debugging  purposes. On Unix systems
this IO will normally be bound to  Unix stdout/stdin. On strictly window
based systems you may redefine these functions to use a window.

    void
    vCprintf(const char *fmt, va_list args)
	Behaves like: vprintf(fmt, args);

    int
    Cputchar(int chr)
	Behaves like: putchar(chr);

    void
    Cflush(void)
	Behaves like fflush(stdout);

    char *
    Cgetline(char *buf, int size)
	Behaves like: fgets(buf, size, stdin); 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32

static FILE *console_in = NULL;
static FILE *console_out = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Whenever a stand-alone XPCE/Something application  writes explicitely to
the console, this stub will allocate a console to write to. Note the use
of _IONBF, instead of _IOLBF  which  would   be  much  more  natural. It
doesn't appear to work however (Windows-NT 4.0, MSVC 4.2).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <windows.h>
#include <io.h>
#include <fcntl.h>

static int
ensure_console()
{ static allocated = 0;

  if ( !allocated )
  { allocated++;
    if ( AllocConsole() )
    { HANDLE hin  = GetStdHandle(STD_INPUT_HANDLE);
      HANDLE hout = GetStdHandle(STD_OUTPUT_HANDLE);
      int in  = _open_osfhandle((long)hin, _O_RDONLY);
      int out = _open_osfhandle((long)hout, _O_APPEND);

      console_in  = _fdopen(in, "r");
      console_out = _fdopen(out, "w");

      setvbuf(console_in,  NULL, _IONBF, 256);
      setvbuf(console_out, NULL, _IONBF, 256);
     }
  }

  return 1;
}

#else /* ~WIN32 */

static FILE *console_in = NULL;
static FILE *console_out = NULL;

static int
ensure_console()
{ console_in = stdin;
  console_out = stdout;

  return 1;
}

#endif /*WIN32*/

void
Stub__vCprintf(const char *fmt, va_list args)
{ if ( ensure_console() )
    vfprintf(console_out, fmt, args);
}


int
Stub__Cputchar(int chr)
{ if ( ensure_console() )
    return fputc(chr, console_out);
  else
    return EOF;				/* signal error */
}


char *
Stub__Cgetline(char *line, int size)
{ if ( ensure_console() )
    return fgets(line, size, console_in);
  else
    return NULL;			/* signal error */
}


void
Stub__Cflush()
{ if ( ensure_console() )
    fflush(console_out);
}
