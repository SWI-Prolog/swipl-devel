/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"
#ifndef __WIN32__
#include <stress.h>
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get Windows Version/Revision info
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ws_version(void)
{ DWORD dwv = GetVersion();

  return LOBYTE(LOWORD(dwv));
}


int
ws_revision(void)
{ DWORD dwv = GetVersion();

  return HIBYTE(LOWORD(dwv));
}


int
iswin32s()
{ if( GetVersion() & 0x80000000 && (GetVersion() & 0xFF) == 3 )
    return TRUE;
  return FALSE;
}


char *
ws_os(void)
{ return iswin32s() ? "win32s" : "win32";
}


status
ws_expose_console()
{ HWND hwnd = rlc_hwnd();

  if ( IsIconic(hwnd) )
    ShowWindow(hwnd, SW_RESTORE);
  else
    ShowWindow(hwnd, SW_SHOW);

  succeed;
}


status
ws_iconify_console()
{ HWND hwnd = rlc_hwnd();
  
  if ( hwnd )
    ShowWindow(hwnd, SW_SHOWMINIMIZED);

  succeed;
}


status
ws_console_label(CharArray label)
{ rlc_title(strName(label), NULL, 0);

  succeed;
}


void
ws_check_intr()
{ rlc_check_intr();
}


void
ws_msleep(int time)
{ Sleep((DWORD) time);
}


int
ws_getpid()
{ return (int) GetCurrentProcessId();
} 


int
ws_mousebuttons()
{ return GetSystemMetrics(SM_CMOUSEBUTTONS);
}


void
ws_initialise(int argc, char **argv)
{ if ( ws_mousebuttons() == 2 )
    ws_emulate_three_buttons(100);

  rlc_word_char('@', TRUE);
}


Int
ws_default_scrollbar_width()
{ int w = GetSystemMetrics(SM_CXVSCROLL);	/* Is this the right one? */

  return toInt(w);
}
