/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <stress.h>


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
ws_free_file_descriptors()
{ return GetFreeFileHandles();
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
{ rlc_title(strName(label));

  succeed;
}


void
ws_check_intr()
{ rlc_check_intr();
}

