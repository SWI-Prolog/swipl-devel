/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#define NOAUTOPROCS
#include <windows.h>
#include "pcewh.h"

static HHOOK defhook;
DWORD _export FAR PASCAL mouse_hook(int code, WPARAM wParam, LPARAM lParam);

int PASCAL
WinMain(HANDLE hInstance, HANDLE x1, LPSTR cmdline, int x2)
{ FARPROC fp, oldfp;
  CALLBACKPTR cbp;

  cbp = GetProc16((PROCPTR) mouse_hook, GETPROC_SETWINDOWSHOOK);
  fp  = MakeProcInstance(cbp, hInstance); 
  defhook = SetWindowsHookEx(WH_MOUSE, fp, hInstance, NULL);

  return 1;
}


int __export FAR PASCAL
WEP(int nParameter)
{ if ( defhook )
    UnhookWindowsHookEx(defhook);

  return 1;
}


DWORD _export FAR PASCAL
mouse_hook(int code, WPARAM wParam, LPARAM lParam)
{ static HWND cwin;			/* current window */

  if ( code >= 0 )
  { MOUSEHOOKSTRUCT FAR* data = MK_FP32(lParam);

    if ( data->hwnd != cwin )
    { if ( cwin )
	SendMessage(cwin, WM_WINEXIT, 0, 0L);
      cwin = data->hwnd;
      if ( cwin )
	SendMessage(cwin, WM_WINENTER, 0, 0L);
    }
  }

  return CallNextHookEx(defhook, code, wParam, lParam);
}
