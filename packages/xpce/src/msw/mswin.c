/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <h/interface.h>

		 /*******************************
		 *	  HINSTANCE STUFF	*
		 *******************************/

HINSTANCE ThePceHInstance;		/* Global handle */

#ifdef USE_DLL_ENTRY
#define initHinstance(argc, argv)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a mistery. Earlier versions of this used rlc_hinstance() to find
the HInstance parameter to  pass  to   the  various  Win32 API functions
requesting one. As we want  to   disconnect  from console.dll, I changed
this to make a DLL entry-point. First   of  all, this yields the symbold
_main() as undefined. See below.

I then tested without this function, and   guess what: it doesn't appear
to matter what you use for PceHInstance. I tried 0 and 42 and the system
has no noticable problems????

More fun: using a DLL entry point   makes  malloc() crash. Now trying to
exploit the initialisation. Hoping that a  HINSTANCE   is  the same as a
HMODULE ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

BOOL WINAPI
pceDLLEntry(HINSTANCE instance, DWORD reason, LPVOID reserved)
{ switch(reason)
  { case DLL_PROCESS_ATTACH:
      pceinst = instance;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If I don't define this, an  undefined   reference  is generated.  Who is
calling this?  Is this is MSVC++ bug?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
main()
{ /*Cprintf("%s:%d: xpce.dll: main() called???\n", __FILE__, __LINE__);*/

  return 0;
}

#else /*USE_DLL_ENTRY*/

void
initHinstance(int argc, char **argv)
{ if ( argc > 0 && (ThePceHInstance = GetModuleHandle(argv[0])) )
    return;
    
  ThePceHInstance = GetModuleHandle("xpce");
}


#endif /*USE_DLL_ENTRY*/


		 /*******************************
		 *	      VERSIONS		*
		 *******************************/

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


#ifdef USE_RLC_FUNCTIONS

#define HostConsoleHWND() rlc_hwnd()

#else /*USE_RLC_FUNCTIONS*/

HWND
HostConsoleHWND()
{ PceCValue val;

  if ( hostQuery(HOST_CONSOLE, &val) )
    return (HWND) val.pointer;

  return NULL;
}

#endif /*USE_RLC_FUNCTIONS*/

status
ws_show_console(Name how)
{ HWND hwnd = HostConsoleHWND();

  if ( hwnd )
  { if ( how == NAME_open )
    { if ( IsIconic(hwnd) )
	ShowWindow(hwnd, SW_RESTORE);
      else
	ShowWindow(hwnd, SW_SHOW);
    } else if ( how == NAME_iconic )
      ShowWindow(hwnd, SW_SHOWMINIMIZED);
    else if ( how == NAME_hidden )
      ShowWindow(hwnd, SW_HIDE);

    succeed;
  }

  fail;
}


status
ws_console_label(CharArray label)
{ HWND hwnd = HostConsoleHWND();

  if ( hwnd )
    SetWindowText(hwnd, strName(label));

  succeed;
}


void
ws_check_intr()
{ hostAction(HOST_CHECK_INTERRUPT);
}


void
ws_msleep(int time)
{ Sleep((DWORD) time);
}


int
ws_getpid()
{ DEBUG(NAME_instance, Cprintf("HINSTANCE is %d\n", PceHInstance));
/*DEBUG(NAME_instance, Cprintf("CONSOLE's HINSTANCE is %d\n",
			       rlc_hinstance()));
*/

  return (int) GetCurrentProcessId();
} 


int
ws_mousebuttons()
{ return GetSystemMetrics(SM_CMOUSEBUTTONS);
}


#ifdef O_IMGLIB
void
remove_ilerrout(int status)
{ unlink("ilerr.out");
}
#endif


void
ws_initialise(int argc, char **argv)
{ initHinstance(argc, argv);

  if ( ws_mousebuttons() == 2 )
    ws_emulate_three_buttons(100);
}


Int
ws_default_scrollbar_width()
{ int w = GetSystemMetrics(SM_CXHSCROLL);	/* Is this the right one? */

  return toInt(w);
}


#define MAXMESSAGE 1024

Name
WinStrError(int error, ...)
{ va_list args;
  char msg[MAXMESSAGE];

  va_start(args, error);
  if ( !FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		      NULL,
		      error,
		      GetUserDefaultLangID(),
		      msg,
		      sizeof(msg),
		      (char **)args) )
  { sprintf(msg, "Unknown WINAPI error %d", error);
  }
  va_end(args);

  return CtoName(msg);
}


int
get_logical_drive_strings(int bufsize, char *buf)
{ return GetLogicalDriveStrings(bufsize, buf);
}
