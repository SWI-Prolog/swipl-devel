/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include "mswin.h"
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


os_platform
ws_platform(void)
{ static int done = FALSE;
  os_platform platform = WINUNKNOWN;

  if ( !done )
  { OSVERSIONINFO info;

    info.dwOSVersionInfoSize = sizeof(info);
    if ( GetVersionEx(&info) )
    { switch( info.dwPlatformId )
      { case VER_PLATFORM_WIN32s:
	  platform = WIN32S;
	  break;
	case VER_PLATFORM_WIN32_WINDOWS:
	  switch( info.dwMinorVersion )
	  { case 0:
	      platform = WIN95;
	      break;
	    case 10:
	      platform = WIN98;
	      break;
	    case 90:
	      platform = WINME;
	      break;
	    default:
	      platform = WINUNKNOWN;
	  }
	  break;
	case VER_PLATFORM_WIN32_NT:
	  platform = NT;
	  break;
      }
    } else
      platform = WINUNKNOWN;
  }

  return platform;
}

char *
ws_os(void)
{ switch(ws_platform())
  { case WINUNKNOWN:
      return "win32";
    case WIN32S:
      return "win32s";
    case WIN95:
    case WIN98:
    case WINME:
      return "win95";			/* doesn't really make a difference */
    case NT:
      return "winnt";
    default:
      return "winunknown";
  }
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
#ifdef __CYGWIN32__
		      args) )		/* Cygwin is according to docs ... */
#else
		      (char **)args) )
#endif
  { sprintf(msg, "Unknown WINAPI error %d", error);
  }
  va_end(args);

  return CtoName(msg);
}


int
get_logical_drive_strings(int bufsize, char *buf)
{ return GetLogicalDriveStrings(bufsize, buf);
}

		 /*******************************
		 *      COMMON DIALOG STUFF	*
		 *******************************/

#include <h/unix.h>
#ifndef _MAX_PATH
#define _MAX_PATH 1024
#endif
#ifndef MAXPATHLEN
#define MAXPATHLEN _MAX_PATH
#endif
#define strapp(s, q) \
	{ int l = strlen(q); \
	  if ( s+l+2 > filter+sizeof(filter) ) \
	  { errorPce(filters, NAME_representation, \
		     CtoString("filter too long")); \
	    fail; \
	  } \
	  strcpy(s, q); \
	  s += l; \
	}

static int
allLetter(const char *s)
{ for(; *s && isletter(*s); s++)
    ;

  return *s ? FALSE : TRUE;
}


Name
getWinFileNameDisplay(DisplayObj d,
		      Name mode,	/* open, save */
		      Chain filters,	/* tuple(Name, Pattern) */
		      CharArray title,
		      CharArray file,	/* default file */
		      Directory dir)	/* initial dir */
{ OPENFILENAME ofn;
  HWND hwnd;
  Name rval = 0;
  EventObj ev = EVENT->value;
  char filter[1024], *ef = filter;
  char buffer[2048];
  char cwdbin[MAXPATHLEN];
  BOOL tmpb;

  memset(&ofn, 0, sizeof(OPENFILENAME));
  ofn.lStructSize = sizeof(OPENFILENAME);

  if ( instanceOfObject(ev, ClassEvent) &&
       (hwnd = getHwndWindow(ev->window)) )
    ofn.hwndOwner = hwnd;

  if ( isDefault(filters) )
  { Name nm = get((Any)NAME_allFiles, NAME_labelName, EAV);
    strapp(ef, strName(nm));
    *ef++ = '\0';
    strapp(ef, "*.*");
    *ef++ = '\0';
  } else
  { Cell cell;
  
    for_cell(cell, filters)
    { if ( instanceOfObject(cell->value, ClassTuple) )
      { Tuple t = cell->value;
	CharArray s1 = t->first, s2 = t->second;
  
	if ( !instanceOfObject(s1, ClassCharArray) )
	{ errorPce(s1, NAME_unexpectedType, TypeCharArray);
	  fail;
	}
	if ( !instanceOfObject(s2, ClassCharArray) )
	{ errorPce(s2, NAME_unexpectedType, TypeCharArray);
	  fail;
	}
	strapp(ef, strName(s1));
	*ef++ = '\0';
	strapp(ef, strName(s2));
	*ef++ = '\0';
      } else if ( instanceOfObject(cell->value, ClassCharArray) )
      { StringObj s = cell->value;
  
	strapp(ef, strName(s));
	*ef++ = '\0';
	strapp(ef, strName(s));
	*ef++ = '\0';
      } else
      { errorPce(cell->value, NAME_unexpectedType, CtoType("char_array|tuple"));
	fail;
      }
    }
  }
  *ef = '\0';
  ofn.lpstrFilter  = filter;
  ofn.nFilterIndex = 0;

  if ( isDefault(file) )
    buffer[0] = '\0';
  else
  { if ( strlen(strName(file)) >= sizeof(buffer) )
    { errorPce(file, NAME_representation, CtoString("name too long"));
      fail;
    }
    strcpy(buffer, strName(file));
  }

  ofn.lpstrFile    = buffer;
  ofn.nMaxFile     = sizeof(buffer)-1;
  if ( notDefault(dir) )
  { 
#ifdef O_XOS				/* should always be true */
    char tmp[MAXPATHLEN];
    char *s;
  
    if ( (s = expandFileName(strName(dir->path), tmp)) )
      ofn.lpstrInitialDir =_xos_os_filename(s, cwdbin);
#else
    ofn.lpstrInitialDir = expandFileName(strName(dir->path), cwdbin);
#endif

/*  Cprintf("InitialDir = '%s'\n", ofn.lpstrInitialDir); */
  }
  if ( notDefault(title) )
  ofn.lpstrTitle = strName(title);

  ofn.Flags = (OFN_HIDEREADONLY|
	       OFN_NOCHANGEDIR);
	       
  if ( mode == NAME_open )
  { ofn.Flags |= OFN_FILEMUSTEXIST;
    tmpb = GetOpenFileName(&ofn);
  } else
    tmpb = GetSaveFileName(&ofn);

  if ( !tmpb )
  { DWORD w;

    if ( !(w=CommDlgExtendedError()) )
      fail;				/* user canceled */

    Cprintf("Get{Open,Save}FileName() failed: %ld\n", w);
    fail;
  }

  if ( buffer[0] )
  { char *base = baseName(buffer);

    if ( !strchr(base, '.') && ofn.nFilterIndex > 0 )
    { char *pattern = filter;
      char *ext;
      int n;
	
      pattern = filter;
      pattern += strlen(pattern)+1;	/* first pattern */
      for(n=1; n<ofn.nFilterIndex; n++)
      { pattern += strlen(pattern)+1;
	pattern += strlen(pattern)+1;
      }
  
      if ( (ext = strrchr(pattern, '.')) && allLetter(ext+1) )
	strcat(buffer, ext);
    }

#ifdef O_XOS				/* should always be true */
    rval = CtoName(_xos_canonical_filename(buffer, filter));
#else
    rval = CtoName(buffer);
#endif
  }

  return rval;
}
