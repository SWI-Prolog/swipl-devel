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

#define UNICODE 1
#define _UNICODE 1
#include "include.h"
#include "mswin.h"
#include <h/interface.h>
#include <tchar.h>

#ifdef UNICODE
#define nameToTCHAR(nm) nameToWC((Name)(nm), NULL)
#define TCHARToName(s)  WCToName(s, _tcslen(s))
#else
#define nameToTCHAR(nm) nameToMB((Name)(nm))
#define TCHARToName(s)  MBToName(s)
#endif


		 /*******************************
		 *	    DLL STUFF		*
		 *******************************/

HINSTANCE ThePceHInstance;		/* Global handle */

BOOL WINAPI
DllMain(HINSTANCE instance, DWORD reason, LPVOID reserved)
{ switch(reason)
  { case DLL_PROCESS_ATTACH:
      ThePceHInstance = instance;
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
  }

  return TRUE;
}


int
pceMTdetach()
{ DEBUG(NAME_thread,
	Cprintf("Detached thread 0x%x\n", GetCurrentThreadId()));
  destroyThreadWindows(ClassFrame);
  destroyThreadWindows(ClassWindow);

  return TRUE;
}


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


HWND
HostConsoleHWND()
{ PceCValue val;

  if ( hostQuery(HOST_CONSOLE, &val) )
    return (HWND) val.pointer;

  return NULL;
}


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
    else if ( how == NAME_fullScreen )
      ShowWindow(hwnd, SW_MAXIMIZE);

    succeed;
  }

  fail;
}


status
ws_console_label(CharArray label)
{ HWND hwnd = HostConsoleHWND();

  if ( hwnd )
    SetWindowText(hwnd, nameToTCHAR((Name)label));

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

  return (int) GetCurrentProcessId();
} 


char *
ws_user()
{ TCHAR buf[256];
  Name nm;
  DWORD len = sizeof(buf)/sizeof(TCHAR);

  if ( GetUserName(buf, &len) )
    return nameToFN(TCHARToName(buf));
  else if ( (nm = getEnvironmentVariablePce(PCE, CtoName("USER"))) )
    return nameToFN(nm);
  else
    return NULL;
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
{ if ( ws_mousebuttons() == 2 )
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
  TCHAR msg[MAXMESSAGE];

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
  { wsprintf(msg, _T("Unknown WINAPI error %d"), error);
  }
  va_end(args);

  return TCHARToName(msg);
}


int
get_logical_drive_strings(int bufsize, char *buf)
{ return GetLogicalDriveStringsA(bufsize, buf);
}

		 /*******************************
		 *      COMMON DIALOG STUFF	*
		 *******************************/

#define nameToFN(s) charArrayToFN((CharArray)(s))

#include <h/unix.h>
#ifndef _MAX_PATH
#define _MAX_PATH 1024
#endif
#ifndef MAXPATHLEN
#define MAXPATHLEN _MAX_PATH
#endif
#define strapp(s, q) \
	{ int l = _tcslen(q); \
	  if ( s+l+2 > filter+sizeof(filter)/sizeof(TCHAR) ) \
	  { errorPce(filters, NAME_representation, NAME_nameTooLong); \
	    fail; \
	  } \
	  _tcscpy(s, q); \
	  s += l; \
	}

static int
allLetter(const TCHAR *s)
{ for(; *s && _istalpha(*s); s++)
    ;

  return *s ? FALSE : TRUE;
}


#ifndef IsDirSep
#define IsDirSep(c) ((c) == '/' || (c) == '\\')
#endif

static TCHAR *
baseNameW(TCHAR *name)
{ TCHAR *base;

  for(base=name; *name; name++)
  { if ( IsDirSep(*name) && name[1] )
      base = name;
  }

  return base;
}


Name
getWinFileNameDisplay(DisplayObj d,
		      Name mode,	/* open, save */
		      Chain filters,	/* tuple(Name, Pattern) */
		      CharArray title,
		      CharArray file,	/* default file */
		      Directory dir,	/* initial dir */
		      Any owner)	/* owner window */
{ OPENFILENAME ofn;
  HWND hwnd;
  Name rval = 0;
  EventObj ev = EVENT->value;
  TCHAR filter[1024], *ef = filter;
  TCHAR buffer[2048];
  BOOL tmpb;

  memset(&ofn, 0, sizeof(OPENFILENAME));
  ofn.lStructSize = sizeof(OPENFILENAME);

  if ( isInteger(owner) )
    ofn.hwndOwner = (void *)valInt(owner);
  else if ( instanceOfObject(owner, ClassFrame) )
    ofn.hwndOwner = getHwndFrame(owner);
  else if ( instanceOfObject(ev, ClassEvent) &&
	    (hwnd = getHwndWindow(ev->window)) )
    ofn.hwndOwner = hwnd;

  if ( isDefault(filters) )
  { Name nm = get((Any)NAME_allFiles, NAME_labelName, EAV);
    strapp(ef, nameToTCHAR(nm));
    *ef++ = L'\0';
    strapp(ef, _T("*.*"));
    *ef++ = L'\0';
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
	strapp(ef, nameToTCHAR((Name)s1));
	*ef++ = L'\0';
	strapp(ef, nameToTCHAR((Name)s2));
	*ef++ = L'\0';
      } else if ( instanceOfObject(cell->value, ClassCharArray) )
      { StringObj s = cell->value;
  
	strapp(ef, nameToTCHAR((Name)s));
	*ef++ = L'\0';
	strapp(ef, nameToTCHAR((Name)s));
	*ef++ = L'\0';
      } else
      { errorPce(cell->value, NAME_unexpectedType, CtoType("char_array|tuple"));
	fail;
      }
    }
  }
  *ef = L'\0';
  ofn.lpstrFilter  = filter;
  ofn.nFilterIndex = 0;

  if ( isDefault(file) )
    buffer[0] = L'\0';
  else
  { const TCHAR *fn = nameToTCHAR(file);

    if ( _tcslen(fn) >= sizeof(buffer) )
    { errorPce(file, NAME_representation, NAME_nameTooLong);
      fail;
    }
    _tcscpy(buffer, fn);
  }

  ofn.lpstrFile    = buffer;
  ofn.nMaxFile     = (sizeof(buffer)/sizeof(TCHAR))-1;
  if ( notDefault(dir) )
    ofn.lpstrInitialDir = nameToTCHAR(dir->path);
  if ( notDefault(title) )
  ofn.lpstrTitle = nameToTCHAR(title);

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
  { TCHAR *base = baseNameW(buffer);

    if ( !_tcschr(base, '.') && ofn.nFilterIndex > 0 )
    { TCHAR *pattern = filter;
      TCHAR *ext;
      int n;
	
      pattern = filter;
      pattern += _tcslen(pattern)+1;	/* first pattern */
      for(n=1; n<ofn.nFilterIndex; n++)
      { pattern += _tcslen(pattern)+1;
	pattern += _tcslen(pattern)+1;
      }
  
      if ( (ext = _tcsrchr(pattern, '.')) && allLetter(ext+1) )
	_tcscat(buffer, ext);
    }

#ifdef O_XOS				/* should always be true */
  { char buf[MAXPATHLEN];

    if ( !_xos_canonical_filenameW(buffer, buf, sizeof(buf), 0) )
    { errorPce(TCHARToName(buffer), NAME_representation, NAME_nameTooLong);
      fail;
    }
    rval = UTF8ToName(buf);
  }
#else
    rval = TCHARToName(buffer);
#endif
  }

  return rval;
}


Name
getWinDirectoryDisplay(DisplayObj d,
		       CharArray title,
		       Directory dir,	/* initial dir */
		       Any owner)	/* owner window */
{ BROWSEINFO bi;
  HWND hwnd;
  Name rval = 0;
  EventObj ev = EVENT->value;
  LPITEMIDLIST pidl;

  memset(&bi, 0, sizeof(bi));

  if ( isInteger(owner) )
    bi.hwndOwner = (void *)valInt(owner);
  else if ( instanceOfObject(owner, ClassFrame) )
    bi.hwndOwner = getHwndFrame(owner);
  else if ( instanceOfObject(ev, ClassEvent) &&
	    (hwnd = getHwndWindow(ev->window)) )
    bi.hwndOwner = hwnd;

  bi.lpszTitle = nameToTCHAR(title);
  bi.ulFlags   = BIF_RETURNONLYFSDIRS;

  if ( (pidl = SHBrowseForFolder(&bi)) )
  { TCHAR *path[MAX_PATH];
    IMalloc *im = NULL;
    Name result = NULL;

    if ( SHGetPathFromIDList(pidl, path) )
    { result = TCHARToName(path);
    }
    
    if ( SHGetMalloc(&im) )
    { imalloc->Free(pidl);
      imalloc->Release();
    }

    return result;
  } else
  { fail;
  }
}
