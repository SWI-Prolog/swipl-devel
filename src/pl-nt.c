/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#if defined(__WINDOWS__) || defined(__WIN32__)

#include <windows.h>
#include <crtdbg.h>
#include <process.h>
#include "pl-incl.h"
#include "pl-ctype.h"
#include <stdio.h>
#include <stdarg.h>
#include "pl-stream.h"
#include <process.h>

		 /*******************************
		 *	    MESSAGE BOX		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There is no way to tell which subsystem   an app belongs too, except for
peeking in its executable-header. This is a bit too much ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
hasConsole(void)
{ HANDLE h;

  if ( GD->os.gui_app == FALSE )	/* has been set explicitly */
    succeed;

					/* I found a console */
  if ( (h = GetStdHandle(STD_OUTPUT_HANDLE)) != INVALID_HANDLE_VALUE )
  { DWORD mode;

    if ( GetConsoleMode(h, &mode) )
      succeed;
  }

					/* assume we are GUI */
  fail;
}


void
PlMessage(const char *fm, ...)
{ va_list(args);

  va_start(args, fm);
  
  if ( hasConsole() )
  { Sfprintf(Serror, "SWI-Prolog: ");
    Svfprintf(Serror, fm, args);
    Sfprintf(Serror, "\n");
  } else
  { char buf[1024];

    vsprintf(buf, fm, args);
    MessageBox(NULL, buf, "SWI-Prolog", MB_OK|MB_TASKMODAL);
  }

  va_end(args);
}

		 /*******************************
		 *	       WIN32		*
		 *******************************/

int
iswin32s()
{ if( GetVersion() & 0x80000000 && (GetVersion() & 0xFF) ==3)
    return TRUE;
  else
    return FALSE;
}


		 /*******************************
		 *	WinAPI ERROR CODES	*
		 *******************************/

char *
WinError()
{ int id = GetLastError();
  char *msg;
  static WORD lang;
  static lang_initialised = 0;

  if ( !lang_initialised )
    lang = MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_UK);

again:
  if ( FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|
		     FORMAT_MESSAGE_IGNORE_INSERTS|
		     FORMAT_MESSAGE_FROM_SYSTEM,
		     NULL,			/* source */
		     id,			/* identifier */
		     lang,
		     (LPTSTR) &msg,
		     0,				/* size */
		     NULL) )			/* arguments */
  { atom_t a = PL_new_atom(msg);

    LocalFree(msg);
    lang_initialised = 1;

    return stringAtom(a);
  } else
  { if ( lang_initialised == 0 )
    { lang = MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT);
      lang_initialised = 1;
      goto again;
    }

    return "Unknown Windows error";
  }
}


		 /*******************************
		 *	  SLEEP/1 SUPPORT	*
		 *******************************/

void
Pause(double t)
{ DWORD msecs = (DWORD)(t * 1000.0);

  Sleep(msecs);
}

		 /*******************************
		 *	 QUERY CPU TIME		*
		 *******************************/

#define nano * 0.0000001
#define ntick 1.0			/* manual says 100.0 ??? */

real
CpuTime()
{ real t;
  HANDLE proc = GetCurrentProcess();
  FILETIME created, exited, kerneltime, usertime;

  if ( GetProcessTimes(proc, &created, &exited, &kerneltime, &usertime) )
  { t = (real)usertime.dwHighDateTime * (4294967296.0 * ntick nano);
    t += (real)usertime.dwLowDateTime  * (ntick nano);
  } else				/* '95, Windows 3.1/win32s */
  { extern long clock_wait_ticks;

    t = (real) (clock() - clock_wait_ticks) / (real) CLOCKS_PER_SEC;
  }

  return t;
}


char *
findExecutable(const char *module, char *exe)
{ int n;
  char buf[MAXPATHLEN];
  HMODULE hmod;

  if ( module )
  { if ( !(hmod = GetModuleHandle(module)) )
      hmod = GetModuleHandle("libpl.dll");
  } else
    hmod = NULL;

  if ( (n = GetModuleFileName(hmod, buf, sizeof(buf))) > 0 )
  { char buf2[MAXPATHLEN];

    buf[n] = EOS;
    _xos_long_file_name(buf, buf2);

    strcpy(exe, buf2);
  } else if ( module )
  { PrologPath(module, buf);

    strcpy(exe, buf);
  } else
    *exe = EOS;

  return exe;
}

		 /*******************************
		 *     SUPPORT FOR SHELL/2	*
		 *******************************/

static char *
win_exec(const char *cmd, const char *how)
{ UINT show;
  char *msg;
  int rval;

  if ( streq(how, "iconic") )
    show = SW_MINIMIZE;
  else /*if ( streq(how, "normal") )*/
    show = SW_NORMAL;

  switch((rval = WinExec(cmd, show)))
  { case 0:
      msg = "Not enough memory";
      break;
    case ERROR_BAD_FORMAT:
      msg = "Bad format";
      break;
    case ERROR_FILE_NOT_FOUND:
      msg = "File not found";
      break;
    case ERROR_PATH_NOT_FOUND:
      msg = "Path not found";
      break;
    default:
      if ( rval > 31 )
	msg = NULL;
      else
	msg = "Unknown error";
      break;
  }

  return msg;
}


int
System(char *command)
{ STARTUPINFO sinfo;
  PROCESS_INFORMATION pinfo;
  int shell_rval;

  memset(&sinfo, 0, sizeof(sinfo));
  sinfo.cb = sizeof(sinfo);

  if ( CreateProcess(NULL,			/* module */
		     command,			/* command line */
		     NULL,			/* Security stuff */
		     NULL,			/* Thread security stuff */
		     FALSE,			/* Inherit handles */
		     NORMAL_PRIORITY_CLASS,	/* flags */
		     NULL,			/* environment */
		     NULL,			/* CWD */
		     &sinfo,			/* startup info */
		     &pinfo) )			/* process into */
  { BOOL rval;
    DWORD code;

    CloseHandle(pinfo.hThread);			/* don't need this */
      
    do
    { MSG msg;

      if ( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
      } else
	Sleep(50);

      rval = GetExitCodeProcess(pinfo.hProcess, &code);
    } while(rval == TRUE && code == STILL_ACTIVE);

    shell_rval = (rval == TRUE ? code : -1);
    CloseHandle(pinfo.hProcess);
  } else
    return shell_rval = -1;

  return shell_rval;
}


word
pl_win_exec(term_t cmd, term_t how)
{ char *s;
  char *h;

  if ( PL_get_chars(cmd, &s, CVT_ALL) &&
       PL_get_atom_chars(how, &h) )
  { char *msg = win_exec(s, h);

    if ( msg )
      return warning("win_exec/2: %s", msg);
    else
      succeed;
  } else
    return warning("win_exec/2: instantiation fault");
}


char *
getenv3(const char *name, char *buf, int len)
{ if ( GetEnvironmentVariable(name, buf, len) )
    return buf;
  
  return NULL;
}

/* What does this return if the variable is not defined?
*/

int
getenvl(const char *name)
{ return GetEnvironmentVariable(name, NULL, 0);
}

#if _DEBUG
void
initHeapDebug(void)
{ int tmpFlag = _CrtSetDbgFlag( _CRTDBG_REPORT_FLAG );

  if ( !(tmpFlag & _CRTDBG_CHECK_ALWAYS_DF) )
  { /*PlMessage("Setting malloc() debugging");*/
    tmpFlag |= _CRTDBG_CHECK_ALWAYS_DF;
    _CrtSetDbgFlag(tmpFlag);
  } /*else
    PlMessage("Malloc debugging already set");*/
}
#endif

foreign_t
pl_win_module_file(term_t module, term_t file)
{ char buf[MAXPATHLEN];
  char *m;
  char *f;

  if ( !PL_get_chars_ex(module, &m, CVT_ALL) )
    fail;
  if ( (f = findExecutable(m, buf)) )
    return PL_unify_atom_chars(file, f);

  fail;
}


		 /*******************************
		 *	DLOPEN AND FRIENDS	*
		 *******************************/

#ifdef EMULATE_DLOPEN

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions emulate the bits from the ELF shared object interface we
need. They are used  by  pl-load.c,   which  defines  the  actual Prolog
interface.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *dlmsg;

void *
dlopen(const char *file, int flags)
{ HINSTANCE h;

  if ( (h = LoadLibrary(file)) )
  { dlmsg = "No Error";
    return (void *)h;
  }

  dlmsg = WinError();
  return NULL;
}


const char *
dlerror()
{ return dlmsg;
}


void *
dlsym(void *handle, char *symbol)
{ void *addr = GetProcAddress(handle, symbol);

  if ( addr )
  { dlmsg = "No Error";
    return addr;
  }
  
  dlmsg = WinError();
  return NULL;
}


int
dlclose(void *handle)
{ FreeLibrary(handle);

  return 0;
}

#endif /*EMULATE_DLOPEN*/

#endif /*__WINDOWS__*/


