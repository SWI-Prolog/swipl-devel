/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Windows (NT) specific stuff
*/

#if defined(__WINDOWS__) || defined(__WIN32__)

#include <windows.h>
#include <process.h>
#undef TRANSPARENT
#include "pl-incl.h"
#include "pl-ctype.h"
#include <stdio.h>
#include <stdarg.h>
#include "pl-stream.h"
#include <console.h>
#include <process.h>


		 /*******************************
		 *	  DUMMY EXTENSION	*
		 *******************************/

PL_extension PL_extensions [] =
{
/*{ "name",	arity,  function,	PL_FA_<flags> },*/

  { NULL,	0, 	NULL,		0 }	/* terminating line */
};

		 /*******************************
		 *	    MESSAGE BOX		*
		 *******************************/

void
PlMessage(const char *fm, ...)
{ va_list(args);
  char buf[1024];

  va_start(args, fm);
  vsprintf(buf, fm, args);
  va_end(args);

  MessageBox(NULL, buf, "SWI-Prolog", MB_OK|MB_TASKMODAL);
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
  { atom_t a = lookupAtom(msg);

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


#endif /*__WINDOWS__*/


