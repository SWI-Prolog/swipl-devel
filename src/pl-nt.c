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


static int shell_finished;
static int shell_rval;

static void
waitthread(void *handle)
{ 
#if 0
					/* works fine, but how to get */
					/* exit status? */
  WaitForSingleObject(handle, INFINITE);
  shell_rval = 0;
#else
  if ( _cwait(&shell_rval, (int)handle, _WAIT_CHILD) < 0 )
    warning("_cwait() failed: %s\n", OsError());
#endif
  shell_finished = TRUE;
}


int
System(char *command)
{ STARTUPINFO sinfo;
  PROCESS_INFORMATION pinfo;
  char *e = command + strlen(command);

  while(e > command && isBlank(e[-1]))
    e--;

  if ( e > command && e[-1] == '&' )	/* notepad & */
  { char *tmp = alloca(e-command);
    char *msg;
    memcpy(tmp, command, e-command);

    e = tmp + (e-command-1);
    while(e > tmp && isBlank(e[-1]))
      e--;
    *e = EOS;

    if ( (msg = win_exec(tmp, "normal")) )
    { warning("shell('%s') failed: %s", command, msg);
      return -1;
    }

    return 0;
  }

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
  { CloseHandle(pinfo.hThread);			/* don't need this */
      
    shell_finished = FALSE;
    if ( _beginthread(waitthread, 10240, pinfo.hProcess) < 0 )
    { warning("Failed to create wait-thread: %s", OsError());
      return -1;
    }
    while ( !shell_finished )
    {
#if !defined(MAKE_PL_DLL)
      rlc_dispatch(NULL);		/* TBD */
#endif
    }
    CloseHandle(pinfo.hProcess);

    return shell_rval;
  } else
  { warning("shell('%s') failed: %s\n", command, WinError());
    return -1;
  }
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


