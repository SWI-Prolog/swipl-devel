/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#if defined(__WINDOWS__) || defined(__WIN32__) || defined(WIN32)

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

  if ( PL_get_chars_ex(cmd, &s, CVT_ALL) &&
       PL_get_chars_ex(how, &h, CVT_ATOM) )
  { char *msg = win_exec(s, h);

    if ( msg )
      return warning("win_exec/2: %s", msg);
    else
      succeed;
  } else
    fail;
}

typedef struct
{ int   eno;
  const char *message;
} shell_error;

static const shell_error se_errors[] = 
{ { 0 ,                     "Out of memory or resources" },
  { ERROR_FILE_NOT_FOUND,   "File not found" },
  { ERROR_PATH_NOT_FOUND,   "path not found" },
  { ERROR_BAD_FORMAT,	    "Invalid .EXE" },
  { SE_ERR_ACCESSDENIED,    "Access denied" },
  { SE_ERR_ASSOCINCOMPLETE, "Incomplete association" },
  { SE_ERR_DDEBUSY,	    "DDE server busy" },
  { SE_ERR_DDEFAIL,         "DDE transaction failed" },
  { SE_ERR_DDETIMEOUT,	    "DDE request timed out" },
  { SE_ERR_DLLNOTFOUND,	    "DLL not found" },
  { SE_ERR_FNF,		    "File not found (FNF)" },
  { SE_ERR_NOASSOC, 	    "No association" },
  { SE_ERR_OOM,		    "Not enough memory" }, 
  { SE_ERR_PNF,		    "Path not found (PNF)" },
  { SE_ERR_SHARE,	    "Sharing violation" },
  { 0,			    NULL }
};
 

word
pl_shell_execute(term_t op, term_t file)
{ char *o, *f;
  HINSTANCE instance;

  if ( !PL_get_chars_ex(op,   &o, CVT_ALL) ||
       !PL_get_chars_ex(file, &f, CVT_ALL) )
    fail;
       
  instance = ShellExecute(NULL, o, f, NULL, NULL, 0);

  if ( (long)instance <= 32 )
  { const shell_error *se;

    for(se = se_errors; se->message; se++)
    { if ( se->eno == (int)instance )
	return PL_error(NULL, 0, se->message, ERR_SHELL_FAILED, file);
    }
    PL_error(NULL, 0, NULL, ERR_SHELL_FAILED, file);
  }

  succeed;
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

		 /*******************************
		 *	      REGISTRY		*
		 *******************************/

static HKEY
reg_open_key(const char *which, int create)
{ HKEY key = HKEY_CURRENT_USER;
  DWORD disp;
  LONG rval;

  while(*which)
  { char buf[256];
    char *s;
    HKEY tmp;

    for(s=buf; *which && !(*which == '/' || *which == '\\'); )
      *s++ = *which++;
    *s = '\0';
    if ( *which )
      which++;

    if ( streq(buf, "HKEY_CLASSES_ROOT") )
    { key = HKEY_CLASSES_ROOT;
      continue;
    } else if ( streq(buf, "HKEY_CURRENT_USER") )
    { key = HKEY_CURRENT_USER;
      continue;
    } else if ( streq(buf, "HKEY_LOCAL_MACHINE") )
    { key = HKEY_LOCAL_MACHINE;
      continue;
    } else if ( streq(buf, "HKEY_USERS") )
    { key = HKEY_USERS;
      continue;
    }

    DEBUG(2, Sdprintf("Trying %s\n", buf));
    if ( RegOpenKeyEx(key, buf, 0L, KEY_READ, &tmp) == ERROR_SUCCESS )
    { RegCloseKey(key);
      key = tmp;
      continue;
    }

    if ( !create )
      return NULL;

    rval = RegCreateKeyEx(key, buf, 0, "", 0,
			  KEY_ALL_ACCESS, NULL, &tmp, &disp);
    RegCloseKey(key);
    if ( rval == ERROR_SUCCESS )
      key = tmp;
    else
      return NULL;
  }

  return key;
}

#define MAXREGSTRLEN 1024

foreign_t
pl_get_registry_value(term_t Key, term_t Name, term_t Value)
{ DWORD type;
  BYTE  data[MAXREGSTRLEN];
  DWORD len = sizeof(data);
  char *k;
  char *name;
  HKEY key;

  if ( !PL_get_chars_ex(Key, &k, CVT_ATOM) ||
       !PL_get_chars_ex(Name, &name, CVT_ATOM) )
    return FALSE;
  if ( !(key=reg_open_key(k, FALSE)) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, PL_new_atom("key"), Key);

  DEBUG(9, Sdprintf("key = %p, name = %s\n", key, name));
  if ( RegQueryValueEx(key, name, NULL, &type, data, &len) == ERROR_SUCCESS )
  { RegCloseKey(key);

    switch(type)
    { case REG_SZ:
	return PL_unify_atom_chars(Value, data);
      case REG_DWORD:
	return PL_unify_integer(Value, *((DWORD *)data));
      default:
	return warning("get_registry_value/2: Unknown registery-type");
    }
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the local, global,  trail  and   argument-stack  defaults  from  the
registry.  They  can  be  on  the   HKEY_CURRENT_USER  as  well  as  the
HKEY_LOCAL_MACHINE  registries  to  allow   for    both   user-only  and
system-wide settings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct regdef
{ const char *name;
  int        *address;
} const regdefs[] =
{ { "localSize",    &GD->defaults.local },
  { "globalSize",   &GD->defaults.global },
  { "trailSize",    &GD->defaults.trail },
  { "argumentSize", &GD->defaults.argument },
  { NULL,           NULL }
};


static void
setStacksFromKey(HKEY key)
{ DWORD type;
  BYTE  data[128];
  DWORD len = sizeof(data);
  const struct regdef *rd;

  for(rd = regdefs; rd->name; rd++)
  { if ( RegQueryValueEx(key, rd->name, NULL, &type, data, &len) ==
							ERROR_SUCCESS &&
	 type == REG_DWORD )
    { DWORD v = *((DWORD *)data);
      
      *rd->address = (int)v;
    }
  }
}


void
getDefaultsFromRegistry()
{ HKEY key;

  if ( (key = reg_open_key("HKEY_LOCAL_MACHINE/Software/SWI/Prolog", FALSE)) )
  { setStacksFromKey(key);
    RegCloseKey(key);
  }
  if ( (key = reg_open_key("HKEY_CURRENT_USER/Software/SWI/Prolog", FALSE)) )
  { setStacksFromKey(key);
    RegCloseKey(key);
  }
}

#endif /*__WINDOWS__*/


