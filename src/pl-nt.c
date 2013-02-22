/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifdef __WINDOWS__
#define WINVER 0x0501
#if (_MSC_VER >= 1300) || __MINGW32__
#include <winsock2.h>			/* Needed on VC8 */
#include <windows.h>
#else
#include <windows.h>			/* Needed for MSVC 5&6 */
#include <winsock2.h>
#endif

#ifdef __MINGW32__
#ifndef _WIN32_IE
#define _WIN32_IE 0x0400
#endif
/* FIXME: these are copied from SWI-Prolog.h. */
#define PL_MSG_EXCEPTION_RAISED -1
#define PL_MSG_IGNORED 0
#define PL_MSG_HANDLED 1
#endif

#include "pl-incl.h"
#include "os/pl-utf8.h"
#include <process.h>
#include "os/pl-ctype.h"
#include <stdio.h>
#include <stdarg.h>
#include "os/SWI-Stream.h"
#include <process.h>
#include <winbase.h>
#ifdef HAVE_CRTDBG_H
#include <crtdbg.h>
#endif


		 /*******************************
		 *	       CONSOLE		*
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


int
PL_wait_for_console_input(void *handle)
{ BOOL rc;
  HANDLE hConsole = handle;

  for(;;)
  { rc = MsgWaitForMultipleObjects(1,
				   &hConsole,
				   FALSE,	/* wait for either event */
				   INFINITE,
				   QS_ALLINPUT);

    if ( rc == WAIT_OBJECT_0+1 )
    { MSG msg;

      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
      }
    } else if ( rc == WAIT_OBJECT_0 )
    { return TRUE;
    } else
    { Sdprintf("MsgWaitForMultipleObjects(): 0x%x\n", rc);
    }
  }
}


		 /*******************************
		 *	    MESSAGE BOX		*
		 *******************************/

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
    int64_t hwndi;
    HWND hwnd = NULL;
    static atom_t ATOM_hwnd = 0;

    if ( !ATOM_hwnd )
      ATOM_hwnd = PL_new_atom("hwnd");

    if ( PL_current_prolog_flag(ATOM_hwnd, PL_INTEGER, &hwndi) )
      hwnd = (HWND)hwndi;

    vsprintf(buf, fm, args);
    MessageBox(hwnd, buf, "SWI-Prolog", MB_OK|MB_TASKMODAL);
  }

  va_end(args);
}



		 /*******************************
		 *	WinAPI ERROR CODES	*
		 *******************************/

const char *
WinError(void)
{ int id = GetLastError();
  char *msg;
  static WORD lang;
  static int lang_initialised = 0;

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

int
Pause(double t)
{ HANDLE h;

  if ( (h = CreateWaitableTimer(NULL, TRUE, NULL)) )
  { LARGE_INTEGER ft;

    ft.QuadPart = -(LONGLONG)(t * 10000000.0); /* 100 nanosecs per tick */

    SetWaitableTimer(h, &ft, 0, NULL, NULL, FALSE);
    for(;;)
    { int rc = MsgWaitForMultipleObjects(1,
					 &h,
					 FALSE,
					 INFINITE,
					 QS_ALLINPUT);
      if ( rc == WAIT_OBJECT_0+1 )
      { MSG msg;

	while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
	{ TranslateMessage(&msg);
	  DispatchMessage(&msg);
	}

	if ( PL_handle_signals() < 0 )
	{ CloseHandle(h);
	  return FALSE;
	}
      } else
	break;
    }
    CloseHandle(h);

    return TRUE;
  } else				/* Pre NT implementation */
  { DWORD msecs = (DWORD)(t * 1000.0);

    while( msecs >= 100 )
    { Sleep(100);
      if ( PL_handle_signals() < 0 )
	return FALSE;
      msecs -= 100;
    }
    if ( msecs > 0 )
      Sleep(msecs);

    return TRUE;
  }
}


		 /*******************************
		 *	  SET FILE SIZE		*
		 *******************************/

#ifndef HAVE_FTRUNCATE

int
ftruncate(int fileno, int64_t length)
{ errno_t e;

  if ( (e=_chsize_s(fileno, length)) == 0 )
    return 0;

  errno = e;
  return -1;
}

#endif


		 /*******************************
		 *	 QUERY CPU TIME		*
		 *******************************/

#define nano * 0.0000001
#define ntick 1.0			/* manual says 100.0 ??? */

double
CpuTime(cputime_kind which)
{ double t;
  HANDLE proc = GetCurrentProcess();
  FILETIME created, exited, kerneltime, usertime;

  if ( GetProcessTimes(proc, &created, &exited, &kerneltime, &usertime) )
  { FILETIME *p;

    switch ( which )
    { case CPU_USER:
	p = &usertime;
	break;
      case CPU_SYSTEM:
	p = &kerneltime;
        break;
      default:
	assert(0);
        return 0.0;
    }
    t = (double)p->dwHighDateTime * (4294967296.0 * ntick nano);
    t += (double)p->dwLowDateTime  * (ntick nano);
  } else				/* '95, Windows 3.1/win32s */
  { t = 0.0;
  }

  return t;
}


static int
CpuCount(void)
{ SYSTEM_INFO si;

  GetSystemInfo(&si);

  return si.dwNumberOfProcessors;
}


void
setOSPrologFlags(void)
{ PL_set_prolog_flag("cpu_count", PL_INTEGER, CpuCount());
}


char *
findExecutable(const char *module, char *exe)
{ int n;
  wchar_t wbuf[MAXPATHLEN];
  HMODULE hmod;

  if ( module )
  { if ( !(hmod = GetModuleHandle(module)) )
    { hmod = GetModuleHandle("libswipl.dll");
      DEBUG(0,
	    Sdprintf("Warning: could not find module from \"%s\"\n"
		     "Warning: Trying %s to find home\n",
		     module,
		     hmod ? "\"LIBPL.DLL\"" : "executable"));
    }
  } else
    hmod = NULL;

  if ( (n = GetModuleFileNameW(hmod, wbuf, MAXPATHLEN)) > 0 )
  { wbuf[n] = EOS;
    return _xos_long_file_name_toA(wbuf, exe, MAXPATHLEN);
  } else if ( module )
  { char buf[MAXPATHLEN];
    PrologPath(module, buf, sizeof(buf));

    strcpy(exe, buf);
  } else
    *exe = EOS;

  return exe;
}

		 /*******************************
		 *     SUPPORT FOR SHELL/2	*
		 *******************************/

typedef struct
{ const char *name;
  UINT        id;
} showtype;

static int
get_showCmd(term_t show, UINT *cmd)
{ char *s;
  showtype *st;
  static showtype types[] =
  { { "hide",		 SW_HIDE },
    { "maximize",	 SW_MAXIMIZE },
    { "minimize",	 SW_MINIMIZE },
    { "restore",	 SW_RESTORE },
    { "show",		 SW_SHOW },
    { "showdefault",	 SW_SHOWDEFAULT },
    { "showmaximized",   SW_SHOWMAXIMIZED },
    { "showminimized",   SW_SHOWMINIMIZED },
    { "showminnoactive", SW_SHOWMINNOACTIVE },
    { "showna",          SW_SHOWNA },
    { "shownoactive",    SW_SHOWNOACTIVATE },
    { "shownormal",      SW_SHOWNORMAL },
					/* compatibility */
    { "normal",		 SW_SHOWNORMAL },
    { "iconic",		 SW_MINIMIZE },
    { NULL, 0 },
  };

  if ( show == 0 )
  { *cmd = SW_SHOWNORMAL;
    succeed;
  }

  if ( !PL_get_chars(show, &s, CVT_ATOM|CVT_EXCEPTION) )
    fail;
  for(st=types; st->name; st++)
  { if ( streq(st->name, s) )
    { *cmd = st->id;
      succeed;
    }
  }

  return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		  PL_new_atom("win_show"), show);
}



static int
win_exec(size_t len, const wchar_t *cmd, UINT show)
{ GET_LD
  STARTUPINFOW startup;
  PROCESS_INFORMATION info;
  int rval;
  wchar_t *wcmd;

  memset(&startup, 0, sizeof(startup));
  startup.cb = sizeof(startup);
  startup.wShowWindow = show;

					/* ensure 0-terminated */
  wcmd = PL_malloc((len+1)*sizeof(wchar_t));
  memcpy(wcmd, cmd, len*sizeof(wchar_t));
  wcmd[len] = 0;

  rval = CreateProcessW(NULL,		/* app */
			wcmd,
			NULL, NULL,	/* security */
			FALSE,		/* inherit handles */
			0,		/* flags */
			NULL,		/* environment */
			NULL,		/* Directory */
			&startup,
			&info);		/* process info */
  PL_free(wcmd);

  if ( rval )
  { CloseHandle(info.hProcess);
    CloseHandle(info.hThread);

    succeed;
  } else
  { term_t tmp = PL_new_term_ref();

    return ( PL_unify_wchars(tmp, PL_ATOM, len, cmd) &&
	     PL_error(NULL, 0, WinError(), ERR_SHELL_FAILED, tmp)
	   );
  }
}


static void
utf8towcs(wchar_t *o, const char *src)
{ for( ; *src; )
  { int wc;

    src = utf8_get_char(src, &wc);
    *o++ = wc;
  }
  *o = 0;
}


int
System(char *command)			/* command is a UTF-8 string */
{ STARTUPINFOW sinfo;
  PROCESS_INFORMATION pinfo;
  int shell_rval;
  size_t len;
  wchar_t *wcmd;

  memset(&sinfo, 0, sizeof(sinfo));
  sinfo.cb = sizeof(sinfo);

  len = utf8_strlen(command, strlen(command));
  wcmd = PL_malloc((len+1)*sizeof(wchar_t));
  utf8towcs(wcmd, command);

  if ( CreateProcessW(NULL,			/* module */
		      wcmd,			/* command line */
		      NULL,			/* Security stuff */
		      NULL,			/* Thread security stuff */
		      FALSE,			/* Inherit handles */
		      CREATE_NO_WINDOW,		/* flags */
		      NULL,			/* environment */
		      NULL,			/* CWD */
		      &sinfo,			/* startup info */
		      &pinfo) )			/* process into */
  { BOOL rval;
    DWORD code;

    CloseHandle(pinfo.hThread);			/* don't need this */
    PL_free(wcmd);

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
  { PL_free(wcmd);
    return shell_rval = -1;
  }

  return shell_rval;
}


word
pl_win_exec(term_t cmd, term_t how)
{ wchar_t *s;
  size_t len;
  UINT h;

  if ( PL_get_wchars(cmd, &len, &s, CVT_ALL|CVT_EXCEPTION) &&
       get_showCmd(how, &h) )
  { return win_exec(len, s, h);
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
  { SE_ERR_NOASSOC,	    "No association" },
  { SE_ERR_OOM,		    "Not enough memory" },
  { SE_ERR_PNF,		    "Path not found (PNF)" },
  { SE_ERR_SHARE,	    "Sharing violation" },
  { 0,			    NULL }
};


static int
win_shell(term_t op, term_t file, term_t how)
{ size_t lo, lf;
  wchar_t *o, *f;
  UINT h;
  HINSTANCE instance;

  if ( !PL_get_wchars(op,   &lo, &o, CVT_ALL|CVT_EXCEPTION|BUF_RING) ||
       !PL_get_wchars(file, &lf, &f, CVT_ALL|CVT_EXCEPTION|BUF_RING) ||
       !get_showCmd(how, &h) )
    fail;

  instance = ShellExecuteW(NULL, o, f, NULL, NULL, h);

  if ( (intptr_t)instance <= 32 )
  { const shell_error *se;

    for(se = se_errors; se->message; se++)
      { if ( se->eno == (int)(intptr_t)instance )
	return PL_error(NULL, 0, se->message, ERR_SHELL_FAILED, file);
    }
    PL_error(NULL, 0, NULL, ERR_SHELL_FAILED, file);
  }

  succeed;
}


static
PRED_IMPL("win_shell", 2, win_shell2, 0)
{ return win_shell(A1, A2, 0);
}


static
PRED_IMPL("win_shell", 3, win_shell3, 0)
{ return win_shell(A1, A2, A3);
}


foreign_t
pl_win_module_file(term_t module, term_t file)
{ char buf[MAXPATHLEN];
  char *m;
  char *f;

  if ( !PL_get_chars(module, &m, CVT_ALL|CVT_EXCEPTION) )
    fail;
  if ( (f = findExecutable(m, buf)) )
    return PL_unify_atom_chars(file, f);

  fail;
}

		 /*******************************
		 *	  WINDOWS MESSAGES	*
		 *******************************/

LRESULT
PL_win_message_proc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
#ifdef O_PLMT
  if ( hwnd == NULL &&
       message == WM_SIGNALLED &&
       wParam == 0 &&			/* or another constant? */
       lParam == 0 )
  { if ( PL_handle_signals() < 0 )
      return PL_MSG_EXCEPTION_RAISED;

    return PL_MSG_HANDLED;
  }
#endif

  return PL_MSG_IGNORED;
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

#ifdef HAVE_LIBLOADERAPI_H
#include <LibLoaderAPI.h>
#else
#ifndef LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR
#define LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR 0x00000100
#endif
#ifndef LOAD_LIBRARY_SEARCH_DEFAULT_DIRS
#define LOAD_LIBRARY_SEARCH_DEFAULT_DIRS 0x00001000
#endif
typedef void * DLL_DIRECTORY_COOKIE;
#endif

static const char *dlmsg;
static DLL_DIRECTORY_COOKIE WINAPI (*f_AddDllDirectoryW)(wchar_t* dir);
static BOOL WINAPI (*f_RemoveDllDirectory)(DLL_DIRECTORY_COOKIE);

static DWORD
load_library_search_flags(void)
{ static int done = FALSE;
  static DWORD flags = 0;

  if ( !done )
  { HMODULE kernel = GetModuleHandle(TEXT("kernel32.dll"));

    if ( (f_AddDllDirectoryW   = (void*)GetProcAddress(kernel, "AddDllDirectory")) &&
	 (f_RemoveDllDirectory = (void*)GetProcAddress(kernel, "RemoveDllDirectory")) )
    { flags = ( LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR|
		LOAD_LIBRARY_SEARCH_DEFAULT_DIRS );
    }
    done = TRUE;
  }

  return flags;
}


static
PRED_IMPL("win_add_dll_directory", 2, win_add_dll_directory, 0)
{ PRED_LD
  char *dirs;

  if ( PL_get_file_name(A1, &dirs, REP_UTF8) )
  { size_t len = utf8_strlen(dirs, strlen(dirs));
    wchar_t *dirw = alloca((len+10)*sizeof(wchar_t));
    DLL_DIRECTORY_COOKIE cookie;

    if ( _xos_os_filenameW(dirs, dirw, len+10) == NULL )
      return PL_representation_error("file_name");
    if ( load_library_search_flags() )
    { if ( (cookie = (*f_AddDllDirectoryW)(dirw)) )
	return PL_unify_int64(A2, (int64_t)cookie);
      return PL_error(NULL, 0, WinError(), ERR_SYSCALL, "AddDllDirectory()");
    } else
      return FALSE;
  } else
    return FALSE;
}


static
PRED_IMPL("win_remove_dll_directory", 1, win_remove_dll_directory, 0)
{ int64_t icookie;

  if ( PL_get_int64_ex(A1, &icookie) )
  { if ( f_RemoveDllDirectory )
    { if ( (*f_RemoveDllDirectory)((DLL_DIRECTORY_COOKIE)icookie) )
	return TRUE;

      return PL_error(NULL, 0, WinError(), ERR_SYSCALL, "RemoveDllDirectory()");
    } else
      return FALSE;
  } else
    return FALSE;
}


static int
is_windows_abs_path(const wchar_t *path)
{ if ( path[1] == ':' && path[0] < 0x80 && iswalpha(path[0]) )
    return TRUE;			/* drive */
  if ( path[0] == '\\' && path[1] == '\\' )
    return TRUE;			/* UNC */

  return FALSE;
}

void *
dlopen(const char *file, int flags)	/* file is in UTF-8, POSIX path */
{ HINSTANCE h;
  DWORD llflags = 0;
  size_t len = utf8_strlen(file, strlen(file));
  wchar_t *wfile = alloca((len+10)*sizeof(wchar_t));

  if ( !wfile )
  { dlmsg = "No memory";
    return NULL;
  }

  if ( _xos_os_filenameW(file, wfile, len+10) == NULL )
  { dlmsg = "Name too long";
    return NULL;
  }

  if ( is_windows_abs_path(wfile) )
    llflags |= load_library_search_flags();

  if ( (h = LoadLibraryExW(wfile, NULL, llflags)) )
  { dlmsg = "No Error";
    return (void *)h;
  }

  dlmsg = WinError();
  return NULL;
}


const char *
dlerror(void)
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
		 *	 SNPRINTF MADNESS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MS-Windows _snprintf() may look like C99 snprintf(), but is is not quite
the same: on overflow, the buffer is   *not* 0-terminated and the return
is negative (unspecified how negative).  The   code  below  works around
this, returning count on overflow. This is still not the same as the C99
version that returns the  number  of   characters  that  would have been
written, but it seems to be enough for our purposes.

See http://www.di-mgt.com.au/cprog.html#snprintf

The above came from the provided link, but it is even worse (copied from
VS2005 docs):

  - If len < count, then len characters are stored in buffer, a
  null-terminator is appended, and len is returned.

  - If len = count, then len characters are stored in buffer, no
  null-terminator is appended, and len is returned.

  - If len > count, then count characters are stored in buffer, no
  null-terminator is appended, and a negative value is returned.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ms_snprintf(char *buffer, size_t count, const char *fmt, ...)
{ va_list ap;
  int ret;

  va_start(ap, fmt);
  ret = _vsnprintf(buffer, count-1, fmt, ap);
  va_end(ap);

  if ( ret < 0 || ret == count )
  { ret = (int)count;
    buffer[count-1] = '\0';
  }

  return ret;
}



		 /*******************************
		 *	      FOLDERS		*
		 *******************************/

#ifdef HAVE_SHLOBJ_H
#include <shlobj.h>
#endif

typedef struct folderid
{ int csidl;
  const char *name;
} folderid;

static const folderid folderids[] =
{ { CSIDL_COMMON_ALTSTARTUP, "common_altstartup" },
  { CSIDL_ALTSTARTUP, "altstartup" },
  { CSIDL_APPDATA, "appdata" },
  { CSIDL_CONTROLS, "controls" },
  { CSIDL_COOKIES, "cookies" },
  { CSIDL_DESKTOP, "desktop" },
  { CSIDL_COMMON_DESKTOPDIRECTORY, "common_desktopdirectory" },
  { CSIDL_DESKTOPDIRECTORY, "desktopdirectory" },
  { CSIDL_COMMON_FAVORITES, "common_favorites" },
  { CSIDL_FAVORITES, "favorites" },
  { CSIDL_FONTS, "fonts" },
  { CSIDL_HISTORY, "history" },
  { CSIDL_INTERNET_CACHE, "internet_cache" },
  { CSIDL_INTERNET, "internet" },
  { CSIDL_DRIVES, "drives" },
  { CSIDL_PERSONAL, "personal" },
  { CSIDL_NETWORK, "network" },
  { CSIDL_NETHOOD, "nethood" },
  { CSIDL_PERSONAL, "personal" },
  { CSIDL_PRINTERS, "printers" },
  { CSIDL_PRINTHOOD, "printhood" },
  { CSIDL_COMMON_PROGRAMS, "common_programs" },
  { CSIDL_PROGRAMS, "programs" },
  { CSIDL_RECENT, "recent" },
  { CSIDL_BITBUCKET, "bitbucket" },
  { CSIDL_SENDTO, "sendto" },
  { CSIDL_COMMON_STARTMENU, "common_startmenu" },
  { CSIDL_STARTMENU, "startmenu" },
  { CSIDL_COMMON_STARTUP, "common_startup" },
  { CSIDL_STARTUP, "startup" },
  { CSIDL_TEMPLATES, "templates" },
  { 0, NULL }
};


static int
unify_csidl_path(term_t t, int csidl)
{ wchar_t buf[MAX_PATH];

  if ( SHGetSpecialFolderPathW(0, buf, csidl, FALSE) )
  { wchar_t *p;

    for(p=buf; *p; p++)
    { if ( *p == '\\' )
	*p = '/';
    }

    return PL_unify_wchars(t, PL_ATOM, -1, buf);
  } else
    return PL_error(NULL, 0, WinError(), ERR_SYSCALL, "SHGetSpecialFolderPath");
}


static
PRED_IMPL("win_folder", 2, win_folder, PL_FA_NONDETERMINISTIC)
{ GET_LD
  int n;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
      if ( PL_is_variable(A1) )
      { n = 0;
	goto generate;
      } else
      { char *s;

	if ( PL_get_chars(A1, &s, CVT_ATOM|CVT_EXCEPTION) )
	{ const folderid *fid;

	  for(fid = folderids; fid->name; fid++)
	  { if ( streq(s, fid->name) )
	      return unify_csidl_path(A2, fid->csidl);
	  }

	  { atom_t dom = PL_new_atom("win_folder");

	    PL_error(NULL, 0, NULL, ERR_DOMAIN, dom, A1);
	    PL_unregister_atom(dom);
	    return FALSE;
	  }
	} else
	  return FALSE;
      }
    case FRG_REDO:
    { fid_t fid;

      n = (int)CTX_INT+1;

      generate:
	fid = PL_open_foreign_frame();
	for(; folderids[n].name; n++)
	{ if ( unify_csidl_path(A2, folderids[n].csidl) &&
	       PL_unify_atom_chars(A1, folderids[n].name) )
	  { PL_close_foreign_frame(fid);
	    ForeignRedoInt(n);
	  }
	  PL_rewind_foreign_frame(fid);
	}
	PL_close_foreign_frame(fid);
	return FALSE;
    }
    default:
      succeed;
  }
}



		 /*******************************
		 *	      REGISTRY		*
		 *******************************/

#define wstreq(s,q) (wcscmp((s), (q)) == 0)

static HKEY
reg_open_key(const wchar_t *which, int create)
{ HKEY key = HKEY_CURRENT_USER;
  DWORD disp;
  LONG rval;

  while(*which)
  { wchar_t buf[256];
    wchar_t *s;
    HKEY tmp;

    for(s=buf; *which && !(*which == '/' || *which == '\\'); )
      *s++ = *which++;
    *s = '\0';
    if ( *which )
      which++;

    if ( wstreq(buf, L"HKEY_CLASSES_ROOT") )
    { key = HKEY_CLASSES_ROOT;
      continue;
    } else if ( wstreq(buf, L"HKEY_CURRENT_USER") )
    { key = HKEY_CURRENT_USER;
      continue;
    } else if ( wstreq(buf, L"HKEY_LOCAL_MACHINE") )
    { key = HKEY_LOCAL_MACHINE;
      continue;
    } else if ( wstreq(buf, L"HKEY_USERS") )
    { key = HKEY_USERS;
      continue;
    }

    DEBUG(2, Sdprintf("Trying %s\n", buf));
    if ( RegOpenKeyExW(key, buf, 0L, KEY_READ, &tmp) == ERROR_SUCCESS )
    { RegCloseKey(key);
      key = tmp;
      continue;
    }

    if ( !create )
      return NULL;

    rval = RegCreateKeyExW(key, buf, 0, L"", 0,
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

static
PRED_IMPL("win_registry_get_value", 3, win_registry_get_value, 0)
{ GET_LD
  DWORD type;
  BYTE  data[MAXREGSTRLEN];
  DWORD len = sizeof(data);
  size_t klen, namlen;
  wchar_t *k, *name;
  HKEY key;

  term_t Key = A1;
  term_t Name = A2;
  term_t Value = A3;

  if ( !PL_get_wchars(Key, &klen, &k, CVT_ATOM|CVT_EXCEPTION) ||
       !PL_get_wchars(Name, &namlen, &name, CVT_ATOM|CVT_ATOM) )
    return FALSE;
  if ( !(key=reg_open_key(k, FALSE)) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, PL_new_atom("key"), Key);

  DEBUG(9, Sdprintf("key = %p, name = %s\n", key, name));
  if ( RegQueryValueExW(key, name, NULL, &type, data, &len) == ERROR_SUCCESS )
  { RegCloseKey(key);

    switch(type)
    { case REG_SZ:
	return PL_unify_wchars(Value, PL_ATOM,
			       len/sizeof(wchar_t)-1, (wchar_t*)data);
      case REG_DWORD:
	return PL_unify_integer(Value, *((DWORD *)data));
      default:
	warning("get_registry_value/2: Unknown registery-type: %d", type);
        fail;
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
getDefaultsFromRegistry(void)
{ HKEY key;

  if ( (key = reg_open_key(L"HKEY_LOCAL_MACHINE/Software/SWI/Prolog", FALSE)) )
  { setStacksFromKey(key);
    RegCloseKey(key);
  }
  if ( (key = reg_open_key(L"HKEY_CURRENT_USER/Software/SWI/Prolog", FALSE)) )
  { setStacksFromKey(key);
    RegCloseKey(key);
  }
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(win)
  PRED_DEF("win_shell", 2, win_shell2, 0)
  PRED_DEF("win_shell", 3, win_shell3, 0)
  PRED_DEF("win_registry_get_value", 3, win_registry_get_value, 0)
  PRED_DEF("win_folder", 2, win_folder, PL_FA_NONDETERMINISTIC)
  PRED_DEF("win_add_dll_directory", 2, win_add_dll_directory, 0)
  PRED_DEF("win_remove_dll_directory", 1, win_remove_dll_directory, 0)
EndPredDefs

#endif /*__WINDOWS__*/

