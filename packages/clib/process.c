/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/*#define O_DEBUG 1*/
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "error.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

static atom_t ATOM_stdin;
static atom_t ATOM_stdout;
static atom_t ATOM_stderr;
static atom_t ATOM_std;
static atom_t ATOM_null;
static atom_t ATOM_process;
static atom_t ATOM_detached;
static atom_t ATOM_cwd;
static atom_t ATOM_window;
static atom_t ATOM_timeout;
static atom_t ATOM_release;
static atom_t ATOM_infinite;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_resource_error1;
static functor_t FUNCTOR_process_error2;
static functor_t FUNCTOR_system_error2;
static functor_t FUNCTOR_pipe1;
static functor_t FUNCTOR_exit1;
static functor_t FUNCTOR_killed1;

#define MAYBE 2

#if O_DEBUG
#define DEBUG(g) g
#else
#define DEBUG(g) (void)0
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ISSUES:
	- Deal with child errors (no cwd, cannot execute, etc.)
	- Windows version
	- Complete test suite
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static int
type_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_type_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
domain_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_domain_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
resource_error(const char *resource)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_resource_error1,
		        PL_CHARS, resource,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


		 /*******************************
		 *	       ADMIN		*
		 *******************************/

#ifdef __WINDOWS__
#include <windows.h>
#include <stdio.h>
#include <fcntl.h>
#include <io.h>
typedef DWORD  pid_t;
#else
#endif

typedef enum std_type
{ std_std,
  std_null,
  std_pipe
} std_type;


typedef struct p_stream
{ term_t   term;			/* P in pipe(P) */
  std_type type;			/* type of stream */
#ifdef __WINDOWS__
  HANDLE   fd[2];			/* pipe handles */
#else
  int      fd[2];			/* pipe handles */
#endif
} p_stream;


typedef struct p_options
{ atom_t exe_name;			/* exe as atom */
#ifdef __WINDOWS__
  wchar_t *exe;				/* Executable */
  wchar_t *cmdline;			/* Command line */
  wchar_t *cwd;				/* CWD of new process */
#else
  char *exe;				/* Executable */
  char **argv;				/* argument vector */
  char *cwd;				/* CWD of new process */
#endif
  term_t pid;				/* process(PID) */
  int pipes;				/* #pipes found */
  p_stream streams[3];
  int   detached;			/* create as detached */
  int   window;				/* Show a window? */
} p_options;


typedef struct wait_options
{ double timeout;
  int	 has_timeout;
  int	 release;
} wait_options;


#ifdef __WINDOWS__
static int win_command_line(term_t t, int arity,
			    const wchar_t *exepath, wchar_t **cmdline);
#endif

static int
get_stream(term_t t, p_options *info, p_stream *stream)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  { if ( a == ATOM_null )
    { stream->type = std_null;
      return TRUE;
    } else if ( a == ATOM_std )
    { stream->type = std_std;
      return TRUE;
    } else
    { return domain_error(t, "process_stream");
    }
  } else if ( PL_is_functor(t, FUNCTOR_pipe1) )
  { stream->term = PL_new_term_ref();
    PL_get_arg(1, t, stream->term);
    stream->type = std_pipe;
    info->pipes++;
    return TRUE;
  } else
    return type_error(t, "process_stream");
}


static int
parse_options(term_t options, p_options *info)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg = PL_new_term_ref();

  info->window = MAYBE;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return type_error(head, "option");
    PL_get_arg(1, head, arg);

    if ( name == ATOM_stdin )
    { if ( !get_stream(arg, info, &info->streams[0]) )
	return FALSE;
    } else if ( name == ATOM_stdout )
    { if ( !get_stream(arg, info, &info->streams[1]) )
	return FALSE;
    } else if ( name == ATOM_stderr )
    { if ( !get_stream(arg, info, &info->streams[2]) )
	return FALSE;
    } else if ( name == ATOM_process )
    { info->pid = PL_copy_term_ref(arg);
    } else if ( name == ATOM_detached )
    { if ( !PL_get_bool(arg, &info->detached) )
	return type_error(arg, "boolean");
    } else if ( name == ATOM_cwd )
    {
#ifdef __WINDOWS__
      if ( !PL_get_wchars(arg, NULL, &info->cwd,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC) )
	return FALSE;
#else
      if ( !PL_get_chars(arg, &info->cwd,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
	return FALSE;
#endif
    } else if ( name == ATOM_window )
    { if ( !PL_get_bool(arg, &info->window) )
	return type_error(arg, "boolean");
    } else
      return domain_error(head, "process_option");
  }

  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  return TRUE;
}


static int
get_exe(term_t exe, p_options *info)
{ int arity;
  term_t arg = PL_new_term_ref();

  if ( !PL_get_name_arity(exe, &info->exe_name, &arity) )
    return type_error(exe, "callable");

  PL_put_atom(arg, info->exe_name);

#ifdef __WINDOWS__
  if ( !PL_get_wchars(arg, NULL, &info->exe, CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC) )
    return FALSE;
  if ( !win_command_line(exe, arity, info->exe, &info->cmdline) )
    return FALSE;
#else /*__WINDOWS__*/
  if ( !PL_get_chars(arg, &info->exe, CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
    return FALSE;

  info->argv = PL_malloc((arity+2)*sizeof(char*));
  memset(info->argv, 0, (arity+2)*sizeof(char*));
  info->argv[0] = strdup(info->exe);
  { int i;

    for(i=1; i<=arity; i++)
    { PL_get_arg(i, exe, arg);
  
      if ( !PL_get_chars(arg, &info->argv[i],
			 CVT_ATOMIC|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
	return FALSE;
    }
    info->argv[i] = NULL;
  }
#endif /*__WINDOWS__*/

  return TRUE;
}


static void
free_options(p_options *info)		/* TBD: close streams */
{ if ( info->exe )
  { PL_free(info->exe);
    info->exe = NULL;
  }
  if ( info->cwd )
  { PL_free(info->cwd);
    info->cwd = NULL;
  }
#ifdef __WINDOWS__
  if ( info->cmdline )
  { PL_free(info->cmdline);
    info->cmdline = NULL;
  }

#else /*__WINDOWS__*/

  if ( info->argv )
  { char **a;
    for(a=info->argv; *a; a++)
    { if ( *a )
	PL_free(*a);
    }
    PL_free(info->argv);

    info->argv = NULL;
  }

#endif /*__WINDOWS__*/
}


		 /*******************************
		 *	   PROCESS READS	*
		 *******************************/

#define	PROCESS_MAGIC	0x29498001

typedef struct process_context
{ int	magic;				/* PROCESS_MAGIC */
#ifdef __WINDOWS__
  HANDLE handle;			/* process handle */
#else
  pid_t	pid;				/* the process id */
#endif
  int   open_mask;			/* Open streams */
  int   pipes[3];			/* stdin/stdout/stderr */
} process_context;

static int wait_for_process(process_context *pc);

static int
process_fd(void *handle, process_context **PC)
{ process_context *pc = (process_context*) ((uintptr_t)handle&~(uintptr_t)0x3);
  int pipe = (int)(uintptr_t)handle & 0x3;

  if ( pc->magic == PROCESS_MAGIC )
  { if ( PC )
      *PC = pc;
    return pc->pipes[pipe];
  }

  return -1;
}


static ssize_t
Sread_process(void *handle, char *buf, size_t size)
{ int fd = process_fd(handle, NULL);

  return (*Sfilefunctions.read)((void*)(uintptr_t)fd, buf, size);
}


static ssize_t
Swrite_process(void *handle, char *buf, size_t size)
{ int fd = process_fd(handle, NULL);

  return (*Sfilefunctions.write)((void*)(uintptr_t)fd, buf, size);
}


static int
Sclose_process(void *handle)
{ process_context *pc;
  int fd = process_fd(handle, &pc);

  if ( fd >= 0 )
  { int which = (int)(uintptr_t)handle & 0x3;
    int rc;

    rc = (*Sfilefunctions.close)((void*)(uintptr_t)fd);
    pc->open_mask &= ~(which+1);
    
    DEBUG(Sdprintf("Closing fd[%d]; mask = 0x%x\n", which, pc->open_mask));

    if ( !pc->open_mask )
    { int rcw = wait_for_process(pc);

      return rcw ? 0 : -1;
    }    

    return rc;
  }

  return -1;
}


static int
Scontrol_process(void *handle, int action, void *arg)
{ process_context *pc;
  int fd = process_fd(handle, &pc);

  switch(action)
  { case SIO_GETFILENO:
    { int *fdp = arg;
      *fdp = fd;
      return 0;
    }
    default:
      return (*Sfilefunctions.control)((void*)(uintptr_t)fd, action, arg);
  }
}


static IOFUNCTIONS Sprocessfunctions =
{ Sread_process,
  Swrite_process,
  NULL,					/* seek */
  Sclose_process,
  Scontrol_process,
  NULL					/* seek64 */
};


static IOSTREAM *
#ifdef __WINDOWS__
open_process_pipe(process_context *pc, int which, HANDLE fd)
#else
open_process_pipe(process_context *pc, int which, int fd)
#endif
{ void *handle;
  int flags;

  pc->open_mask |= (which+1);
#ifdef __WINDOWS__
  pc->pipes[which] = _open_osfhandle((long)fd, _O_BINARY);
#else
  pc->pipes[which] = fd;
#endif

  if ( which == 0 )
    flags = SIO_OUTPUT|SIO_RECORDPOS;
  else
    flags = SIO_INPUT|SIO_RECORDPOS;

  handle = (void *)((uintptr_t)pc | (uintptr_t)which);

  return Snew(handle, flags, &Sprocessfunctions);
}


		 /*******************************
		 *	       OS STUFF		*
		 *******************************/


#ifdef __WINDOWS__

CRITICAL_SECTION process_lock;
#define LOCK()   EnterCriticalSection(&process_lock);
#define UNLOCK() LeaveCriticalSection(&process_lock);

static void
win_init()
{ InitializeCriticalSection(&process_lock);
}


static atom_t
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

    return a;
  } else
  { if ( lang_initialised == 0 )
    { lang = MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT);
      lang_initialised = 1;
      goto again;
    }

    return PL_new_atom("Unknown Windows error");
  }
}


static int
win_error(const char *op)
{ atom_t msg = WinError();
  term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_system_error2,
		        PL_CHARS, op,
		        PL_ATOM, msg,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


typedef struct arg_string
{ size_t  len;
  wchar_t *text;
  wchar_t quote;
} arg_string;

#define QMISC	0x1
#define QDBLQ	0x2
#define QSBLQ	0x4

static int
set_quote(arg_string *as)
{ int needq = 0;
  const wchar_t *s = as->text;

  for(; *s; s++)
  { if ( !iswalnum(*s) )
    { if ( *s == '"' )
	needq |= QDBLQ;
      else if ( *s == '\'' )
	needq |= QSBLQ;
      else
	needq |= QMISC;
    }
  }

  if ( !needq )
  { as->quote = 0;
    return TRUE;
  }
  needq &= ~QMISC;
  switch( needq )
  { case QDBLQ:
      as->quote = '\'';
      return TRUE;
    case 0:
    case QSBLQ:
      as->quote = '"';
      return TRUE;
    default:
      return FALSE;
  }
}


static int
win_command_line(term_t t, int arity, const wchar_t *exe, wchar_t **cline)
{ if ( arity > 0 )
  { arg_string *av = PL_malloc((arity+1)*sizeof(*av));
    term_t arg = PL_new_term_ref();
    size_t cmdlen;
    wchar_t *cmdline, *o;
    const wchar_t *b;
    int i;

    if ( (b=wcsrchr(exe, '\\')) )
      b++;
    else
      b = exe;
    av[0].text = (wchar_t*)b;
    av[0].len = wcslen(av[0].text);
    set_quote(&av[0]);
    cmdlen = av[0].len+(av[0].quote?2:0)+1;

    for( i=1; i<=arity; i++)
    { PL_get_arg(i, t, arg);

      if ( !PL_get_wchars(arg, &av[i].len, &av[i].text,
			  CVT_ATOMIC|CVT_EXCEPTION|BUF_MALLOC) )
	return FALSE;

      if ( wcslen(av[i].text) != av[i].len )
	return domain_error(arg, "no_zero_code_atom");

      if ( !set_quote(&av[i]) )
	return domain_error(arg, "dos_quotable_atom");

      cmdlen += av[i].len+(av[i].quote?2:0)+1;
    }
    
    cmdline = PL_malloc(cmdlen*sizeof(wchar_t));
    for( o=cmdline,i=0; i<=arity; )
    { wchar_t *s = av[i].text;
	  
      if ( av[i].quote )
	*o++ = av[i].quote;
      wcsncpy(o, s, av[i].len);
      o += av[i].len;
      if ( i > 0 )
	PL_free(s);			/* do not free shared exename */
      if ( av[i].quote )
	*o++ = av[i].quote;

      if (++i <= arity)
	*o++ = ' ';
    }
    *o = 0;
    PL_free(av);

    *cline = cmdline;
  } else
  { *cline = NULL;
  }

  return TRUE;
}


typedef struct win_process
{ DWORD pid;
  HANDLE handle;
  struct win_process *next;
} win_process;


static win_process *processes;

static void
register_process(DWORD pid, HANDLE h)
{ win_process *wp = PL_malloc(sizeof(*wp));

  wp->pid = pid;
  wp->handle = h;
  LOCK();
  wp->next = processes;
  processes = wp;
  UNLOCK();
}


static int
unregister_process(DWORD pid)
{ win_process **wpp, *wp;

  LOCK();
  for(wpp=&processes, wp=*wpp; wp; wpp=&wp->next, wp=*wpp)
  { if ( wp->pid == pid )
    { *wpp = wp->next;
      PL_free(wp);
      UNLOCK();
      return TRUE;
    }
  }

  UNLOCK();
  return FALSE;
}


static HANDLE
find_process_from_pid(DWORD pid, const char *pred)
{ win_process *wp;

  LOCK();
  for(wp=processes; wp; wp=wp->next)
  { if ( wp->pid == pid )
    { HANDLE h = wp->handle;
      UNLOCK();
      return h;
    }
  }

  UNLOCK();
  
  if ( pred ) 
  { term_t ex = PL_new_term_ref();

    PL_put_integer(ex, pid);
    pl_error(NULL, 2, NULL, ERR_EXISTENCE,
	     "process", ex);
  }

  return (HANDLE)0;
}


#define WP_TIMEOUT 2

static int
wait_process_handle(HANDLE process, ULONG *rc, DWORD timeout)
{ DWORD wc;

retry:
  wc = MsgWaitForMultipleObjects(1,
				 &process,
				 FALSE,	/* return on any event */
				 timeout,
				 QS_ALLINPUT);

  switch(wc)
  { case WAIT_OBJECT_0:
      if ( !GetExitCodeProcess(process, rc) )
      { win_error("GetExitCodeProcess");
	CloseHandle(process);
	return FALSE;
      }
      CloseHandle(process);
      return TRUE;
    case WAIT_OBJECT_0+1:
    { MSG msg;

      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
	if ( PL_handle_signals() < 0 )
	  return FALSE;
      }
      goto retry;
    }
    case WAIT_TIMEOUT:
      return WP_TIMEOUT;
    default:
      win_error("WaitForSingleObject");
      CloseHandle(process);
      return FALSE;
  }
}


static int
wait_for_pid(pid_t pid, term_t code, wait_options *opts)
{ HANDLE *h;

  if ( (h=find_process_from_pid(pid, "process_wait")) )
  { ULONG rc;
    DWORD timeout;
    int wc;

    if ( opts->has_timeout )
      timeout = (DWORD)(opts->timeout * 1000.0);
    else
      timeout = INFINITE;

    if ( !(wc=wait_process_handle(h, &rc, timeout)) )
      return FALSE;
    if ( wc == WP_TIMEOUT )
      return PL_unify_atom(code, ATOM_timeout);

    unregister_process(pid);

    return PL_unify_term(code, 
			 PL_FUNCTOR, FUNCTOR_exit1,
			   PL_LONG, rc);
  } else
  { return FALSE;
  }
}


static int
wait_for_process(process_context *pc)
{ int rc;
  ULONG prc;

  rc = wait_process_handle(pc->handle, &prc, INFINITE);
  CloseHandle(pc->handle);
  PL_free(pc);
  
  return rc;
}


static int
win_wait_success(atom_t exe, HANDLE process)
{ ULONG rc;

  if ( !wait_process_handle(process, &rc, INFINITE) )
    return FALSE;

  if ( rc != 0 )
  { term_t code = PL_new_term_ref();
    term_t ex = PL_new_term_ref();

    PL_unify_term(ex,
		  PL_FUNCTOR, FUNCTOR_error2,
		    PL_FUNCTOR, FUNCTOR_process_error2,
		      PL_ATOM, exe,
		      PL_FUNCTOR, FUNCTOR_exit1,
		        PL_LONG, rc,
		    PL_VARIABLE);
    return PL_raise_exception(ex);
  }

  return TRUE;
}


static int
create_pipes(p_options *info)
{ int i;
  SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);          /* Length in bytes */
  sa.bInheritHandle = 1;            /* the child must inherit these handles */
  sa.lpSecurityDescriptor = NULL;

  for(i=0; i<3; i++)
  { p_stream *s = &info->streams[i];

    if ( s->term )
    { if ( !CreatePipe(&s->fd[0], &s->fd[1], &sa, 1<<13) )
      { return win_error("CreatePipe");
      }
    }
  }

  return TRUE;
}


static IOSTREAM *
Sopen_handle(HANDLE h, const char *mode)
{ return Sfdopen(_open_osfhandle((long)h, _O_BINARY), mode);
}


static HANDLE
open_null_stream(DWORD access)
{ SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);          /* Length in bytes */
  sa.bInheritHandle = 1;            /* the child must inherit these handles */
  sa.lpSecurityDescriptor = NULL;

  return CreateFile("nul",
		    access,
		    FILE_SHARE_READ|FILE_SHARE_WRITE,
		    &sa,		/* security */
		    OPEN_EXISTING,
		    0,
		    NULL);
}


static int
console_app(void)
{ HANDLE h;

  if ( (h = GetStdHandle(STD_OUTPUT_HANDLE)) != INVALID_HANDLE_VALUE )
  { DWORD mode;

    if ( GetConsoleMode(h, &mode) )
      return TRUE;
  }

  return FALSE;
}


static int
do_create_process(p_options *info)
{ int flags = 0;
  PROCESS_INFORMATION pi;
  STARTUPINFOW si;

  switch(info->window)
  { case MAYBE:
      if ( !console_app() )
	flags |= CREATE_NO_WINDOW;
      break;
    case TRUE:
      break;
    case FALSE:
      flags |= CREATE_NO_WINDOW;
      break;
  }

  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;

				      /* stdin */
  switch( info->streams[0].type )
  { case std_pipe:
      si.hStdInput = info->streams[0].fd[0];
      SetHandleInformation(info->streams[0].fd[1],
			   HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdInput = open_null_stream(GENERIC_READ);
      break;
    case std_std:
      si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
      break;
  }
				      /* stdout */
  switch( info->streams[1].type )
  { case std_pipe:
      si.hStdOutput = info->streams[1].fd[1];
      SetHandleInformation(info->streams[1].fd[0],
			   HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdOutput = open_null_stream(GENERIC_WRITE);
      break;
    case std_std:
      si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
      break;
  }
				      /* stderr */
  switch( info->streams[2].type )
  { case std_pipe:
      si.hStdError = info->streams[2].fd[1];
      SetHandleInformation(info->streams[2].fd[0],
			   HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdError = open_null_stream(GENERIC_WRITE);
      break;
    case std_std:
      si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
      break;
  }

  if ( CreateProcessW(info->exe,
		      info->cmdline,
		      NULL,		/* Process security */
		      NULL,		/* Thread security */
		      TRUE,		/* Inherit handles */
		      flags,		/* Creation flags */
		      NULL,		/* Environment */
		      info->cwd,	/* Directory */
		      &si,		/* Startup info */
		      &pi) )		/* Process information */
  { CloseHandle(pi.hThread);

    if ( info->pipes > 0 && info->pid == 0 )
    { IOSTREAM *s;
      process_context *pc = PL_malloc(sizeof(*pc));

      DEBUG(Sdprintf("Wait on pipes\n"));

      memset(pc, 0, sizeof(*pc));
      pc->magic  = PROCESS_MAGIC;
      pc->handle = pi.hProcess;

      if ( info->streams[0].type == std_pipe )
      { CloseHandle(info->streams[0].fd[0]);
	s = open_process_pipe(pc, 0, info->streams[0].fd[1]);
	PL_unify_stream(info->streams[0].term, s);
      }
      if ( info->streams[1].type == std_pipe )
      { CloseHandle(info->streams[1].fd[1]);
	s = open_process_pipe(pc, 1, info->streams[1].fd[0]);
	PL_unify_stream(info->streams[1].term, s);
      }
      if ( info->streams[2].type == std_pipe )
      { CloseHandle(info->streams[2].fd[1]);
	s = open_process_pipe(pc, 2, info->streams[2].fd[0]);
	PL_unify_stream(info->streams[2].term, s);
      }
      
      return TRUE;
    } else if ( info->pipes > 0 )
    { IOSTREAM *s;

      if ( info->streams[0].type == std_pipe )
      { CloseHandle(info->streams[0].fd[0]);
	s = Sopen_handle(info->streams[0].fd[1], "w");
	PL_unify_stream(info->streams[0].term, s);
      }
      if ( info->streams[1].type == std_pipe )
      { CloseHandle(info->streams[1].fd[1]);
	s = Sopen_handle(info->streams[1].fd[0], "r");
	PL_unify_stream(info->streams[1].term, s);
      }
      if ( info->streams[2].type == std_pipe )
      { CloseHandle(info->streams[2].fd[1]);
	s = Sopen_handle(info->streams[2].fd[0], "r");
	PL_unify_stream(info->streams[2].term, s);
      }
    }

    if ( info->pid )
    { register_process(pi.dwProcessId, pi.hProcess);
      return PL_unify_integer(info->pid, pi.dwProcessId);
    }

    return win_wait_success(info->exe_name, pi.hProcess);
  } else
  { return win_error("CreateProcess");
  }
}

#else /*__WINDOWS__*/

static int
create_pipes(p_options *info)
{ int i;

  for(i=0; i<3; i++)
  { p_stream *s = &info->streams[i];

    if ( s->term )
    { if ( pipe(s->fd) )
      { assert(errno = EMFILE);
	return resource_error("open_files");
      }
    }
  }

  return TRUE;
}


static int
unify_exit_status(term_t code, int status)
{ if ( WIFEXITED(status) )
  { return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_exit1,
			   PL_INT, (int)WEXITSTATUS(status));
  } else if ( WIFSIGNALED(status) )
  { return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_killed1,
			   PL_INT, (int)WTERMSIG(status));
  } else
  { assert(0);
    return FALSE;
  }
}


static int
wait_for_pid(pid_t pid, term_t code, wait_options *opts)
{ pid_t p2;
  int status;

  if ( opts->has_timeout && opts->timeout == 0.0 )
  { if ( (p2=waitpid(pid, &status, WNOHANG)) == pid )
      return unify_exit_status(code, status);
    else if ( p2 == 0 )
      return PL_unify_atom(code, ATOM_timeout);
    else
      return pl_error(NULL, 0, "waitpid", ERR_ERRNO, errno);
  }

  for(;;)
  { if ( (p2=waitpid(pid, &status, 0)) == pid )
      return unify_exit_status(code, status);

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    } else
    { return pl_error(NULL, 0, "waitpid", ERR_ERRNO, errno);
    }
  }
}


static int
wait_for_process(process_context *pc)
{ for(;;)
  { int status;
    pid_t p2;
    
    if ( (p2=waitpid(pc->pid, &status, 0)) == pc->pid )
    { PL_free(pc);
      return TRUE;
    }

    if ( errno == EINTR && PL_handle_signals() >= 0 )
      continue;

    PL_free(pc);
    return FALSE;
  }
}


static int
wait_success(atom_t name, pid_t pid)
{ pid_t p2;

  for(;;)
  { int status;

    if ( (p2=waitpid(pid, &status, 0)) == pid )
    { if ( WIFEXITED(status) && WEXITSTATUS(status) == 0 )
      { return TRUE;
      } else
      { term_t code = PL_new_term_ref();
	term_t ex = PL_new_term_ref();

	unify_exit_status(code, status);
	PL_unify_term(ex,
		      PL_FUNCTOR, FUNCTOR_error2,
		        PL_FUNCTOR, FUNCTOR_process_error2,
		          PL_ATOM, name,
		          PL_TERM, code,
		        PL_VARIABLE);
	return PL_raise_exception(ex);
      }
    } 

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    }
  }
}


static int
do_create_process(p_options *info)
{ int pid;

  if ( !(pid=fork()) )			/* child */
  { int fd;

    PL_cleanup_fork();

    if ( info->cwd )
    { if ( chdir(info->cwd) )
      { perror(info->cwd);
	exit(1);
      }
    }
    
					/* stdin */
    switch( info->streams[0].type )
    { case std_pipe:
	dup2(info->streams[0].fd[0], 0);
	close(info->streams[0].fd[1]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_RDONLY)) >= 0 )
	  dup2(fd, 0);
        break;
      case std_std:
	break;
    }
					/* stdout */
    switch( info->streams[1].type )
    { case std_pipe:
	dup2(info->streams[1].fd[1], 1);
        close(info->streams[1].fd[0]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_WRONLY)) >= 0 )
	  dup2(fd, 1);
        break;
      case std_std:
	break;
    }
					/* stderr */
    switch( info->streams[2].type )
    { case std_pipe:
	dup2(info->streams[2].fd[1], 2);
        close(info->streams[2].fd[0]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_WRONLY)) >= 0 )
	  dup2(fd, 2);
        break;
      case std_std:
	break;
    }

    if ( execv(info->exe, info->argv) )
    { perror(info->exe);
      exit(1);
    }

    return pl_error(NULL, 0, "execv", ERR_ERRNO, errno);
  } else				/* parent */
  { if ( info->pipes > 0 && info->pid == 0 )
    { IOSTREAM *s;
      process_context *pc = PL_malloc(sizeof(*pc));

      DEBUG(Sdprintf("Wait on pipes\n"));

      memset(pc, 0, sizeof(*pc));
      pc->magic = PROCESS_MAGIC;
      pc->pid = pid;

      if ( info->streams[0].type == std_pipe )
      { close(info->streams[0].fd[0]);
	s = open_process_pipe(pc, 0, info->streams[0].fd[1]);
	PL_unify_stream(info->streams[0].term, s);
      }
      if ( info->streams[1].type == std_pipe )
      { close(info->streams[1].fd[1]);
	s = open_process_pipe(pc, 1, info->streams[1].fd[0]);
	PL_unify_stream(info->streams[1].term, s);
      }
      if ( info->streams[2].type == std_pipe )
      { close(info->streams[2].fd[1]);
	s = open_process_pipe(pc, 2, info->streams[2].fd[0]);
	PL_unify_stream(info->streams[2].term, s);
      }

      return TRUE;
    } else if ( info->pipes > 0 )
    { IOSTREAM *s;

      if ( info->streams[0].type == std_pipe )
      { close(info->streams[0].fd[0]);
	s = Sfdopen(info->streams[0].fd[1], "w");
	PL_unify_stream(info->streams[0].term, s);
      }
      if ( info->streams[1].type == std_pipe )
      { close(info->streams[1].fd[1]);
	s = Sfdopen(info->streams[1].fd[0], "r");
	PL_unify_stream(info->streams[1].term, s);
      }
      if ( info->streams[2].type == std_pipe )
      { close(info->streams[2].fd[1]);
	s = Sfdopen(info->streams[2].fd[0], "r");
	PL_unify_stream(info->streams[2].term, s);
      }
    }

    if ( info->pid )
      return PL_unify_integer(info->pid, pid);
    
    return wait_success(info->exe_name, pid);
  }
}

#endif /*__WINDOWS__*/


		 /*******************************
		 *	      BINDING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Basic process creation interface takes

	* Exe file
	* List of arguments
	* standard streams		% std, null, pipe(S)
	* Working directory
	* detached			% Unix
	* window			% Windows
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
process_create(term_t exe, term_t options)
{ p_options info;
  int rc = FALSE;

  memset(&info, 0, sizeof(info));

  if ( !get_exe(exe, &info) )
    goto out;
  if ( !parse_options(options, &info) )
    goto out;
  if ( !create_pipes(&info) )
    goto out;

  rc = do_create_process(&info);

out:
  free_options(&info);

  return rc;
}


static int
get_pid(term_t pid, pid_t *p)
{ int n;

  if ( !PL_get_integer(pid, &n) )
    return type_error(pid, "integer");
  if ( n < 0 )
    return domain_error(pid, "not_less_than_zero");
  
  *p = n;
  return TRUE;
}


static foreign_t
process_wait(term_t pid, term_t code, term_t options)
{ pid_t p;
  wait_options opts;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();

  if ( !get_pid(pid, &p) )
    return FALSE;
  
  memset(&opts, 0, sizeof(opts));
  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return type_error(head, "option");
    PL_get_arg(1, head, arg);
    if ( name == ATOM_timeout )
    { atom_t a;

      if ( !(PL_get_atom(arg, &a) && a == ATOM_infinite) )
      { if ( !PL_get_float(arg, &opts.timeout) )
	  return type_error(arg, "timeout");
	opts.has_timeout = TRUE;
      }
    } else if ( name == ATOM_release )
    { if ( !PL_get_bool(arg, &opts.release) )
	return type_error(arg, "boolean");
      if ( opts.release == FALSE )
	return domain_error(arg, "true");
    } else
      return domain_error(head, "process_wait_option");
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  return wait_for_pid(p, code, &opts);
}


static foreign_t
process_kill(term_t pid, term_t signal)
{ int p;

  if ( !get_pid(pid, &p) )
    return FALSE;

{
#ifdef __WINDOWS__
  HANDLE h;

  if ( !(h=find_process_from_pid(p, "process_kill")) )
    return FALSE;

  if ( TerminateProcess(h, 255) )
    return TRUE;

  return win_error("TerminateProcess");
#else /*__WINDOWS__*/
  int sig;

  if ( !PL_get_signum_ex(signal, &sig) )
    return FALSE;

  if ( kill(p, sig) == 0 )
    return TRUE;
  
  switch(errno)
  { case EPERM:
      return pl_error("process_kill", 2, NULL, ERR_PERMISSION,
		      pid, "kill", "process");
    case ESRCH:
      return pl_error("process_kill", 2, NULL, ERR_EXISTENCE,
		      "process", pid);
    default:
      return pl_error("process_kill", 2, "kill", ERR_ERRNO, errno);
  }
#endif /*__WINDOWS__*/
}
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_process()
{ 
#ifdef __WINDOWS__
  win_init();
#endif

  MKATOM(stdin);
  MKATOM(stdout);
  MKATOM(stderr);
  MKATOM(std);
  MKATOM(null);
  MKATOM(process);
  MKATOM(detached);
  MKATOM(cwd);
  MKATOM(window);
  MKATOM(timeout);
  MKATOM(release);
  MKATOM(infinite);

  MKFUNCTOR(pipe, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(domain_error, 2);
  MKFUNCTOR(process_error, 2);
  MKFUNCTOR(system_error, 2);
  MKFUNCTOR(resource_error, 1);
  MKFUNCTOR(exit, 1);
  MKFUNCTOR(killed, 1);

  PL_register_foreign("process_create", 2, process_create, 0);
  PL_register_foreign("process_wait", 3, process_wait, 0);
  PL_register_foreign("process_kill", 2, process_kill, 0);
}
