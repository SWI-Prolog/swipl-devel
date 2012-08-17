/* popen.c
  RunSilent() is by Steven Szelei,
  and pt_popen()/pt_pclose() is by Kurt Keller
  Modified and comments translated by Steve Donovan

  Please note an extension; if your commmand contains '2>&1'
  then any error output will be redirected as well to the pipe.

  Put this file in scite\lua\src\lib and add to your project

  to modify liolib.c in the same dir,
  replace conditional at line 47 with:

  #ifndef USE_POPEN
  #ifdef __WINDOWS__
  #define USE_POPEN 1
  FILE* pt_popen(const char *cmd, const char*mode);
  int pt_pclose(FILE *file);
  uintptr_t RunSilent(const char* strCommand);
  #define popen pt_popen
  #define pclose pt_pclose
  #define system RunSilent
  #endif
  #else
  #define USE_POPEN	0
  #endif

*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog note:

This        file        is         copied          verbatim         from
http://lua-users.org/wiki/PipesOnWindows, where it is   contributed  for
using pipes with the LUA programming  language. LUA is distributed under
the MIT licence which is claimed to be compatible (but less restrictive)
with the LGPL license. We  therefore  assume   we  can  use this file in
SWI-Prolog without introducing new license problems.

This version is heavily modified:

	* Support Unicode commands (commands are specified in UTF-8)
	* make popen()/pclose() thread-safe.
	* Fix leak process-handles

If you find this file and know better, please contact info@swi-prolog.org.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <windows.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <io.h>
#include "../pl-utf8.h"

DWORD RunSilent(const char* strCommand)
{
	STARTUPINFO StartupInfo;
	PROCESS_INFORMATION ProcessInfo;
	char Args[4096];
	char *pEnvCMD = NULL;
	char *pDefaultCMD = "CMD.EXE";
	ULONG rc;

	memset(&StartupInfo, 0, sizeof(StartupInfo));
	StartupInfo.cb = sizeof(STARTUPINFO);
	StartupInfo.dwFlags = STARTF_USESHOWWINDOW;
	StartupInfo.wShowWindow = SW_HIDE;

	Args[0] = 0;

	pEnvCMD = getenv("COMSPEC");

	if(pEnvCMD){
		strcpy(Args, pEnvCMD);
	} else{
		strcpy(Args, pDefaultCMD);
	}

	/* "/c" option - Do the command then terminate the command window */
	strcat(Args, " /c ");
	/*the application you would like to run from the command window */
	strcat(Args, strCommand);

	if (!CreateProcess( NULL, Args, NULL, NULL, FALSE,
		CREATE_NEW_CONSOLE,
		NULL,
		NULL,
		&StartupInfo,
		&ProcessInfo))
	{
		return GetLastError();
	}

	WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
	if(!GetExitCodeProcess(ProcessInfo.hProcess, &rc))
		rc = 0;

	CloseHandle(ProcessInfo.hThread);
	CloseHandle(ProcessInfo.hProcess);

	return rc;

}

/*------------------------------------------------------------------------------
  Globals for the Routines pt_popen() / pt_pclose()
------------------------------------------------------------------------------*/

CRITICAL_SECTION lock;
#define LOCK()   EnterCriticalSection(&lock);
#define UNLOCK() LeaveCriticalSection(&lock);

static void
pt_init()
{ InitializeCriticalSection(&lock);
}


typedef struct pipe_context
{ struct pipe_context *next;
  FILE   *fd;
  HANDLE in[2];
  HANDLE out[2];
  HANDLE err[2];
  char   mode;				/* 'r' or 'w' */
} pipe_context;


static pipe_context *pipes = NULL;

static pipe_context *
allocPipeContext()
{ pipe_context *pc = malloc(sizeof(*pc));

  if ( !pc )
    return NULL;

  pc->in[0]   = INVALID_HANDLE_VALUE;
  pc->in[1]   = INVALID_HANDLE_VALUE;
  pc->out[0]  = INVALID_HANDLE_VALUE;
  pc->out[1]  = INVALID_HANDLE_VALUE;
  pc->err[0]  = INVALID_HANDLE_VALUE;
  pc->err[1]  = INVALID_HANDLE_VALUE;

  return pc;
}


static void
discardPipeContext(pipe_context *pc)
{ if (pc->in[0]  != INVALID_HANDLE_VALUE)
    CloseHandle(pc->in[0]);
  if (pc->in[1]  != INVALID_HANDLE_VALUE)
    CloseHandle(pc->in[1]);
  if (pc->out[0] != INVALID_HANDLE_VALUE)
    CloseHandle(pc->out[0]);
  if (pc->out[1] != INVALID_HANDLE_VALUE)
    CloseHandle(pc->out[1]);
  if (pc->err[0] != INVALID_HANDLE_VALUE)
    CloseHandle(pc->err[0]);
  if (pc->err[1] != INVALID_HANDLE_VALUE)
    CloseHandle(pc->err[1]);

  free(pc);
}



static void
linkPipeContext(pipe_context *pc)
{ LOCK();
  pc->next = pipes;
  pipes = pc;
  UNLOCK();
}


static int
my_pipe(HANDLE *readwrite)
{
  SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);          /* Length in bytes */
  sa.bInheritHandle = 1;            /* the child must inherit these handles */
  sa.lpSecurityDescriptor = NULL;

  if (! CreatePipe (&readwrite[0],&readwrite[1],&sa,1 << 13))
  {
    errno = -1; /* EMFILE; que? */
    return -1;
  }

  return 0;
}

/*------------------------------------------------------------------------------
  Replacement for 'popen()' under Windows.

  cmd is taken to be encoded in UTF-8 for compatibility with the Unix
  version.

  NOTE: if cmd contains '2>&1', we connect the standard error file handle
    to the standard output file handle.
------------------------------------------------------------------------------*/

static void
utf8towcs(wchar_t *o, const char *src)
{ for( ; *src; )
  { int wc;

    src = utf8_get_char(src, &wc);
    *o++ = wc;
  }
  *o = 0;
}


FILE *
pt_popen(const char *cmd, const char *mode)
{ FILE *fptr = NULL;
  PROCESS_INFORMATION piProcInfo;
  STARTUPINFOW siStartInfo;
  int success, redirect_error = 0;
  wchar_t *wcmd = NULL;
  wchar_t *err2out;
  pipe_context *pc;

  size_t utf8len = utf8_strlen(cmd, strlen(cmd));
  if ( !(wcmd = malloc((utf8len+1)*sizeof(wchar_t))) )
  { return NULL;
  }
  utf8towcs(wcmd, cmd);

  if ( !(pc=allocPipeContext()) )
    goto finito;
  if ( !mode || !*mode )
    goto finito;
  pc->mode = *mode;
  if ( pc->mode != 'r' && pc->mode != 'w' )
    goto finito;

  /*
   * Shall we redirect stderr to stdout ? */
  if ( (err2out=wcsstr(wcmd, L"2>&1")) != NULL)
  { /* this option doesn't apply to win32 shells, so we clear it out! */
     wcsncpy(err2out, L"    ", 4);
     redirect_error = 1;
  }

  /*
   * Create the Pipes... */
  if (my_pipe(pc->in)  == -1 ||
      my_pipe(pc->out) == -1)
    goto finito;
  if ( !redirect_error )
  { if ( my_pipe(pc->err) == -1)
      goto finito;
  }

  /*
   * Now create the child process */
  ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
  siStartInfo.cb           = sizeof(STARTUPINFO);
  siStartInfo.hStdInput    = pc->in[0];
  siStartInfo.hStdOutput   = pc->out[1];
  if ( redirect_error )
    siStartInfo.hStdError  = pc->out[1];
  else
    siStartInfo.hStdError  = pc->err[1];
  siStartInfo.dwFlags      = STARTF_USESTDHANDLES;

  success = CreateProcessW(NULL,
			   wcmd,	// command line
			   NULL,	// process security attributes
			   NULL,	// primary thread security attributes
			   TRUE,	// handles are inherited
			   CREATE_NO_WINDOW,  // creation flags: without window (?)
			   NULL,	// use parent's environment
			   NULL,	// use parent's current directory
			   &siStartInfo, // STARTUPINFO pointer
			   &piProcInfo); // receives PROCESS_INFORMATION

  if ( !success )
    goto finito;

  CloseHandle(piProcInfo.hThread);
  CloseHandle(piProcInfo.hProcess);

  /*
   * These handles listen to the Child process */
  CloseHandle(pc->in[0]);  pc->in[0]  = INVALID_HANDLE_VALUE;
  CloseHandle(pc->out[1]); pc->out[1] = INVALID_HANDLE_VALUE;
  if ( pc->err[1] != INVALID_HANDLE_VALUE )
  { CloseHandle(pc->err[1]);
    pc->err[1] = INVALID_HANDLE_VALUE;
  }

  if ( pc->mode == 'r' )
    fptr = _fdopen(_open_osfhandle((intptr_t)pc->out[0],_O_BINARY),"r");
  else
    fptr = _fdopen(_open_osfhandle((intptr_t)pc->in[1],_O_BINARY),"w");

finito:
  if ( fptr )
  { pc->fd = fptr;
    linkPipeContext(pc);
  } else
  { if ( pc )
      discardPipeContext(pc);
  }
  if ( wcmd )
    free(wcmd);

  return fptr;
}

/*------------------------------------------------------------------------------
  Replacement for 'pclose()' under Win32
------------------------------------------------------------------------------*/
int
pt_pclose(FILE *fd)
{ pipe_context **ppc;
  int rc;

  if ( !fd )
  { errno = EINVAL;
    return -1;
  }

  rc = fclose(fd);
  LOCK();
  for(ppc = &pipes; *ppc; ppc=&(*ppc)->next)
  { pipe_context *pc = *ppc;

    if ( pc->fd == fd )
    { *ppc = pc->next;

      UNLOCK();
      if ( pc->err[0] != INVALID_HANDLE_VALUE )
	CloseHandle(pc->err[0]);
      if ( pc->mode == 'r' )
      { CloseHandle(pc->in[1]);
      } else
      { CloseHandle(pc->out[0]);
      }

      free(pc);

      return rc;
    }
  }

  UNLOCK();
  errno = EINVAL;
  return -1;
}


