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

#include "include.h"
#include <h/unix.h>
#include <h/interface.h>

#ifndef MAXCMDLINE
#define MAXCMDLINE 10240
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

static int
getEnvironmentSizeProcess(Process p)
{ int size = 0;
  Cell cell;

  if ( isNil(p->environment) )
    return 0;

  for_cell(cell, p->environment->attributes)
  { Attribute a = cell->value;
    
    size += valInt(getSizeCharArray(a->name)) +
	    valInt(getSizeCharArray(a->value)) + 2;
  }
  
  return size+1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a Win32 environment block as requested  by the Win32 API. This is
rather different from the Unix version,   which  expects a char** ending
with a NULL pointer. Win32  is  a   char  *  holding  name=value fields,
separated by '\0'. The last is terminated by a secondary '\0'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
createWin32EnvriomentBlock(Process p)
{ if ( notNil(p->environment) )
  { char *buf = pceMalloc(getEnvironmentSizeProcess(p));
    char *s = buf;
    Cell cell;

    if ( !buf )
    { errorPce(p, NAME_notEnoughMemory);
      return NULL;
    }

    for_cell(cell, p->environment->attributes)
    { Attribute a = cell->value;
      
      s = strcpyskip(s, toCharp(a->name));
      *s++ = '=';
      s = strcpyskip(s, toCharp(a->value));
      *s++ = '\0';
    }

    *s++ = '\0';
    return buf;
  }

  return NULL;
}


status
openProcess(Process p, CharArray cmd, int argc, CharArray *argv)
{ status rval = SUCCEED;

  if ( notDefault(cmd) )
  { if ( notNil(p->pid) )
      return errorPce(p, NAME_noChangeAfterOpen);

    assign(p, name, cmd);
    assign(p, arguments, newObjectv(ClassVector, argc, (Any *)argv));
  }

  if ( isNil(p->pid) )
  { HANDLE wrfd[2];			/* output to process */
    HANDLE rdfd[2];			/* input from process */
    STARTUPINFO startinfo;
    PROCESS_INFORMATION *processinfo;
    SECURITY_ATTRIBUTES sa;
    char cmdline[MAXCMDLINE];
    char dirbuf[MAXPATHLEN];
    char *cwd = NULL;
    Any a;
    char *env = createWin32EnvriomentBlock(p);

    if ( p->use_tty == ON )
    { Cprintf("%s: Warning: no TTY control in Win32\n", pp(p));
    }

    sa.nLength              = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle       = TRUE;

    if ( !CreatePipe(&wrfd[0], &wrfd[1], &sa, 0) ) /* read, write */
      return errorPce(p, NAME_noPipe, APIError());
    if ( !CreatePipe(&rdfd[0], &rdfd[1], &sa, 0) )
    { CloseHandle(wrfd[0]);
      CloseHandle(wrfd[1]);
      return errorPce(p, NAME_noPipe, APIError());
    }

    if ( notDefault(p->directory) )
    { _xos_limited_os_filename(strName(p->directory->path), dirbuf);
      cwd = dirbuf;
    }

    _xos_limited_os_filename(strName(p->name), cmdline);
    for_vector(p->arguments, a,
	       { strcat(cmdline, " ");
		 strcat(cmdline, toCharp(a));
	       });
    processinfo = alloc(sizeof(*processinfo));
    memset(&startinfo, 0, sizeof(startinfo));
    memset(processinfo, 0, sizeof(*processinfo));
    
    startinfo.cb	  = sizeof(startinfo);
    startinfo.dwFlags     = STARTF_USESTDHANDLES;
    startinfo.hStdInput   = wrfd[0];
    startinfo.hStdOutput  = rdfd[1];
    startinfo.hStdError   = rdfd[1];

    if ( CreateProcess(NULL,			/* executable */
		       cmdline,			/* cmdline */
		       NULL,			/* Process security */
		       NULL,			/* Thread security */
		       TRUE,			/* Inherited handles */
		       DETACHED_PROCESS|CREATE_NEW_PROCESS_GROUP,
		       env,			/* environment char ** */
		       cwd,			/* directory */
		       &startinfo,
		       processinfo) )
    { CloseHandle(processinfo->hThread); 	/* don't need this */
      pidProcess(p, toInt(processinfo->dwProcessId));
      p->rdfd   = (int) rdfd[0];
      p->wrfd   = (int) wrfd[1];
      p->ws_ref = processinfo;
      assign(p, status, NAME_running);

      DEBUG(NAME_process,
	    Cprintf("%s: Created Process %d --> %s --> %d, id = %d\n",
		    pp(p), p->wrfd, cmdline, p->rdfd,
		    processinfo->dwProcessId));

      inputStream((Stream)p, DEFAULT);
    } else
    { errorPce(p, NAME_execError, APIError());
      unalloc(sizeof(*processinfo), processinfo);
      rval = FAIL;
    }

    if ( env )
      pceFree(env);

    CloseHandle(wrfd[0]);
    CloseHandle(rdfd[1]);
  }

  return rval;
}


void
ws_kill_process(Process p, int sig)
{ PROCESS_INFORMATION *pi = p->ws_ref;

  if ( sig == 1 ||
       sig == 9 ||
       sig == 15 )
  { TerminateProcess(pi->hProcess, sig);
  } else if ( sig == 3 )
  { GenerateConsoleCtrlEvent(CTRL_C_EVENT, (unsigned long)pi->hProcess);
  } else
    Cprintf("%s: process->kill only supports INT, KILL\n", pp(p));
}


void
ws_done_process(Process p)
{ PROCESS_INFORMATION *pi = p->ws_ref;

  CloseHandle(pi->hProcess);

  unalloc(sizeof(*pi), pi);
  p->ws_ref = NULL;
}
