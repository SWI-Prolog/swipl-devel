/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <windows.h>
#include <malloc.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

static int   breakargs(char *program, char *line, char **argv);
static char *program_name(HANDLE hInstance);
static void  bind_terminal(void);
static int   attach_console(void);

static char *program;

int PASCAL
WinMain(HANDLE hInstance, HANDLE hPrevInstance,
	LPSTR lpszCmdLine, int nCmdShow)
{ char *	argv[100];
  int		argc;

  program = program_name(hInstance);
  argc = breakargs(program, lpszCmdLine, argv);

  bind_terminal();
/*attach_console();*/
//PL_set_feature("verbose", PL_ATOM, "silent");
  if ( !PL_initialise(argc, argv) )
    PL_halt(1);
  
  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


static void
ok(const char *msg)
{ MessageBox(NULL, msg, program, MB_OK|MB_TASKMODAL);
}



		 /*******************************
		 *	  USE MS-Console	*
		 *******************************/

static IOFUNCTIONS console_functions;

static int    use_console;		/* use the console */
static HANDLE cin;			/* console input handle */
static HANDLE cout;			/* console output handle */
static HANDLE cerr;

typedef struct
{ HANDLE input;
  LPVOID buf;
  DWORD  len;
  DWORD  done;
  DWORD  rc;
} input_context;

static DWORD WINAPI
getInput(LPVOID h)
{ input_context *ctx = h;

  ctx->rc = ReadConsole(ctx->input, ctx->buf, ctx->len, &ctx->done, NULL);

  return ctx->rc;
}
  
static int
read_console(void *h, char *buf, unsigned len)
{ HANDLE th;
  input_context ctx;
  DWORD tid;

  ctx.input = cin;
  ctx.buf = buf;
  ctx.len = len;

  th = CreateThread(NULL, 10240, getInput, &ctx, 0, &tid);

  for(;;)
  { DWORD rc = MsgWaitForMultipleObjects(1, &th, FALSE,
					 INFINITE, QS_ALLINPUT);
    if ( rc == WAIT_OBJECT_0 )
    { CloseHandle(th);
      if ( ctx.rc )
      { return ctx.done;
      }
      return -1;			/* error */
    } else /*if ( rc == WAIT_OBJECT_0+1 )*/
    { MSG msg;

      if ( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
      }
    }
  }
}


static int
write_console(void *h, char *buf, unsigned len)
{ HANDLE co = (h == Soutput->handle ? cout : cerr);
  DWORD done;

  if ( WriteConsole(co, buf, len, &done, NULL) )
    return done;

  return -1;
}


static int
attach_console(void)
{ static int done = 0;

  switch(done)
  { case 0:
      if ( AllocConsole() )
      { cin  = GetStdHandle(STD_INPUT_HANDLE);
	cout = GetStdHandle(STD_OUTPUT_HANDLE);
	cerr = GetStdHandle(STD_ERROR_HANDLE);
	
	if ( cin  != INVALID_HANDLE_VALUE &&
	     cerr != INVALID_HANDLE_VALUE &&
	     cout != INVALID_HANDLE_VALUE )
	{ use_console = TRUE;
	  done = 1;

	  return TRUE;
	}
      }
      done = -1;
      return FALSE;
    case 1:
      return TRUE;			/* already done so */
    default:
      return FALSE;			/* tried but failed */
  }
}



		 /*******************************
		 *	       I/O		*
		 *******************************/

static int
do_read(void *handle, char *buffer, int size)
{ if ( use_console )
    return read_console(handle, buffer, size);

  MessageBox(NULL,
	     "The application tries to read.\n"
	     "A console will be attached to help diagnose the problem.",
	     program, MB_OK|MB_TASKMODAL);

  if ( attach_console() )
    return read_console(handle, buffer, size);

  return -1;
}


static int
do_write(void *handle, char *buffer, int size)
{ if ( use_console )
    return write_console(handle, buffer, size);

  if ( handle == Serror->handle )
  { MessageBox(NULL,
	       "The application produced an error message.\n"
	       "A console will be attached to help diagnose the problem.",
	       program, MB_OK|MB_TASKMODAL);

    if ( attach_console() )
      return write_console(handle, buffer, size);
  }

  return size;
}


static void
bind_terminal(void)
{ console_functions       = *Sinput->functions;
  console_functions.read  = do_read;
  console_functions.write = do_write;

  Sinput->functions  = &console_functions;
  Soutput->functions = &console_functions;
  Serror->functions  = &console_functions;
}



		 /*******************************
		 *		UTIL		*
		 *******************************/

static void
long_name(char *file)
{ char buf[MAXPATHLEN];
  char *i = file;
  char *o = buf;
  char *ok = buf;
  int changed = 0;

  while(*i)
  { int dirty = FALSE;

    while(*i && *i != '\\')
    { if ( *i == '~' )
	dirty++;
      *o++ = *i++;
    }
    if ( dirty )
    { WIN32_FIND_DATA data;
      HANDLE h;

      *o = '\0';
      if ( (h=FindFirstFile(buf, &data)) != INVALID_HANDLE_VALUE )
      { strcpy(ok, data.cFileName);
	FindClose(h);
	o = ok + strlen(ok);
	changed++;
      }
    }
    if ( *i )
      *o++ = *i++;
    ok = o;
  }

  if ( changed )
  { *o = '\0';
    strcpy(file, buf);
  }
}


static char *
program_name(HANDLE hInstance)
{ char program[MAXPATHLEN];

  GetModuleFileName(hInstance, program, sizeof(program));
  long_name(program);
  
  return strcpy(malloc(strlen(program)+1), program);
}


static int
breakargs(char *program, char *line, char **argv)
{ int argc = 1;

  argv[0] = program;

  while(*line)
  { while(*line && isspace(*line))
      line++;

    if ( *line == '"' )			/* Windows-95 quoted arguments */
    { char *start = line+1;
      char *end = start;

      while( *end && *end != '"' )
	end++;
      if ( *end == '"' )
      { *end = '\0';
        argv[argc++] = start;
	line = end+1;
	continue;
      }
    }

    if ( *line )
    { argv[argc++] = line;
      while(*line && !isspace(*line))
	line++;
      if ( *line )
	*line++ = '\0';
    }
  }
  argv[argc] = NULL;			/* add trailing NULL pointer to argv */

  return argc;
}      
