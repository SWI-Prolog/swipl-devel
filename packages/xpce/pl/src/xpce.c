/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
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

static char *program;

int PASCAL
WinMain(HANDLE hInstance, HANDLE hPrevInstance,
	LPSTR lpszCmdLine, int nCmdShow)
{ char *	argv[100];
  int		argc;

  program = program_name(hInstance);
  argc = breakargs(program, lpszCmdLine, argv);

  bind_terminal();
  if ( !PL_initialise(argc, argv) )
    PL_halt(1);
  
  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}

		 /*******************************
		 *	       I/O		*
		 *******************************/

static int
do_read(void *handle, char *buffer, int size)
{ MessageBox(NULL,
	     "Application tries to read",
	     program,
	     MB_OK|MB_TASKMODAL);

  return 0;				/* return END-OF-FILE */
}

#define MAXMSG 1024

static int
count_lines(const char *s, int len)
{ int lines = 0;

  while(len-- > 0)
  { if ( *s++ == '\n' )
      lines++;
  }

  return lines;
}


static int
do_write(void *handle, char *buffer, int size)
{ char msg[MAXMSG];
  static int action = IDYES;
  char logfilename[MAXPATHLEN];
  static HANDLE logfile = 0;
  int l;
  int tlen = size+count_lines(buffer, size);
  char *tbuffer = alloca(tlen);

  if ( tbuffer )
  { char *f = buffer;
    char *t = tbuffer;
    int done = 0;

    while( done++ < size )
    { if ( *f == '\n' )
	*t++ = '\r';
      *t++ = *f++;
    }
  } else				/* no room, poor mens solution */
  { tlen = size;
    tbuffer = buffer;
  }

  if ( (l=GetModuleFileName(NULL, logfilename, MAXPATHLEN)) > 0 )
  { char *t = logfilename+l;
    
    while(t>logfilename && t[-1] != '\\' && t[-1] != '.')
      t--;
    if ( t[-1] == '.' )
      strcpy(t, "log");
    else
      strcpy(logfilename+l, ".log");
  } else
    strcpy(logfilename, "xpce.log");

  switch(action)
  { case IDYES:
    { char *t, *s = buffer;
      int space, ml;

      strcpy(msg, 
	     "The application produced the output below\n"
	     "Press [yes] to continue and show this box on new output\n"
	     "Press [no] to continue and write further output to ");
      strcat(msg,
	     logfilename);
      strcat(msg,
	     "\n"
	     "Press [cancel] to terminate the application\n\n");
      ml = strlen(msg);
      t = msg+ml;
      space = MAXMSG - ml - 1;
      if ( tlen > space )
      { strncpy(t, tbuffer, space-4);
	strcpy(t+space-4, " ...");
      } else
      { strncpy(t, tbuffer, tlen);
	t[tlen] = '\0';
      }
	
      switch((action=MessageBox(NULL,
				msg,
				program,
				MB_ICONEXCLAMATION|
				MB_YESNOCANCEL|MB_TASKMODAL)))
      { case IDNO:
	{ logfile = CreateFile(logfilename,
			       GENERIC_WRITE,
			       FILE_SHARE_READ,
			       NULL,
			       CREATE_ALWAYS,
			       FILE_ATTRIBUTE_NORMAL,
			       NULL);
	  goto writelog;
	}
	case IDCANCEL:
	  PL_halt(1);
      }
      break;
    }
    case IDNO:
    { DWORD len;

    writelog:
      WriteFile(logfile, tbuffer, tlen, &len, NULL);
      break;
    }
  }

  return size;
}


static void
bind_terminal(void)
{ static IOFUNCTIONS funcs;

  funcs = *Sinput->functions;
  funcs.read     = do_read;
  funcs.write    = do_write;

  Sinput->functions  = &funcs;
  Soutput->functions = &funcs;
  Serror->functions  = &funcs;
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
