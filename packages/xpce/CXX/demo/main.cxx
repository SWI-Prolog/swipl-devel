/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <stdlib.h>
#include <pce/Pce.h>
#include <pce/Chain.h>

static PceArg PcNframes("frames");
static PceArg PcNkind("kind");
static PceArg PcNtoplevel("toplevel");

extern "C" {
int	pceInitialise(int handles, char *home, int argc, char **argv);
int	pceDispatch(int fd, int timeout);
void	Cprintf(const char *fmt, ...);
}


int
main(int argc, char* argv[])
{ int frames = TRUE;

  if ( !pceInitialise(0, (char *)0, argc, argv) )
  { Cprintf("Sorry, failed to initialise XPCE\n");
    exit(1);
  }
  
  if ( !pceInitApplication(argc, argv) )
  { Cprintf("Failed to run pceInitApplication()\n");
    exit(1);
  }

  while(frames)
  { PceCell cell;

    pceDispatch(0, 1000);

    for(frames = FALSE, cell = AsChain(TheDisplay.get(PcNframes)).head();
	cell;
	++cell)
    { if ( cell.value().get(PcNkind) == PcNtoplevel )
      { frames = TRUE;
	break;
      }
    }
  }

  exit(0);
  return 0;
}


#ifdef WIN32
#include <windows.h>
#include <string.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 512
#endif
#ifndef MAXARGV
#define MAXARGV 100
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
long_file_name(char *buffer)
	Translate a filename, possibly holding 8+3 abbreviated parts into
	the `real' filename.  I couldn't find a direct call for this.  If
	you have it, I'd be glad to receive a better implementation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
long_file_name(char *file)
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


static int
parse_command_line(char *program, char *line, char **argv)
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


int PASCAL
WinMain(HANDLE hInstance, HANDLE hPrevInstance,
	LPSTR lpszCmdLine, int nCmdShow)
{ char program[MAXPATHLEN];
  char *argv[MAXARGV];
  int argc;

  GetModuleFileName(hInstance, program, sizeof(program));
  long_file_name(program);
  argc = parse_command_line(program, lpszCmdLine, argv);

  return main(argc, argv);
}
#endif
