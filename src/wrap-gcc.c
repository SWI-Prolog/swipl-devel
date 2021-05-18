/*  Part of SWI-Prolog

    Author:        Danielle Church
    E-mail:        dani.church@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>

#define FALSE 0
#define TRUE 1

int
main(int argc, char *argv[])
{ const char *pgo_suffix = argv[1];
  char *outfile = NULL, *tempfile = NULL;
  int dash_o = FALSE;
  int status;

  if (argc < 3)
  { fprintf(stderr, "Usage: %s pgo_tag /path/to/gcc args...\n", argv[0]);
    return 1;
  }

  argv += 2;
  argc -= 2;

  for (int i = 0; i < argc; i++)
  { if ( dash_o )
    { dash_o = FALSE;
      outfile = argv[i];
      tempfile = strdup(outfile);
      for(char *tagloc = strstr(tempfile, pgo_suffix);
	  tagloc;
	  tagloc = strstr(tempfile, pgo_suffix))
      { memmove(tagloc, tagloc + strlen(pgo_suffix),
		strlen(tagloc) - strlen(pgo_suffix) + 1 /* null byte */);
      }
      argv[i] = tempfile;
    } else if (strcmp(argv[i], "-o") == 0)
      dash_o = TRUE;
  }

  if ( !fork() )
    execvp(argv[0], argv);
  wait(&status);

  if (status == 0 && tempfile)
  { if (rename(tempfile, outfile) < 0)
    { fprintf(stderr, "ERROR: moving %s to %s failed: %s\n",
	      tempfile, outfile, strerror(errno));
      return 1;
    }
    return 0;
  }
  if (WIFEXITED(status)) exit(WEXITSTATUS(status));
  if (WIFSIGNALED(status)) kill(getpid(), WTERMSIG(status));
  return -1;
}
