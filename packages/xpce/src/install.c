/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SYNOPSIS:
	install [-v] [-c] [-m mode] from to
	install [-v] [-c] [-m mode] file file ... dir

Simple install replacement  to  be  used   for  XPCE  installation.  The
configure install.sh script is very slow,   while  all operating systems
appear to be using different versions of install. This should solve this
problem.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define bool int
#define shift (argc--, argv++)

#define CPBUFSIZE 8192

#ifndef O_BINARY
#define O_BINARY 0
#endif

		 /*******************************
		 *	       SETTINGS		*
		 *******************************/

unsigned short mode;
bool	 set_mode;
bool	 copy;
char *	 program;
int	 verbose=0;

static int
get_mode(const char *s, unsigned short *m)
{ unsigned short mode = 0;

  while(*s >= '0' && *s <= '7')
  { mode = (mode << 3) + *s++ - '0';
  }
  
  if ( *s )
    return FALSE;
  
  *m = mode;
  return TRUE;
}


static void
get_file_mode(const char *name, int fd, mode_t *m)
{ struct stat buf;

#ifdef HAVE_FSTAT
  if ( fstat(fd, &buf) == 0 )
#else
  if ( stat(name, &buf) == 0 )
#endif
    *m = buf.st_mode;
  else
    *m = 0755;				/* default (hack) */
}


static int
install_file(const char *from, const char *to)
{ int fdfrom, fdto;
  char buf[CPBUFSIZE];
  int rval;
  int n;
  mode_t m;

  if ( verbose == 1 )
  { putchar('.');
    fflush(stdout);
  }

  if ( (fdfrom = open(from, O_RDONLY|O_BINARY)) < 0 )
  { perror(from);
    return FALSE;
  }

  if ( set_mode )
    m = mode;
  else
    get_file_mode(from, fdfrom, &m);

  unlink(to);
  if ( (fdto = open(to, O_WRONLY|O_BINARY|O_CREAT|O_TRUNC, m)) < 0 )
  { perror(to);
    close(fdfrom);
    return FALSE;
  }

  while( (n = read(fdfrom, buf, CPBUFSIZE)) > 0 )
  { char *b = buf;

    while(n > 0)
    { int n2;

      if ( (n2=write(fdto, b, n)) < 0 )
      { perror(to);
	rval = FALSE;
	goto out;
      }
      b += n2;
      n -= n2;
    }
  }
  if ( n < 0 )
  { perror(from);
    rval = FALSE;
  } else
    rval = TRUE;

out:
  close(fdfrom);
  close(fdto);

  return rval;
}


const char *
basename(const char *path)
{ const char *base;

  for(base=path; *path; path++)
  { if ( *path == '/' )
      base = path+1;
  }

  return base;
}


static int
install_file_in_dir(const char *file, const char *dir)
{ char path[MAXPATHLEN];

  sprintf(path, "%s/%s", dir, basename(file));
  return install_file(file, path);
}


static int
isdir(const char *path)
{ struct stat buf;

  if ( stat(path, &buf) < 0 ||
       (buf.st_mode & S_IFMT) != S_IFDIR )
    return FALSE;

  return TRUE;
}


void
usage()
{ fprintf(stderr, "  Usage: %s options file ... directory\n", program);
  fprintf(stderr, "     or: %s options from to\n", program);
  fprintf(stderr, "options: [-v] [-c] [-m mode]\n");
  exit(1);
}


int
main(int argc, char **argv)
{ char *out;
  int errors = 0;

  program = argv[0];
  shift;
  while(argc > 0 && argv[0][0] == '-')
  { char *opts = &argv[0][1];

    shift;
    for( ; *opts; opts++ )
    { switch( *opts )
      { case 'c':
	  copy = TRUE;
	  break;
	case 'v':
	  verbose++;
	  break;
	case 'm':
	  if ( argc > 0 && get_mode(argv[0], &mode) )
	  { shift;
	    set_mode = TRUE;
	  }
	  break;
	default:
	  usage();
      }
    }
  }
  
  if ( argc == 0 )
    usage();
  out = argv[argc-1];

  if ( isdir(out) )
  { int i;

    for(i=0; i<argc-1; i++)
    { if ( !install_file_in_dir(argv[i], out) )
	errors++;
    }
  } else
  { if ( !install_file(argv[0], out) )
      errors++;
  }

  return errors ? 1 : 0;
}
