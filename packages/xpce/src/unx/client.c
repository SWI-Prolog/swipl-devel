/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <string.h>

#define MAX_UN_ADDRESS_LEN 108

#define CPSIZE 4096			/* IO copy buffer  */

extern int errno;			/* pass errors  */

char *program;				/* my name */

#define EOS		'\0'
#define streq(s, q)	(strcmp(s, q) == 0)
#define max(a, b)	((a) > (b) ? (a) : (b))

#ifndef FALSE
#define FALSE 0
#define TRUE  1
#endif

static void usage(void);		/* print usage information */

		 /*******************************
		 *	      CONNECT		*
		 *******************************/

int
connectPce(address)
char *address;
{ struct sockaddr *addr;
  int len;
  int id;
  char *s;

  if ( (s = strchr(address, ':')) )
  { struct sockaddr_in a;
    struct hostent *hp;
    int port = atoi(&s[1]);
    char host[100];

    strncpy(host, address, s-address);
    host[s-address] = EOS;
    addr = (struct sockaddr *) &a;
    len = sizeof(a);
    memset(&a, 0, sizeof(a));

    a.sin_family = AF_INET;
    a.sin_port   = port;
 
    if ( !(hp = gethostbyname(host)) )
      return -1;
    memcpy(&a.sin_addr, hp->h_addr, hp->h_length);

    if ( (id = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
      return id;
  
    return connect(id, addr, len) ? -1 : id;
  } else
  { struct sockaddr_un a;

    addr = (struct sockaddr *) &a;
    a.sun_family = AF_UNIX;
    if ( (len = strlen(address)) > MAX_UN_ADDRESS_LEN )
    { errno = ENAMETOOLONG;
      return -1;
    }
    strcpy(a.sun_path, address);
    if ( (id = socket(PF_UNIX, SOCK_STREAM, 0)) < 0 )
      return id;
    len += sizeof(a.sun_family);
  
    return connect(id, addr, len) ? -1 : id;
  }
}

		 /*******************************
		 *	      COPY I/O		*
		 *******************************/

int
copy_block(f, t)
int f, t;
{ int bytes;
  char buf[CPSIZE];

  if ( (bytes = read(f, buf, CPSIZE)) > 0 )
  { if ( write(t, buf, bytes) != bytes )
    { perror("write");
      exit(1);
    }
  } else if ( bytes < 0 )
  { perror("read");
  }

  return bytes;
}



void
copy2(f1, t1, f2, t2)
int f1, t1, f2, t2;
{ int eof = FALSE;

  for( ; !eof; )
  { fd_set fds;
    int ready;

    FD_ZERO(&fds);
    FD_SET(f1, &fds);
    FD_SET(f2, &fds);

    ready = select(max(f1, f2)+1, &fds, NULL, NULL, NULL);

    while( ready-- )
    { if ( FD_ISSET(f1, &fds) )
      { eof = (copy_block(f1, t1) == 0);
	FD_CLR(f1, &fds);
      } else if ( FD_ISSET(f2, &fds) )
      { eof = (copy_block(f2, t2) == 0);
	FD_CLR(f2, &fds);
      } else
      { fprintf(stderr, "Input from unselected fd???\n");
      }
    }
  }
}




		 /*******************************
		 *		MAIN		*
		 *******************************/

#define SHIFT argc--, argv++

#define NEXT_ARG(var) \
	{ if ( argc <= 0 ) usage(); \
	  var = argv[0]; \
	  SHIFT; \
	}
	  

int
main(argc, argv)
int argc;
char **argv;
{ char *address;
  char *command = NULL;
  int block = FALSE;
  int quiet = FALSE;
  int id;

  program = argv[0];
  SHIFT;
  
  if ( argc == 0 )
    usage();
  address = argv[0];
  SHIFT;

  while(argc > 0 && argv[0][0] == '-')
  { char *s = &argv[0][1];
    SHIFT;

    for(; *s; s++)
    { switch(*s)
      { case 'c':
          NEXT_ARG(command);
          break;
	case 'b':
	  block++;
	  break;
	case 'q':
	  quiet++;
	  break;
	default:
	  usage();
      }
    }
  }
  
  if ( (id = connectPce(address)) < 0 )
  { if ( !quiet )
      perror("connect");
    exit(2);
  }

  if ( command )
  { FILE *fd = fdopen(id, "w");

    fprintf(fd, "%s\n", command);
    fflush(fd);

    if ( block )
    { while(copy_block(id, 1) > 0)
	;
    }
    fclose(fd);
  } else
  { copy2(id, 1, 0, id);
  }

  exit(0);				/* TBD  */
}

static void
usage()
{ fprintf(stderr, "Usage: %s file|host:port [-q] [-[b]c message]\n", program);
  exit(1);
}
