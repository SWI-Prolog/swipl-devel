/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <memory.h>

#ifndef TRUE
#define FALSE (0)
#define TRUE (1)
#endif
#define EOS '\0'

char *program;				/* name of the program */
char *manglefile;			/* file with to-mangle symbols */
char *aoutfile;				/* file to mangle (a.out) */

typedef struct symbol *Symbol;

struct symbol
{ const char	*value;
  Symbol	next;
};

#define MANGLEHASHSIZE 1024

static Symbol mangletable[MANGLEHASHSIZE];

#if sun
#define memmove strncpy
#endif

const char *
strsave(const char *s)
{ int l = strlen(s)+1;
  char *q = malloc(l);

  memmove(q, s, l);

  return (const char *) q;
}


int
hashkey(const char *s)
{ unsigned int key = 0;

  while(*s)
    key += *s++;

  return key % MANGLEHASHSIZE;
}


int
memberSymbol(const char *s)
{ int key = hashkey(s);
  Symbol symb = mangletable[key];
  
  for(; symb; symb = symb->next)
    if ( strcmp(symb->value, s) == 0 )
      return TRUE;
  
  return FALSE;
}


void
addSymbol(const char *s)
{ if ( !memberSymbol(s) )
  { int key = hashkey(s);
    Symbol symb = malloc(sizeof(struct symbol));

    symb->value = strsave(s);
    symb->next = mangletable[key];
    mangletable[key] = symb;
  }
}


int
read_manglefile(char *filename)
{ FILE *fd = fopen(filename, "r");
  char buf[1024];
  int done = 0;

  if ( !fd )
  { perror(filename);
    exit(1);
  }

  while( fgets(buf, 1024, fd) )
  { int len = strlen(buf);
    if ( len > 0 )
      buf[len-1] = '\0';
    addSymbol(buf);
    done++;
  }

  fclose(fd);

  return done;
}


#define MAX_SYMBOL_LENGHT 256

const char mangle_chars[] = "~`!@#$%^&*()-_+={}[]|<>,./?:;'";
int mangle_index[MAX_SYMBOL_LENGHT];

void
mangle(char *s)
{ int l = strlen(s);
  int key = mangle_index[l]++;
  char buf[MAX_SYMBOL_LENGHT];
  int w = strlen(mangle_chars);
  char *q = buf;

  if ( key == 0 )
  { *q++ = mangle_chars[0];
  } else
  { while(key > 0)
    { *q++ = mangle_chars[key % w];
      key /= w;
    }
  }
  *q = '\0';

  if ( strlen(buf) > l )
  { fprintf(stderr, "%s: too many identifiers of length %d\n", program, l);
    exit(1);
  }
  
  memmove(s, buf, strlen(buf));
}


int
mangle_area(char *s, char *e)
{ char *q;
  int done = 0;

  while(s < e)
  { int l;

    q = s;
    for(l=0; l++ < MAX_SYMBOL_LENGHT && *s && s < e; s++)
      ;

    if ( *s == EOS && memberSymbol(q) )
    { mangle(q);
      done++;
    } else
    { while(*s && s < e)
	s++;
    }

    s++;
  }

  return done;
}


int
mangle_file(char *filename)
{ int fd;
  struct stat buf;
  long size;
  char *image;
  int done;

  if ( (fd = open(filename, O_RDWR)) < 0 || fstat(fd, &buf) )
  { perror(filename);
    exit(1);
  }

  size = buf.st_size;
  image = malloc(size);
  if ( read(fd, image, size) != size )
  { perror(filename);
    exit(1);
  }
  done = mangle_area(image, &image[size]);
  lseek(fd, 0, SEEK_SET);
  if ( write(fd, image, size) != size )
  { perror(filename);
    exit(1);
  }
  close(fd);

  return done;
}


void
usage()
{ fprintf(stderr, "%s symbol-list aoutfile\n", program);
  exit(1);
}


int
main(int argc, char *argv[])
{ int todo, done;

  program    = argv[0];

  if ( argc != 3 )
    usage();

  manglefile = argv[1];
  aoutfile   = argv[2];

  todo = read_manglefile(manglefile);
  done = mangle_file(aoutfile);

  if ( todo == done )
    printf("Mangled all %d symbols\n", done);
  else
    printf("Mangled %d of %d symbols\n", done, todo);

  exit(0);
}

