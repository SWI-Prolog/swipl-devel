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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Missing sun type declarations to allow gcc -Wall
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <sys/types.h>
#include <sys/time.h>

extern int sscanf (const char *, const char *, ...);
extern int fscanf (FILE *, const char *, ...);
extern int printf (const char *, ...);
extern int _filbuf (FILE *);
extern int _flsbuf (unsigned int, FILE *);
extern long time (long *);
extern int fprintf (FILE *, const char *, ...);
extern int fflush (FILE *);
extern long int strtol (const char *, char **, int);
extern int vprintf (const char *, char * );
extern int vsprintf (char *, const char *, char * );
extern int vfprintf (FILE *, const char *, char * );
extern long unsigned int fread (void *, long unsigned int,
				long unsigned int, FILE *);
extern long unsigned int fwrite (const void *, long unsigned int,
				 long unsigned int, FILE *);
extern int select (int width,
		   fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
		   struct timeval *timeout);
extern int ungetc (int, FILE *);
extern int getw(FILE *stream);
extern int putw(int data, FILE *stream);
extern int pclose(FILE *stream);
extern int fclose(FILE *stream);
extern void bcopy(void *b1, void *b2, int length);
extern int fseek (FILE *, long int, int);
extern char *getwd(char *pathname);
extern int ioctl(int fd, int request, void *arg);
extern void bzero(void *b, int length);
extern void perror(char *s);
extern int putenv(char *s);
