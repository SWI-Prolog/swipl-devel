/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Missing sun type declarations to allow gcc -Wall
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern int sscanf (const char *, const char *, ...);
/* extern int fscanf (FILE *, const char *, ...); */
extern int printf (const char *, ...);
/* extern int _filbuf (FILE *); */
/* extern int _flsbuf (unsigned int, FILE *); */
extern long time (long *);
/* extern int fprintf (FILE *, const char *, ...); */
/* extern int fflush (FILE *); */
extern long int strtol (const char *, char **, int);
extern int vprintf (const char *, char * );
extern int vsprintf (char *, const char *, char * );
/* extern int vfprintf (FILE *, const char *, char * ); */
/* extern long unsigned int fread (void *, long unsigned int, */
/* 				long unsigned int, FILE *); */
/* extern long unsigned int fwrite (const void *, long unsigned int, */
/* 				 long unsigned int, FILE *); */
/* extern int ungetc (int, FILE *); */
/* extern int getw(FILE *stream); */
/* extern int putw(int data, FILE *stream); */
/* extern int pclose(FILE *stream); */
/* extern int fclose(FILE *stream); */
extern void bcopy(void *b1, void *b2, int length);
/* extern long fseek (FILE *, long, int); */
extern char *getwd(char *pathname);
extern int ioctl(int fd, int request, void *arg);
extern void bzero(void *b, int length);
extern void perror(char *s);
extern int putenv(char *s);
extern void *sbrk(int incr);
extern int brk(void *addr);
extern void srandom(int seed);
extern long random(void);
extern int getpagesize(void);
extern int getdtablesize(void);
extern int remove(const char *);
extern int rename(const char *old, const char *new);
extern void *valloc(size_t size);
extern int system(const char *cmd);
extern int readlink(char *path, char *buf, int bufsiz);
extern int gettimeofday(struct timeval *, struct timeval *);
extern int select (int width,
		   fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
		   struct timeval *timeout);
extern int setitimer(int, struct itimerval *,struct itimerval *);
extern int munmap(caddr_t, size_t);

