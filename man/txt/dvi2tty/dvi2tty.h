#define Copyright "dvi2tty.c  Copyright (C) 1984, 1985, 1986 Svante Lindahl.\n\
Copyright (C) 1988 M.J.E. Mol 1989, 1990"

#include <stdio.h>
#include <string.h>
#if defined(MSDOS)
#include <stdlib.h>
#include <alloc.h>     /* changed from malloc.h  (aci 17/6/91) */
#endif

#define TRUE        1
#define FALSE       0
#define nil         NULL

/*
 * ERROR CODES , don't start with 0
 */

#define illop    1              /* illegal op-code                   */
#define stkof    2              /* stack over-flow                   */
#define stkuf    3              /* stack under-flow                  */
#define stkrq    4              /* stack requirement                 */
#define lnerq    5              /* line allocation                   */
#define badid    6              /* id is not right                   */
#define bdsgn    7              /* signature is wrong                */
#define fwsgn    8              /* too few signatures                */
#define nopre    9              /* no pre-amble where expected       */
#define nobop   10              /* no bop-command where expected     */
#define nopp    11              /* no postpost where expected        */
#define bdpre   12              /* unexpected preamble occured       */
#define bdbop   13              /* unexpected bop-command occured    */
#define bdpst   14              /* unexpected post-command occured   */
#define bdpp    15              /* unexpected postpost               */
#define nopst   16              /* no post-amble where expected      */
#define illch   17              /* character code out of range       */
#define filop   18              /* cannot access file                */
#define filcr   19              /* cannot creat file                 */
#if !defined(MSDOS)
#define pipcr   20              /* cannot creat pipe                 */
#endif

/*---------------------------------------------------------------------------*/

typedef char bool;

typedef struct prlistptr {      /* list of pages selected for output         */
    int       pag;                      /* number of pages                   */
    bool      all;                      /* pages in interval selected        */
    struct prlistptr *prv;              /* previous item in list             */
    struct prlistptr *nxt;              /* next item in list                 */
} printlisttype;

/*---------------------------------------------------------------------------*/

extern bool   outputtofile;            /* output to file or stdout(dvi2tty.c)*/
extern bool   pageswitchon;            /* user-set pages to print(dvistuff.c)*/
extern bool   sequenceon;              /* not TeX pagenrs (dvistuff.c)       */
extern bool   scascii;                 /* Scand. nat. chars (dvistuff.c)     */
extern bool   noffd;                   /* output ^L or formfeed (dvistuff.c) */

extern printlisttype *currentpage;     /* current page to print (dvi2tty.c)  */
extern printlisttype *firstpage;       /* first page selected (dvi2tty.c)    */
extern printlisttype *lastpage;        /* last page selected (dvi2tty.c)     */

extern int            ttywidth;        /* screen width (dvi2tty.c)           */
extern int            espace;          /* extra screen width (dvi2tty.c)     */
extern long           foo;             /* temporary 'register' (dvi2tty.c)   */
extern int            opcode;          /* dvi opcode (dvistuff.c)            */

extern FILE *DVIfile;                  /* dvi file (dvi2tty.c)               */
extern FILE *output;                   /* output file (dvi2tty.c)            */

/*---------------------------------------------------------------------------*/

/* dvi2tty.c */
#if defined(MSDOS)
void errorexit(int);
#else
void errorexit();
#endif

/* dvistuff.c */
#if defined(MSDOS)
void dvimain(void);
#else
void dvimain();
#endif
