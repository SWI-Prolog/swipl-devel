/******************************************************************************
 * Marcel Mol: 1990-02-04  (UUCP: marcel@duteca.tudelft.nl)
 *               First attempt to recognize symbol fonts, so bullets (in
 *               itemized lists) are translated to a proper character instead
 *               an awfull ligature.
 *               Version 4.0.
 * Marcel Mol: 1990-02-01  (UUCP: marcel@duteca.tudelft.nl)
 *               Included port to VMS (off Joseph Vasallo and Seppo Rantala)
 *               into latest version. Hope things still work, cannot test it ...
 * Joseph Vasallo & Seppo Rantala: 1989-09-05 (Internet: rantala@tut.FI)
 *   Ported to work under VAX/VMS V4.4 & VAXC V2.4 or higher.
 *   Fixed bugs in using Swedish/Finnish characters.
 * Marcel Mol: 1989-02-14  (UUCP: duteca!marcel)
 *               Fixed check for .dvi extension.
 *               Allowed more ligatures.
 *               Fixed side effect bugs (2 gets as function arguments).
 *               Version 3.2.
 * Marcel Mol: 1989-01-19  (UUCP: duteca!marcel)
 *               Changed in option handling, no change
 *               in user interface (only the undocumented 
 *               feature -e).
 *               Version 3.1.
 * Marcel Mol: 1989-01-11  (UUCP: duteca!marcel)
 *               Changed some longs to ints.
 *               It now also runs on MSDOS Microsoft C 5.1
 *               New version: 3.0
 * Marcel Mol: 1989-01-03  (UUCP: duteca!marcel)
 *               Fixed a bugs concerning pager programs
 *               and scanning environment variable DVI2TTY.
 * Marcel Mol: 1988-10-25  (UUCP: duteca!marcel)
 *        dvi2tty.c dvi2tty.h dvistuff.c commands.h
 *               Converted program to C.
 *               improved spacing between words/characters.
 * bogart:/usr/alla/zap/dvitty/dvitty.p  1986-08-15 20:24:31,
 *               Version to be sent to mod.sources ready.
 * New option since last version:
 *   -Fprog      Pipe output to prog. Can be used to get a different
 *               pager than the default.
 * bogart:/usr/alla/zap/dvitty/dvitty.p  1986-01-13 21:49:31,
 *   Environment variable DVITTY is read and options can be set from it.
 *   These are the currently implemented options:
 *      -ofile   Write output to file, else write to stdout,
 *               possibly piped through a pager if stdout is a tty.
 *      -plist   Print pages whos TeX-page-number are in list.
 *               List is on the form  1,3:6,8  to choose pages
 *               1,3-6 and 8. TeX-nrs can be negative: -p-1:-4,4
 *      -Plist   Print pages whos sequential number are in list.
 *      -wn      Print the lines with width n characters, default is
 *               80. Wider lines gives better results.
 *      -q       Don't try to pipe to a pager.
 *      -f       Try to pipe to a pager if output is a tty.
 *      -Fname   Specify a pager program.                  
 *               Default of -q and -f is a compile time option, a constant.
 *      -l       Write '^L' instead of formfeed between pages.
 *      -u       Don't try to find Scandinavian characters (they will
 *               print as a:s and o:s if this option is choosen).
 *      -s       Scandinavian characters printed as }{|][\.
 *               Default of -s and -u is a compile time option, a constant.
 * bogart:/usr/alla/zap/dvitty/dvitty.p  1986-01-10 18:51:03,
 *   Argument parsing, and random access functions (external, in C)
 *   and other OS-dependent stuff (in C). Removed private 'pager' &
 *   tries to pipe through PAGER (environment var) or, if PAGER not
 *   defined, /usr/ucb/more. Some changes for efficency.
 * bogart:/usr/alla/svante/dvitty/dvitty.p  1985-07-15 20:51:00,
 *   The code for processing dvi-files running on UNIX (UCB-Pascal)
 *   but no argument parsing.
 * VERA::SS:<SVANTE-LINDAHL.WORK>DVITTY.PAS.140, 30-Mar-85 05:43:56,
 *   Edit: Svante Lindahl
 * VERA::SS:<SVANTE-LINDAHL.WORK>DVITTY.PAS.136, 15-Jan-85 13:52:59,
 *   Edit: Svante Lindahl, final Twenex version !!!??
 * VERA::SS:<SVANTE-LINDAHL.WORK>DVITTY.PAS.121, 14-Jan-85 03:10:22,
 *   Edit: Svante Lindahl, cleaned up and fixed a lot of little things
 * VERA::SS:<SVANTE-LINDAHL.WORK>DVITTY.PAS.25, 15-Dec-84 05:29:56,
 *   Edit: Svante Lindahl, COMND-interface, including command line scanning
 * VERA::SS:<SVANTE-LINDAHL.WORK>DVITTY.PAS.23, 10-Dec-84 21:24:41,
 *   Edit: Svante Lindahl, added command line scanning with Rscan-JSYS
 * VERA::<SVANTE-LINDAHL.DVITTY>DVITTY.PAS.48,  8-Oct-84 13:26:30,
 *  Edit: Svante Lindahl, fixed switch-parsing, destroyed by earlier patches
 * VERA::<SVANTE-LINDAHL.DVITTY>DVITTY.PAS.45, 29-Sep-84 18:29:53,
 *  Edit: Svante Lindahl
 *
 * dvitty - get an ascii representation of a dvi-file, suitable for ttys
 *
 * This program, and any documentation for it, is copyrighted by Svante
 * Lindahl. It may be copied for non-commercial use only, provided that
 * any and all copyright notices are preserved.
 *
 * Please report any bugs and/or fixes to:
 *
 * UUCP: {seismo,mcvax,cernvax,diku,ukc,unido}!enea!ttds!zap
 * ARPA: enea!ttds!zap@seismo.CSS.GOV
 *  or   Svante_Lindahl_NADA%QZCOM.MAILNET@MIT-MULTICS.ARPA
 * EAN:  zap@cs.kth.sunet
 */


#include "dvi2tty.h"
#if defined(VMS)
#include unixio
#endif

    /*-----------------------------------------------------------------------*/
    /* The following constants may be toggled before compilation to          */
    /* customize the default behaviour of the program for your site.         */
    /* Whichever their settings are, the defaults can be overridden at       */
    /* runtime.                                                              */
    /*-----------------------------------------------------------------------*/

#define DEFSCAND    FALSE     /* default is Scandinavian, toggle this if you */
                              /* don't have terminals with Scand. nat. chars */
#define WANTPAGER   FALSE     /* default: try to pipe through a pager (like  */
                              /* more) if stdout is tty and no -o switch     */
#define DEFPAGER    "more"   /* CHANGE TO YOUR LOCAL PAGER            */

    /*------------------ end of customization constants ---------------------*/

#define MAXLEN          100    /* size of char-arrays for strings            */
#if defined(MSDOS) || defined(VMS)
#define OPTSET      "wepPousl"   /* legal options                            */
#define OPTWARG     "wepPo"      /* options with argument                    */
#else
#define OPTSET      "wepPousqlfF"/* legal options                            */
#define OPTWARG     "wepPoF"     /* options with argument                    */
#endif

/*
 * USAGE CODES
 */

#define wrnge  1                /* width switch arg out of range     */
#define ign    2                /* ignore cause, print 'Usage:..'    */
#define nan    3                /* not a number where one expected   */
#define gae    4                /* garbage at end                    */
#define bdlst  5                /* bad page-numberlist               */
#define onef   6                /* only one dvifile allowed          */
#define bdopt  7                /* bad option                        */
#define onepp  8                /* only one page list allowed        */
#define noarg  9                /* argument expected                 */

char *dvi2tty = "@(#) dvi2tty.c  4.0 04/02/90 M.J.E. Mol (c) 1989, 1990";

/*---------------------------------------------------------------------------*/

printlisttype * currentpage;    /* current page to print                     */
printlisttype * firstpage;      /* first page selected                       */
printlisttype * lastpage;       /* last page selected                        */

FILE *          DVIfile;
FILE *          output;
bool            outputtofile;   /* tells if output goes to file or stdout    */
int             ttywidth;       /* max nr of chars per printed line          */
int             espace;         /* to fake calcs with ttywidth               */

long            foo;            /* utility variable, "register"              */
#if !defined(MSDOS) && !defined(VMS)
bool            pager;          /* tells if output is piped to a pager       */
char  *         path;           /* name of the pager to run                  */
#endif
char  *         progname;       /* our name                                  */
int             Argc;
char **         Argv;
char            DVIfilename[MAXLEN];
char *          OUTfilename;
char            optch;          /* for option handling                       */

/*---------------------------------------------------------------------------*/

#if defined(MSDOS)
void    main      (int, char **);
void    setoption (char *);
void    getargs   (void);
void    getpages  (int, char *);
void    plcnxt    (int);
void    getfname   (char *);
int     getinteger(int *, int *, char *);
void    usage     (int);
#else
char *  getenv    ();
FILE *  popen     ();

void    main      ();
void    setoption ();
void    getargs   ();
void    getpages  ();
void    plcnxt    ();
void    getfname   ();
int     getinteger();
void    usage     ();
#endif


/****************************************************************************/
/*                                                                          */
/*                                 M A I N                                  */
/*                                                                          */
/****************************************************************************/

void main(argc, argv)
int argc;
char ** argv;
{

    progname = *argv;
    Argc = argc;
    Argv = argv;

    getargs();                              /* read command line arguments   */
#if defined(MSDOS)
    if ((DVIfile = fopen(DVIfilename, "rb")) == NULL)
#else
#if defined(VMS)
    if ((DVIfile = fopen(DVIfilename, "r","ctx=rec")) == NULL)
#else
    if ((DVIfile = fopen(DVIfilename, "r")) == NULL)
#endif
#endif
        errorexit(filop);                   /* can't open dvifile            */

    if (outputtofile) {                     /* open the outfile, if needed   */
        if ((output = fopen(OUTfilename, "w")) == NULL)
            errorexit(filcr);
#if !defined(MSDOS) && !defined(VMS)
        pager = FALSE;
#endif
    }
    else {
        output = stdout;
#if !defined(MSDOS) && !defined(VMS)
        if (isatty(fileno(output)) && pager) {   /* try to pipe to a pager   */
            if ((output = popen(path, "w")) == NULL) /* get pipe to pager    */
                errorexit(pipcr);                /* make output to output    */
        }
        else
            pager = FALSE;
#endif
    }

    dvimain();

#if !defined(MSDOS) && !defined(VMS)
    if (pager)
        pclose(output);                     /* close pipe to pager            */
#endif

    exit(0);

} /* main */

/*----------------------------------------------------------------------------*/

void setoption(optarg)
char *optarg;
{
    int j = 0;
    int ret;

    while (strchr(OPTSET, optch) != NULL) {
        switch (optch) {
#if !defined(MSDOS) && !defined(VMS)
		case 'q' : pager = FALSE; break;
		case 'f' : pager = TRUE; break;
#endif
		case 'l' : noffd = TRUE; break;
		case 's' : scascii = TRUE; break;
		case 'u' : scascii = FALSE; break;
		case 'P' :
			sequenceon = TRUE;     /* fall through */
		case 'p' :
			if (pageswitchon)
				 usage(onepp);
			getpages(j, optarg);
			break;
		case 'w' :
			if (getinteger(&ttywidth, &j, optarg))
				usage(nan);
			if (optarg[j] != '\0')
				usage(gae);
			if ((ttywidth < 16) || (ttywidth > 132))
				usage(wrnge);
			break;
		case 'e' :
			if (getinteger(&espace, &j, optarg))
				usage(nan);
			if (optarg[j] != '\0')
				usage(gae);
			break;
		case 'o' :
			OUTfilename = optarg;
			outputtofile = TRUE;
			j = strlen(optarg);
			break;
#if !defined(MSDOS) && !defined(VMS)
		case 'F' :
			pager = TRUE;
			path = optarg;
			j = strlen(optarg);
			break;
#endif
		default  :
			usage(bdopt);
				}
        if ((optch = optarg[j]) == '\0')
            break;
        j++;
		if ((strchr(OPTWARG, optch) != NULL) && (optarg[j]=='\0')) {
			if (--Argc <= 0)
				usage(noarg);
			optarg = *++Argv;
			j = 0;
		}
	}
} /* setoption */

/*----------------------------------------------------------------------------*/

void getargs()
{
    char *str, *envp;
    bool DVIfound;                      /* if a dvi filename found           */

    if (Argc <= 1)
        usage(ign);

    pageswitchon = FALSE;       /* show all pages                            */
    sequenceon   = FALSE;       /* selected pages are TeX-numbered           */
    outputtofile = FALSE;       /* write to stdout                           */
#if !defined(MSDOS) && !defined(VMS)
    pager        = WANTPAGER;   /* want paging, compile time option          */
#endif
    noffd        = FALSE;       /* print formfeed between pages              */
    scascii      = DEFSCAND;    /* scandinavian, compile time option         */
    ttywidth     = 80;          /* default terminal width                    */
	espace       = 0;           /* to fake ttywidth calcs                    */
    DVIfound     = FALSE;

#if !defined(MSDOS) && !defined(VMS)
    if ((path = getenv("PAGER")) == NULL)   /* find default pathname of page */
		path = DEFPAGER;             /* program in case paging is wanted */
#endif

    if ((envp = getenv("DVI2TTY")) != NULL) {
        while (*envp == ' ')
			envp++;
        while (*envp) {                     /* environment var args          */
            if (strchr(OPTSET, optch = *envp++) != NULL) {
                /*
				 * we always pass one option, and arrange for optarg ourselves,
				 * so setoption does not mess up Argv
                 */
                if (strchr(OPTWARG, optch) != NULL) {
                    while (*envp == ' ')
                        envp++;
                    if (*envp == '\0')
                        usage(noarg);
                    str = envp;             /* str points to optarg          */
                    while ((*envp != ' ') && (*envp != '\0'))
                        *envp++;            /* set envp just after optarg    */
                    if (*envp != '\0')
                        *envp++ = '\0';     /* end optarg string             */
                }
                else
                    str = "";
                setoption(str);
            }
            else
                usage(bdopt);
            while (*envp == ' ')
                 envp++;
        }
    }

    while (--Argc > 0) {                    /* command line args             */
        str = *++Argv;
        if (*str != '-') {                  /* argument is not an option     */
            if (DVIfound)                   /* only one dvi file allowed     */
                usage(onef);
            getfname(str);
            DVIfound = TRUE;
        }
        else if (strchr(OPTSET, optch = *++str) != NULL) {
            str++;                      /* point to rest of argument if any  */
            if ((strchr(OPTWARG, optch) != NULL) && (*str == '\0')) {
                if (--Argc <= 0)
                    usage(noarg);
                str = *++Argv;
            }
            setoption(str);
        }
        else
            usage(bdopt);
    }

    if (!DVIfound)
        usage(ign);

} /* getargs */

/*---------------------------------------------------------------------------*/

void getpages(j, str)
int j;
char *str;
{
    int i, c;
    int num;

    pageswitchon = TRUE;
    firstpage = (printlisttype *) malloc(sizeof(printlisttype));
    firstpage->all = FALSE;
    firstpage->nxt = nil;
    firstpage->pag = 0;
    lastpage = firstpage;
    currentpage = firstpage;
    if (getinteger(&num, &j, str))
        usage(nan);
    plcnxt((int) num);
    while (str[j]) {
        c = str[j];
        if (c == ',' || c == ':') {
            j++;
            if (getinteger(&num, &j, str))
                usage(nan);
        }
        else
            break;
        if (c == ',')
            plcnxt(num);
        else {
            if (currentpage->pag < 0) {
                if (num > 0) {
                    currentpage->all = TRUE;
                    plcnxt(num);
                }
                else if (num < currentpage->pag)
                    for (i = currentpage->pag - 1; i >= num; i--)
                        plcnxt(i);
                else
                    usage(bdlst);
            }
            else {
                if (num < currentpage->pag)
                    usage(bdlst);
                for (i = currentpage->pag + 1; i <= num; i++)
                    plcnxt(i);
            }
        }
    }
    if ((str[j] != ' ') && (str[j] != NULL)) {
        usage(gae);
    }
    currentpage = firstpage;

} /* getpages */


void plcnxt(pagnr)      /* place page-nr next in list */
int pagnr;
{
    currentpage = lastpage;
    currentpage->pag = pagnr;
    lastpage = (printlisttype *) malloc(sizeof(printlisttype));
    lastpage->all = FALSE;
    lastpage->nxt = nil;
    lastpage->pag = 0;
    currentpage->nxt = lastpage;

} /* plcnxt */

/*----------------------------------------------------------------------------*/

void getfname(str)
char *str;
{
    int   i;

    i = strlen(str);
    if (i == 0)
        usage(ign);
    strcpy(DVIfilename, str);
    if (!((i >= 5) && (str[i-1] == 'i') && (str[i-2] == 'v') &&
          (str[i-3] == 'd') && (str[i-4] == '.'))) {
        strcat(DVIfilename, ".dvi");
    }

} /* getfname */

/*----------------------------------------------------------------------------*/

int getinteger(dest, j, str)
int *dest;
int *j;
char *str;
{
    int  cum;
    int  sgn;
    char ch;

    ch = str[*j];
    if (ch == '-') {
        sgn = -1;
        ch  = str[++(*j)];
    }
    else
        sgn = 1;
    if ((ch >= '0') && (ch <= '9')) {
        cum = 0;
        while ((ch >= '0') && (ch <= '9')) {
            cum = cum*10 + ch - '0';
            ch = str[++(*j)];
        }
        *dest = sgn * cum;
        return 0;                   /* return ok */
    }
    return 1;                       /* return error */

}   /* getinteger */

/*----------------------------------------------------------------------------*/

void errorexit(errorcode)
int errorcode;
{

    fprintf(stderr, "%s: ", progname);
    switch (errorcode) {
        case  illop : fprintf(stderr, "Illegal op-code found: %d\n", opcode);
                      break;
        case  stkof : fprintf(stderr, "Stack overflow\n");
                      break;
        case  stkuf : fprintf(stderr, "Stack underflow\n");
                      break;
        case  stkrq : fprintf(stderr, "Cannot create dvi stack\n");
                      break;
        case  lnerq : fprintf(stderr, "Cannot allocate memory\n");
                      break;
        case  badid : fprintf(stderr, "Id-byte is not correct: %d\n ", opcode);
                      break;
        case  bdsgn : fprintf(stderr, "Bad signature: %d (not 223)\n",
                                      (int) foo);
                      break;
        case  fwsgn : fprintf(stderr, "%d signature bytes (min. 4)\n",
                                      (int) foo);
                      break;
        case  nopre : fprintf(stderr, "Missing preamble\n");
                      break;
        case  nobop : fprintf(stderr, "Missing beginning-of-page command\n");
                      break;
        case  nopp  : fprintf(stderr, "Missing post-post command\n");
                      break;
        case  bdpre : fprintf(stderr, "Preamble occured inside a page\n");
                      break;
        case  bdbop : fprintf(stderr, "BOP-command occured inside a page\n");
                      break;
        case  bdpst : fprintf(stderr, "Postamble occured before end-of-page\n");
                      break;
        case  bdpp  : fprintf(stderr, "Postpost occured before post-command\n");
                      break;
        case  nopst : fprintf(stderr, "Missing postamble\n");
                      break;
        case  illch : fprintf(stderr, "Character code out of range, 0..127\n");
                      break;
        case  filop : fprintf(stderr, "Cannot open dvifile\n");
                      break;
        case  filcr : fprintf(stderr, "Cannot create outfile\n");
                      break;
#if !defined(MSDOS) && !defined(VMS)
        case  pipcr : fprintf(stderr, "Cannot create pipe to pager\n");
                      break;
#endif
        default     : fprintf(stderr, "Unkown error code\n");
                      break;
    };
    if (outputtofile)
#if defined(VMS)
        remove(OUTfilename);
#else
        unlink(OUTfilename);
#endif
    exit(errorcode);

}  /* errorexit */

/*----------------------------------------------------------------------------*/

void usage(uerr)
int uerr;
{

    if (uerr != ign) {
        fprintf(stderr,"%s: ", progname);
        switch (uerr) {
            case   ign    : fprintf(stderr, "%s", Copyright);
                            break;
            case   wrnge  : fprintf(stderr, "width arg out of range:16-132");
                            break;
            case   nan    : fprintf(stderr, "numeric argument expected for option %c",
                                            optch);
                            break;
            case   gae    : fprintf(stderr, "garbage in argument for option %c",
                                            optch);
                            break;
            case   bdlst  : fprintf(stderr, "mal-formed list of pagenumbers");
                            break;
            case   onef   : fprintf(stderr, "only one infile argument allowed");
                            break;
            case   noarg  : fprintf(stderr, "option argument expected for option %c",
                                            optch);
                            break;
            case   bdopt  : fprintf(stderr, "bad option %c", optch);
                            break;
            case   onepp  : fprintf(stderr, "only one pagelist allowed");
                            break;
            default       : fprintf(stderr, "unknown usage error");
                            break;
        }
        fprintf(stderr, "\n");
    }
    fprintf(stderr, "Usage: %s [ options ] dvifile[.dvi]\n", progname);
    fprintf(stderr, "Options are:\n");
    fprintf(stderr,
            " -ofile   Write output to file, else write to stdout.\n");
    fprintf(stderr,
			" -plist   Print pages in list, using TeX page numbers.\n");
    fprintf(stderr,
			" -Plist   Print pages in list, using page sequence in dvifile.\n");
    fprintf(stderr,
			" -wn      Set line width to n characters.\n");
#if !defined(MSDOS) && !defined(VMS)
	fprintf(stderr,
			" -f       Try to pipe to a pager if output is a tty");
    if (WANTPAGER)
        fprintf(stderr, " (default).\n");
    else
        fprintf(stderr, ".\n");
	fprintf(stderr,
			" -q       Don't try to pipe to a pager");
    if (WANTPAGER)
        fprintf(stderr, ".\n");
    else
        fprintf(stderr, " (default).\n");
	fprintf(stderr,
			" -Fprog   Pipe output to pager prog.\n");
#endif
    fprintf(stderr,
            " -l       Write ''^L'' instead of formfeed between pages.\n");
    fprintf(stderr,
            " -u       National Swedish/Finnish characters printed as aaoAAO");
    if (DEFSCAND)
        fprintf(stderr, ".\n");
    else
        fprintf(stderr, " (default).\n");
    fprintf(stderr,
            " -s       National Swedish/Finnish characters printed as }{|][\\");
    if (DEFSCAND)
        fprintf(stderr, " (default).\n");
    else
        fprintf(stderr, ".\n");
    exit(uerr);

} /* usage */

