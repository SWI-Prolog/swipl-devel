/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <signal.h>
#include <h/kernel.h>
#include <h/interface.h>

static void
errorSignal(int sig)
{ char *msg;
  char tmp[25];

  switch(sig)
  {
#ifdef SIGQUIT
    case SIGQUIT:		msg = "Quit";			break;
#endif
    case SIGILL:		msg = "Illegal intruction";	break;
#ifdef SIGEMT
    case SIGEMT:		msg = "EMT trap";		break;
#endif
#ifdef SIGBUS
    case SIGBUS:		msg = "Bus error";		break;
#endif
    case SIGSEGV:		msg = "Segmentation violation";	break;
#ifdef SIGSYS
    case SIGSYS:		msg = "Bad system call";	break;
#endif
#ifdef SIGPIPE
    case SIGPIPE:		msg = "Pipe error";		break;
#endif
    case SIGFPE:		msg = "Floating point exception"; break;
    default:			msg = tmp;
				sprintf(tmp, "%d", sig);	break;
  }

  errorPce(PCE, NAME_signal, CtoName(msg));
}


void
catchErrorSignals(Bool yes)
{ Func handler = (yes == ON ? (Func)errorSignal : (Func)SIG_DFL);

#ifdef SIGQUIT
  hostAction(HOST_SIGNAL, SIGQUIT, handler);
#endif
  hostAction(HOST_SIGNAL, SIGILL,  handler);
#ifdef SIGEMT
  hostAction(HOST_SIGNAL, SIGEMT,  handler);
#endif
#ifdef SIGBUS
  hostAction(HOST_SIGNAL, SIGBUS,  handler);
#endif
  hostAction(HOST_SIGNAL, SIGSEGV, handler);
#ifdef SIGSYS
  hostAction(HOST_SIGNAL, SIGSYS,  handler);
#endif
  hostAction(HOST_SIGNAL, SIGFPE,  handler);
}

#if sun && !solaris

static int	printFrame(long pc, int an, long *av, long sigpc);
static void	printArgument(int arg);

static int dumping = FALSE;	/* dump core on error in stack trace */

#ifdef sparc
/*  Push a number of stack frames, so the one we are interested in are
    pushed into memory!

 ** Fri Feb 24 17:57:05 1989  jan@swivax.UUCP (Jan Wielemaker)  */

static int
pushStack(int n)
{ if ( n == 0 )
    return 1;

  return pushStack(n-1) + n;
}
#endif /*sparc*/

#include <frame.h>

static void
_pcePrintStack(int depth, int sigpc)
{ struct frame *f;		/* this frame */
  struct frame *pf;		/* parent frame */
  struct frame *cf;		/* child frame */
  int pc;			/* program counter */
  int an;			/* number of arguments */
  int *av;			/* argument vector */
  long *lp;
  int failed = 0;

  if (dumping)		/* produce core on error in stack trace */
    abort();
  dumping = TRUE;

  lp = (long *) &depth;

#ifdef sparc
  { struct frame fr;			/* SUN-4, SunOs 4.0 */
    int offset;

    offset = (long *)(&fr.fr_argd[0]) - (long *)(&fr);
    pushStack(10);			/* HACK ? */
    f = (struct frame *)(&lp[-offset]);
  }
#else
  f = (struct frame *)(&lp[-2]);	/* SUN-3, SunOs 3.4 and 4.0 */
#endif

  cf = NULL;

  do
  { if ( depth-- <= 0 )
      break;

    pf = f->fr_savfp;
#ifdef sparc
    if ( pf == NULL )
      break;
#endif
    pc = (cf == NULL ? (int)pcePrintStack : cf->fr_savpc-1);
    av = f->fr_arg;
    an = 4;

    if (printFrame(pc, an, (long *)av, sigpc) == FALSE)
      if ( failed++ > 5 )
	break;

    cf = f;
    f = pf;
  } while(f != NULL);

  dumping = FALSE;
}


static int
printFrame(long pc, int an, long *av, long sigpc)
{ int percent;
  char *f;
  int rval = TRUE;

  if ( getFunctionNameFromAddress == NULL )
  { f = c_function_name_from_address(pc, &percent);
    if ( !f )
      f = "???";
    else if ( streq(f, "_sigtramp") )
    { pc = sigpc;
      f = c_function_name_from_address(pc, &percent);
      if ( !f )
	f = "???";
    }
  } else
  { f = (*getFunctionNameFromAddress)(pc, &percent);
    if ( streq(f, "_sigtramp") )
    { pc = sigpc;
      f = (*getFunctionNameFromAddress)(pc, &percent);
    }
  }

  switch(an)
  { int i;

    case 0:	printf("%s()", f);
		break;
    case 1:	printf("%s(", f);
    		printArgument(av[0]);
    		printf(")");
		break;
    default:	printf("%s(", f);
    		printArgument(av[0]);
		for(i=1; i<an; i++)
		{ if (i > 10)
		  { printf(", ...");
		    break;
		  }
		  printf(", ");
		  printArgument(av[i]);
		}
		printf(")");
		break;
  }

  if ( percent > 0 )
    printf( " %d%%\n",percent );
  else
    printf("\n");

  return rval;
}


static void
printArgument(int arg)
{ printf("%d: ", arg); fflush(stdout);
  printf("%s", pp(arg));
}


#else /*~sun*/

static void
_pcePrintStack(depth, sigpc)
int depth, sigpc;
{ printf("Cannot print stack on this machine\n");
}

#endif

void
pcePrintStack(int depth)
{ _pcePrintStack(depth, 0);
}


status
confirmTerminal(char *question, char *def)
{ char line[256];

  printf("%s [%s] ? ", question, *def == 'n' ? "ny" : "yn");
  fflush(stdout);
  if (gets(line) == NULL)
    return *def == 'y';
  switch(line[0])
  { case 'n':
    case 'N':	return FALSE;
    case 'y':
    case 'Y':	return TRUE;
    case '\0':	return *def == 'y' ? TRUE : FALSE;
    default:	printf("Please answer 'yes' or 'no'\n");
		return confirmTerminal(question, def);
  }
}
