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

#ifndef O_RUNTIME

status
confirmTerminal(char *question, char *def)
{ char line[256];

  Cprintf("%s [%s] ? ", question, *def == 'n' ? "ny" : "yn");
  if ( Cgetline(line, sizeof(line)) == NULL )
    return *def == 'y';
  switch(line[0])
  { case 'n':
    case 'N':	return FALSE;
    case 'y':
    case 'Y':	return TRUE;
    case '\0':	return *def == 'y' ? TRUE : FALSE;
    default:	Cprintf("Please answer 'yes' or 'no'\n");
		return confirmTerminal(question, def);
  }
}

#endif /*O_RUNTIME*/
