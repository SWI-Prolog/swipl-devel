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
