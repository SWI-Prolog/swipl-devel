/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#ifdef __linux__
#define _XOPEN_SOURCE			/* GNU glibc grantpt() prototypes */
#endif
#include <h/kernel.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Portability issues

The initial version of this module  was   based  on Unix.  It depends on
four notions in Unix: pipes, pseudo-terminals, the fork/exec combination
to create a new process and  the   kill  mechanims  to communicate in an
asynchronous manner between processes.

This call subsumes from class stream,  that implements the communication
to  the  exernal  process.   In  Unix,    pipes  are  sockets  and  this
communication is thus uniform.  In Win32  it appears there are different
types for these things that all require their own communicaton.

Win32 doesn't know about psuedo  terminals.  Pipe()/Fork()/exec() may be
simulated using CreatePipe() and CreateProcess().  The kill mechanism is
reduced to the facility to terminate the inferior process.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __unix__
#define HAVE_FORK	1
#define HAVE_PTYS	1
#define HAVE_KILL	1
#define HAVE_WAIT	1
#define HAVE_TERMIOS_H	1
#define HAVE_TERMIO_H	1
#define HAVE_SYS_WAIT_H	1
#define HAVE_SYS_IOCTL_H 1
#define USE_SIGCHLD	1
#endif

#include <h/unix.h>
#include <h/interface.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <fcntl.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#if defined(HAVE_SYS_IOCTL_H) && !defined(sun) /* leads to redefines */
#include <sys/ioctl.h>
#endif

#ifdef __WIN32__
#define environ _environ	/* declared in STDLIB.H */
#else
extern char **environ;		/* Unix version */
#endif

					/* this fixes RS6000/AIX problems */
#if !defined(TCGETS) && defined(HAVE_TERMIO_H)
#include <termio.h>
#endif

#if HAVE_STROPTS_H && HAVE_GRANTPT	/* Solaris */
#define USE_GRANTPT 1
#include <stropts.h>
#ifdef __mips__				/* These prototypes are nowhere!? */
extern int   grantpt(int filedes);
extern char *ptsname(int fildes);
extern int   unlockpt(int fildes);
#endif
#endif

static status	killProcess(Process p, Any sig);

#define OsError() getOsErrorPce(PCE)

#ifndef USE_GRANTPT
forwards int		getSlave(Process p, const char *line);
static int		getPseudoTTY(Process p, char *line,
				     int *master, int *slave);
#endif
forwards status		copyTty(Process, char *, int);

static Chain		ProcessChain;	/* running process objects */
static int		initialised;	/* signals initialised */

static Name signames[] =
{ NAME_null,				/* 0 is not defined */
  NAME_hup,				/* 1 */
  NAME_int,				/* 2 */
  NAME_quit,				/* 3 */
  NAME_ill,				/* 4 */
  NAME_trap,				/* 5 */
  NAME_abrt,				/* 6 */
  NAME_emt,				/* 7 */
  NAME_fpe,				/* 8 */
  NAME_kill,				/* 9 */
  NAME_bus,				/* 10 */
  NAME_segv,				/* 11 */
  NAME_sys,				/* 12 */
  NAME_pipe,				/* 13 */
  NAME_alrm,				/* 14 */
  NAME_term,				/* 15 */
  NAME_urg,				/* 16 */
  NAME_stop,				/* 17 */
  NAME_tstp,				/* 18 */
  NAME_cont,				/* 19 */
  NAME_chld,				/* 20 */
  NAME_ttin,				/* 21 */
  NAME_ttou,				/* 22 */
  NAME_io,				/* 23 */
  NAME_xcpu,				/* 24 */
  NAME_xfsz,				/* 25 */
  NAME_vtalrm,				/* 26 */
  NAME_prof,				/* 27 */
  NAME_winch,				/* 28 */
  NAME_lost,				/* 29 */
  NAME_usr1,				/* 30 */
  NAME_usr2,				/* 31 */
  NULL
};


#ifdef USE_SIGCHLD

#ifdef HAVE_SIGINFO_H
#include <siginfo.h>
#endif

#ifndef SA_NOMASK
#define SA_NOMASK 0
#endif
#ifndef SA_RESTART
#define SA_RESTART 0
#endif
#ifndef SA_NOCLDWAIT
#define SA_NOCLDWAIT 0
#endif
#ifndef SA_NOCLDSTOP
#define SA_NOCLDSTOP 0
#endif
#ifndef SA_SIGINFO
#define SA_SIGINFO 0
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Catching childs that have changed status. There   appear to be many ways
for doing this. Posix doesn't  provide   signal  context, so by default,
waitpid() is used to find out  what   child  changed status. On Solaris,
this appears to lead to a loop. Therefore we use the context information
passed to the handler. I've tried to  configure all this without testing
for Solaris itself to exploit these features automatically in compatible
operating system. Be careful.

Note this function is called asynchronously, and is therefore dangerous.
It would be better to merge  it   into  the event-queue, but X11 doesn't
provide an interface for this, as far as  I know. Maybe posting an event
to myself?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
syncSend(Any rec, Name sel, int argc, const Any *argv)
{ ArgVector(av, argc+2);
  int i, ac = 0;
  Timer t;

  av[ac++] = rec;
  av[ac++] = sel;
  for(i=0; i<argc; i++)
    av[ac++] = argv[i];

  t = newObject(ClassTimer, ZERO,
		newObject(ClassAnd,
			  newObjectv(ClassMessage, ac, av),
			  newObject(ClassMessage, RECEIVER, NAME_free, 0),
			  0), 0);

  statusTimer(t, NAME_once);
}



static void
#if USE_SIGINFO
child_changed(int sig, siginfo_t *info, void *uctx)
#else
child_changed(int sig)
#endif
{ Any rstat = NIL;
  Any sel   = NIL;
  Process p = NIL;
		
#ifdef UNION_WAIT
#define wait_t union wait
#else
#define wait_t int
#endif

#if USE_SIGINFO
  DEBUG(NAME_process, Cprintf("child %d changed called\n", info->si_pid));

  for_chain(ProcessChain, p,
	    { int pid = valInt(p->pid);
	      
	      if ( pid == info->si_pid )
	      { switch( info->si_code )
		{ case CLD_EXITED:
		    sel   = NAME_exited;
		    rstat = toInt(info->si_status);
		    break;
		  case CLD_KILLED:
		  case CLD_STOPPED:
		    sel   = NAME_killed;
		    rstat = signames[info->si_status];
		    break;
		  case CLD_DUMPED:
		    sel   = NAME_exited;
		    rstat = toInt(-1);
		    break;
		  case CLD_CONTINUED:
		    break;
		}

		break;
	      }
	    });

  if ( notNil(rstat) )
  { DEBUG(NAME_process, Cprintf("Posting %s->%s: %s\n",
				pp(p), pp(sel), pp(rstat)));
    syncSend(p, sel, 1, &rstat);
  }

#else /*USE_SIGINFO*/

  DEBUG(NAME_process, Cprintf("child_changed() called\n"));

  for_chain(ProcessChain, p,
	    { int pid = valInt(p->pid);
	      wait_t status;

	      if ( waitpid(pid, &status, WNOHANG|WUNTRACED) == pid )
	      { if ( WIFSTOPPED(status) )
		{ sel   = NAME_stopped;
		  rstat = signames[WSTOPSIG(status)];
		} else if ( WIFSIGNALED(status) )
		{ sel   = NAME_killed;
		  rstat = signames[WTERMSIG(status)];
		} else if ( WIFEXITED(status) )
		{ sel   = NAME_exited;
		  rstat = toInt(WEXITSTATUS(status));
		}

		if ( notNil(rstat) )
		{ DEBUG(NAME_process, Cprintf("Posting %s->%s: %s\n",
				pp(p), pp(sel), pp(rstat)));
		  syncSend(p, sel, 1, &rstat);
		}
	      }
	    });

#endif /*USE_SIGINFO*/

#if !defined(BSD_SIGNALS) && !defined(HAVE_SIGACTION)
  signal(sig, child_changed);
#endif
}

#endif /*USE_SIGCHLD*/

void
killAllProcesses(int status)
{ Cell cell;

  for_cell(cell, ProcessChain)
  { Process p = cell->value;

    errorPce(p, NAME_killedOnExit);
    killProcess(p, NAME_hup);
    killProcess(p, NAME_kill);
  }
}


static void
setupProcesses()
{ if ( !initialised )
  {
#if defined(SIGCHLD) && defined(HAVE_WAIT)
#ifdef HAVE_SIGACTION
    struct sigaction action, oaction;

    memset((char *) &action, 0, sizeof(action));
#if USE_SIGINFO
    action.sa_sigaction = child_changed;
#else
    action.sa_handler   = child_changed;
#endif
    action.sa_flags     = SA_SIGINFO|SA_NOMASK|SA_RESTART;

    sigaction(SIGCHLD, &action, &oaction);
#else
    hostAction(HOST_SIGNAL, SIGCHLD, child_changed);
#endif
#endif
    at_pce_exit(killAllProcesses, ATEXIT_FIFO);
    initialised++;
  }
}


static status
initialiseProcess(Process p, CharArray cmd, int argc, CharArray *argv)
{ initialiseStream((Stream)p, NIL, NIL, NIL, DEFAULT);

  assign(p, name, cmd);
  assign(p, arguments, newObjectv(ClassVector, argc, (Any *)argv));
  assign(p, status, NAME_inactive);
#ifdef HAVE_PTYS
  assign(p, use_tty, ON);
#else
  assign(p, use_tty, OFF);
#endif
  assign(p, directory, DEFAULT);

  succeed;
}


static status
unlinkProcess(Process p)
{ closeInputProcess(p);			/* close input */
  deleteChain(ProcessChain, p);
  if ( notNil(p->pid) )
    killProcess(p, NAME_hup);
  if ( notNil(p->pid) )			/* doesn't listen ... */
    killProcess(p, NAME_kill);

  succeed;
}


status					/* exported to msw/msprocess.c */
pidProcess(Process p, Int pid)
{ setupProcesses();

  assign(p, pid, pid);
  appendChain(ProcessChain, p);

  succeed;
}


static status
doneProcess(Process p)
{ ws_done_process(p);

  deleteChain(ProcessChain, p);
  assign(p, pid, NIL);

  succeed;
}


status
closeInputProcess(Process p)
{ closeInputStream((Stream) p);
  closeOutputStream((Stream) p);

  assign(p, tty, NIL);

  succeed;
}


		 /*******************************
		 *     ENVIRONMENT HANDLING	*
		 *******************************/

static void
initEnvironment(Process p)
{ if ( notNil(p->environment) )
  { int i = 0;
    Cell cell;
    Name fmt = CtoName("%s=%s");

    environ = pceMalloc(sizeof(char *) *
			(valInt(p->environment->attributes->size) + 1));

    for_cell(cell, p->environment->attributes)
    { Attribute a = cell->value;
      StringObj str = answerObject(ClassString, fmt, a->name, a->value, 0);

      environ[i++] = save_string(strName(str));
      doneObject(str);
    }
    environ[i] = NULL;
  }
}


static Sheet
getEnvironmentProcess(Process p)
{ if ( isNil(p->environment) )
  { char **env = environ;

    assign(p, environment, newObject(ClassSheet, 0));
    for(; *env; env++)
    { char buf[LINESIZE];
      char *q;
      int l;

      DEBUG(NAME_environment, Cprintf("env = %s\n", *env));
      if ( (q=strchr(*env, '=')) )
      { strncpy(buf, *env, (l = q - *env));
	buf[l] = EOS;
	valueSheet(p->environment, CtoName(buf), CtoName(&q[1]));
      } else
	valueSheet(p->environment, CtoName(*env), CtoName(""));
    }
  }

  answer(p->environment);
}


static status
environmentProcess(Process p, Name name, CharArray value)
{ return valueSheet(getEnvironmentProcess(p), name, value);
}


#ifdef HAVE_FORK			/* The Unix fork()/exec one */

		 /*******************************
		 *	       OPEN		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Solaris stuff with many thanks to Andrew Chittenden, ADT
(asc@concurrent.co.uk)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CHILD_BASE		128
#define CHILD_NOPTY		(CHILD_BASE+1)
#define CHILD_NOEXEC		(CHILD_BASE+2)

static status
openProcess(Process p, CharArray cmd, int argc, CharArray *argv)
{ if ( notDefault(cmd) )
  { if ( notNil(p->pid) )
      return errorPce(p, NAME_noChangeAfterOpen);

    assign(p, name, cmd);
    assign(p, arguments, newObjectv(ClassVector, argc, (Any *)argv));
  }

  if ( isNil(p->pid) )
  { if ( p->use_tty == ON )
    { int master, slave;
      int pid;
#if USE_GRANTPT
      char *line = NULL;
#else
      char line[100];
#endif

#if USE_GRANTPT
      if ( (master = open("/dev/ptmx", O_RDWR)) < 0 )
#else
      if ( !getPseudoTTY(p, line, &master, &slave) )
#endif
      { Cprintf("[PCE: Failed to get pseudo tty: %s]\n",
		strName(OsError()));
	fail;
      }

      if ( (pid = fork()) == 0 )	/* child process */
      { int i, argc;
	char **argv;
	int maxfd = getdtablesize();

	if ( notDefault(p->directory) )
	  cdDirectory(p->directory);
	initEnvironment(p);
	DEBUG(NAME_process, Cprintf("Environment initialised\n"));
#ifdef HAVE_SETSID
	if ( setsid() < 0 )
	  Cprintf("[PCE: setsid() failed: %s]\n", strName(OsError()));
#else
      { int fd;

	if ( (fd = open("/dev/tty", 2)) )
	{ ioctl(fd, TIOCNOTTY, NULL);	/* detach from controlling tty */
	  close(fd);
	}
      }
#endif
#ifdef USE_GRANTPT
	if ( grantpt(master) < 0 ||
	     unlockpt(master) < 0 ||
	     (line = ptsname(master)) == NULL ||
	     (slave = open(line, O_RDWR)) < 0 )
	{ Cprintf("[PCE: failed to get slave pty: %s]\n", strName(OsError()));
	  exit(1);
	}
	ioctl(slave, I_PUSH, "ptem");	/* don't worry it these fail */
	ioctl(slave, I_PUSH, "ldterm");
	ioctl(slave, I_PUSH, "ttcompat");
#endif /*USE_GRANTPT*/

	DEBUG(NAME_process, Cprintf("Slave %s at %d\n", line, slave));
	if ( !copyTty(p, line, slave) )
	{ Cprintf("[PCE: failed to reset %s: %s]\n", line, strName(OsError()));
	  exit(CHILD_NOPTY);
	}
	DEBUG(NAME_process, Cprintf("%s initialised\n", line));

	for(i=0; i<=2; i++)		/* dup slave to stdin/stdout/stderr */
	{ if ( slave != i )
	    dup2(slave, i);
	}

	for(i=3; i < maxfd; i++)	/* close remaining open fd's */
	  close(i);

#ifdef TIOCSCTTY
	ioctl(0, TIOCSCTTY, NULL);
#endif        

	argv = (char **)alloca(sizeof(char *) *
			       (valInt(p->arguments->size) + 2));
	argc = valInt(p->arguments->size);

	argv[0] = strName(p->name);

	for(i=0; i<argc; i++)
	  argv[i+1] = toCharp(p->arguments->elements[i]);
	argv[i+1] = NULL;

	if ( execvp(strName(p->name), argv) )
	{ Cprintf("[PCE: Failed to start %s: %s]\n",
		  strName(p->name), strName(OsError()));
	  exit(CHILD_NOEXEC);
	}
      } else				/* parent process  */
      {
#ifndef USE_GRANTPT
	close(slave);
#else
	if ( (line = ptsname(master)) != NULL )
#endif
	  assign(p, tty, CtoName(line));
	pidProcess(p, toInt(pid));
	p->rdfd = p->wrfd = master;
	p->rdstream = NULL;
	assign(p, status, NAME_running);
      }
    } else				/* no tty: use pipes */
    { int wrfd[2];
      int rdfd[2];
      int pid;

      if ( pipe(wrfd) )
      	return errorPce(p, NAME_noPipe, OsError());
      if ( pipe(rdfd) )
      { close(wrfd[0]);
	close(wrfd[1]);
	return errorPce(p, NAME_noPipe, OsError());
      }

      if ( (pid = fork()) == 0 )	/* child process */
      { int i;
	char **argv;
	int argc;

	if ( notDefault(p->directory) )
	  cdDirectory(p->directory);
	initEnvironment(p);

	for(i = getdtablesize()-1; i >= 0; i--)
	  if ( i != wrfd[0] && i != rdfd[1] )
	    close(i);



	dup2(wrfd[0], 0);
	dup2(rdfd[1], 1);
	dup2(rdfd[1], 2);
	close(wrfd[0]);
	close(rdfd[1]);

	argv = (char **)alloca(sizeof(char *) *
			       (valInt(p->arguments->size) + 2));
	argc = valInt(p->arguments->size);

	argv[0] = strName(p->name);

	for(i=0; i<argc; i++)
	  argv[i+1] = toCharp(p->arguments->elements[i]);
	argv[i+1] = NULL;

	if ( execvp(strName(p->name), argv) )
	{ Cprintf("[PCE: Failed to start %s: %s]\n",
		  strName(p->name), strName(OsError()));
	  exit(CHILD_NOEXEC);
	}
      } else
      { close(wrfd[0]);
	close(rdfd[1]);
	pidProcess(p, toInt(pid));
	p->wrfd = wrfd[1];
	p->rdfd = rdfd[0];
	assign(p, status, NAME_running);
      }
    }
    
    inputStream((Stream)p, DEFAULT);
  }

  succeed;
}

#else /*HAVE_FORK*/
#ifdef __WIN32__
extern status openProcess(Process p, CharArray cmd, int argc, CharArray *argv);
#endif
#endif /*HAVE_FORK*/


static status
killProcess(Process p, Any sig)
{ int n;

  if ( isDefault(sig) )
    sig = NAME_term;

  if ( isInteger(sig) )
    n = valInt(sig);
  else
  { for(n=1; signames[n]; n++)
      if ( signames[n] == sig )
	break;
    if ( !signames[n] )
      return errorPce(p, NAME_unknownSignal, sig);
  }

  if ( isNil(p->pid) )
  { if ( n != 1 && n != 9 && n != 15 )
      errorPce(p, NAME_notOpen);
    fail;
  }

#ifdef HAVE_KILL
  kill(valInt(p->pid), n);
#else
  ws_kill_process(p, n);
#endif

  succeed;
}


static status
stoppedProcess(Process p, Name sig)
{ DEBUG(NAME_process, Cprintf("Process %s: stopped on %s\n",
			      pp(p->name), pp(sig)));
  assign(p, status, NAME_stopped);
  assign(p, code, sig);

  succeed;
}


static status
killedProcess(Process p, Name sig)
{ DEBUG(NAME_process, Cprintf("Process %s: killed on %s\n",
			      pp(p->name), pp(sig)));
  assign(p, status, NAME_killed);
  assign(p, code, sig);
  addCodeReference(p);
  doneProcess(p);
  if ( notNil(p->terminate_message) )
    forwardReceiverCodev(p->terminate_message, p, 1, (Any *)&sig);
  delCodeReference(p);

  succeed;
}


static status
exitedProcess(Process p, Int stat)
{ DEBUG(NAME_process, Cprintf("Process %s: exited with status %s\n",
			      pp(p->name), pp(stat)));

  if ( p->status != NAME_exited )
  { addCodeReference(p);

    assign(p, status, NAME_exited);
    assign(p, code, stat);
    doneProcess(p);

#ifdef CHILD_NOPTY
    if ( stat == toInt(CHILD_NOPTY) )
    { errorPce(p, NAME_ptyError);
      closeInputProcess(p);
    } else
#endif
#ifdef CHILD_NOEXEC
    if ( stat == toInt(CHILD_NOEXEC) )
    { closeInputProcess(p);
      errorPce(p, NAME_execError, CtoName(""));
    } else
#endif
    if ( stat != ZERO )
      errorPce(p, NAME_processExitStatus, stat);

    if ( notNil(p->terminate_message) )
      forwardReceiverCodev(p->terminate_message, p, 1, (Any *)&stat);
    delCodeReference(p);
  }

  succeed;
}


static status
endOfFileProcess(Process p)
{ DEBUG(NAME_stream, Cprintf("Process %s: end of input\n", pp(p)));

  send(p, NAME_exited, ZERO, 0);

  succeed;
}


static status
useTtyProcess(Process p, Bool val)
{ if ( notNil(p->pid) )
    return errorPce(p, NAME_noChangeAfterOpen);

  assign(p, use_tty, val);

  succeed;
}


static status
directoryProcess(Process p, Directory dir)
{ if ( notNil(p->pid) )
    return errorPce(p, NAME_noChangeAfterOpen);

  assign(p, directory, dir);
  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_open[] =
        { "command=[char_array]", "argument=char_array ..." };
static char *T_initialise[] =
        { "command=char_array", "argument=char_array ..." };
static char *T_environment[] =
        { "name=name", "value=char_array" };

/* Instance Variables */

static vardecl var_process[] =
{ IV(NAME_name, "char_array", IV_GET,
     NAME_command, "Name of command executed"),
  IV(NAME_arguments, "vector", IV_GET,
     NAME_command, "Vector with arguments"),
  IV(NAME_status, "name", IV_GET,
     NAME_status, "Status of the associated process"),
  IV(NAME_code, "name|int*", IV_GET,
     NAME_status, "Signal name or exit status"),
  SV(NAME_useTty, "bool", IV_GET|IV_STORE, useTtyProcess,
     NAME_tty, "Use pseudo-tty (@on) or pipes (@off)"),
  IV(NAME_tty, "name*", IV_GET,
     NAME_tty, "Pseudo-tty used for communication"),
  IV(NAME_terminateMessage, "code*", IV_BOTH,
     NAME_input, "Forwarded when the process terminates"),
  IV(NAME_pid, "int*", IV_GET,
     NAME_status, "Process id of child process"),
  IV(NAME_directory, "[directory]", IV_GET,
     NAME_environment, "Directory to start the child"),
  IV(NAME_environment, "sheet*", IV_NONE,
     NAME_environment, "Environment for the process")
};

/* Send Methods */

static senddecl send_process[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseProcess,
     DEFAULT, "Create process from command and arguments"),
  SM(NAME_unlink, 0, NULL, unlinkProcess,
     DEFAULT, "Cleanup process"),
  SM(NAME_kill, 1, "signal=[1..31|name]", killProcess,
     NAME_control, "Send signal to the [term] process"),
  SM(NAME_open, 2, T_open, openProcess,
     NAME_control, "Start the process [with new command]"),
  SM(NAME_directory, 1, "[directory]", directoryProcess,
     NAME_environment, "Start process in this directory"),
  SM(NAME_environment, 2, T_environment, environmentProcess,
     NAME_environment, "Set environment variable"),
  SM(NAME_endOfFile, 0, NULL, endOfFileProcess,
     NAME_input, "Send when end-of-file is reached"),
  SM(NAME_exited, 1, "status=int", exitedProcess,
     NAME_input, "Process has exited with status"),
  SM(NAME_killed, 1, "signal=name", killedProcess,
     NAME_input, "Process has terminated on named signal"),
  SM(NAME_stopped, 1, "signal=name", stoppedProcess,
     NAME_input, "Process has stopped on named signal"),
  SM(NAME_close, 0, NULL, closeOutputStream,
     NAME_output, "Same as ->close_output")
};

/* Get Methods */

static getdecl get_process[] =
{ GM(NAME_environment, 0, "sheet", NULL, getEnvironmentProcess,
     NAME_environment, "Sheet with process' environment")
};

/* Resources */

#define rc_process NULL
/*
static classvardecl rc_process[] =
{ 
};
*/

/* Class Declaration */

static Name process_termnames[] = { NAME_name };

ClassDecl(process_decls,
          var_process, send_process, get_process, rc_process,
          1, process_termnames,
          "$Rev$");

status
makeClassProcess(Class class)
{ declareClass(class, &process_decls);

  ProcessChain = globalObject(NAME_runningProcesses, ClassChain, 0);

  succeed;
}


#ifdef HAVE_PTYS
#ifndef USE_GRANTPT
		/********************************
		*        PROCESS/TTY STUFF	*
		********************************/

#define TTYPA 'a'
#define TTYPZ 'z'

static int
getPseudoTTY(Process p, char *line, int *master, int *slave)
{ char c;
  struct stat stb;
  int i;
  int fd;
  int idx;

  if ( stat("/dev/ptc", &stb) == 0 )
  { int n;

    for(n=0; n<25; n++)
    { sprintf(line, "/dev/ptc/%d", n);
      if ( (fd = open(line, 2)) >= 0 )
      { chmod(line, 0622);

	if ( (*slave = getSlave(p, line)) >= 0 )
	{ *master = fd;
	  return TRUE;
	}

	close(fd);			/* slave is blocked */
      }
    }
  }

  strcpy(line, "/dev/pty");
  idx = strlen(line);

  for (c = TTYPA; c <= TTYPZ; c++)
  { line[idx] = c;
    line[idx+1] = '0';
    line[idx+2] = EOS;

    if ( stat(line, &stb) < 0 )
      continue;

    for (i = 0; i < 16; i++)
    { line[idx+1] = "0123456789abcdef"[i];
      if ( (fd = open(line, 2)) >= 0 )
      { /*chown(line, pwd->pw_uid, pwd->pw_gid);*/
	chmod(line, 0622);

	if ( (*slave = getSlave(p, line)) >= 0 )
	{ *master = fd;
	  return TRUE;
	}

	close(fd);
      }
    }
  }

  return errorPce(p, NAME_outOfPtys);
}

/*  Get the slave side of the psuedo tty.  This is where the child is
    connected to.
*/

static int
getSlave(Process p, const char *line)
{ char slave[100];

  strcpy(slave, line);

  if ( prefixstr(slave, "/dev/pty") )
    slave[strlen("/dev/")] = 't';
  else if ( prefixstr(slave, "/dev/ptc/") )
    slave[strlen("/dev/pt")] = 's';
  else
    return -1;

  /*chown(slave, pwd->pw_uid, pwd->pw_gid);*/
  chmod(slave, 0622);
  DEBUG(NAME_process, Cprintf("Opening slave %s\n", slave));
  return open(slave, 2);
}

#endif /* !USE_GRANTPT */

static status
copyTty(Process p, char *pty, int fd)
{ struct termios buf;

#ifdef COPY_TTY
  int ttyfd;
  int init = TRUE;

  if ( (ttyfd = open("/dev/tty", 0)) < 0 )
  { errorPce(p, NAME_openTty, CtoName("/dev/tty"), OsError());
    init = FALSE;
  }
  if ( init && ioctl(ttyfd, TCGETS, &buf) )
  { errorPce(p, NAME_ioctlGet, CtoName("/dev/tty"), OsError());
    init = FALSE;
  }
  
  if ( !init )
#endif
#ifndef ECHOCTL
#define ECHOCTL 0
#endif
  { buf.c_iflag = ICRNL|IXON;
    buf.c_oflag = 0;
    buf.c_cflag = CLOCAL|HUPCL|CREAD|CS8|B38400;
    buf.c_lflag = ISIG|ICANON|ECHOCTL;
#ifdef TERMIOS_HAS_C_LINE
    buf.c_line  = 0;
#endif

    buf.c_cc[VINTR]    = Control('C');
    buf.c_cc[VQUIT]    = Control('\\');
    buf.c_cc[VERASE]   = DEL;
    buf.c_cc[VKILL]    = Control('U');
    buf.c_cc[VEOF]     = Control('D');
    buf.c_cc[VEOL]     = Control('@');
    buf.c_cc[VEOL2]    = Control('@');
#ifdef VSWTCH
    buf.c_cc[VSWTCH]   = Control('@');
#endif
    buf.c_cc[VSTART]   = Control('Q');
    buf.c_cc[VSTOP]    = Control('S');
    buf.c_cc[VSUSP]    = Control('Z');
#ifdef VREPRINT
    buf.c_cc[VREPRINT] = Control('R');
#endif
#ifdef VDISCARD
    buf.c_cc[VDISCARD] = Control('O');
#endif
#ifdef VWERASE
    buf.c_cc[VWERASE]  = Control('W');
#endif
#ifdef VLNEXT
    buf.c_cc[VLNEXT]   = Control('V');
#endif
  } 

#ifndef TCSETS
#if defined(TCSETATTR)
#define TCSETS TCSETATTR
#elif defined(TCSETA)
#define TCSETS TCSETA
#endif
#endif

  if ( ioctl(fd, TCSETS, &buf) )
    return errorPce(p, NAME_ioctlSet, CtoName(pty), OsError());

#ifdef COPY_TTY
  close(ttyfd);
#endif

  succeed;
}

#endif /*HAVE_PTYS*/

