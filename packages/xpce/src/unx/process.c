/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

#if !O_NO_PROCESS

#include <h/unix.h>
#include <h/interface.h>
#include <termios.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#if !sun				/* leads to redefines */
#include <sys/ioctl.h>
#endif
#include <fcntl.h>
#include <unistd.h>

extern char **environ;

					/* this fixes RS6000/AIX problems */
#ifndef TCGETS
#include <termio.h>
#endif

#if solaris
#include <stropts.h>
#endif


static status	closeInputProcess P((Process));
static status	killProcess(Process p, Any sig);

#define OsError() getOsErrorPce(PCE)

#if !solaris
forwards int		getSlave(Process p, char *line);
forwards int		getMaster P((Process, char *));
#endif
forwards status		copyTty P((Process, char *, int));

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


#if !defined(O_WAIT_UNION) && O_SYSTEM_V
#define O_WAIT_UNION 0
#endif


static void
child_changed(int sig)
{ Process p;

#if O_WAIT_UNION
#define wait_t union wait
#else
#define wait_t int
#endif

  DEBUG(NAME_process, printf("child_changed() called\n"));

  for_chain(ProcessChain, p,
	    { int pid = valInt(p->pid);
	      wait_t status;

	      if ( waitpid(pid, &status, WNOHANG|WUNTRACED) == pid )
	      { if ( WIFSTOPPED(status) )
		  send(p, NAME_stopped, signames[WSTOPSIG(status)], 0);
	        else if ( WIFSIGNALED(status) )
		  send(p, NAME_killed, signames[WTERMSIG(status)], 0);
	        else if ( WIFEXITED(status) )
		  send(p, NAME_exited, toInt(WEXITSTATUS(status)), 0);
	      }
	    });
#if O_SIG_AUTO_RESET
  signal(sig, child_changed);
#endif
}


void
killAllProcesses(void)
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
  { hostAction(HOST_SIGNAL, SIGCHLD, child_changed);
    hostAction(HOST_ONEXIT, killAllProcesses, NULL);
    initialised++;
  }
}


static status
initialiseProcess(Process p, CharArray cmd, int argc, CharArray *argv)
{ initialiseStream((Stream)p, NIL, NIL, NIL, DEFAULT);

  assign(p, name, cmd);
  assign(p, arguments, newObjectv(ClassVector, argc, (Any *)argv));
  assign(p, status, NAME_inactive);
  assign(p, use_tty, ON);
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


static status
pidProcess(Process p, Int pid)
{ setupProcesses();

  assign(p, pid, pid);
  appendChain(ProcessChain, p);

  succeed;
}


static status
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

    environ = malloc(sizeof(char *) *
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

      DEBUG(NAME_environment, printf("env = %s\n", *env));
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


		 /*******************************
		 *	       OPEN		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Solaris stuff with many thanks to Andrew Chittenden, ADT
(asc@concurrent.co.uk)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
    { int master;
      int pid;
#if solaris
      char *line = NULL;
#else
      char line[100];
#endif

#if solaris
      if ( (master = open("/dev/ptmx", O_RDWR)) < 0 )
#else
      if ( (master = getMaster(p, line)) < 0 )
#endif
      { fprintf(stderr, "[PCE: Failed to get pseaudo tty: %s]\n",
		strName(OsError()));
	fail;
      }

      if ( (pid = fork()) == 0 )	/* child process */
      { int i, argc;
	char **argv;
	int slave;
	int maxfd = getdtablesize();

	if ( notDefault(p->directory) )
	  cdDirectory(p->directory);
	initEnvironment(p);
	DEBUG(NAME_process, fprintf(stderr, "Environment initialised\n"));
#if __linux__ || hpux || USG
	if ( setsid() < 0 )
	  fprintf(stderr, "[PCE: setsid() failed: %s]\n",
		  strName(OsError()));
#else
      { int fd;

	if ( (fd = open("/dev/tty", 2)) )
	{ ioctl(fd, TIOCNOTTY, NULL);	/* detach from controlling tty */
	  close(fd);
	}
      }
#endif

#if solaris
	if ( grantpt(master) < 0 ||
	     unlockpt(master) < 0 ||
	     (line = ptsname(master)) == NULL ||
	     (slave = open(line, O_RDWR)) < 0 ||
	     ioctl(slave, I_PUSH, "ptem") < 0 ||
	     ioctl(slave, I_PUSH, "ldterm") < 0 ||
	     ioctl(slave, I_PUSH, "ttcompat") < 0 )
	{ fprintf(stderr, "[PCE: failed to get slave pty: %s]\n",
		  strName(OsError()));
	  exit(1);
	}
#else
	if ( (slave = getSlave(p, line)) < 0 )
	{ fprintf(stderr, "[PCE: failed to open %s: %s]\n",
		  line, strName(OsError()));
	  exit(1);
	}
#endif

	DEBUG(NAME_process, fprintf(stderr, "Slave %s at %d\n", line, slave));
	if ( !copyTty(p, line, slave) )
	{ fprintf(stderr, "[PCE: failed to reset %s: %s]\n",
		  line, strName(OsError()));
	  exit(1);
	}
	DEBUG(NAME_process, fprintf(stderr, "%s initialised\n", line));

	for(i=0; i<=2; i++)		/* dup slave to stdin/stdout/stderr */
	  if ( slave != i )
	    dup2(slave, i);

	for(i=3; i < maxfd; i++)	/* close remaining open fd's */
	  close(i);

	argv = alloca(sizeof(char *) * (valInt(p->arguments->size) + 2));
	argc = valInt(p->arguments->size);

	argv[0] = strName(p->name);

	for(i=0; i<argc; i++)
	  argv[i+1] = toCharp(p->arguments->elements[i]);
	argv[i+1] = NULL;

	if ( execvp(strName(p->name), argv) )
	{ errorPce(p, NAME_cannotStartProcess, OsError());
	  exit(1);
	}
      } else				/* parent process  */
      {
#if solaris
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

	argv = alloca(sizeof(char *) * (valInt(p->arguments->size) + 2));
	argc = valInt(p->arguments->size);

	argv[0] = strName(p->name);

	for(i=0; i<argc; i++)
	  argv[i+1] = toCharp(p->arguments->elements[i]);
	argv[i+1] = NULL;

	if ( execvp(strName(p->name), argv) )
	{ errorPce(p, NAME_cannotStartProcess, OsError());
	  exit(1);
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


static status
killProcess(Process p, Any sig)
{ int n;

  if ( isNil(p->pid) )
    return errorPce(p, NAME_notOpen);

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

  kill(valInt(p->pid), n);

  succeed;
}


static status
stoppedProcess(Process p, Name sig)
{ DEBUG(NAME_process, printf("Process %s: stopped on %s\n",
			     pp(p->name), pp(sig)));
  assign(p, status, NAME_stopped);
  assign(p, code, sig);

  succeed;
}


static status
killedProcess(Process p, Name sig)
{ DEBUG(NAME_process, printf("Process %s: killed on %s\n",
			     pp(p->name), pp(sig)));
  assign(p, status, NAME_killed);
  assign(p, code, sig);
  addCodeReference(p);
  deleteChain(ProcessChain, p);
  assign(p, pid, NIL);
  if ( notNil(p->terminate_message) )
    forwardReceiverCodev(p->terminate_message, p, 1, (Any *)&sig);
  delCodeReference(p);

  succeed;
}


static status
exitedProcess(Process p, Int stat)
{ DEBUG(NAME_process, printf("Process %s: exited with status %s\n",
			     pp(p->name), pp(stat)));
  assign(p, status, NAME_exited);
  assign(p, code, stat);
  addCodeReference(p);
  deleteChain(ProcessChain, p);
  assign(p, pid, NIL);
  if ( notNil(p->terminate_message) )
    forwardReceiverCodev(p->terminate_message, p, 1, (Any *)&stat);
  delCodeReference(p);
  if ( stat != ZERO )
    errorPce(p, NAME_processExitStatus, stat);

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


status
makeClassProcess(Class class)
{ sourceClass(class, makeClassProcess, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_command, "char_array", NAME_get,
	     "Name of command executed");
  localClass(class, NAME_arguments, NAME_command, "vector", NAME_get,
	     "Vector with arguments");
  localClass(class, NAME_status, NAME_status, "name", NAME_get,
	     "Status of the associated process");
  localClass(class, NAME_code, NAME_status, "name|int*", NAME_get,
	     "Signal name or exit status");
  localClass(class, NAME_useTty, NAME_tty, "bool", NAME_get,
	     "Use pseudo-tty (@on) or pipes (@off)");
  localClass(class, NAME_tty, NAME_tty, "name*", NAME_get,
	     "Pseudo-tty used for communication");
  localClass(class, NAME_terminateMessage, NAME_input, "code*", NAME_both,
	     "Forwarded when the process terminates");
  localClass(class, NAME_pid, NAME_status, "int*", NAME_get,
	     "Process id of child process");
  localClass(class, NAME_directory, NAME_environment, "[directory]", NAME_get,
	     "Directory to start the child");
  localClass(class, NAME_environment, NAME_environment, "sheet*", NAME_none,
	     "Environment for the process");

  termClass(class, "process", 1, NAME_name);

  storeMethod(class, NAME_useTty, useTtyProcess);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "command=char_array", "argument=char_array ...",
	     "Create process from command and arguments",
	     initialiseProcess);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Cleanup process",
	     unlinkProcess);
  sendMethod(class, NAME_open, NAME_control, 2,
	     "command=[char_array]", "argument=char_array ...",
	     "Start the process [with new command]",
	     openProcess);
  sendMethod(class, NAME_kill, NAME_control, 1, "signal=[1..31|name]",
	     "Send signal to the [term] process",
	     killProcess);
  sendMethod(class, NAME_close, NAME_output, 0,
	     "Same as ->close_output",
	     closeOutputStream);
  sendMethod(class, NAME_stopped, NAME_input, 1, "signal=name",
	     "Process has stopped on named signal",
	     stoppedProcess);
  sendMethod(class, NAME_killed, NAME_input, 1, "signal=name",
	     "Process has terminated on named signal",
	     killedProcess);
  sendMethod(class, NAME_exited, NAME_input, 1, "status=int",
	     "Process has exited with status",
	     exitedProcess);
  sendMethod(class, NAME_directory, NAME_environment, 1, "[directory]",
	     "Start process in this directory",
	     directoryProcess);
  sendMethod(class, NAME_environment, NAME_environment, 2,
	     "name=name", "value=char_array",
	     "Set environment variable",
	     environmentProcess);

  getMethod(class, NAME_environment, NAME_environment, "sheet", 0,
	    "Sheet with process' environment",
	    getEnvironmentProcess);

  ProcessChain = globalObject(NAME_runningProcesses, ClassChain, 0);

  succeed;
}

#if !solaris
		/********************************
		*        PROCESS/TTY STUFF	*
		********************************/

static int
getMaster(Process p, char *line)
{ char c;
  struct stat stb;
  int i;
  int fd;
  int idx = strlen("/dev/pty");

  strcpy(line, "/dev/pty");
  

  for (c = 'p'; c <= 's'; c++)
  { line[idx] = c;
    line[idx+1] = '0';
    line[idx+2] = EOS;

    if ( stat(line, &stb) < 0 )
      break;

    for (i = 0; i < 16; i++)
    { line[idx+1] = "0123456789abcdef"[i];
      if ( (fd = open(line, 2)) >= 0 )
      { /*chown(line, pwd->pw_uid, pwd->pw_gid);*/
	chmod(line, 0622);

	return fd;
      }
    }
  }

  if ( stat("/dev/ptc", &stb) == 0 )
  { int n;

    for(n=0; n<25; n++)
    { sprintf(line, "/dev/ptc/%d", n);
      if ( (fd = open(line, 2)) >= 0 )
      { chmod(line, 0622);

	return fd;
      }
    }
  }


  errorPce(p, NAME_outOfPtys);
  return -1;
}

/*  Get the slave side of the psuedo tty.  This is where the child is
    connected to.
*/

static int
getSlave(Process p, char *line)
{ if ( prefixstr(line, "/dev/pty") )
    line[strlen("/dev/")] = 't';
  else if ( prefixstr(line, "/dev/ptc/") )
    line[strlen("/dev/pt")] = 's';
  else
    return -1;
  /*chown(line, pwd->pw_uid, pwd->pw_gid);*/
  chmod(line, 0622);
  DEBUG(NAME_process, printf("Opening slave %s\n", line));
  return open(line, 2);
}

#endif /* !solaris */

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
#if !_AIX && !hpux && !solaris
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

#else /*O_NO_PROCESS*/

void
killAllProcesses(void)
{
}

status
makeClassProcess(Class class)
{ sourceClass(class, makeClassProcess, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_command, "char_array", NAME_get,
	     "Name of command executed");
  localClass(class, NAME_arguments, NAME_command, "vector", NAME_get,
	     "Vector with arguments");
  localClass(class, NAME_status, NAME_status, "name", NAME_get,
	     "Status of the associated process");
  localClass(class, NAME_code, NAME_status, "name|int*", NAME_get,
	     "Signal name or exit status");
  localClass(class, NAME_useTty, NAME_tty, "bool", NAME_get,
	     "Use pseudo-tty (@on) or pipes (@off)");
  localClass(class, NAME_tty, NAME_tty, "name*", NAME_get,
	     "Pseudo-tty used for communication");
  localClass(class, NAME_terminateMessage, NAME_input, "code*", NAME_both,
	     "Forwarded when the process terminates");
  localClass(class, NAME_pid, NAME_status, "int*", NAME_get,
	     "Process id of child process");
  localClass(class, NAME_directory, NAME_environment, "[directory]", NAME_get,
	     "Directory to start the child");
  localClass(class, NAME_environment, NAME_environment, "sheet*", NAME_none,
	     "Environment for the process");

  termClass(class, "process", 1, NAME_name);

  succeed;
}

#endif
