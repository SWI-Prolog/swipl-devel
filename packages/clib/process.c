/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Stream.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>   
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <assert.h>
#include "clib.h"
#include <signal.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unix process management.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static IOSTREAM *
name_to_stream(const char *name)
{ IOSTREAM *s;
  term_t t = PL_new_term_ref();

  PL_put_atom_chars(t, name);
  if ( PL_get_stream_handle(t, &s) )
    return s;

  return NULL;
}


static void
flush_stream(const char *name)
{ IOSTREAM *s;

  if ( (s = name_to_stream(name)) )
    Sflush(s);

  PL_release_stream(s);
}



static foreign_t
pl_fork(term_t a0)
{ pid_t pid;

  flush_stream("user_output");		/* general call to flush all IO? */

  if ( (pid = fork()) < 0 )
    return PL_warning("fork/1: failed: %s", strerror(errno));

  if ( pid > 0 )
    return PL_unify_integer(a0, pid);
  else
    return PL_unify_atom_chars(a0, "child");
}


static foreign_t
pl_exec(term_t cmd)
{ int argc;
  atom_t name;

  if ( PL_get_name_arity(cmd, &name, &argc) )
  { term_t a = PL_new_term_ref();
    char **argv = malloc(sizeof(char*) * (argc + 2));
    int i;

    argv[0] = (char *)PL_atom_chars(name);

    for(i=1; i<=argc; i++)
    { char *s;

      if ( PL_get_arg(i, cmd, a) &&
	   PL_get_chars(a, &s, CVT_ALL|BUF_MALLOC) )
	argv[i] = s;
      else
	return pl_error("exec", 1, NULL, ERR_ARGTYPE, i, a, "atomic");
    }
    argv[argc+1] = NULL;

    execvp(argv[0], argv);
    return pl_error("exec", 1, NULL, ERR_ERRNO, errno, argv[0], "execute");
  }
  
  return pl_error("exec", 1, NULL, ERR_ARGTYPE, 1, cmd, "compound");
}


static foreign_t
pl_wait(term_t Pid, term_t Status)
{ int status;
  pid_t pid = wait(&status);

  if ( pid == -1 )
    return pl_error("wait", 2, NULL, ERR_ERRNO, errno);

  if ( PL_unify_integer(Pid, pid) )
  { if ( WIFEXITED(status) )
      return PL_unify_term(Status,
			   CompoundArg("exited", 1),
			   IntArg(WEXITSTATUS(status)));
    if ( WIFSIGNALED(status) )
      return PL_unify_term(Status,
			   CompoundArg("signaled", 1),
			   IntArg(WTERMSIG(status)));
    if ( WIFSTOPPED(status) )
      return PL_unify_term(Status,
			   CompoundArg("stopped", 1),
			   IntArg(WSTOPSIG(status)));
    assert(0);
  }

  return FALSE;
}


static foreign_t
pl_kill(term_t Pid, term_t Sig)
{ int pid;
  int sig;

  if ( !PL_get_integer(Pid, &pid) )
    return pl_error("kill", 2, NULL, ERR_ARGTYPE, 1, Pid, "pid");
  if ( !PL_get_integer(Sig, &sig) )
    return pl_error("kill", 2, NULL, ERR_ARGTYPE, 2, Sig, "signal");

  if ( kill(pid, sig) < 0 )
    return pl_error("kill", 1, NULL, ERR_ERRNO, Pid);

  return TRUE;
}


		 /*******************************
		 *	   STREAM STUFF		*
		 *******************************/

static foreign_t
pl_pipe(term_t Read, term_t Write)
{ int fd[2];
  IOSTREAM *in, *out;

  if ( pipe(fd) != 0 )
    return pl_error("pipe", 2, NULL, ERR_ERRNO, errno, "");

  in  = Sfdopen(fd[0], "r");
  out = Sfdopen(fd[1], "w");

  if ( PL_open_stream(Read, in) &&
       PL_open_stream(Write, out) )
    return TRUE;

  return FALSE;
}


static int
get_stream_no(term_t t, IOSTREAM **s, int *fn)
{ if ( PL_get_integer(t, fn) )
    return TRUE;
  if ( PL_get_stream_handle(t, s) )
  { *fn = Sfileno(*s);
    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_dup(term_t from, term_t to)
{ IOSTREAM *f = NULL, *t = NULL;
  int rval = FALSE;
  int fn, tn;

  if ( !get_stream_no(from, &f, &fn) ||
       !get_stream_no(to, &t, &tn) )
    goto out;
  
  if ( dup2(fn, tn) < 0 )
  { pl_error("dup", 2, NULL, ERR_ERRNO, errno, "");
    goto out;
  } else
  { rval = TRUE;
  }

out:
  if ( f )
    PL_release_stream(f);
  if ( t )
    PL_release_stream(t);

  return rval;
}


static foreign_t
pl_environ(term_t l)
{ extern char **environ;
  char **e;
  term_t t = PL_copy_term_ref(l);
  term_t t2 = PL_new_term_ref();
  term_t nt = PL_new_term_ref();
  term_t vt = PL_new_term_ref();
  functor_t FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);

  for(e = environ; *e; e++)
  { char *s = strchr(*e, '=');
    
    if ( !s )
      s = *e + strlen(*e);

    { int len = s-*e;
      char *name = alloca(len+1);

      strncpy(name, *e, len);
      name[len] = '\0';
      PL_put_atom_chars(nt, name);
      PL_put_atom_chars(vt, s+1);
      PL_cons_functor(nt, FUNCTOR_equal2, nt, vt);
      if ( !PL_unify_list(t, t2, t) ||
	   !PL_unify(t2, nt) )
	return FALSE;
    }
  }  
  
  return PL_unify_nil(t);
} 


		 /*******************************
		 *	    DEAMON IO		*
		 *******************************/

static atom_t error_file;		/* file for output */
static int    error_fd;			/* and its fd */

static int
read_eof(void *handle, char *buf, int count)
{ return 0;
}


static int
write_null(void *handle, char *buf, int count)
{ if ( error_fd )
  { if ( error_fd >= 0 )
      write(error_fd, buf, count);
  } else if ( error_file )
  { error_fd = open(PL_atom_chars(error_file), O_WRONLY|O_CREAT|O_TRUNC, 0644);
    return write_null(handle, buf, count);
  } 

  return count;
}


static long
seek_error(void *handle, long pos, int whence)
{ return -1;
}


static int
close_null(void *handle)
{ return 0;
}


static IOFUNCTIONS dummy =
{ read_eof,
  write_null,
  seek_error,
  close_null,
  NULL
};


static void
close_underlying_fd(IOSTREAM *s)
{ if ( s )
  { int fd;
  
    if ( (fd = Sfileno(s)) >= 0 )
      close(fd);

    s->functions = &dummy;
    s->flags &= ~SIO_FILE;		/* no longer a file */
    s->flags |= SIO_LBUF;		/* do line-buffering */
  }
}


static foreign_t
pl_detach_IO()
{ char buf[100];

  sprintf(buf, "/tmp/pl-out.%d", (int)getpid());
  error_file = PL_new_atom(buf);

  close_underlying_fd(Serror);
  close_underlying_fd(Soutput);
  close_underlying_fd(Sinput);
  close_underlying_fd(name_to_stream("user_input"));
  close_underlying_fd(name_to_stream("user_output"));
  close_underlying_fd(name_to_stream("user_error"));

#ifdef HAVE_SETSID
  setsid();
#else
{ int fd;

  if ( (fd = open("/dev/tty", 2)) )
  { ioctl(fd, TIOCNOTTY, NULL);		/* detach from controlling tty */
    close(fd);
  }
}
#endif

  return TRUE;
}


install_t
install_unix()
{ PL_register_foreign("fork",      1, pl_fork, 0);
  PL_register_foreign("exec",      1, pl_exec, 0);
  PL_register_foreign("wait",      2, pl_wait, 0);
  PL_register_foreign("kill",      2, pl_kill, 0);
  PL_register_foreign("pipe",      2, pl_pipe, 0);
  PL_register_foreign("dup",       2, pl_dup, 0);
  PL_register_foreign("detach_IO", 0, pl_detach_IO, 0);
  PL_register_foreign("environ",   1, pl_environ, 0);
}







