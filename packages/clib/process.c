/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "error.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>

static atom_t ATOM_stdin;
static atom_t ATOM_stdout;
static atom_t ATOM_stderr;
static atom_t ATOM_std;
static atom_t ATOM_null;
static atom_t ATOM_process;
static atom_t ATOM_detached;
static atom_t ATOM_cwd;
static atom_t ATOM_window;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_resource_error1;
static functor_t FUNCTOR_process_error2;
static functor_t FUNCTOR_pipe1;
static functor_t FUNCTOR_exit1;
static functor_t FUNCTOR_killed1;

#define DEBUG(g) (void)0

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ISSUES:
	- Deal with child errors (no cwd, cannot execute, etc.)
	- Windows version
	- Complete test suite
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static int
type_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_type_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
domain_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_domain_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
resource_error(const char *resource)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_resource_error1,
		        PL_CHARS, resource,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


		 /*******************************
		 *	       ADMIN		*
		 *******************************/

typedef enum std_type
{ std_std,
  std_null,
  std_pipe
} std_type;


typedef struct p_stream
{ term_t term;				/* P in pipe(P) */
  std_type type;			/* type of stream */
  int    fd[2];				/* pipe handles */
} p_stream;


typedef struct p_options
{ atom_t exe_name;			/* exe as atom */
  char *exe;				/* Executable */
  char **argv;				/* argument vector */
  term_t pid;				/* process(PID) */
  int pipes;				/* #pipes found */
  p_stream streams[3];
  char *cwd;				/* CWD of new process */
  int   detached;			/* create as detached */
} p_options;


static int
get_stream(term_t t, p_options *info, p_stream *stream)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  { if ( a == ATOM_null )
    { stream->type = std_null;
      return TRUE;
    } else if ( a == ATOM_std )
    { stream->type = std_std;
      return TRUE;
    } else
    { return domain_error(t, "process_stream");
    }
  } else if ( PL_is_functor(t, FUNCTOR_pipe1) )
  { stream->term = PL_new_term_ref();
    PL_get_arg(1, t, stream->term);
    stream->type = std_pipe;
    info->pipes++;
    return TRUE;
  } else
    return type_error(t, "process_stream");
}


static int
parse_options(term_t options, p_options *info)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg = PL_new_term_ref();

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return type_error(head, "option");
    PL_get_arg(1, head, arg);

    if ( name == ATOM_stdin )
    { if ( !get_stream(arg, info, &info->streams[0]) )
	return FALSE;
    } else if ( name == ATOM_stdout )
    { if ( !get_stream(arg, info, &info->streams[1]) )
	return FALSE;
    } else if ( name == ATOM_stderr )
    { if ( !get_stream(arg, info, &info->streams[2]) )
	return FALSE;
    } else if ( name == ATOM_process )
    { info->pid = PL_copy_term_ref(arg);
    } else if ( name == ATOM_detached )
    { if ( !PL_get_bool(arg, &info->detached) )
	return type_error(arg, "boolean");
    } else if ( name == ATOM_cwd )
    { if ( !PL_get_chars(arg, &info->cwd,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
	return FALSE;
    } else if ( name == ATOM_window )
    { Sdprintf("Window: TBD\n");
    } else
      return domain_error(head, "process_option");
  }

  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  return TRUE;
}


static int
get_exe(term_t exe, p_options *info)
{ int i, arity;
  term_t arg = PL_new_term_ref();

  if ( !PL_get_name_arity(exe, &info->exe_name, &arity) )
    return type_error(exe, "callable");

  PL_put_atom(arg, info->exe_name);
  if ( !PL_get_chars(arg, &info->exe, CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
    return FALSE;

  info->argv = PL_malloc((arity+2)*sizeof(char*));
  memset(info->argv, 0, (arity+2)*sizeof(char*));
  info->argv[0] = strdup(info->exe);
  for(i=1; i<=arity; i++)
  { PL_get_arg(i, exe, arg);

    if ( !PL_get_chars(arg, &info->argv[i],
		       CVT_ATOMIC|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
      return FALSE;
  }
  info->argv[i] = NULL;

  return TRUE;
}


static void
free_options(p_options *info)		/* TBD: close streams */
{ if ( info->exe )
  { PL_free(info->exe);
    info->exe = NULL;
  }

  if ( info->argv )
  { char **a;
    for(a=info->argv; *a; a++)
    { if ( *a )
	PL_free(*a);
    }
    PL_free(info->argv);

    info->argv = NULL;
  }
}


		 /*******************************
		 *	   PROCESS READS	*
		 *******************************/

#define	PROCESS_MAGIC	0x29498001

typedef struct process_context
{ int	magic;				/* PROCESS_MAGIC */
  pid_t	pid;				/* the process id */
  int   open_mask;			/* Open streams */
  int	pipes[3];			/* stdin/stdout/stderr */
} process_context;


static int
process_fd(void *handle, process_context **PC)
{ process_context *pc = (process_context*) ((uintptr_t)handle&~(uintptr_t)0x3);
  int pipe = (int)(uintptr_t)handle & 0x3;

  if ( pc->magic == PROCESS_MAGIC )
  { if ( PC )
      *PC = pc;
    return pc->pipes[pipe];
  }

  return -1;
}


static ssize_t
Sread_process(void *handle, char *buf, size_t size)
{ int fd = process_fd(handle, NULL);

  return (*Sfilefunctions.read)((void*)(uintptr_t)fd, buf, size);
}


static ssize_t
Swrite_process(void *handle, char *buf, size_t size)
{ int fd = process_fd(handle, NULL);

  return (*Sfilefunctions.write)((void*)(uintptr_t)fd, buf, size);
}


static int
Sclose_process(void *handle)
{ process_context *pc;
  int fd = process_fd(handle, &pc);

  if ( fd >= 0 )
  { int which = (int)(uintptr_t)handle & 0x3;
    int rc;

    rc = (*Sfilefunctions.close)((void*)(uintptr_t)fd);
    pc->open_mask &= ~which;
    
    DEBUG(Sdprintf("Closing fd[%d]; mask = 0x%x\n", which, pc->open_mask));

    if ( !pc->open_mask )
    { pid_t p2;
      
      for(;;)
      { int status;

	if ( (p2=waitpid(pc->pid, &status, 0)) == pc->pid )
	  break;
	if (  p2 == -1 && errno == EINTR )
	{ if ( PL_handle_signals() < 0 )
	  { PL_free(pc);
	    return -1;
	  }
	}
      }
      
      PL_free(pc);
    }    

    return rc;
  }

  return -1;
}


static int
Scontrol_process(void *handle, int action, void *arg)
{ process_context *pc;
  int fd = process_fd(handle, &pc);

  switch(action)
  { case SIO_GETFILENO:
    { int *fdp = arg;
      *fdp = fd;
      return 0;
    }
    default:
      return (*Sfilefunctions.control)((void*)(uintptr_t)fd, action, arg);
  }
}


static IOFUNCTIONS Sprocessfunctions =
{ Sread_process,
  Swrite_process,
  NULL,					/* seek */
  Sclose_process,
  Scontrol_process,
  NULL					/* seek64 */
};


static IOSTREAM *
open_process_pipe(process_context *pc, int which, int fd)
{ void *handle;
  int flags;

  pc->open_mask |= which;
  pc->pipes[which] = fd;
  
  if ( which == 0 )
    flags = SIO_OUTPUT|SIO_RECORDPOS;
  else
    flags = SIO_INPUT|SIO_RECORDPOS;

  handle = (void *)((uintptr_t)pc | (uintptr_t)which);

  return Snew(handle, flags, &Sprocessfunctions);
}


		 /*******************************
		 *	       OS STUFF		*
		 *******************************/


static int
create_pipes(p_options *info)
{ int i;

  for(i=0; i<3; i++)
  { p_stream *s = &info->streams[i];

    if ( s->term )
    { if ( pipe(s->fd) )
      { assert(errno = EMFILE);
	return resource_error("open_files");
      }
    }
  }

  return TRUE;
}


static int
unify_exit_status(term_t code, int status)
{ if ( WIFEXITED(status) )
  { return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_exit1,
			   PL_INT, (int)WEXITSTATUS(status));
  } else if ( WIFSIGNALED(status) )
  { return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_killed1,
			   PL_INT, (int)WTERMSIG(status));
  } else
  { assert(0);
    return FALSE;
  }
}


static int
wait_success(atom_t name, pid_t pid)
{ pid_t p2;

  for(;;)
  { int status;

    if ( (p2=waitpid(pid, &status, 0)) == pid )
    { if ( WIFEXITED(status) && WEXITSTATUS(status) == 0 )
      { return TRUE;
      } else
      { term_t code = PL_new_term_ref();
	term_t ex = PL_new_term_ref();

	unify_exit_status(code, status);
	PL_unify_term(ex,
		      PL_FUNCTOR, FUNCTOR_error2,
		        PL_FUNCTOR, FUNCTOR_process_error2,
		          PL_ATOM, name,
		          PL_TERM, code,
		        PL_VARIABLE);
	return PL_raise_exception(ex);
      }
    } 

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    }
  }
}


static int
do_create_process(p_options *info)
{ int pid;

  if ( !(pid=fork()) )			/* child */
  { int fd;

    PL_cleanup_fork();

    if ( info->cwd )
    { if ( chdir(info->cwd) )
      { perror(info->cwd);
	exit(1);
      }
    }
    
					/* stdin */
    switch( info->streams[0].type )
    { case std_pipe:
	dup2(info->streams[0].fd[0], 0);
	close(info->streams[0].fd[1]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_RDONLY)) >= 0 )
	  dup2(fd, 0);
        break;
      case std_std:
	break;
    }
					/* stdout */
    switch( info->streams[1].type )
    { case std_pipe:
	dup2(info->streams[1].fd[1], 1);
        close(info->streams[1].fd[0]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_WRONLY)) >= 0 )
	  dup2(fd, 1);
        break;
      case std_std:
	break;
    }
					/* stderr */
    switch( info->streams[2].type )
    { case std_pipe:
	dup2(info->streams[2].fd[1], 2);
        close(info->streams[2].fd[0]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_WRONLY)) >= 0 )
	  dup2(fd, 2);
        break;
      case std_std:
	break;
    }

    if ( execv(info->exe, info->argv) )
    { perror(info->exe);
      exit(1);
    }

    return pl_error(NULL, 0, "execv", ERR_ERRNO, errno);
  } else				/* parent */
  { if ( info->pipes > 0 && info->pid == 0 )
    { IOSTREAM *s;
      process_context *pc = PL_malloc(sizeof(*pc));

      DEBUG(Sdprintf("Wait on pipes\n"));

      memset(pc, 0, sizeof(*pc));
      pc->magic = PROCESS_MAGIC;
      pc->pid = pid;

      if ( info->streams[0].type == std_pipe )
      { close(info->streams[0].fd[0]);
	s = open_process_pipe(pc, 0, info->streams[0].fd[1]);
	PL_unify_stream(info->streams[0].term, s);
      }
      if ( info->streams[1].type == std_pipe )
      { close(info->streams[1].fd[1]);
	s = open_process_pipe(pc, 1, info->streams[1].fd[0]);
	PL_unify_stream(info->streams[1].term, s);
      }
      if ( info->streams[2].type == std_pipe )
      { close(info->streams[2].fd[1]);
	s = open_process_pipe(pc, 2, info->streams[2].fd[0]);
	PL_unify_stream(info->streams[2].term, s);
      }

      return TRUE;
    } else if ( info->pipes > 0 )
    { IOSTREAM *s;

      if ( info->streams[0].type == std_pipe )
      { close(info->streams[0].fd[0]);
	s = Sfdopen(info->streams[0].fd[1], "w");
	PL_unify_stream(info->streams[0].term, s);
      }
      if ( info->streams[1].type == std_pipe )
      { close(info->streams[1].fd[1]);
	s = Sfdopen(info->streams[1].fd[0], "r");
	PL_unify_stream(info->streams[1].term, s);
      }
      if ( info->streams[2].type == std_pipe )
      { close(info->streams[2].fd[1]);
	s = Sfdopen(info->streams[2].fd[0], "r");
	PL_unify_stream(info->streams[2].term, s);
      }
    }

    if ( info->pid )
      return PL_unify_integer(info->pid, pid);
    
    return wait_success(info->exe_name, pid);
  }
}



		 /*******************************
		 *	      BINDING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Basic process creation interface takes

	* Exe file
	* List of arguments
	* standard streams		% std, null, pipe(S)
	* Working directory
	* detached			% Unix
	* window			% Windows
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
process_create(term_t exe, term_t options)
{ p_options info;
  int rc = FALSE;

  memset(&info, 0, sizeof(info));

  if ( !get_exe(exe, &info) )
    goto out;
  if ( !parse_options(options, &info) )
    goto out;
  if ( !create_pipes(&info) )
    goto out;

  rc = do_create_process(&info);

out:
  free_options(&info);

  return rc;
}


static int
get_pid(term_t pid, pid_t *p)
{ int n;

  if ( !PL_get_integer(pid, &n) )
    return type_error(pid, "integer");
  if ( n < 0 )
    return domain_error(pid, "not_less_than_zero");
  
  *p = n;
  return TRUE;
}


static foreign_t
process_wait(term_t pid, term_t code, term_t options)
{ int p, p2;
  int status;

  if ( !get_pid(pid, &p) )
    return FALSE;

  for(;;)
  { if ( (p2=waitpid(p, &status, 0)) == p )
      return unify_exit_status(code, status);

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    } else
    { return pl_error(NULL, 0, "waitpid", ERR_ERRNO, errno);
    }
  }
}


static foreign_t
process_kill(term_t pid, term_t signal)
{ int p, sig;

  if ( !get_pid(pid, &p) )
    return FALSE;
  if ( !PL_get_signum_ex(signal, &sig) )
    return FALSE;

  if ( kill(p, sig) == 0 )
    return TRUE;
  
  switch(errno)
  { case EPERM:
      return pl_error("process_kill", 2, NULL, ERR_PERMISSION,
		      pid, "kill", "process");
    case ESRCH:
      return pl_error("process_kill", 2, NULL, ERR_EXISTENCE,
		      "process", pid);
    default:
      return pl_error("process_kill", 2, "kill", ERR_ERRNO, errno);
  }
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_process()
{ MKATOM(stdin);
  MKATOM(stdout);
  MKATOM(stderr);
  MKATOM(std);
  MKATOM(null);
  MKATOM(process);
  MKATOM(detached);
  MKATOM(cwd);
  MKATOM(window);

  MKFUNCTOR(pipe, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(domain_error, 2);
  MKFUNCTOR(process_error, 2);
  MKFUNCTOR(resource_error, 1);
  MKFUNCTOR(exit, 1);
  MKFUNCTOR(killed, 1);

  PL_register_foreign("process_create", 2, process_create, 0);
  PL_register_foreign("process_wait", 3, process_wait, 0);
  PL_register_foreign("process_kill", 2, process_kill, 0);
}
