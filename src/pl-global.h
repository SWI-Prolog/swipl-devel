/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

#ifndef PL_GLOBAL_H_INCLUDED
#define PL_GLOBAL_H_INCLUDED

#ifndef ulong
#define ulong unsigned long
#endif

#ifndef GLOBAL			/* global variables */
#define GLOBAL extern
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module packs SWI-Prolog global data-structures into two structures.
The structure PL_global_data contains all global data that is related to
the state of the system as a  whole, and PL_local_data contains all data
that is related to a Prolog thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	     CODE DATA		*
		 *******************************/

typedef struct
{
#if VMCODE_IS_ADDRESS
  char   *_dewam_table;			/* decoding table */
  long	  _dewam_table_offset;		/* offset of 1st */
  code    _wam_table[I_HIGHEST+1];	/* code --> address */
  void  **_interpreter_jmp_table;	/* interpreters table */
#else
  int	  struct_may_not_be_empty;	/* empty structure is illegal */
#endif
} PL_code_data_t;

typedef struct
{ atom_t	file;			/* current source file */
  int	  	line;			/* current line */
  long		character;		/* current character location */
} source_location;

		 /*******************************
		 *	    GLOBAL DATA		*
		 *******************************/

typedef struct
{ char *top_of_heap;			/* highest allocated heap address */
  char *base_of_heap;			/* lowest allocated heap address */
  ulong rounded_heap_base;		/* heap-base rounded downwards */
  pl_defaults_t	    defaults;		/* system default settings */
  pl_options_t	    options;		/* command-line options */
  State		stateList;		/* list of loaded states */
  int		initialised;		/* Heap is initialised */
  int		io_initialised;		/* I/O system has been initialised */
  cleanup_status cleaning;		/* Inside PL_cleanup() */
  int		bootsession;		/* -b boot compilation */
  int		debug_level;		/* Maintenance debugging: 0..9 */
  void *	resourceDB;		/* program resource database */

#ifdef HAVE_SIGNAL
  sig_handler sig_handlers[MAXSIGNAL];	/* How Prolog preceives signals */
#endif  
#ifdef O_LOGICAL_UPDATE
  unsigned long generation;		/* generation of the database */
#endif

  struct
  { int		argc;			/* main(int argc, char **argv) */
    char **	argv;
    int		_c_argc;		/* stripped options */
    char **	_c_argv;
    int		notty;			/* -tty: donot use ioctl() */
    int		optimise;		/* -O: optimised compilation */
  } cmdline;

  struct
  { char *	executable;		/* running executable */
  } paths;

  struct
  { long	heap;			/* heap in use */
    int		atoms;			/* No. of atoms defined */
    long	atomspace;		/* # bytes used to store atoms */
#ifdef O_ATOMGC
    long	atomspacefreed;		/* Freed atom-space */
#endif
    int		functors;		/* No. of functors defined */
    int		predicates;		/* No. of predicates defined */
    int		modules;		/* No. of modules in the system */
    long	codes;			/* No. of byte codes generated */
#ifdef O_PLMT
    int		threads_created;	/* # threads created */
    int		threads_finished;	/* # finished threads */
    real	thread_cputime;		/* Total CPU time of threads */
#endif
  } statistics;

  struct
  { Module	user;			/* user module */
    Module	system;			/* system predicate module */
  } modules;

  struct
  { Table	modules;		/* atom --> module */
    Table	record_lists;		/* Available record lists */
  } tables;

  struct
  { buffer	functions;		/* index --> function */
    ArithFunction table[ARITHHASHSIZE];	/* functor --> function table */
  } arith;

  struct
  { buffer	array;			/* index --> atom */
    int		buckets;		/* # buckets in char * --> atom */
    Atom *	table;			/* hash-table */
    int		lookups;		/* # atom lookups */
    int		cmps;			/* # string compares for lookup */
#ifdef O_ATOMGC
    int		gc_active;		/* Atom-GC is in progress */
    int		builtin;		/* Locked atoms (atom-gc) */
    int		no_hole_before;		/* You won't find a hole before here */
    int		margin;			/* # atoms to grow before collect */
    int		non_garbage;		/* # atoms for after last AGC */
    int		gc;			/* # atom garbage collections */
    long	collected;		/* # collected atoms */
    real	gc_time;		/* Time spent on atom-gc */
    PL_agc_hook_t gc_hook;		/* Current hook */
#endif
    atom_t	for_code[256];		/* code --> one-char-atom */
  } atoms;

  struct
  { Table	breakpoints;		/* Breakpoint table */
  } comp;

  struct
  { ExtensionCell _ext_head;		/* head of registered extensions */
    ExtensionCell _ext_tail;		/* tail of this chain */

    InitialiseHandle initialise_head;	/* PL_initialise_hook() */
    InitialiseHandle initialise_tail;

    int		  _loaded;		/* system extensions are loaded */
  } foreign;

  struct				/* pl-format.c */
  { Table	predicates;
  } format;

  struct
  { Table	table;			/* flag key --> flag */
  } flags;

  struct 
  { Table	table;			/* global (read-only) features */
  } feature;

  struct
  { buffer	array;			/* index --> functor */
    int		buckets;		/* # buckets in atom --> functor */
    FunctorDef* table;			/* hash-table */
  } functors;

  struct
  { TempFile		_tmpfile_head;
    TempFile		_tmpfile_tail;
    CanonicalDir	_canonical_dirlist;
    char *		myhome;		/* expansion of ~ */
    char *		fred;		/* last expanded ~user */
    char *		fredshome;	/* home of fred */
    OnHalt		on_halt_list;	/* list of onhalt hooks */
    int			halting;	/* process is shutting down */
    int			gui_app;	/* Win32: Application is a gui app */
    IOFUNCTIONS		iofunctions;	/* initial IO functions */
    IOFUNCTIONS 	org_terminal;	/* IO+Prolog terminal functions */
    IOFUNCTIONS		rl_functions;	/* IO+Terminal+Readline functions */
  } os;

  struct
  { Procedure	garbage_collect0;
    Procedure 	block3;
    Procedure	catch3;
    Procedure	true0;
    Procedure	fail0;
    Procedure	event_hook1;
    Procedure	print_message2;
    Procedure	foreign_registered2;	/* $foreign_registered/2 */
    Procedure	prolog_trace_interception4;
    Procedure	portray;		/* portray/1 */
    Procedure   dcall1;			/* $call/1 */
    Procedure	call_cleanup3;		/* call_cleanup/2 */

    SourceFile  reloading;		/* source file we are re-loading */
    int		active_marked;		/* #prodedures marked active */
    int		static_dirty;		/* #static dirty procedures */

#ifdef O_CLAUSEGC
    DefinitionChain dirty;		/* List of dirty static procedures */
#endif
  } procedures;

  struct
  { buffer	source_files;		/* index --> file */
    int		_source_index;		/* current index */
    Table	_source_table;		/* file --> index */
  } files;

#ifdef HAVE_TGETENT
  struct
  { int    initialised;			/* initialisation status */
    char  *_string_area;		/* static area for tgetent */
    char  *_buf_area;			/* another one */
    Table  _capabilities;		/* User-level capability table */
  } terminal;
#endif

  struct alloc_pool alloc_pool;		/* Main allocation pool */
#ifdef O_PLMT
  FreeChunk	    left_over_pool;	/* Left-over from threads */

  struct
  { struct _at_exit_goal *exit_goals;	/* Global thread_at_exit/1 goals */
    int		    enabled;		/* threads are enabled */
#ifdef WIN32
    HINSTANCE	    instance;		/* Win32 process instance */
#endif
  } thread;
#endif
} PL_global_data_t;


		 /*******************************
		 *	     LOCAL DATA		*
		 *******************************/

typedef struct PL_local_data
{ LocalFrame    environment;		/* Current local frame */
  Choice	choicepoints;		/* Choice-point chain */
  FliFrame      foreign_environment;	/* Current foreign context */
  Word		mark_bar;		/* Mark globals > this one */
  pl_stacks_t   stacks;			/* Prolog runtime stacks */
  ulong		bases[STG_MASK+1];	/* area base addresses */
  ulong		pending_signals;	/* PL_raise() pending signals */
  record_t	pending_exception;	/* Pending exception from signal */
  int		current_signal;		/* Currently handled signal */
  int		critical;		/* heap is being modified */
  int		aborted;		/* thread asked for abort */
  Stack		outofstack;		/* thread is out of stack */
  int		trim_stack_requested;	/* perform a trim-stack */
#ifdef O_PLMT
  int		exit_requested;		/* Thread is asked to exit */
#endif
  int		in_arithmetic;		/* doing arithmetic */
  int		autoload_nesting;	/* Nesting level in autoloader */
  void *	glob_info;		/* pl-glob.c */

  struct
  { int		active;			/* doing pipe I/O */
    jmp_buf	context;		/* context of longjmp() */
  } pipe;

  struct
  { atom_t	current;		/* current global prompt */
    char *	first;			/* how to prompt first line */
    int		first_used;		/* did we do the first line? */
    int		next;			/* prompt on next read operation */
  } prompt;

  source_location read_source;		/* file, line, char of last term */
  int	 _fileerrors;			/* current file-error status */
  const char   *float_format;		/* floating point format */

  struct
  { term_t	term;			/* exception term */
    term_t	bin;			/* temporary handle for exception */
    term_t	printed;		/* already printed exception */
    term_t	tmp;			/* tmp for errors */
    term_t	pending;		/* used by the debugger */
  } exception;

  struct
  { ulong	inferences;		/* inferences in this thread */
#ifdef O_PROFILE
    int		profiling;		/* profiler is on */
    ulong	profile_ticks;		/* profile ticks total */
#endif /* O_PROFILE */
    ulong	last_cputime;		/* milliseconds last CPU time */
    ulong	last_systime;		/* milliseconds last SYSTEM time */
    ulong	last_walltime;		/* milliseconds last Wall time */
    double	user_cputime;		/* User saved CPU time */
    double	system_cputime;		/* Kernel saved CPU time */
  } statistics;

  struct
  { Module	typein;			/* module for type in goals */
    Module	source;			/* module we are reading clauses in */
  } modules;

  struct
  { long 	generator;		/* See PL_atom_generator() */
  } atoms;

  struct
  { struct assoc* bags;			/* findall/setof bags storage */
  } bags;

  struct
  { VarDef *	vardefs;		/* compiler variable analysis */
    int		nvardefs;
    int		filledVars;
  } comp;

  struct
  { char *	_CWDdir;
    int		_CWDlen;
#ifdef __BEOS__
    status_t	dl_error;		/* dlopen() emulation in pl-beos.c */
#endif
  } os;

  struct
  { Table	  table;		/* Feature table */
    pl_features_t mask;			/* Masked access to booleans */
  } feature;

  struct
  { FindData	find;			/* /<ports> <goal> in tracer */
  } trace;

  struct
  { AbortHandle	_abort_head;		/* PL_abort_hook() */
    AbortHandle _abort_tail;
    
    PL_dispatch_hook_t _dispatch_events; /* PL_dispatch_hook() */

    buffer	_discardable_buffer;	/* PL_*() character buffers */
    buffer	_buffer_ring[BUFFER_RING_SIZE];
    int		_current_buffer_id;
  } fli;

  struct				/* Local IO stuff */
  { IOSTREAM *streams[6];		/* handles for standard streams */
#if 0
    IOSTREAM *user_input;		/* current user input */
    IOSTREAM *user_output;		/* current user output */
    IOSTREAM *user_error;		/* current user error */
    IOSTREAM *curin;			/* current input stream */
    IOSTREAM *curout;			/* current output stream */
    IOSTREAM *din;			/* debugger input */
    IOSTREAM *dout;			/* debuffer output */
    IOSTREAM *log;			/* stream used for protocolling */
    IOSTREAM *term;			/* terminal stream */
#endif
    struct input_context *input_stack;	/* maintain input stream info */
    struct output_context *output_stack; /* maintain output stream info */
  } IO;

#ifdef O_LIMIT_DEPTH
  struct
  { ulong limit;
    ulong reached;
  } depth_info;
#endif

  struct
  { long _total_marked;			/* # marked global cells */
    long _trailcells_deleted;		/* # garbage trailcells */
    long _relocation_chains;		/* # relocation chains (debugging) */
    long _relocation_cells;		/* # relocation cells */
    long _relocated_cells;		/* # relocated cells */
    long _needs_relocation;		/* # cells that need relocation */
    long _local_marked;			/* # marked local -> global ptrs */
    long _marks_swept;			/* # marks swept */
    long _marks_unswept;		/* # marks swept */
    long _alien_relocations;		/* # alien_into_relocation_chain() */
    long _local_frames;			/* frame count for debugging */
    long _choice_count;			/* choice-point count for debugging */

    pl_gc_status_t	status;		/* Garbage collection status */
  } gc;

#ifdef O_SHIFT_STACKS
  pl_shift_status_t	_shift_status;	/* Stack shifter status */
#endif
  
  pl_debugstatus_t _debugstatus;	/* status of the debugger */

#ifdef O_PLMT
  struct
  { long   magic;			/* PL_THREAD_MAGIC (checking) */
    struct _PL_thread_info_t *info;	/* info structure */
    unsigned forall_flags;		/* forThreadLocalData() flags */
					/* Message queues */
    pthread_mutex_t      queue_mutex;	/* Message queue mutex */
    pthread_cond_t       cond_var;	/* condition variable of queue */
    struct _thread_msg   *msg_head;	/* Head of message queue */
    struct _thread_msg   *msg_tail;	/* Tail of message queue */
    struct _thread_sig   *sig_head;	/* Head of signal queue */
    struct _thread_sig   *sig_tail;	/* Tail of signal queue */
    struct _at_exit_goal *exit_goals;	/* thread_at_exit/1 goals */
#ifdef WIN32
    HWND hwnd;				/* Window for signalling */
#endif
  } thread;

  struct alloc_pool alloc_pool;		/* Thread allocation pool */
#endif
} PL_local_data_t;

GLOBAL PL_global_data_t PL_global_data;
GLOBAL PL_code_data_t	PL_code_data;
GLOBAL PL_local_data_t  PL_local_data;

#ifndef O_PLMT
#define GET_LD
#define ARG_LD
#define ARG1_LD void
#define PASS_LD
#define PASS_LD1
#define LOCAL_LD  (&PL_local_data)
#define GLOBAL_LD (&PL_local_data)
#define LD	  GLOBAL_LD
#endif /*O_PLMT*/
#define GD (&PL_global_data)
#define CD (&PL_code_data)

#define hTop			(GD->top_of_heap)
#define hBase			(GD->base_of_heap)
#define heap_base		(GD->rounded_heap_base)
#define functor_array		(GD->functors.array)
#define atom_array		(GD->atoms.array)
#define systemDefaults		(GD->defaults)
#define features		(LD->feature.mask)

#define environment_frame 	(LD->environment)
#define fli_context	  	(LD->foreign_environment)
#define source_file_name	(LD->read_source.file)
#define source_line_no		(LD->read_source.line)
#define source_char_no		(LD->read_source.character)
#define exception_term		(LD->exception.term)
#define exception_bin		(LD->exception.bin)
#define exception_printed	(LD->exception.printed)
#define fileerrors		(LD->_fileerrors)
#define gc_status		(LD->gc.status)
#define shift_status		(LD->_shift_status)
#define debugstatus		(LD->_debugstatus)
#define depth_limit		(LD->depth_info.limit)
#define depth_reached		(LD->depth_info.reached)
#define base_addresses		(LD->bases)
#define Suser_input		(LD->IO.streams[0])
#define Suser_output		(LD->IO.streams[1])
#define Suser_error		(LD->IO.streams[2])
#define Scurin			(LD->IO.streams[3])
#define Scurout			(LD->IO.streams[4])
#define Sprotocol		(LD->IO.streams[5])
#define Sdin			Suser_input 		/* not used for now */
#define Sdout			Suser_output

#ifdef VMCODE_IS_ADDRESS
#define dewam_table		(CD->_dewam_table)
#define dewam_table_offset	(CD->_dewam_table_offset)
#define wam_table		(CD->_wam_table)
#define interpreter_jmp_table	(CD->_interpreter_jmp_table)
#endif /*VMCODE_IS_ADDRESS*/

#endif /*PL_GLOBAL_H_INCLUDED*/
