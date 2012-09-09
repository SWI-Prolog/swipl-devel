/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PL_GLOBAL_H_INCLUDED
#define PL_GLOBAL_H_INCLUDED

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
  unsigned char   *_dewam_table;	/* decoding table */
  intptr_t	  _dewam_table_offset;	/* offset of 1st */
  void  **_interpreter_jmp_table;	/* interpreters table */
					/* must be last! (why?) */
  code    _wam_table[I_HIGHEST];	/* code --> address */
#else
  int	  struct_may_not_be_empty;	/* empty structure is illegal */
#endif
  struct				/* see initSupervisors() */
  { code exit[3];			/* I_EXIT */
    code next_clause[3];		/* S_NEXTCLAUSE */
    code virgin[3];			/* S_VIRGIN */
    code undef[3];			/* S_UNDEF */
    code dynamic[3];			/* S_DYNAMIC */
    code thread_local[3];		/* S_THREAD_LOCAL */
    code multifile[3];			/* S_MULTIFILE */
    code staticp[3];			/* S_STATIC */
  } supervisors;
} PL_code_data_t;

typedef struct
{ atom_t	file;			/* current source file */
  int		line;			/* current line */
  int		linepos;		/* position in the line */
  int64_t	character;		/* current character location */
  int64_t	byte;			/* byte offset of location */
} source_location;

		 /*******************************
		 *	    GLOBAL DATA		*
		 *******************************/

struct PL_global_data
{ uintptr_t	heap_base;		/* heap-base rounded downwards */
  pl_defaults_t	defaults;		/* system default settings */
  pl_options_t	options;		/* command-line options */
  State		stateList;		/* list of loaded states */
  int		initialised;		/* Heap is initialised */
  int		io_initialised;		/* I/O system has been initialised */
  cleanup_status cleaning;		/* Inside PL_cleanup() */
  int		bootsession;		/* -b boot compilation */
  int		debug_level;		/* Maintenance debugging: 0..9 */
  struct bit_vector *debug_topics;	/* debug topics enabled */
  void *	resourceDB;		/* program resource database */

#ifdef HAVE_SIGNAL
  sig_handler sig_handlers[MAXSIGNAL];	/* How Prolog preceives signals */
#endif
#ifdef O_LOGICAL_UPDATE
  gen_t		generation;		/* generation of the database */
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
  { char *	CWDdir;
    size_t	CWDlen;
    char *	executable;		/* Running executable */
#ifdef __WINDOWS__
    char *	module;			/* argv[0] module passed */
#endif
  } paths;

  struct
  { size_t	atoms;			/* No. of atoms defined */
    size_t	atom_string_space;	/* # bytes used to store atoms */
    size_t	atom_string_space_freed;/* # bytes in freed atoms */
    size_t	stack_space;		/* # bytes on stacks */
    int		functors;		/* No. of functors defined */
    int		predicates;		/* No. of predicates defined */
    int		modules;		/* No. of modules in the system */
    size_t	clauses;		/* No. clauses */
    size_t	codes;			/* No. of VM codes generated */
    double	start_time;		/* When Prolog was started */
    double	user_cputime;		/* User CPU time (whole process) */
    double	system_cputime;		/* Kernel CPU time (whole process) */
#ifdef O_PLMT
    int		threads_created;	/* # threads created */
    int		threads_finished;	/* # finished threads */
    double	thread_cputime;		/* Total CPU time of threads */
#endif
  } statistics;

  struct
  { Module	user;			/* user module */
    Module	system;			/* system predicate module */
  } modules;

  struct
  { Table	modules;		/* atom --> module */
  } tables;

  struct
  { Table	record_lists;		/* Available record lists */
    RecordList	head;			/* first record list */
    RecordList	tail;			/* last record list */
  } recorded_db;

  struct
  { ArithF     *functions;		/* index --> function */
    size_t	functions_allocated;	/* Size of above array */
  } arith;

  struct
  { size_t	highest;		/* Highest atom index */
    atom_array	array;
    unsigned int buckets;		/* # buckets in char * --> atom */
    Atom *	table;			/* hash-table */
    Atom	builtin_array;		/* Builtin atoms */
    int		lookups;		/* # atom lookups */
    int		cmps;			/* # string compares for lookup */
#ifdef O_ATOMGC
    int		gc_active;		/* Atom-GC is in progress */
    size_t	builtin;		/* Locked atoms (atom-gc) */
    size_t	no_hole_before;		/* You won't find a hole before here */
    size_t	margin;			/* # atoms to grow before collect */
    size_t	non_garbage;		/* # atoms for after last AGC */
    int		gc;			/* # atom garbage collections */
    int64_t	collected;		/* # collected atoms */
    size_t	unregistered;		/* # candidate GC atoms */
    double	gc_time;		/* Time spent on atom-gc */
    PL_agc_hook_t gc_hook;		/* Current hook */
#endif
    atom_t     *for_code[256];		/* code --> one-char-atom */
    PL_blob_t  *types;			/* registered atom types */
  } atoms;

#ifdef O_PLMT
  struct
  { int		active;			/* #GC active */
    int		agc_waiting;		/* AGC is waiting for us */
  } gc;
#endif

  struct
  { Table	breakpoints;		/* Breakpoint table */
  } comp;

  struct
  { ExtensionCell _ext_head;		/* head of registered extensions */
    ExtensionCell _ext_tail;		/* tail of this chain */

    InitialiseHandle initialise_head;	/* PL_initialise_hook() */
    InitialiseHandle initialise_tail;
    PL_dispatch_hook_t dispatch_events; /* PL_dispatch_hook() */

    int		  _loaded;		/* system extensions are loaded */
  } foreign;

#ifdef O_GMP
  struct
  { int initialised;			/* is GMP initialised? */
    int keep_alloc_functions;		/* do not change allocation */
  } gmp;
#endif

  struct				/* pl-format.c */
  { Table	predicates;
  } format;

  struct
  { Table	table;			/* flag key --> flag */
  } flags;

  struct
  { Table	table;			/* global (read-only) features */
  } prolog_flag;

  struct
  { size_t	highest;		/* Next index to handout */
    functor_array array;		/* index --> functor */
    int		buckets;		/* # buckets in atom --> functor */
    FunctorDef* table;			/* hash-table */
  } functors;

  struct
  { Code	catch_exit_address;	/* See findCatchExit() */
  } exceptions;

  struct
  { Table		tmp_files;	/* Known temporary files */
    CanonicalDir	_canonical_dirlist;
    char *		myhome;		/* expansion of ~ */
    char *		fred;		/* last expanded ~user */
    char *		fredshome;	/* home of fred */
    OnHalt		on_halt_list;	/* list of onhalt hooks */
    int			halting;	/* process is shutting down */
    int			gui_app;	/* Win32: Application is a gui app */
    IOFUNCTIONS		iofunctions;	/* initial IO functions */
    IOFUNCTIONS		org_terminal;	/* IO+Prolog terminal functions */
    IOFUNCTIONS		rl_functions;	/* IO+Terminal+Readline functions */
  } os;

  struct
  { Procedure	dgarbage_collect1;
    Procedure	catch3;
    Procedure	true0;
    Procedure	fail0;
    Procedure	equals2;		/* =/2 */
    Procedure	is2;			/* is/2 */
    Procedure	strict_equal2;		/* ==/2 */
    Procedure	not_strict_equal2;	/* \==/2 */
    Procedure	event_hook1;
    Procedure	exception_hook4;
    Procedure	print_message2;
    Procedure	foreign_registered2;	/* $foreign_registered/2 */
    Procedure	prolog_trace_interception4;
    Procedure	portray;		/* portray/1 */
    Procedure   dcall1;			/* $call/1 */
    Procedure   call3;			/* call/3*/
    Procedure	setup_call_catcher_cleanup4; /* setup_call_catcher_cleanup/4 */
    Procedure	undefinterc4;		/* $undefined_procedure/4 */
    Procedure   dthread_init0;		/* $thread_init/0 */
    Procedure   dc_call_prolog0;	/* $c_call_prolog/0 */
#ifdef O_ATTVAR
    Procedure	dwakeup1;		/* system:$wakeup/1 */
    Procedure	portray_attvar1;	/* $attvar:portray_attvar/1 */
#endif
#ifdef O_CALL_RESIDUE
    Procedure	call_residue_vars2;	/* $attvar:call_residue_vars/2 */
#endif
    Procedure   comment_hook3;		/* prolog:comment_hook/3 */

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
    int			enabled;	/* threads are enabled */
    Table		mutexTable;	/* Name --> mutex table */
    int			mutex_next_id;	/* next id for anonymous mutexes */
#ifdef __WINDOWS__
    HINSTANCE		instance;	/* Win32 process instance */
#endif
    counting_mutex     *mutexes;	/* Registered mutexes */
    int			thread_max;	/* Maximum # threads */
    PL_thread_info_t  **threads;	/* Pointers to thread-info */
  } thread;
#endif /*O_PLMT*/
};


		 /*******************************
		 *	     LOCAL DATA		*
		 *******************************/

#define LD_MAGIC	0x3cfd82b4	/* Valid local-data structure */

struct PL_local_data
{ uintptr_t	magic;			/* LD_MAGIC */
  LocalFrame    environment;		/* Current local frame */
  Choice	choicepoints;		/* Choice-point chain */
  FliFrame      foreign_environment;	/* Current foreign context */
  QueryFrame    query;			/* Currently open query */
  Word		mark_bar;		/* Mark globals > this one */
#ifdef O_GVAR
  Word		frozen_bar;		/* Frozen part of the global stack */
#endif
  pl_stacks_t   stacks;			/* Prolog runtime stacks */
  uintptr_t	bases[STG_MASK+1];	/* area base addresses */
  int		alerted;		/* Special mode. See updateAlerted() */
  int		critical;		/* heap is being modified */
  int		break_level;		/* current break level */
  Stack		outofstack;		/* thread is out of stack */
  int		trim_stack_requested;	/* perform a trim-stack */
#ifdef O_PLMT
  int		exit_requested;		/* Thread is asked to exit */
#endif
  int		in_arithmetic;		/* doing arithmetic */
  int		in_print_message;	/* Inside printMessage() */
  int		autoload_nesting;	/* Nesting level in autoloader */
  void *	glob_info;		/* pl-glob.c */
  IOENC		encoding;		/* default I/O encoding */
  ClauseRef	freed_clauses;		/* List of pending freeable clauses */

  struct
  { int64_t	pending;		/* PL_raise() pending signals */
    int		current;		/* currently processing signal */
    int		is_sync;		/* current signal is synchronous */
    record_t	exception;		/* Pending exception from signal */
#ifdef O_PLMT
    simpleMutex sig_lock;		/* lock delivery and processing */
#endif
  } signal;

  struct
  { int		active;			/* doing pipe I/O */
    jmp_buf	context;		/* context of longjmp() */
  } pipe;

  struct
  { char       *getstr_buffer;		/* getString() buffer */
    size_t	getstr_buffer_size;	/* size of getstr_buffer */
    struct wic_state *current_state;	/* qlf-creation state */
  } qlf;

  struct
  { atom_t	current;		/* current global prompt */
    atom_t	first;			/* how to prompt first line */
    int		first_used;		/* did we do the first line? */
    int		next;			/* prompt on next read operation */
  } prompt;

  source_location read_source;		/* file, line, char of last term */

  struct
  { term_t	term;			/* exception term */
    term_t	bin;			/* temporary handle for exception */
    term_t	printed;		/* already printed exception */
    term_t	tmp;			/* tmp for errors */
    term_t	pending;		/* used by the debugger */
    int		in_hook;		/* inside exception_hook() */
    int		processing;		/* processing an exception */
    exception_frame *throw_environment;	/* PL_throw() environments */
  } exception;

#ifdef O_ATTVAR
  struct
  { term_t	head;			/* Head of wakeup list */
    term_t	tail;			/* Tail of this list */
  } attvar;
#endif

  struct
  { term_t	dummy;			/* see trimStacks() */
  } trim;

  struct
  { term_t	h[TMP_PTR_SIZE];	/* temporary handles. See unify_ptrs */
    int		top;			/* Top-of-stack index */
  } tmp;

#ifdef O_GVAR
  struct
  { Table	nb_vars;		/* atom --> value */
    int		grefs;			/* references to global stack */
  } gvar;
#endif

  struct
  { int64_t	inferences;		/* inferences in this thread */
    uintptr_t	last_cputime;		/* milliseconds last CPU time */
    uintptr_t	last_systime;		/* milliseconds last SYSTEM time */
    uintptr_t	last_real_time;		/* Last Real Time (seconds since Epoch) */
    double	last_walltime;		/* Last Wall time (m-secs since start) */
    double	user_cputime;		/* User saved CPU time */
    double	system_cputime;		/* Kernel saved CPU time */
  } statistics;

#ifdef O_GMP
  struct
  { int		persistent;		/* do persistent operations */
    size_t	allocated;		/* memory allocated */
    ar_context *context;		/* current allocation context */
    mp_mem_header *head;		/* linked list of allocated chunks */
    mp_mem_header *tail;
  } gmp;
#endif

#ifdef O_PROFILE
  struct
  { int		active;			/* profiler is on */
    int		accounting;		/* we are accounting */
    int		sum_ok;			/* siblings are counted */
    struct call_node *current;		/* `current' node */
    struct call_node *roots;		/* list of root-nodes */
    uintptr_t	ticks;			/* profile ticks total */
    uintptr_t	accounting_ticks;	/* Ticks in profCall() and friends */
    uintptr_t	nodes;			/* #Recorded nodes */
    double	time_at_start;		/* Time at last start */
    double	time;			/* recorded CPU time */
  } profile;
#endif /* O_PROFILE */

  struct
  { Module	typein;			/* module for type in goals */
    Module	source;			/* module we are reading clauses in */
  } modules;

  struct
  { intptr_t	generator;		/* See PL_atom_generator() */
    atom_t	unregistering;		/* See PL_unregister_atom() */
  } atoms;

  struct
  { VarDef *	vardefs;		/* compiler variable analysis */
    int		nvardefs;
    int		filledVars;
  } comp;

  struct
  { struct
    { Number	base;
      Number	top;
      Number	max;
    } stack;
#ifdef O_GMP
    struct
    { gmp_randstate_t state;
      int initialised;
    } random;
#endif
  } arith;

#if O_CYCLIC
  struct
  { segstack lstack;			/* Stack for cycle-links */
    segstack vstack;			/* Stack for visited marks */
  } cycle;
#endif

  struct
  {
#ifdef __BEOS__
    status_t	dl_error;		/* dlopen() emulation in pl-beos.c */
#endif
    int		rand_initialised;	/* have we initialised random? */
#ifdef O_DDE
    unsigned	dde_instance;		/* Actually DWORD */
#endif
  } os;

  struct
  { Table	  table;		/* Feature table */
    pl_features_t mask;			/* Masked access to booleans */
    int		  write_attributes;	/* how to write attvars? */
    occurs_check_t occurs_check;	/* Unify and occurs check */
    access_level_t access_level;	/* Current access level */
  } prolog_flag;

  struct
  { FindData	find;			/* /<ports> <goal> in tracer */
  } trace;

  struct
  { struct findall_bag *bags;		/* Known bags */
    struct findall_bag *default_bag;	/* Bag we keep around */
  } bags;

  struct
  { AbortHandle	_abort_head;		/* PL_abort_hook() */
    AbortHandle _abort_tail;

    buffer	_discardable_buffer;	/* PL_*() character buffers */
    buffer	_buffer_ring[BUFFER_RING_SIZE];
    int		_current_buffer_id;

    int		SP_state;		/* For SICStus interface */
  } fli;

  struct				/* Local IO stuff */
  { IOSTREAM *streams[6];		/* handles for standard streams */
    st_check stream_type_check;		/* Check bin/text streams? */
					/* do not copy from parent */
    struct input_context *input_stack;	/* maintain input stream info */
    struct output_context *output_stack; /* maintain output stream info */
  } IO;

  struct
  { fid_t	numbervars_frame;	/* Numbervars choice-point */
  } var_names;

#ifdef O_LIMIT_DEPTH
  struct
  { uintptr_t limit;
    uintptr_t reached;
  } depth_info;
#endif

  pl_shift_status_t shift_status;	/* Stack shifter status */
  pl_debugstatus_t _debugstatus;	/* status of the debugger */
  struct btrace *btrace_store;		/* C-backtraces */

#ifdef O_PLMT
  struct
  { intptr_t   magic;			/* PL_THREAD_MAGIC (checking) */
    struct _PL_thread_info_t *info;	/* info structure */
    unsigned forall_flags;		/* forThreadLocalData() flags */
					/* Communication */
    message_queue messages;		/* Message queue */
    struct _thread_sig   *sig_head;	/* Head of signal queue */
    struct _thread_sig   *sig_tail;	/* Tail of signal queue */
    struct _at_exit_goal *exit_goals;	/* thread_at_exit/1 goals */
    DefinitionChain local_definitions;	/* P_THREAD_LOCAL predicates */
  } thread;

  struct alloc_pool alloc_pool;		/* Thread allocation pool */
#endif

  struct
  { intptr_t _total_marked;		/* # marked global cells */
    intptr_t _trailcells_deleted;	/* # garbage trailcells */
    intptr_t _relocation_chains;	/* # relocation chains (debugging) */
    intptr_t _relocation_cells;		/* # relocation cells */
    intptr_t _relocated_cells;		/* # relocated cells */
    intptr_t _needs_relocation;		/* # cells that need relocation */
    intptr_t _local_marked;		/* # marked local -> global ptrs */
    intptr_t _marks_swept;		/* # marks swept */
    intptr_t _marks_unswept;		/* # marks swept */
    intptr_t _alien_relocations;	/* # alien_into_relocation_chain() */
    intptr_t _local_frames;		/* frame count for debugging */
    intptr_t _choice_count;		/* choice-point count for debugging */
    int  *_start_map;			/* bitmap with legal global starts */
    sigset_t saved_sigmask;		/* Saved signal mask */
    int64_t inferences;			/* #inferences at last GC */
    pl_gc_status_t	status;		/* Garbage collection status */
#ifdef O_CALL_RESIDUE
    int			marked_attvars;	/* do not GC attvars */
#endif
    int active;				/* GC is running in this thread */
					/* These must be at the end to be */
					/* able to define O_DEBUG in only */
					/* some modules */
#if defined(O_DEBUG) || defined(SECURE_GC)
    intptr_t _trailtops_marked;		/* # marked trailtops */
    Word *_mark_base;			/* Array of marked cells addresses */
    Word *_mark_top;			/* Top of this array */
    Table _check_table;			/* relocation address table */
    Table _local_table;			/* marked local variables */
    int  _relocated_check;		/* Verify relocated addresses? */
    unsigned int incr_seed;		/* Seed for random stack increments */
#endif
  } gc;
};

GLOBAL PL_global_data_t PL_global_data;
GLOBAL PL_code_data_t	PL_code_data;
GLOBAL PL_local_data_t  PL_local_data;
#ifdef O_MULTIPLE_ENGINES
GLOBAL PL_local_data_t *PL_current_engine_ptr;
#endif

#define GD (&PL_global_data)
#define CD (&PL_code_data)

#define functor_array		(GD->functors.array)
#define systemDefaults		(GD->defaults)

#define environment_frame	(LD->environment)
#define fli_context		(LD->foreign_environment)
#define source_file_name	(LD->read_source.file)
#define source_line_no		(LD->read_source.line)
#define source_line_pos		(LD->read_source.linepos)
#define source_char_no		(LD->read_source.character)
#define source_byte_no		(LD->read_source.byte)
#define exception_term		(LD->exception.term)
#define exception_bin		(LD->exception.bin)
#define exception_printed	(LD->exception.printed)
#define gc_status		(LD->gc.status)
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
#define Sdin			Suser_input		/* not used for now */
#define Sdout			Suser_output

#ifdef VMCODE_IS_ADDRESS
#define dewam_table		(CD->_dewam_table)
#define dewam_table_offset	(CD->_dewam_table_offset)
#define wam_table		(CD->_wam_table)
#define interpreter_jmp_table	(CD->_interpreter_jmp_table)
#endif /*VMCODE_IS_ADDRESS*/

#endif /*PL_GLOBAL_H_INCLUDED*/
