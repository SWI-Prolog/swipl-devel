/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
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
  int	critical;			/* heap is being modified */
  pl_defaults_t	    defaults;		/* system default settings */
  pl_options_t	    options;		/* command-line options */
  State		stateList;		/* list of loaded states */
  int		initialised;		/* Heap is initialised */
  int		io_initialised;		/* I/O system has been initialised */
  int		bootsession;		/* -b boot compilation */
  int		debug_level;		/* Maintenance debugging: 0..9 */
  void *	resourceDB;		/* program resource database */

  struct
  { Feature	list;			/* global features */
    pl_features_t mask;			/* Masked access to booleans */
  } feature;

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
    int		functors;		/* No. of functors defined */
    int		predicates;		/* No. of predicates defined */
    int		modules;		/* No. of modules in the system */
    long	codes;			/* No. of byte codes generated */
  } statistics;

  struct
  { Module	user;			/* user module */
    Module	system;			/* system predicate module */
  } modules;

  struct
  { Table	modules;		/* atom --> module */
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
  { buffer	array;			/* index --> functor */
    int		buckets;		/* # buckets in atom --> functor */
    FunctorDef* table;			/* hash-table */
  } functors;

  struct
  { Operator	table[OPERATORHASHSIZE]; /* global operator table */
  } op;

  struct
  { TempFile		_tmpfile_head;
    TempFile		_tmpfile_tail;
    CanonicalDir	_canonical_dirlist;
    char *		myhome;		/* expansion of ~ */
    char *		fred;		/* last expanded ~user */
    char *		fredshome;	/* home of fred */
    OnHalt		on_halt_list;	/* list of onhalt hooks */
    int			halting;	/* process is shutting down */
    int			prompt_next;	/* prompt on next read operation */
    int			gui_app;	/* Win32: Application is a gui app */
    IOFUNCTIONS		iofunctions;	/* initial IO functions */
    IOFUNCTIONS 	org_terminal;	/* IO+Prolog terminal functions */
    IOFUNCTIONS		rl_functions;	/* IO+Terminal+Readline functions */
  } os;

  struct
  { Procedure	alt0;			/* $alt/0, see C_OR */
    Procedure	garbage_collect0;
    Procedure 	block3;
    Procedure	catch3;
    Procedure	true0;
    Procedure	fail0;
    Procedure	event_hook1;
    Procedure	print_message2;
    Procedure	foreign_registered2;	/* $foreign_registered/2 */
    Procedure	prolog_trace_interception4;
    Procedure	portray;		/* portray/1 */
  } procedures;

  struct
  { buffer	source_files;
  } files;
} PL_global_data_t;


		 /*******************************
		 *	     LOCAL DATA		*
		 *******************************/

typedef struct
{ LocalFrame environment;		/* Current local frame */
  FliFrame   foreign_environment;	/* Current foreign context */
  pl_stacks_t stacks;			/* Prolog runtime stacks */
  ulong		bases[STG_MASK+1];	/* area base addresses */
#ifdef HAVE_SIGNAL
  sig_handler sig_handlers[MAXSIGNAL];	/* How Prolog preceives signals */
#endif  
  ulong		pending_signals;	/* PL_raise() pending signals */
  int		current_signal;		/* Currently handled signal */
  int		aborted;		/* thread asked for abort */
  Stack		outofstack;		/* thread is out of stack */
  int		trim_stack_requested;	/* perform a trim-stack */
  int		in_arithmetic;		/* doing arithmetic */

  struct
  { int		active;			/* doing pipe I/O */
    jmp_buf	context;		/* context of longjmp() */
  } pipe;

  struct
  { atom_t	current;		/* current global prompt */
    char *	first;			/* how to prompt first line */
    int		first_used;		/* did we do the first line? */
  } prompt;

  source_location read_source;		/* file, line, char of last term */
  int	 _fileerrors;			/* current file-error status */
  atom_t _float_format;			/* floating point format */

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
  } statistics;

  struct
  { Module	typein;			/* module for type in goals */
    Module	source;			/* module we are reading clauses in */
  } modules;

  struct
  { long 	generator;		/* See PL_atom_generator() */
  } atoms;

  struct
  { Assoc	bags;			/* findall/setof bags storage */
  } bags;

  struct
  { VarDef *	_vardefs;		/* compiler variable analysis */
    int		_nvardefs;
    int		_filledVars;
  } comp;

  struct
  { char *	_CWDdir;
    int		_CWDlen;
  } os;

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
  { IOSTREAM *	curin;			/* current input stream */
    IOSTREAM *	curout;			/* current output stream */
    IOSTREAM *	din;			/* debugger input */
    IOSTREAM *	dout;			/* debuffer output */
    IOSTREAM *	log;			/* stream used for protocolling */
    IOSTREAM *	term;			/* terminal stream */
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

    pl_gc_status_t	status;		/* Garbage collection status */
  } gc;

#ifdef O_SHIFT_STACKS
  pl_shift_status_t	_shift_status;	/* Stack shifter status */
#endif
  
  pl_debugstatus_t _debugstatus;	/* status of the debugger */

#ifdef O_PLMT
  struct
  { struct _PL_thread_info_t *info;	/* info structure */
    long magic;
  } thread;
#endif
} PL_local_data_t;

GLOBAL PL_global_data_t PL_global_data;
GLOBAL PL_code_data_t	PL_code_data;

#ifndef O_PLMT
GLOBAL PL_local_data_t  PL_local_data;
#define GET_LD
#define LOCAL_LD  (&PL_local_data)
#define GLOBAL_LD (&PL_local_data)
#define LD	  GLOBAL_LD
#endif
#define GD (&PL_global_data)
#define CD (&PL_code_data)

#define hTop			(GD->top_of_heap)
#define hBase			(GD->base_of_heap)
#define heap_base		(GD->rounded_heap_base)
#define functor_array		(GD->functors.array)
#define atom_array		(GD->atoms.array)
#define systemDefaults		(GD->defaults)
#define features		(GD->feature.mask)

#define environment_frame 	(LD->environment)
#define fli_context	  	(LD->foreign_environment)
#define source_file_name	(LD->read_source.file)
#define source_line_no		(LD->read_source.line)
#define source_char_no		(LD->read_source.character)
#define LD_sig_handler(n)	(LD->sig_handlers[(n)])
#define signalled		(LD->pending_signals)
#define exception_term		(LD->exception.term)
#define exception_bin		(LD->exception.bin)
#define exception_printed	(LD->exception.printed)
#define fileerrors		(LD->_fileerrors)
#define float_format		(LD->_float_format)
#define gc_status		(LD->gc.status)
#define shift_status		(LD->_shift_status)
#define debugstatus		(LD->_debugstatus)
#define depth_limit		(LD->depth_info.limit)
#define depth_reached		(LD->depth_info.reached)
#define base_addresses		(LD->bases)
#define Scurin			(LD->IO.curin)
#define Scurout			(LD->IO.curout)
#define Sdin			(LD->IO.din)
#define Sdout			(LD->IO.dout)
#define Slog			(LD->IO.log)
#define Sterm			(LD->IO.term)

#ifdef VMCODE_IS_ADDRESS
#define dewam_table		(CD->_dewam_table)
#define dewam_table_offset	(CD->_dewam_table_offset)
#define wam_table		(CD->_wam_table)
#define interpreter_jmp_table	(CD->_interpreter_jmp_table)
#endif /*VMCODE_IS_ADDRESS*/

#endif /*PL_GLOBAL_H_INCLUDED*/
