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


		 /*******************************
		 *	    GLOBAL DATA		*
		 *******************************/

typedef struct
{ char *top_of_heap;			/* highest allocated heap address */
  char *base_of_heap;			/* lowest allocated heap address */
  ulong rounded_heap_base;		/* heap-base rounded downwards */
  int	critical;			/* heap is being modified */
  char *cannot_save_program;		/* why program cannot be saved */
  pl_loaderstatus_t _loaderstatus;	/* status of foreign code loader */
  pl_defaults_t	    defaults;		/* system default settings */
  pl_options_t	    options;		/* command-line options */
  State		stateList;		/* list of loaded states */
  ulong		bases[STG_MASK+1];	/* area base addresses */
  int		initialised;		/* Heap is initialised */
  int		io_initialised;		/* I/O system has been initialised */
  int		bootsession;		/* -b boot compilation */
  int		debug_level;		/* Maintenance debugging: 0..9 */
  Feature	_feature_list;		/* global features */
  void *	resourceDB;		/* program resource database */

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
    int		locked;			/* table is locked */
    Atom *	table;			/* hash-table */
    int		lookups;		/* # atom lookups */
    int		cmps;			/* # string compares for lookup */
  } atoms;

  struct
  { Table	breakpoints;		/* Breakpoint table */
  } comp;

  struct
  { ExtensionCell _ext_head;		/* head of registered extensions */
    ExtensionCell _ext_tail;		/* tail of this chain */
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
    int		locked;			/* table is locked */
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
#ifdef HAVE_SIGNAL
  sig_handler sig_handlers[MAXSIGNAL];	/* How Prolog preceives signals */
#endif  
  ulong		pending_signals;	/* PL_signal() pending signals */
  int		aborted;		/* thread asked for abort */
  int		outofstack;		/* thread is out of stack */
  int		autoload;		/* do autoloading */
  int		in_arithmetic;		/* doing arithmetic */

  struct
  { atom_t	  file;			/* current source file */
    int	  	  line;			/* current line */
    long	  character;		/* current character location */
  } read_source;

  int	 _fileerrors;			/* current file-error status */
  atom_t _float_format;			/* floating point format */

  struct
  { term_t	term;			/* exception term */
    term_t	bin;			/* temporary handle for exception */
    term_t	printed;		/* already printed exception */
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
  { Atom 	generator;		/* See PL_atom_generator() */
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
    
    InitialiseHandle _initialise_head;	/* PL_initialise_hook() */
    InitialiseHandle _initialise_tail;

    PL_dispatch_hook_t _dispatch_events; /* PL_dispatch_hook() */

    buffer	_discardable_buffer;	/* PL_*() character buffers */
    buffer	_buffer_ring[BUFFER_RING_SIZE];
    int		_current_buffer_id;
  } fli;

#ifdef O_LIMIT_DEPTH
  struct
  { ulong limit;
    ulong reached;
  } depth_info;
#endif

  pl_gc_status_t	_gc_status;	/* Garbage collection status */
#ifdef O_SHIFT_STACKS
  pl_shift_status_t	_shift_status;	/* Stack shifter status */
#endif
  
  pl_debugstatus_t _debugstatus;	/* status of the debugger */
  pl_features_t	   _features;		/* thread-local features */
} PL_local_data_t;

GLOBAL PL_local_data_t  PL_local_data;
GLOBAL PL_global_data_t PL_global_data;
GLOBAL PL_code_data_t	PL_code_data;

#define LD (&PL_local_data)
#define GD (&PL_global_data)
#define CD (&PL_code_data)

#define hTop			(GD->top_of_heap)
#define hBase			(GD->base_of_heap)
#define heap_base		(GD->rounded_heap_base)
#define loaderstatus		(GD->_loaderstatus)
#define base_addresses		(GD->bases)
#define functor_array		(GD->functors.array)
#define atom_array		(GD->atoms.array)
#define systemDefaults		(GD->defaults)

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
#define gc_status		(LD->_gc_status)
#define shift_status		(LD->_shift_status)
#define debugstatus		(LD->_debugstatus)
#define features		(LD->_features)
#define depth_limit		(LD->depth_info.limit)
#define depth_reached		(LD->depth_info.reached)

#ifdef VMCODE_IS_ADDRESS
#define dewam_table		(CD->_dewam_table)
#define dewam_table_offset	(CD->_dewam_table_offset)
#define wam_table		(CD->_wam_table)
#define interpreter_jmp_table	(CD->_interpreter_jmp_table)
#endif /*VMCODE_IS_ADDRESS*/

#endif /*PL_GLOBAL_H_INCLUDED*/
