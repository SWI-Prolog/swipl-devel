/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*#define O_DEBUG 1*/			/* include crash/0 */
#include "pl-incl.h"
#include "os/pl-ctype.h"

#if O_DEBUG
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See how the system reacts on segmentation faults.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
pl_crash()
{ intptr_t *lp = NULL;

  Sdprintf("You asked for it ... Writing to address 0\n");

  *lp = 5;

  Sdprintf("Oops, this doesn't appear to be a secure OS\n");

  fail;
}
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Link all foreign language predicates.  The arguments to FRG are:

	FRG(name, arity, function, flags).

Flags almost always is TRACE_ME.  Additional common flags:

	P_TRANSPARENT	Predicate is module transparent
	P_NONDET	Predicate can be resatisfied

Deprecated: the modern interface uses PRED_IMPL()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define NOTRACE PL_FA_NOTRACE
#define META    PL_FA_TRANSPARENT
#define NDET	PL_FA_NONDETERMINISTIC
#define VA	PL_FA_VARARGS
#define CREF	PL_FA_CREF
#define ISO	PL_FA_ISO

#define FRG(n, a, f, flags) { n, a, f, flags }

static const PL_extension foreigns[] = {
#if O_DEBUG
  FRG("crash",			0, pl_crash,			0),
#endif
#ifdef __WINDOWS__
  FRG("win_exec",		2, pl_win_exec,			0),
  FRG("win_module_file",	2, pl_win_module_file,		0),
#endif

  FRG("halt",			1, pl_halt,		      ISO),
  FRG("getenv",			2, pl_getenv,			0),
  FRG("setenv",			2, pl_setenv,			0),
  FRG("unsetenv",		1, pl_unsetenv,			0),
  FRG("sub_atom",		5, pl_sub_atom,		 NDET|ISO),
  FRG("sleep",			1, pl_sleep,			0),
  FRG("break",			0, pl_break,			0),

  FRG("write_canonical",	1, pl_write_canonical,	      ISO),
  FRG("write_term",		2, pl_write_term,	 META|ISO),
  FRG("write_term",		3, pl_write_term3,	 META|ISO),
  FRG("write",			1, pl_write,		      ISO),
  FRG("writeq",			1, pl_writeq,		      ISO),
  FRG("writeln",		1, pl_writeln,		        0),
  FRG("print",			1, pl_print,			0),

  FRG("read",			1, pl_read,		      ISO),
  FRG("$raw_read",		1, pl_raw_read,			0),
  FRG("$raw_read",		2, pl_raw_read2,		0),
  FRG("current_functor",	2, pl_current_functor,	     NDET),
  FRG("char_conversion",	2, pl_char_conversion,	      ISO),
  FRG("current_char_conversion",2, pl_current_char_conversion, NDET|ISO),

  FRG("!",			0, pl_metacut,		      ISO),

  FRG("$rc_handle",		1, pl_rc_handle,		0),
  FRG("$rc_members",		2, pl_rc_members,		0),
  FRG("$rc_open",		5, pl_rc_open,			0),
  FRG("$rc_open_archive",	2, pl_rc_open_archive,		0),
  FRG("$rc_close_archive",	1, pl_rc_close_archive,		0),
  FRG("$rc_save_archive",	2, pl_rc_save_archive,		0),
  FRG("$rc_append_file",	5, pl_rc_append_file,		0),

  FRG("abolish",		1, pl_abolish1,		     META|ISO),
  FRG("abolish",		2, pl_abolish,		     META),
  FRG("nth_clause",		3, pl_nth_clause,       NDET|META|CREF),
  FRG("retractall",		1, pl_retractall,	 META|ISO),
#ifdef O_MAINTENANCE
  FRG("$list_generations",	1, pl_list_generations,	     META),
  FRG("$check_procedure",	1, pl_check_procedure,	     META),
#endif

  FRG("$c_current_predicate",	2, pl_current_predicate,  NDET|META),
  FRG("current_predicate",	1, pl_current_predicate1, NDET|META|ISO),
  FRG("$set_predicate_attribute", 3, pl_set_predicate_attribute,META),
  FRG("$get_predicate_attribute", 3, pl_get_predicate_attribute,META),
  FRG("$require",		1, pl_require,		     META),
  FRG("$default_predicate",	2, pl_default_predicate,     META),

  FRG("repeat",			0, pl_repeat,		 NDET|ISO),
  FRG("fail",			0, pl_fail,		      ISO),
  FRG("true",			0, pl_true,		      ISO),
  FRG("$fail",			0, pl_fail,		  NOTRACE),

  FRG("trace",			0, pl_trace,		  NOTRACE),
  FRG("notrace",		0, pl_notrace,		  NOTRACE),
  FRG("tracing",		0, pl_tracing,		  NOTRACE),
  FRG("$spy",			1, pl_spy,		     META),
  FRG("$nospy",			1, pl_nospy,		     META),
  FRG("$leash",			2, pl_leash,		  NOTRACE),
  FRG("$visible",		2, pl_visible,		  NOTRACE),
  FRG("$debuglevel",		2, pl_debuglevel,		0),

#if COUNTING
  FRG("$count",			0, pl_count,			0),
#endif /* COUNTING */

  FRG("prolog_current_frame",	1, pl_prolog_current_frame,	0),

  FRG("dwim_match",		3, pl_dwim_match,		0),
  FRG("$dwim_predicate",	2, pl_dwim_predicate,	     NDET),

#ifdef O_PROLOG_HOOK
  FRG("set_prolog_hook",	3, pl_set_prolog_hook,	        0),
#endif
  FRG("context_module",		1, pl_context_module,	     META),

#if O_STRING
  FRG("sub_string",		5, pl_sub_string,	     NDET),
#endif /* O_STRING */

  FRG("format",			2, pl_format,		     META),
#ifdef O_DEBUG
  FRG("$check_definition",	1, pl_check_definition,      META),
#endif

  FRG("$atom_hashstat",		2, pl_atom_hashstat,		0),
  FRG("$current_prolog_flag",	5, pl_prolog_flag5,	     NDET),
  FRG("current_prolog_flag",	2, pl_prolog_flag,	 NDET|ISO),
  FRG("$garbage_collect",	1, pl_garbage_collect,		0),
#ifdef O_ATOMGC
  FRG("garbage_collect_atoms",	0, pl_garbage_collect_atoms,	0),
  FRG("garbage_collect_clauses", 0, pl_garbage_collect_clauses,	0),
#ifdef O_DEBUG_ATOMGC
  FRG("track_atom",		2, pl_track_atom,		0),
#endif
#endif
  FRG("current_flag",		1, pl_current_flag,	     NDET),

  FRG("read",			2, pl_read2,		      ISO),
  FRG("write",			2, pl_write2,		      ISO),
  FRG("writeln",		2, pl_writeln2,		        0),
  FRG("writeq",			2, pl_writeq2,		      ISO),
  FRG("print",			2, pl_print2,			0),
  FRG("write_canonical",	2, pl_write_canonical2,	      ISO),
  FRG("format",			3, pl_format3,		     META),

  FRG("format_predicate",	2, pl_format_predicate,	     META),
  FRG("current_format_predicate", 2, pl_current_format_predicate,
						        META|NDET),
  FRG("get_time",		1, pl_get_time,			0),

#ifdef O_PLMT
  FRG("thread_create",		3, pl_thread_create,	 META|ISO),
  FRG("thread_exit",		1, pl_thread_exit,		0),
  FRG("thread_signal",		2, pl_thread_signal,	 META|ISO),
  FRG("thread_at_exit",		1, pl_thread_at_exit,	     META),
  FRG("open_xterm",		5, pl_open_xterm,		0),
#endif

  FRG("thread_self",		1, pl_thread_self,	      ISO),
  FRG("with_mutex",		2, pl_with_mutex,	 META|ISO),
  FRG("$get_pid",		1, pl_get_pid,			0),

  /* DO NOT ADD ENTRIES BELOW THIS ONE */
  FRG((char *)NULL,		0, (Func)NULL,			0)
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The extensions chain is used   to allow calling PL_register_extensions()
*before* PL_initialise() to get foreign   extensions in embedded systems
defined before the state is loaded, so executing directives in the state
can use foreign extensions.

If an extension is registered before the  system extension is loaded, it
will be added to the chain. Right  after the system registers the system
predicates, the extensions will be registered.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct extension_cell
{ PL_extension *extensions;
  char *module;
  ExtensionCell next;
};

#define ext_head		(GD->foreign._ext_head)
#define ext_tail		(GD->foreign._ext_tail)
#define extensions_loaded	(GD->foreign._loaded)

static char *
dupStr(const char *str)
{ if (str)
  { size_t len = strlen(str)+1;
    char *m = PL_malloc(len);
    memcpy(m, str, len);
    return m;
  }
  return NULL;
}


static PL_extension *
dupExtensions(const PL_extension *e)
{ int i;
  PL_extension *dup, *o;
  int len = 0;

  while(e[len++].predicate_name)
    ;
  o = dup = PL_malloc(len*sizeof(*e));

  for ( i=0; i<len; i++, o++, e++)
  { o->predicate_name = dupStr(e->predicate_name);
    o->arity = e->arity;
    o->function = e->function;
    o->flags = e->flags;
  }

  return dup;
}


void
rememberExtensions(const char *module, const PL_extension *e)
{ ExtensionCell cell = PL_malloc(sizeof *cell);

  cell->extensions = dupExtensions(e);
  cell->next = NULL;
  cell->module = dupStr(module);

  if ( ext_tail )
  { ext_tail->next = cell;
    ext_tail = cell;
  } else
  { ext_head = ext_tail = cell;
  }
}


void
cleanupExtensions(void)
{ ExtensionCell c, next;

  for(c=ext_head; c; c=next)
  { next = c->next;
    if (c->module)
      PL_free(c->module);

    if (c->extensions)
    { PL_extension *e = c->extensions;

      for(;e->predicate_name; e++)
	PL_free((void *)e->predicate_name);

      PL_free(c->extensions);
    }

    PL_free(c);
  }

  ext_head = ext_tail = NULL;
}



static void
registerBuiltins(const PL_extension *f)
{ Module m = MODULE_system;

  for(; f->predicate_name; f++)
  { Procedure proc;
    atom_t name	= PL_new_atom(f->predicate_name);
    functor_t fdef = lookupFunctorDef(name, f->arity);

    PL_unregister_atom(name);
    if ( (proc = lookupProcedure(fdef, m)) )
    { Definition def = proc->definition;
      set(def, P_FOREIGN|HIDE_CHILDS|P_LOCKED);

      if ( f->flags & PL_FA_NOTRACE )	       clear(def, TRACE_ME);
      if ( f->flags & PL_FA_TRANSPARENT )      set(def, P_TRANSPARENT);
      if ( f->flags & PL_FA_NONDETERMINISTIC ) set(def, P_NONDET);
      if ( f->flags & PL_FA_VARARGS )	       set(def, P_VARARG);
      if ( f->flags & PL_FA_CREF )	       set(def, P_FOREIGN_CREF);
      if ( f->flags & PL_FA_ISO )	       set(def, P_ISO);

      def->impl.function = f->function;
      createForeignSupervisor(def, f->function);
    } else
    { assert(0);
    }
  }
}


#define DECL_PLIST(id) \
	extern const PL_extension PL_predicates_from_ ## id[]
#define REG_PLIST(id) \
	registerBuiltins(PL_predicates_from_ ## id)

DECL_PLIST(alloc);
DECL_PLIST(atom);
DECL_PLIST(arith);
DECL_PLIST(bag);
DECL_PLIST(comp);
DECL_PLIST(flag);
DECL_PLIST(index);
DECL_PLIST(list);
DECL_PLIST(module);
DECL_PLIST(prims);
DECL_PLIST(strings);
DECL_PLIST(variant);
DECL_PLIST(copyterm);
DECL_PLIST(prologflag);
DECL_PLIST(trace);
DECL_PLIST(pro);
DECL_PLIST(read);
DECL_PLIST(thread);
DECL_PLIST(profile);
DECL_PLIST(wic);
DECL_PLIST(attvar);
DECL_PLIST(gvar);
DECL_PLIST(win);
DECL_PLIST(file);
DECL_PLIST(files);
DECL_PLIST(glob);
DECL_PLIST(btree);
DECL_PLIST(ctype);
DECL_PLIST(tai);
DECL_PLIST(setup);
DECL_PLIST(gc);
DECL_PLIST(proc);
DECL_PLIST(srcfile);
DECL_PLIST(write);
DECL_PLIST(dlopen);
DECL_PLIST(system);
DECL_PLIST(op);
DECL_PLIST(rec);
DECL_PLIST(termhash);
DECL_PLIST(dde);
DECL_PLIST(term);
DECL_PLIST(debug);
DECL_PLIST(locale);
DECL_PLIST(dict);
DECL_PLIST(cont);
DECL_PLIST(trie);
DECL_PLIST(tabling);
DECL_PLIST(mutex);

void
initBuildIns(void)
{ ExtensionCell ecell;
  Module m = MODULE_system;

  GD->procedures.dirty = newHTable(32);

  registerBuiltins(foreigns);
  REG_PLIST(alloc);
  REG_PLIST(atom);
  REG_PLIST(arith);
  REG_PLIST(bag);
  REG_PLIST(comp);
  REG_PLIST(flag);
  REG_PLIST(index);
  REG_PLIST(list);
  REG_PLIST(module);
  REG_PLIST(prims);
  REG_PLIST(strings);
  REG_PLIST(variant);
  REG_PLIST(copyterm);
  REG_PLIST(prologflag);
  REG_PLIST(trace);
  REG_PLIST(pro);
  REG_PLIST(read);
  REG_PLIST(thread);
  REG_PLIST(profile);
  REG_PLIST(wic);
  REG_PLIST(file);
  REG_PLIST(files);
  REG_PLIST(glob);
  REG_PLIST(btree);
  REG_PLIST(ctype);
  REG_PLIST(tai);
  REG_PLIST(setup);
  REG_PLIST(gc);
  REG_PLIST(proc);
  REG_PLIST(srcfile);
  REG_PLIST(write);
  REG_PLIST(dlopen);
  REG_PLIST(system);
  REG_PLIST(op);
  REG_PLIST(rec);
  REG_PLIST(term);
  REG_PLIST(termhash);
#ifdef O_ATTVAR
  REG_PLIST(attvar);
#endif
#ifdef O_GVAR
  REG_PLIST(gvar);
#endif
#ifdef __WINDOWS__
  REG_PLIST(win);
  REG_PLIST(dde);
#endif
#ifdef O_LOCALE
  REG_PLIST(locale);
#endif
  REG_PLIST(debug);
  REG_PLIST(dict);
  REG_PLIST(cont);
  REG_PLIST(trie);
  REG_PLIST(tabling);
  REG_PLIST(mutex);

#define LOOKUPPROC(name) \
	{ GD->procedures.name = lookupProcedure(FUNCTOR_ ## name, m); \
	  DEBUG(CHK_SECURE, assert(GD->procedures.name)); \
	}

  LOOKUPPROC(dgarbage_collect1);
  LOOKUPPROC(catch3);
  LOOKUPPROC(reset3);
  LOOKUPPROC(dmeta_call1);
  LOOKUPPROC(true0);
  LOOKUPPROC(fail0);
  LOOKUPPROC(equals2);
  LOOKUPPROC(is2);
  LOOKUPPROC(strict_equal2);
  LOOKUPPROC(not_strict_equal2);
  LOOKUPPROC(print_message2);
  LOOKUPPROC(dcall1);
  LOOKUPPROC(setup_call_catcher_cleanup4);
  LOOKUPPROC(dthread_init0);
  LOOKUPPROC(dc_call_prolog0);
  LOOKUPPROC(dinit_goal3);
#ifdef O_ATTVAR
  LOOKUPPROC(dwakeup1);
#endif
#if O_DEBUGGER
  PROCEDURE_event_hook1 =
	PL_predicate("prolog_event_hook", 1, "user");
#endif
  PROCEDURE_exception_hook4  =
	PL_predicate("prolog_exception_hook", 4, "user");
					/* allow debugging in call/1 */
  clear(PROCEDURE_dcall1->definition, HIDE_CHILDS|TRACE_ME);
  set(PROCEDURE_dcall1->definition, P_DYNAMIC|P_LOCKED);

  PL_meta_predicate(PL_predicate("assert",           1, "system"), ":");
  PL_meta_predicate(PL_predicate("asserta",          1, "system"), ":");
  PL_meta_predicate(PL_predicate("assertz",          1, "system"), ":");
  PL_meta_predicate(PL_predicate("assert",           2, "system"), ":-");
  PL_meta_predicate(PL_predicate("asserta",          2, "system"), ":-");
  PL_meta_predicate(PL_predicate("assertz",          2, "system"), ":-");
  PL_meta_predicate(PL_predicate("retract",          1, "system"), ":");
  PL_meta_predicate(PL_predicate("retractall",       1, "system"), ":");
  PL_meta_predicate(PL_predicate("clause",           2, "system"), ":?");

  PL_meta_predicate(PL_predicate("format",           2, "system"), "+:");
  PL_meta_predicate(PL_predicate("format",           3, "system"), "++:");
  PL_meta_predicate(PL_predicate("format_predicate", 2, "system"), "+0");

  PL_meta_predicate(PL_predicate("notrace",          1, "system"), "0");
  PL_meta_predicate(PL_predicate("with_mutex",       2, "system"), "+0");
  PL_meta_predicate(PL_predicate("with_output_to",   2, "system"), "+0");
#ifdef O_PLMT
  PL_meta_predicate(PL_predicate("thread_create",    3, "system"), "0?+");
  PL_meta_predicate(PL_predicate("thread_at_exit",   1, "system"), "0");
  PL_meta_predicate(PL_predicate("thread_signal",    2, "system"), "+0");
#endif
  PL_meta_predicate(PL_predicate("prolog_frame_attribute", 3, "system"), "++:");
  PL_meta_predicate(PL_predicate("compile_predicates", 1, "system"), ":");

  for( ecell = ext_head; ecell; ecell = ecell->next )
    bindExtensions(ecell->module, ecell->extensions);

  extensions_loaded = TRUE;
}
