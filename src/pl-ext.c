/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: link built_in predicates
*/

#include "pl-incl.h"
#include "pl-itf.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Link all foreign language predicates.  The arguments to ADD are:

	ADD(name, arity, function, flags).

Flags almost always is TRACE_ME.  Additional common flags:

	TRANSPARENT		Predicate is module transparent
	NONDETERMINISTIC	Predicate can be resatisfied
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ADD(n, a, f, flags) { n, f, flags, a }

static struct foreign {
  char	*name;
  Func	 function;
  short	 flags;
  short	 arity;
} foreigns[] = {
  ADD("nl",			0, pl_nl,			TRACE_ME),
  ADD("put",			1, pl_put,			TRACE_ME),
  ADD("get0",			1, pl_get0,			TRACE_ME),
  ADD("get",			1, pl_get,			TRACE_ME),
  ADD("get_single_char",	1, pl_get_single_char,		TRACE_ME),
  ADD("seeing",			1, pl_seeing,			TRACE_ME),
  ADD("telling",		1, pl_telling,			TRACE_ME),
  ADD("seen",			0, pl_seen,			TRACE_ME),
  ADD("tab",			1, pl_tab,			TRACE_ME),
  ADD("tmp_file",		2, pl_tmp_file,			TRACE_ME),
  ADD("delete_file",		1, pl_delete_file,		TRACE_ME),
  ADD("access_file",		2, pl_access_file,		TRACE_ME),
  ADD("read_link",		3, pl_read_link,		TRACE_ME),
  ADD("exists_file",		1, pl_exists_file,		TRACE_ME),
  ADD("exists_directory",	1, pl_exists_directory,		TRACE_ME),
  ADD("rename_file",		2, pl_rename_file,		TRACE_ME),
  ADD("same_file",		2, pl_same_file,		TRACE_ME),
  ADD("time_file",		2, pl_time_file,		TRACE_ME),
  ADD("told",			0, pl_told,			TRACE_ME),
  ADD("see",			1, pl_see,			TRACE_ME),
  ADD("tell",			1, pl_tell,			TRACE_ME),
  ADD("append",			1, pl_append,			TRACE_ME),
  ADD("ttyflush",		0, pl_ttyflush,			TRACE_ME),
  ADD("flush",			0, pl_flush,			TRACE_ME),
  ADD("tty_fold",		2, pl_tty_fold,			TRACE_ME),
  ADD("prompt",			2, pl_prompt,			TRACE_ME),
  ADD("prompt1",		1, pl_prompt1,			TRACE_ME),
  ADD("expand_file_name",	2, pl_expand_file_name,		TRACE_ME),
  ADD("absolute_file_name",	2, pl_absolute_file_name,	TRACE_ME),
  ADD("$file_base_name",	2, pl_file_base_name,		TRACE_ME),
  ADD("$file_dir_name",		2, pl_file_dir_name,		TRACE_ME),
  ADD("fileerrors",		2, pl_fileerrors,		TRACE_ME),
  ADD("chdir",			1, pl_chdir,			TRACE_ME),

  ADD("halt",			0, pl_halt,			TRACE_ME),
  ADD("$shell",			2, pl_shell,			TRACE_ME),
  ADD("getenv",			2, pl_getenv,			TRACE_ME),
  ADD("setenv",			2, pl_setenv,			TRACE_ME),
  ADD("unsetenv",		1, pl_unsetenv,			TRACE_ME),
  ADD("wildcard_match",		2, pl_wildcard_match,		TRACE_ME),
  ADD("$apropos_match",		2, pl_apropos_match,		TRACE_ME),
  ADD("$argv",			1, pl_argv,			TRACE_ME),
  ADD("$option",		3, pl_option,			TRACE_ME),
  ADD("convert_time",		8, pl_convert_time,		TRACE_ME),
  ADD("sleep",			1, pl_sleep,			TRACE_ME),
  ADD("break",			0, pl_break,			TRACE_ME),
  ADD("$break",			1, pl_break1,			TRACE_ME),

  ADD("display",		1, pl_display,			TRACE_ME),
  ADD("displayq",		1, pl_displayq,			TRACE_ME),
  ADD("write",			1, pl_write,			TRACE_ME),
  ADD("writeq",			1, pl_writeq,			TRACE_ME),
  ADD("print",			1, pl_print,			TRACE_ME),
  ADD("$print",			2, pl_dprint,		TRANSPARENT|TRACE_ME),

  ADD("read_variables",		2, pl_read_variables,		TRACE_ME),
  ADD("read_variables",		3, pl_read_variables3,		TRACE_ME),
  ADD("read",			1, pl_read,			TRACE_ME),
  ADD("read_clause",		1, pl_read_clause,		TRACE_ME),
  ADD("read_clause",		2, pl_read_clause2,		TRACE_ME),
  ADD("$raw_read",		1, pl_raw_read,		GC_SAVE|TRACE_ME),
  ADD("current_op",		3, pl_current_op,	NONDETERMINISTIC|TRACE_ME),
  ADD("current_atom",		1, pl_current_atom,	NONDETERMINISTIC|TRACE_ME),
  ADD("current_functor",	2, pl_current_functor,	NONDETERMINISTIC|TRACE_ME),
  ADD("$complete_atom",		3, pl_complete_atom,		TRACE_ME),
  ADD("$atom_completions",	2, pl_atom_completions,		TRACE_ME),
  ADD("$op",			3, pl_op1,			TRACE_ME),

  ADD("!",			0, pl_metacut,			TRACE_ME),
  ADD("functor",		3, pl_functor,			TRACE_ME),
  ADD("arg",			3, pl_arg,     NONDETERMINISTIC|TRACE_ME),
  ADD("=..",			2, pl_univ,			TRACE_ME),
  ADD("name",			2, pl_name,			TRACE_ME),
  ADD("int_to_atom",		3, pl_int_to_atom,		TRACE_ME),
  ADD("format_number",		3, pl_format_number,		TRACE_ME),
  ADD("concat",			3, pl_concat,			TRACE_ME),
  ADD("$concat_atom",		2, pl_concat_atom,		TRACE_ME),
  ADD("atom_length",		2, pl_atom_length,		TRACE_ME),
  ADD("$term_to_atom",		3, pl_term_to_atom,		TRACE_ME),
  ADD("numbervars",		4, pl_numbervars,		TRACE_ME),
  ADD("free_variables",		2, pl_free_variables,		TRACE_ME),
  ADD("$e_free_variables",	2, pl_e_free_variables,		TRACE_ME),

  ADD("$open_wic",		1, pl_open_wic,			TRACE_ME),
  ADD("$close_wic",		0, pl_close_wic,		TRACE_ME),
  ADD("$add_clause_wic",	2, pl_add_clause_wic,		TRACE_ME),
  ADD("$add_directive_wic",	1, pl_add_directive_wic,	TRACE_ME),
  ADD("$start_module_wic",	2, pl_start_module_wic,		TRACE_ME),
  ADD("$export_wic",		2, pl_export_wic,		TRACE_ME),
  ADD("$import_wic",		3, pl_import_wic,		TRACE_ME),

  ADD("abolish",    2, pl_abolish,    TRANSPARENT|TRACE_ME),
  ADD("$clause",    3, pl_clause,     NONDETERMINISTIC|TRANSPARENT|TRACE_ME),
  ADD("nth_clause", 3, pl_nth_clause, NONDETERMINISTIC|TRANSPARENT|TRACE_ME),
  ADD("retract",    1, pl_retract,    NONDETERMINISTIC|TRANSPARENT|TRACE_ME),
  ADD("$xr_member", 2, pl_xr_member,  NONDETERMINISTIC|TRACE_ME),

  ADD("flag",			3, pl_flag,			TRACE_ME),
  ADD("recorda",		3, pl_recorda,			TRACE_ME),
  ADD("recordz",		3, pl_recordz,			TRACE_ME),
  ADD("recorded",		3, pl_recorded,	NONDETERMINISTIC|TRACE_ME),
  ADD("erase",			1, pl_erase,			TRACE_ME),
  ADD("assert",			1, pl_assertz,		TRANSPARENT|TRACE_ME),
  ADD("asserta",		1, pl_asserta,		TRANSPARENT|TRACE_ME),
  ADD("assertz",		1, pl_assertz,		TRANSPARENT|TRACE_ME),
  ADD("assert",			2, pl_assertz2,		TRANSPARENT|TRACE_ME),
  ADD("asserta",		2, pl_asserta2,		TRANSPARENT|TRACE_ME),
  ADD("assertz",		2, pl_assertz2,		TRANSPARENT|TRACE_ME),
  ADD("$record_clause",		2, pl_record_clause,		TRACE_ME),

  ADD("$c_current_predicate",	2, pl_current_predicate,  NONDETERMINISTIC|TRANSPARENT|TRACE_ME),
  ADD("$predicate_attribute",	3, pl_predicate_attribute,TRANSPARENT|TRACE_ME),
  ADD("$source_file",		2, pl_source_file,	  TRANSPARENT|TRACE_ME),
  ADD("$time_source_file",	2, pl_time_source_file,	  NONDETERMINISTIC|TRACE_ME),
  ADD("$start_consult",		1, pl_start_consult,		TRACE_ME),
  ADD("$make_system_source_files",0,pl_make_system_source_files,TRACE_ME),

  ADD("var",			1, pl_var,			TRACE_ME),
  ADD("nonvar",			1, pl_nonvar,			TRACE_ME),
  ADD("integer",		1, pl_integer,			TRACE_ME),
  ADD("float",			1, pl_float,			TRACE_ME),
  ADD("number",			1, pl_number,			TRACE_ME),
  ADD("atom",			1, pl_atom,			TRACE_ME),
  ADD("atomic",			1, pl_atomic,			TRACE_ME),
  ADD("ground",			1, pl_ground,			TRACE_ME),

  ADD("==",			2, pl_equal,			TRACE_ME),
  ADD("\\==",			2, pl_nonequal,			TRACE_ME),
  ADD("=",			2, pl_unify,			TRACE_ME),
  ADD("\\=",			2, pl_notunify,			TRACE_ME),
  ADD("compare",		3, pl_compare,			TRACE_ME),
  ADD("@<",			2, pl_lessStandard,		TRACE_ME),
  ADD("@=<",			2, pl_lessEqualStandard,	TRACE_ME),
  ADD("@>",			2, pl_greaterStandard,		TRACE_ME),
  ADD("@>=",			2, pl_greaterEqualStandard,	TRACE_ME),
  ADD("=@=",			2, pl_structural_equal,		TRACE_ME),
  ADD("\\=@=",			2, pl_structural_nonequal,	TRACE_ME),

  ADD("repeat",			0, pl_repeat,	NONDETERMINISTIC|TRACE_ME),
  ADD("fail",			0, pl_fail,			TRACE_ME),
  ADD("$fail",			0, pl_fail,			0),
  ADD("abort",			0, pl_abort,			TRACE_ME),

  ADD("statistics",		2, pl_statistics,		TRACE_ME),

  ADD("between",		3, pl_between,	NONDETERMINISTIC|TRACE_ME),
  ADD("succ",			2, pl_succ,			TRACE_ME),
  ADD("plus",			3, pl_plus,			TRACE_ME),
  ADD("<",			2, pl_lessNumbers,		TRACE_ME),
  ADD(">",			2, pl_greaterNumbers,		TRACE_ME),
  ADD("=<",			2, pl_lessEqualNumbers,		TRACE_ME),
  ADD(">=",			2, pl_greaterEqualNumbers,	TRACE_ME),
  ADD("=\\=",			2, pl_nonEqualNumbers,		TRACE_ME),
  ADD("=:=",			2, pl_equalNumbers,		TRACE_ME),
  ADD("is",			2, pl_is,		TRANSPARENT|TRACE_ME),

  ADD("trace",			0, pl_trace,			0),
  ADD("notrace",		0, pl_notrace,			0),
  ADD("tracing",		0, pl_tracing,			0),
  ADD("debug",			0, pl_debug,			0),
  ADD("nodebug",		0, pl_nodebug,			0),
  ADD("$debugging",		0, pl_debugging,		0),
  ADD("$spy",			1, pl_spy,		TRANSPARENT|TRACE_ME),
  ADD("$nospy",			1, pl_nospy,		TRANSPARENT|TRACE_ME),
  ADD("$leash",			2, pl_leash, 			0),
  ADD("$visible",		2, pl_visible,			0),
  ADD("unknown",		2, pl_unknown,		TRANSPARENT|TRACE_ME),
  ADD("$style_check",		2, pl_style_check,		TRACE_ME),

  ADD("$list_references",	2, pl_list_references,		TRACE_ME),
  ADD("$list_active_procedures",0, pl_list_active_procedures,	TRACE_ME),
#if COUNTING
  ADD("$count",			0, pl_count,			TRACE_ME),
#endif /* COUNTING */

  ADD("$profile",		2, pl_profile,			TRACE_ME),
  ADD("reset_profiler",		0, pl_reset_profiler,		TRACE_ME),
  ADD("profile_count",		3, pl_profile_count,	TRANSPARENT|TRACE_ME),
  ADD("profile_box",		5, pl_profile_box,	TRANSPARENT|TRACE_ME),

  ADD("prolog_current_frame",	1, pl_prolog_current_frame,	TRACE_ME),
  ADD("prolog_frame_attribute",	3, pl_prolog_frame_attribute,	TRACE_ME),
  ADD("$trace_continuation",	1, pl_trace_continuation,	TRACE_ME),
  ADD("prolog_skip_level",	2, pl_skip_level,		0),

  ADD("$write_on_atom",		2, pl_write_on_atom,		TRACE_ME),
#if O_STRING
  ADD("$write_on_string",	2, pl_write_on_string,		TRACE_ME),
#endif
  ADD("$write_on_list",		2, pl_write_on_list, 		TRACE_ME),
  ADD("dwim_match",		3, pl_dwim_match,		TRACE_ME),
  ADD("$dwim_predicate",	2, pl_dwim_predicate,	NONDETERMINISTIC|TRACE_ME),

  ADD("$novice",		2, pl_novice,			TRACE_ME),
  ADD("protocol",		1, pl_protocol,			TRACE_ME),
  ADD("protocola",		1, pl_protocola,		TRACE_ME),
  ADD("noprotocol",		0, pl_noprotocol,		TRACE_ME),
  ADD("protocolling",		1, pl_protocolling,		TRACE_ME),

  ADD("$current_module",	2, pl_current_module,	NONDETERMINISTIC|TRACE_ME),
  ADD("$module",		2, pl_module,			TRACE_ME),
  ADD("$set_source_module",	2, pl_set_source_module,	TRACE_ME),
  ADD("$declare_module",	2, pl_declare_module,		TRACE_ME),
  ADD("context_module",		1, pl_context_module,	TRANSPARENT|TRACE_ME),
  ADD("$strip_module",		3, pl_strip_module,	TRANSPARENT|TRACE_ME),
  ADD("import",			1, pl_import,		TRANSPARENT|TRACE_ME),
  ADD("export",			1, pl_export,		TRANSPARENT|TRACE_ME),
  ADD("$check_export",		0, pl_check_export,	TRANSPARENT|TRACE_ME),
  ADD("export_list",		2, pl_export_list,		TRACE_ME),
  ADD("index",			1, pl_index,		TRANSPARENT|TRACE_ME),
#if O_AIX_FOREIGN
  ADD("$load_foreign",		1, pl_load_foreign1,	TRANSPARENT|TRACE_ME),
#else
  ADD("$load_foreign",		5, pl_load_foreign,	TRANSPARENT|TRACE_ME),
#endif
#if HAVE_DLOPEN
  ADD("open_shared_object",	2, pl_open_shared_object,	TRACE_ME),
  ADD("close_shared_object",	1, pl_close_shared_object,	TRACE_ME),
  ADD("call_shared_object_function",
				2, pl_call_shared_object_function,
							TRANSPARENT|TRACE_ME),
#endif /*HAVE_DLOPEN*/

#if O_DDE
  ADD("open_dde_conversation",	3, pl_open_dde_conversation,	TRACE_ME),
  ADD("close_dde_conversation",	1, pl_close_dde_conversation,	TRACE_ME),
  ADD("dde_request",		3, pl_dde_request,		TRACE_ME),
#endif

#if O_STRING
  ADD("string",			1, pl_string,			TRACE_ME),
  ADD("string_length",		2, pl_string_length,		TRACE_ME),
  ADD("string_to_atom",		2, pl_string_to_atom,		TRACE_ME),
  ADD("string_to_list",		2, pl_string_to_list,		TRACE_ME),
  ADD("substring",		4, pl_substring,		TRACE_ME),
#endif /* O_STRING */

  ADD("save",			2, pl_save,			TRACE_ME),
  ADD("restore",		1, pl_restore,			TRACE_ME),
  ADD("$save_program",		2, pl_save_program,		TRACE_ME),

  ADD("is_list",		1, pl_is_list,			TRACE_ME),
  ADD("proper_list",		1, pl_proper_list,		TRACE_ME),
  ADD("$length",		2, pl_length,			TRACE_ME),
  ADD("memberchk",		2, pl_memberchk,		TRACE_ME),
  ADD("msort",			2, pl_msort,			TRACE_ME),
  ADD("sort",			2, pl_sort,			TRACE_ME),
  ADD("format",			2, pl_format,			TRACE_ME),
  ADD("$collect_bag",		2, pl_collect_bag,	GC_SAVE|TRACE_ME),
  ADD("$record_bag",		2, pl_record_bag,		TRACE_ME),
  ADD("$please",		3, pl_please,			TRACE_ME),

#if O_COMPILE_OR
  ADD("$alt",			1, pl_alt,		NONDETERMINISTIC),
#endif /* O_COMPILE_OR */
  ADD("$atom_hashstat",		2, pl_atom_hashstat,		TRACE_ME),
  ADD("$tty",			0, pl_tty,			TRACE_ME),
  ADD("$grep",			3, pl_grep,	NONDETERMINISTIC|TRACE_ME),
  ADD("feature",		2, pl_feature,	NONDETERMINISTIC|TRACE_ME),
  ADD("limit_stack",		2, pl_limit_stack,		TRACE_ME),
  ADD("trim_stacks",		0, pl_trim_stacks,		TRACE_ME),
#if O_SHIFT_STACKS
  ADD("stack_parameter",	4, pl_stack_parameter,		TRACE_ME),
#endif
  ADD("$garbage_collect",	1, pl_garbage_collect,		TRACE_ME),
  ADD("$collect_parms",		2, pl_collect_parms,		TRACE_ME),
  ADD("copy_term",		2, pl_copy_term,		TRACE_ME),
  ADD("current_key",		1, pl_current_key,	NONDETERMINISTIC|TRACE_ME),
  ADD("current_flag",		1, pl_current_flag,	NONDETERMINISTIC|TRACE_ME),

  ADD("open",			3, pl_open,			TRACE_ME),
  ADD("open_null_stream",	1, pl_open_null_stream,		TRACE_ME),
  ADD("close",			1, pl_close,			TRACE_ME),
  ADD("current_stream",		3, pl_current_stream,	NONDETERMINISTIC|TRACE_ME),
  ADD("flush_output",		1, pl_flush_output,		TRACE_ME),
  ADD("stream_position",	3, pl_stream_position,		TRACE_ME),
  ADD("set_input",		1, pl_set_input,		TRACE_ME),
  ADD("set_output",		1, pl_set_output,		TRACE_ME),
  ADD("current_input",		1, pl_current_input,		TRACE_ME),
  ADD("current_output",		1, pl_current_output,		TRACE_ME),
  ADD("character_count",	2, pl_character_count,		TRACE_ME),
  ADD("line_count",		2, pl_line_count,		TRACE_ME),
  ADD("line_position",		2, pl_line_position,		TRACE_ME),
  ADD("source_location",	2, pl_source_location,		TRACE_ME),

  ADD("nl",			1, pl_nl1,			TRACE_ME),
  ADD("tab",			2, pl_tab2,			TRACE_ME),
  ADD("put",			2, pl_put2,			TRACE_ME),
  ADD("get",			2, pl_get2,			TRACE_ME),
  ADD("get0",			2, pl_get02,			TRACE_ME),
  ADD("read",			2, pl_read2,			TRACE_ME),
  ADD("write",			2, pl_write2,			TRACE_ME),
  ADD("writeq",			2, pl_writeq2,			TRACE_ME),
  ADD("print",			2, pl_print2,			TRACE_ME),
  ADD("display",		2, pl_display2,			TRACE_ME),
  ADD("displayq",		2, pl_displayq2,		TRACE_ME),
  ADD("format",			3, pl_format3,			TRACE_ME),

  ADD("tty_get_capability",	3, pl_tty_get_capability,	TRACE_ME),
  ADD("tty_goto",		2, pl_tty_goto,			TRACE_ME),
  ADD("tty_put",		2, pl_tty_put,			TRACE_ME),
  ADD("format_predicate",	2, pl_format_predicate,	TRANSPARENT|TRACE_ME),
  ADD("set_tty",		2, pl_set_tty,			TRACE_ME),
  ADD("wait_for_input",		3, pl_wait_for_input,		TRACE_ME),
  ADD("get_time",		1, pl_get_time,			TRACE_ME),
  ADD("size_file",		2, pl_size_file,		TRACE_ME),
  ADD("$default_module",	3, pl_default_module,	TRANSPARENT|TRACE_ME),
#if O_PROLOG_FUNCTIONS
  ADD("$arithmetic_function",   1,pl_arithmetic_function,TRANSPARENT|TRACE_ME),
#endif
  ADD("current_arithmetic_function", 1, pl_current_arithmetic_function,
				       NONDETERMINISTIC|TRANSPARENT|TRACE_ME),

  /* DO NOT ADD ENTRIES BELOW THIS ONE */
  ADD((char *)NULL,		0, (Func)NULL,			0)
};

void
initBuildIns(void)
{ struct foreign *f;
  register Definition def;
  PL_extension *e;

  for(f = &foreigns[0]; f->name; f++)
  { def = lookupProcedure(lookupFunctorDef(lookupAtom(f->name), f->arity), 
					         MODULE_system)->definition;
    set(def, FOREIGN|SYSTEM);
    clear(def, TRACE_ME);
    set(def, f->flags);
    def->definition.function = f->function;
    def->indexPattern = 0;
    def->indexCardinality = 0;
  }

  PROCEDURE_alt1 = lookupProcedure(FUNCTOR_alt1, MODULE_system);
  PROCEDURE_garbage_collect0 = lookupProcedure(FUNCTOR_garbage_collect0,
					       MODULE_system);
  PROCEDURE_block3 = lookupProcedure(FUNCTOR_block3, MODULE_system);

  for(e = &PL_extensions[0]; e->predicate_name; e++)
  { short flags = TRACE_ME;

    if ( e->flags & PL_FA_NOTRACE )	     flags &= ~TRACE_ME;
    if ( e->flags & PL_FA_TRANSPARENT )	     flags |= TRANSPARENT;
    if ( e->flags & PL_FA_NONDETERMINISTIC ) flags |= NONDETERMINISTIC;
    if ( e->flags & PL_FA_GCSAVE )	     flags |= GC_SAVE;

    def = lookupProcedure(lookupFunctorDef(lookupAtom(e->predicate_name),
					   e->arity), 
			  MODULE_user)->definition;
    set(def, FOREIGN);
    set(def, flags);
    def->definition.function = e->function;
    def->indexPattern = 0;
    def->indexCardinality = 0;
  }    
}
