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

/*#define O_DEBUG 1*/			/* include crash/0 */
#include "pl-incl.h"
#include "pl-ctype.h"

#if O_DEBUG
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See how the system reacts on segmentation faults.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
pl_crash()
{ long *lp = NULL;

  Sdprintf("You asked for it ... Writing to address 0\n");

  *lp = 5;

  Sdprintf("Oops, this doesn't appear to be a protected OS\n");

  fail;
}
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Link all foreign language predicates.  The arguments to FRG are:

	FRG(name, arity, function, flags).

Flags almost always is TRACE_ME.  Additional common flags:

	METAPRED		Predicate is module transparent
	NONDETERMINISTIC	Predicate can be resatisfied
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define NDET NONDETERMINISTIC	/* make a bit shorter */
#define META METAPRED		/* same */

#define FRG(n, a, f, flags) { n, f, flags, a }

static const struct foreign {
  const char *  name;
  Func		function;
  unsigned long flags;
  int		arity;
} foreigns[] = {
#if O_DEBUG
  FRG("crash",			0, pl_crash,			TRACE_ME),
#endif
  FRG("nl",			0, pl_nl,			TRACE_ME),
  FRG("put",			1, pl_put,			TRACE_ME),
  FRG("put_byte",		1, pl_put,			TRACE_ME),
  FRG("put_code",		1, pl_put,			TRACE_ME),
  FRG("put_char",		1, pl_put,			TRACE_ME),
  FRG("get0",			1, pl_get_byte,			TRACE_ME),
  FRG("get_byte",		1, pl_get_byte,			TRACE_ME),
  FRG("get_code",		1, pl_get_byte,			TRACE_ME),
  FRG("get_char",		1, pl_get_char,			TRACE_ME),
  FRG("get",			1, pl_get,			TRACE_ME),
  FRG("skip",			1, pl_skip,			TRACE_ME),
  FRG("skip",			2, pl_skip2,			TRACE_ME),
  FRG("get_single_char",	1, pl_get_single_char,		TRACE_ME),
  FRG("$push_input_context",	0, pl_push_input_context,	TRACE_ME),
  FRG("$pop_input_context",	0, pl_pop_input_context,	TRACE_ME),
  FRG("seeing",			1, pl_seeing,			TRACE_ME),
  FRG("telling",		1, pl_telling,			TRACE_ME),
  FRG("seen",			0, pl_seen,			TRACE_ME),
  FRG("tab",			1, pl_tab,			TRACE_ME),
  FRG("tmp_file",		2, pl_tmp_file,			TRACE_ME),
  FRG("delete_file",		1, pl_delete_file,		TRACE_ME),
  FRG("delete_directory",	1, pl_delete_directory,		TRACE_ME),
  FRG("make_directory",		1, pl_make_directory,		TRACE_ME),
  FRG("access_file",		2, pl_access_file,		TRACE_ME),
  FRG("read_link",		3, pl_read_link,		TRACE_ME),
  FRG("exists_file",		1, pl_exists_file,		TRACE_ME),
  FRG("exists_directory",	1, pl_exists_directory,		TRACE_ME),
  FRG("rename_file",		2, pl_rename_file,		TRACE_ME),
  FRG("same_file",		2, pl_same_file,		TRACE_ME),
  FRG("time_file",		2, pl_time_file,		TRACE_ME),
  FRG("told",			0, pl_told,			TRACE_ME),
  FRG("see",			1, pl_see,			TRACE_ME),
  FRG("tell",			1, pl_tell,			TRACE_ME),
  FRG("append",			1, pl_append,			TRACE_ME),
  FRG("ttyflush",		0, pl_ttyflush,			TRACE_ME),
  FRG("flush_output",		0, pl_flush_output,		TRACE_ME),
  FRG("prompt",			2, pl_prompt,			TRACE_ME),
  FRG("prompt1",		1, pl_prompt1,			TRACE_ME),
  FRG("expand_file_name",	2, pl_expand_file_name,		TRACE_ME),
  FRG("$absolute_file_name",	2, pl_absolute_file_name,	TRACE_ME),
  FRG("is_absolute_file_name",	1, pl_is_absolute_file_name,	TRACE_ME),
  FRG("file_base_name",		2, pl_file_base_name,		TRACE_ME),
  FRG("file_directory_name",	2, pl_file_dir_name,		TRACE_ME),
  FRG("file_name_extension",	3, pl_file_name_extension,	TRACE_ME),
  FRG("prolog_to_os_filename",	2, pl_prolog_to_os_filename,	TRACE_ME),
  FRG("$mark_executable",	1, pl_mark_executable,		TRACE_ME),
#ifdef __WIN32__
  FRG("win_exec",		2, pl_win_exec,			TRACE_ME),
  FRG("win_shell",		2, pl_shell_execute,		TRACE_ME),
  FRG("win_module_file",	2, pl_win_module_file,		TRACE_ME),
  FRG("win_registry_get_value", 3, pl_get_registry_value,	TRACE_ME),
#ifdef O_XOS
  FRG("make_fat_filemap",	1, pl_make_fat_filemap,		TRACE_ME),
#endif
#endif

  FRG("$on_signal",		4, pl_on_signal,	   META|TRACE_ME),

  FRG("fileerrors",		2, pl_fileerrors,		TRACE_ME),
  FRG("working_directory",	2, pl_working_directory,	TRACE_ME),

  FRG("halt",			1, pl_halt,			TRACE_ME),
  FRG("$shell",			2, pl_shell,			TRACE_ME),
  FRG("getenv",			2, pl_getenv,			TRACE_ME),
  FRG("setenv",			2, pl_setenv,			TRACE_ME),
  FRG("unsetenv",		1, pl_unsetenv,			TRACE_ME),
  FRG("wildcard_match",		2, pl_wildcard_match,		TRACE_ME),
  FRG("$apropos_match",		2, pl_apropos_match,		TRACE_ME),
  FRG("sub_atom",		5, pl_sub_atom,		   NDET|TRACE_ME),
  FRG("$option",		3, pl_option,		   NDET|TRACE_ME),
  FRG("convert_time",		2, pl_convert_time2,		TRACE_ME),
  FRG("convert_time",		8, pl_convert_time,		TRACE_ME),
  FRG("sleep",			1, pl_sleep,			TRACE_ME),
  FRG("break",			0, pl_break,			TRACE_ME),
  FRG("$break",			1, pl_break1,			TRACE_ME),
  FRG("notrace",		1, pl_notrace1,			META),
#ifdef O_LIMIT_DEPTH
  FRG("$depth_limit",		3, pl_depth_limit,		TRACE_ME),
  FRG("$depth_limit_true",	5, pl_depth_limit_true,	   NDET|TRACE_ME),
  FRG("$depth_limit_false",	4, pl_depth_limit_false,	TRACE_ME),
#endif

  FRG("write_canonical",	1, pl_write_canonical,		TRACE_ME),
  FRG("write_term",		2, pl_write_term,		TRACE_ME),
  FRG("write_term",		3, pl_write_term3,		TRACE_ME),
  FRG("write",			1, pl_write,			TRACE_ME),
  FRG("writeq",			1, pl_writeq,			TRACE_ME),
  FRG("print",			1, pl_print,			TRACE_ME),

  FRG("read_term",		2, pl_read_term,		TRACE_ME),
  FRG("read_term",		3, pl_read_term3,		TRACE_ME),
  FRG("read",			1, pl_read,			TRACE_ME),
  FRG("read_clause",		1, pl_read_clause,		TRACE_ME),
  FRG("read_clause",		2, pl_read_clause2,		TRACE_ME),
  FRG("$raw_read",		1, pl_raw_read,			TRACE_ME),
  FRG("$raw_read",		2, pl_raw_read2,		TRACE_ME),
  FRG("current_op",		3, pl_current_op,     NDET|META|TRACE_ME),
  FRG("$local_op",		3, pl_local_op,	      NDET|META|TRACE_ME),
  FRG("$builtin_op",		3, pl_builtin_op,	   NDET|TRACE_ME),
  FRG("current_atom",		1, pl_current_atom,	   NDET|TRACE_ME),
  FRG("current_functor",	2, pl_current_functor,	   NDET|TRACE_ME),
  FRG("$complete_atom",		3, pl_complete_atom,		TRACE_ME),
  FRG("$atom_completions",	2, pl_atom_completions,		TRACE_ME),
  FRG("op",			3, pl_op,		   META|TRACE_ME),
  FRG("char_conversion",	2, pl_char_conversion,		TRACE_ME),
  FRG("current_char_conversion",2, pl_current_char_conversion,	NDET|TRACE_ME),

  FRG("!",			0, pl_metacut,			TRACE_ME),
  FRG("functor",		3, pl_functor,			TRACE_ME),
  FRG("arg",			3, pl_arg,		   NDET|TRACE_ME),
  FRG("setarg",			3, pl_setarg,			TRACE_ME),
  FRG("=..",			2, pl_univ,			TRACE_ME),
  FRG("name",			2, pl_name,			TRACE_ME),
  FRG("atom_chars",		2, pl_atom_chars,		TRACE_ME),
  FRG("atom_codes",		2, pl_atom_codes,		TRACE_ME),
  FRG("number_chars",		2, pl_number_chars,		TRACE_ME),
  FRG("number_codes",		2, pl_number_codes,		TRACE_ME),
  FRG("char_code",		2, pl_char_code,		TRACE_ME),
  FRG("char_type",		2, pl_char_type,	   NDET|TRACE_ME),
  FRG("code_type",		2, pl_code_type,	   NDET|TRACE_ME),
  FRG("int_to_atom",		3, pl_int_to_atom,		TRACE_ME),
  FRG("$format_number",		3, pl_format_number,		TRACE_ME),
  FRG("atom_prefix",		2, pl_atom_prefix,		TRACE_ME),
  FRG("atom_concat",		3, pl_atom_concat,	   NDET|TRACE_ME),
  FRG("$concat_atom",		2, pl_concat_atom,		TRACE_ME),
  FRG("concat_atom",		3, pl_concat_atom3,		TRACE_ME),
  FRG("atom_length",		2, pl_atom_length,		TRACE_ME),
  FRG("atom_to_term",		3, pl_atom_to_term,		TRACE_ME),
  FRG("numbervars",		4, pl_numbervars,		TRACE_ME),
  FRG("free_variables",		2, pl_free_variables,		TRACE_ME),
  FRG("$e_free_variables",	2, pl_e_free_variables,		TRACE_ME),

  FRG("$open_wic",		1, pl_open_wic,			TRACE_ME),
  FRG("$close_wic",		0, pl_close_wic,		TRACE_ME),
  FRG("$add_directive_wic",	1, pl_add_directive_wic,	TRACE_ME),
  FRG("$import_wic",		2, pl_import_wic,		TRACE_ME),

  FRG("$rc_handle",		1, pl_rc_handle,		TRACE_ME),
  FRG("$rc_members",		2, pl_rc_members,		TRACE_ME),
  FRG("$rc_open",		5, pl_rc_open,			TRACE_ME),
  FRG("$rc_open_archive",	2, pl_rc_open_archive,		TRACE_ME),
  FRG("$rc_close_archive",	1, pl_rc_close_archive,		TRACE_ME),
  FRG("$rc_save_archive",	2, pl_rc_save_archive,		TRACE_ME),
  FRG("$rc_append_file",	5, pl_rc_append_file,		TRACE_ME),
  FRG("copy_stream_data",	2, pl_copy_stream_data2,	TRACE_ME),
  FRG("copy_stream_data",	3, pl_copy_stream_data3,	TRACE_ME),

  FRG("$qlf_start_module",	1, pl_qlf_start_module,		TRACE_ME),
  FRG("$qlf_start_sub_module",	1, pl_qlf_start_sub_module,	TRACE_ME),
  FRG("$qlf_start_file",	1, pl_qlf_start_file,		TRACE_ME),
  FRG("$qlf_end_part",		0, pl_qlf_end_part,		TRACE_ME),
  FRG("$qlf_open",		1, pl_qlf_open,			TRACE_ME),
  FRG("$qlf_close",		0, pl_qlf_close,		TRACE_ME),
  FRG("$qlf_load",		2, pl_qlf_load,		   META|TRACE_ME),
  FRG("$qlf_assert_clause",	2, pl_qlf_assert_clause,	TRACE_ME),
  FRG("$qlf_info",		4, pl_qlf_info,			TRACE_ME),

  FRG("abolish",    		1, pl_abolish1,		   META|TRACE_ME),
  FRG("abolish",    		2, pl_abolish,		   META|TRACE_ME),
  FRG("clause",    		2, pl_clause2,	      NDET|META|TRACE_ME),
  FRG("clause",    		3, pl_clause3,	      NDET|META|TRACE_ME),
  FRG("$clause",	        4, pl_clause4,	      NDET|META|TRACE_ME),
  FRG("nth_clause", 		3, pl_nth_clause,     NDET|META|TRACE_ME),
  FRG("retract",    		1, pl_retract,        NDET|META|TRACE_ME),
  FRG("retractall",		1, pl_retractall,	   META|TRACE_ME),
#if O_DEBUGGER
  FRG("$xr_member",		2, pl_xr_member,      NDET|META|TRACE_ME),
  FRG("$wam_list",		1, pl_wam_list,			TRACE_ME),
  FRG("$fetch_vm",		4, pl_fetch_vm,			TRACE_ME),
  FRG("$clause_term_position",	3, pl_clause_term_position,	TRACE_ME),
  FRG("$break_pc",		3, pl_break_pc,		   NDET|TRACE_ME),
  FRG("$break_at",		3, pl_break_at,			TRACE_ME),
  FRG("$current_break",		2, pl_current_break,	   NDET|TRACE_ME),
#endif  
#ifdef O_MAINTENANCE
  FRG("$list_generations",	1, pl_list_generations,	   META|TRACE_ME),
  FRG("$check_procedure",	1, pl_check_procedure,	   META|TRACE_ME),
#endif

  FRG("flag",			3, pl_flag,			TRACE_ME),
  FRG("recorda",		3, pl_recorda,			TRACE_ME),
  FRG("recordz",		3, pl_recordz,			TRACE_ME),
  FRG("recorded",		3, pl_recorded,		   NDET|TRACE_ME),
  FRG("erase",			1, pl_erase,			TRACE_ME),
  FRG("$term_complexity",	3, pl_term_complexity,		TRACE_ME),
  FRG("assert",			1, pl_assertz,		   META|TRACE_ME),
  FRG("asserta",		1, pl_asserta,		   META|TRACE_ME),
  FRG("assertz",		1, pl_assertz,		   META|TRACE_ME),
  FRG("assert",			2, pl_assertz2,		   META|TRACE_ME),
  FRG("asserta",		2, pl_asserta2,		   META|TRACE_ME),
  FRG("assertz",		2, pl_assertz2,		   META|TRACE_ME),
  FRG("$record_clause",		3, pl_record_clause,		TRACE_ME),
  FRG("redefine_system_predicate", 1, pl_redefine_system_predicate,
							   META|TRACE_ME),

  FRG("$c_current_predicate",	2, pl_current_predicate,  NDET|META|TRACE_ME),
  FRG("current_predicate",	1, pl_current_predicate1, NDET|META|TRACE_ME),
  FRG("$set_predicate_attribute", 3, pl_set_predicate_attribute,META|TRACE_ME),
  FRG("$get_predicate_attribute", 3, pl_get_predicate_attribute,META|TRACE_ME),
  FRG("$get_clause_attribute",  3, pl_get_clause_attribute,	TRACE_ME),
  FRG("$require",		1, pl_require,		   META|TRACE_ME),
  FRG("source_file",		2, pl_source_file,    NDET|META|TRACE_ME),
  FRG("$time_source_file",	2, pl_time_source_file,	   NDET|TRACE_ME),
  FRG("$start_consult",		1, pl_start_consult,		TRACE_ME),
  FRG("$make_system_source_files",0,pl_make_system_source_files,TRACE_ME),
  FRG("$default_predicate",	2, pl_default_predicate,   META|TRACE_ME),
  FRG("$clause_from_source",	3, pl_clause_from_source,	TRACE_ME),

  FRG("var",			1, pl_var,			TRACE_ME),
  FRG("nonvar",			1, pl_nonvar,			TRACE_ME),
  FRG("integer",		1, pl_integer,			TRACE_ME),
  FRG("float",			1, pl_float,			TRACE_ME),
  FRG("number",			1, pl_number,			TRACE_ME),
  FRG("atom",			1, pl_atom,			TRACE_ME),
  FRG("atomic",			1, pl_atomic,			TRACE_ME),
  FRG("ground",			1, pl_ground,			TRACE_ME),
  FRG("compound",		1, pl_compound,			TRACE_ME),
  FRG("callable",		1, pl_callable,			TRACE_ME),

  FRG("==",			2, pl_equal,			TRACE_ME),
  FRG("\\==",			2, pl_nonequal,			TRACE_ME),
  FRG("=",			2, pl_unify,			TRACE_ME),
  FRG("unify_with_occurs_check",2, pl_unify_with_occurs_check,  TRACE_ME),
  FRG("\\=",			2, pl_notunify,			TRACE_ME),
  FRG("compare",		3, pl_compare,			TRACE_ME),
  FRG("@<",			2, pl_lessStandard,		TRACE_ME),
  FRG("@=<",			2, pl_lessEqualStandard,	TRACE_ME),
  FRG("@>",			2, pl_greaterStandard,		TRACE_ME),
  FRG("@>=",			2, pl_greaterEqualStandard,	TRACE_ME),
  FRG("=@=",			2, pl_structural_equal,		TRACE_ME),
  FRG("\\=@=",			2, pl_structural_nonequal,	TRACE_ME),

  FRG("repeat",			0, pl_repeat,		   NDET|TRACE_ME),
  FRG("fail",			0, pl_fail,			TRACE_ME),
  FRG("true",			0, pl_true,			TRACE_ME),
  FRG("$fail",			0, pl_fail,			0),
  FRG("abort",			0, pl_abort,			TRACE_ME),

  FRG("statistics",		2, pl_statistics,		TRACE_ME),

  FRG("between",		3, pl_between,		   NDET|TRACE_ME),
  FRG("succ",			2, pl_succ,			TRACE_ME),
  FRG("plus",			3, pl_plus,			TRACE_ME),
  FRG("<",			2, pl_lessNumbers,	   META|TRACE_ME),
  FRG(">",			2, pl_greaterNumbers,	   META|TRACE_ME),
  FRG("=<",			2, pl_lessEqualNumbers,	   META|TRACE_ME),
  FRG(">=",			2, pl_greaterEqualNumbers, META|TRACE_ME),
  FRG("=\\=",			2, pl_nonEqualNumbers,	   META|TRACE_ME),
  FRG("=:=",			2, pl_equalNumbers,	   META|TRACE_ME),
  FRG("is",			2, pl_is,		   META|TRACE_ME),

  FRG("trace",			0, pl_trace,			0),
  FRG("notrace",		0, pl_notrace,			0),
  FRG("tracing",		0, pl_tracing,			0),
  FRG("$spy",			1, pl_spy,		   META|TRACE_ME),
  FRG("$nospy",			1, pl_nospy,		   META|TRACE_ME),
  FRG("$leash",			2, pl_leash, 			0),
  FRG("$visible",		2, pl_visible,			0),
  FRG("$debuglevel",		2, pl_debuglevel,		TRACE_ME),
  FRG("$style_check",		2, pl_style_check,		TRACE_ME),

#if COUNTING
  FRG("$count",			0, pl_count,			TRACE_ME),
#endif /* COUNTING */

  FRG("$profile",		2, pl_profile,			TRACE_ME),
  FRG("reset_profiler",		0, pl_reset_profiler,		TRACE_ME),
  FRG("profile_count",		3, pl_profile_count,	   META|TRACE_ME),
  FRG("profile_box",		5, pl_profile_box,	   META|TRACE_ME),

  FRG("prolog_current_frame",	1, pl_prolog_current_frame,	TRACE_ME),
  FRG("prolog_frame_attribute",	3, pl_prolog_frame_attribute,	TRACE_ME),
  FRG("prolog_choice_attribute",3, pl_prolog_choice_attribute,	TRACE_ME),
  FRG("prolog_skip_level",	2, pl_skip_level,		0),

  FRG("$write_on_string",	2, pl_write_on_string,		TRACE_ME),
  FRG("dwim_match",		3, pl_dwim_match,		TRACE_ME),
  FRG("$dwim_predicate",	2, pl_dwim_predicate,	   NDET|TRACE_ME),

  FRG("protocol",		1, pl_protocol,			TRACE_ME),
  FRG("protocola",		1, pl_protocola,		TRACE_ME),
  FRG("noprotocol",		0, pl_noprotocol,		TRACE_ME),
  FRG("protocolling",		1, pl_protocolling,		TRACE_ME),

#ifdef O_PROLOG_HOOK
  FRG("set_prolog_hook",	3, pl_set_prolog_hook,	        TRACE_ME),
#endif
  FRG("$current_module",	2, pl_current_module,	   NDET|TRACE_ME),
  FRG("$module",		2, pl_module,			TRACE_ME),
  FRG("$set_source_module",	2, pl_set_source_module,	TRACE_ME),
  FRG("$term_expansion_module",	1, pl_term_expansion_module,NDET|TRACE_ME),
  FRG("$goal_expansion_module",	1, pl_goal_expansion_module,NDET|TRACE_ME),
  FRG("$declare_module",	2, pl_declare_module,		TRACE_ME),
  FRG("context_module",		1, pl_context_module,	   META|TRACE_ME),
  FRG("$strip_module",		3, pl_strip_module,	   META|TRACE_ME),
  FRG("import",			1, pl_import,		   META|TRACE_ME),
  FRG("export",			1, pl_export,		   META|TRACE_ME),
  FRG("$check_export",		0, pl_check_export,	   META|TRACE_ME),
  FRG("export_list",		2, pl_export_list,		TRACE_ME),
  FRG("index",			1, pl_index,		   META|TRACE_ME),
  FRG("hash",			1, pl_hash,		   META|TRACE_ME),
#ifdef O_HASHTERM
  FRG("hash_term",		2, pl_hash_term,		TRACE_ME),
#endif
  FRG("$open_shared_object",	3, pl_open_shared_object,  META|TRACE_ME),
#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD) || defined(EMULATE_DLOPEN)
  FRG("close_shared_object",	1, pl_close_shared_object, META|TRACE_ME),
  FRG("call_shared_object_function",
				2, pl_call_shared_object_function,
							   META|TRACE_ME),
#endif /*HAVE_DLOPEN*/

#if O_DDE
  FRG("open_dde_conversation",	3, pl_open_dde_conversation,	TRACE_ME),
  FRG("close_dde_conversation",	1, pl_close_dde_conversation,	TRACE_ME),
  FRG("dde_request",		4, pl_dde_request,		TRACE_ME),
  FRG("dde_execute",		3, pl_dde_execute,		TRACE_ME),
  FRG("dde_poke",		4, pl_dde_poke,			TRACE_ME),
  FRG("$dde_register_service",	2, pl_dde_register_service,	TRACE_ME),
#endif /*O_DDE*/

#if O_STRING
  FRG("string",			1, pl_string,			TRACE_ME),
  FRG("string_concat",		3, pl_string_concat,	   NDET|TRACE_ME),
  FRG("string_length",		2, pl_string_length,		TRACE_ME),
  FRG("string_to_atom",		2, pl_string_to_atom,		TRACE_ME),
  FRG("string_to_list",		2, pl_string_to_list,		TRACE_ME),
  FRG("sub_string",		5, pl_sub_string,	   NDET|TRACE_ME),
#endif /* O_STRING */

  FRG("is_list",		1, pl_is_list,			TRACE_ME),
  FRG("$length",		2, pl_length,			TRACE_ME),
  FRG("memberchk",		2, pl_memberchk,		TRACE_ME),
  FRG("msort",			2, pl_msort,			TRACE_ME),
  FRG("sort",			2, pl_sort,			TRACE_ME),
  FRG("format",			2, pl_format,			TRACE_ME),
  FRG("$collect_bag",		2, pl_collect_bag,		TRACE_ME),
  FRG("$except_bag",		1, pl_except_bag, 		TRACE_ME),
  FRG("$record_bag",		1, pl_record_bag,		TRACE_ME),
#ifdef O_DEBUG
  FRG("$check_definition",	1, pl_check_definition,    META|TRACE_ME),
#endif

  FRG("$atom_hashstat",		2, pl_atom_hashstat,		TRACE_ME),
  FRG("$current_prolog_flag",	5, pl_feature5,		   NDET|TRACE_ME),
  FRG("current_prolog_flag",	2, pl_feature,		   NDET|TRACE_ME),
  FRG("deterministic",		0, pl_deterministic,		TRACE_ME),
  FRG("set_prolog_flag",	2, pl_set_feature,		TRACE_ME),
  FRG("trim_stacks",		0, pl_trim_stacks,		TRACE_ME),
#if O_SHIFT_STACKS
  FRG("stack_parameter",	4, pl_stack_parameter,		TRACE_ME),
#endif
  FRG("$garbage_collect",	1, pl_garbage_collect,		TRACE_ME),
#ifdef O_ATOMGC
  FRG("garbage_collect_atoms",	0, pl_garbage_collect_atoms,	TRACE_ME),
  FRG("current_atom",		2, pl_current_atom2,	   NDET|TRACE_ME),
#ifdef O_DEBUG_ATOMGC
  FRG("track_atom",		2, pl_track_atom,		TRACE_ME),
#endif
#endif
  FRG("copy_term",		2, pl_copy_term,		TRACE_ME),
  FRG("current_key",		1, pl_current_key,	   NDET|TRACE_ME),
  FRG("current_flag",		1, pl_current_flag,	   NDET|TRACE_ME),

  FRG("open",			3, pl_open,			TRACE_ME),
  FRG("open",			4, pl_open4,			TRACE_ME),
  FRG("open_null_stream",	1, pl_open_null_stream,		TRACE_ME),
  FRG("close",			1, pl_close,			TRACE_ME),
  FRG("close",			2, pl_close2,			TRACE_ME),
  FRG("stream_property",	2, pl_stream_property,	   NDET|TRACE_ME),
  FRG("flush_output",		1, pl_flush_output1,		TRACE_ME),
  FRG("set_stream_position",	2, pl_set_stream_position,	TRACE_ME),
  FRG("seek",			4, pl_seek,			TRACE_ME),
  FRG("set_input",		1, pl_set_input,		TRACE_ME),
  FRG("set_output",		1, pl_set_output,		TRACE_ME),
  FRG("set_stream",		2, pl_set_stream,		TRACE_ME),
  FRG("current_input",		1, pl_current_input,		TRACE_ME),
  FRG("current_output",		1, pl_current_output,		TRACE_ME),
  FRG("character_count",	2, pl_character_count,		TRACE_ME),
  FRG("line_count",		2, pl_line_count,		TRACE_ME),
  FRG("line_position",		2, pl_line_position,		TRACE_ME),
  FRG("source_location",	2, pl_source_location,		TRACE_ME),
  FRG("at_end_of_stream",	1, pl_at_end_of_stream1,	TRACE_ME),
  FRG("at_end_of_stream",	0, pl_at_end_of_stream0,	TRACE_ME),
  FRG("peek_byte",		2, pl_peek_byte2,		TRACE_ME),
  FRG("peek_byte",		1, pl_peek_byte1,		TRACE_ME),
  FRG("peek_code",		2, pl_peek_code2,		TRACE_ME),
  FRG("peek_code",		1, pl_peek_code1,		TRACE_ME),
  FRG("peek_char",		2, pl_peek_char2,		TRACE_ME),
  FRG("peek_char",		1, pl_peek_char1,		TRACE_ME),

  FRG("nl",			1, pl_nl1,			TRACE_ME),
  FRG("tab",			2, pl_tab2,			TRACE_ME),
  FRG("put",			2, pl_put2,			TRACE_ME),
  FRG("put_byte",		2, pl_put2,			TRACE_ME),
  FRG("put_code",		2, pl_put2,			TRACE_ME),
  FRG("put_char",		2, pl_put2,			TRACE_ME),
  FRG("get",			2, pl_get2,			TRACE_ME),
  FRG("get0",			2, pl_get_byte2,		TRACE_ME),
  FRG("get_byte",		2, pl_get_byte2,		TRACE_ME),
  FRG("get_code",		2, pl_get_byte2,		TRACE_ME),
  FRG("get_char",		2, pl_get_char2,		TRACE_ME),
  FRG("read",			2, pl_read2,			TRACE_ME),
  FRG("write",			2, pl_write2,			TRACE_ME),
  FRG("writeq",			2, pl_writeq2,			TRACE_ME),
  FRG("print",			2, pl_print2,			TRACE_ME),
  FRG("write_canonical",	2, pl_write_canonical2,		TRACE_ME),
  FRG("format",			3, pl_format3,			TRACE_ME),

  FRG("tty_get_capability",	3, pl_tty_get_capability,	TRACE_ME),
  FRG("tty_goto",		2, pl_tty_goto,			TRACE_ME),
  FRG("tty_put",		2, pl_tty_put,			TRACE_ME),
  FRG("tty_size",		2, pl_tty_size,			TRACE_ME),
  FRG("format_predicate",	2, pl_format_predicate,	   META|TRACE_ME),
  FRG("current_format_predicate", 2, pl_current_format_predicate,
						      META|NDET|TRACE_ME),
  FRG("wait_for_input",		3, pl_wait_for_input,		TRACE_ME),
  FRG("get_time",		1, pl_get_time,			TRACE_ME),
  FRG("size_file",		2, pl_size_file,		TRACE_ME),
  FRG("$size_stream",		2, pl_size_stream,		TRACE_ME),
  FRG("$default_module",	3, pl_default_module,	   META|TRACE_ME),
#if O_PROLOG_FUNCTIONS
  FRG("$arithmetic_function",   1, pl_arithmetic_function, META|TRACE_ME),
  FRG("current_arithmetic_function", 1, pl_current_arithmetic_function,
						      NDET|META|TRACE_ME),
  FRG("$prolog_arithmetic_function", 1, pl_prolog_arithmetic_function,
						      NDET|META|TRACE_ME),
#endif

#ifdef O_PLMT
  FRG("thread_create",		3, pl_thread_create,	   META|TRACE_ME),
  FRG("thread_join",		2, pl_thread_join,	        TRACE_ME),
  FRG("thread_exit",		1, pl_thread_exit,		TRACE_ME),
  FRG("current_thread",		2, pl_current_thread,      NDET|TRACE_ME),
  FRG("thread_kill",		2, pl_thread_kill,              TRACE_ME),
  FRG("thread_send_message",	2, pl_thread_send_message,	TRACE_ME),
  FRG("thread_get_message",	1, pl_thread_get_message,	TRACE_ME),
  FRG("thread_peek_message",	1, pl_thread_peek_message,	TRACE_ME),
  FRG("thread_signal",		2, pl_thread_signal,	   META|TRACE_ME),
  FRG("thread_at_exit",		1, pl_thread_at_exit,	   META|TRACE_ME),
  FRG("mutex_create",		1, pl_mutex_create,		TRACE_ME),
  FRG("mutex_destroy",		1, pl_mutex_destroy,		TRACE_ME),
  FRG("mutex_lock",		1, pl_mutex_lock,		TRACE_ME),
  FRG("mutex_trylock",		1, pl_mutex_trylock,		TRACE_ME),
  FRG("mutex_unlock",		1, pl_mutex_unlock,		TRACE_ME),
  FRG("mutex_unlock_all",	0, pl_mutex_unlock_all,		TRACE_ME),
  FRG("current_mutex",		3, pl_current_mutex,	   NDET|TRACE_ME),
  FRG("open_xterm",		3, pl_open_xterm,		TRACE_ME),
#endif

  FRG("thread_self",		1, pl_thread_self,	        TRACE_ME),
  FRG("with_mutex",		2, pl_with_mutex,	   META|TRACE_ME),
  FRG("$get_pid",		1, pl_get_pid,			TRACE_ME),

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
  ExtensionCell next;
};

#define ext_head		(GD->foreign._ext_head)
#define ext_tail		(GD->foreign._ext_tail)
#define extensions_loaded	(GD->foreign._loaded)

static void
bindExtensions(const PL_extension *e)
{ Definition def;

  for(; e->predicate_name; e++)
  { short flags = TRACE_ME;
    Module m;
    atom_t name = PL_new_atom(e->predicate_name);
    char *s;

    for(s=e->predicate_name; isAlpha(*s); s++)
      ;

    if ( *s == ':' )			/* module:predicate */
    { m = PL_new_module(PL_new_atom_nchars(s-e->predicate_name,
					   e->predicate_name));
      name = PL_new_atom(s+1);
    } else
    { name = PL_new_atom(e->predicate_name);
      m = (environment_frame ? contextModule(environment_frame)
	   		     : MODULE_user);
    }

    if ( e->flags & PL_FA_NOTRACE )	     flags &= ~TRACE_ME;
    if ( e->flags & PL_FA_TRANSPARENT )	     flags |= METAPRED;
    if ( e->flags & PL_FA_NONDETERMINISTIC ) flags |= NONDETERMINISTIC;
    if ( e->flags & PL_FA_VARARGS )	     flags |= P_VARARG;

    def = lookupProcedure(lookupFunctorDef(name, e->arity), m)->definition;
    PL_unregister_atom(name);
    set(def, FOREIGN);
    if ( m == MODULE_system )
      set(def, SYSTEM|HIDE_CHILDS);
    set(def, flags);
    def->definition.function = e->function;
    def->indexPattern = 0;
    def->indexCardinality = 0;
  }    
}


void
PL_register_extensions(const PL_extension *e)
{ if ( extensions_loaded )
    bindExtensions(e);
  else
  { ExtensionCell cell = malloc(sizeof *cell);

    cell->extensions = (PL_extension *) e;
    cell->next = NULL;
    if ( ext_tail )
    { ext_tail->next = cell;
      ext_tail = cell;
    } else
    { ext_head = ext_tail = cell;
    }
  }
}


void
cleanupExtensions(void)
{ ExtensionCell c, next;

  for(c=ext_head; c; c=next)
  { next = c->next;
    free(c);
  }

  ext_head = ext_tail = NULL;
}



void
initBuildIns(void)
{ const struct foreign *f;
  Definition def;
  ExtensionCell ecell;
  Module m = MODULE_system;

  for(f = &foreigns[0]; f->name; f++)
  { atom_t name	= PL_new_atom(f->name);
    functor_t fdef = lookupFunctorDef(name, f->arity);

    PL_unregister_atom(name);
    def = lookupProcedure(fdef, MODULE_system)->definition;
    set(def, FOREIGN|SYSTEM|LOCKED);
    clear(def, TRACE_ME);
    set(def, f->flags);
    def->definition.function = f->function;
    def->indexPattern = 0;
    def->indexCardinality = 0;
    if ( false(def, NONDETERMINISTIC) && 
	 f->arity <= 2 )
      set(valueFunctor(fdef), INLINE_F);
  }

  PROCEDURE_garbage_collect0 = lookupProcedure(FUNCTOR_dgarbage_collect1, m);
  PROCEDURE_block3	     = lookupProcedure(FUNCTOR_block3, 		  m);
  PROCEDURE_catch3           = lookupProcedure(FUNCTOR_catch3, 		  m);
  PROCEDURE_true0            = lookupProcedure(FUNCTOR_true0, 		  m);
  PROCEDURE_fail0            = lookupProcedure(FUNCTOR_fail0, 		  m);
  PROCEDURE_print_message2   = lookupProcedure(FUNCTOR_print_message2, 	  m);
  PROCEDURE_dcall1	     = lookupProcedure(FUNCTOR_dcall1,		  m);
  PROCEDURE_call_cleanup3    = lookupProcedure(FUNCTOR_call_cleanup3,	  m); 
					/* allow debugging in call/1 */
  clear(PROCEDURE_dcall1->definition, HIDE_CHILDS);
  set(PROCEDURE_dcall1->definition, DYNAMIC);

  for( ecell = ext_head; ecell; ecell = ecell->next )
    bindExtensions(ecell->extensions);

  extensions_loaded = TRUE;
}
