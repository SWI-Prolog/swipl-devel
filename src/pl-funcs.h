/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Function type declarations for all common functions
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This header file defines all public functions of the SWI-Prolog  system.
Depending  on  the  PROTO  cpp  flag  ANSI  declarations  or  old  style
declarations are used.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if __GNUC__
#define FI(type) int	/* type that promotes to int */
#else
#define FI(type) type
#endif

#define Put(c)			put_character((Char) (c))
#define Get0()			get_character()
#define unifyAtomic(p, w)	unify_atomic(p, (word)(w))

		/* pl-alloc.c */
char		*store_string P((char *)),
		*store_string_local P((char *));
int		stringHashValue P((char *, int size));
Void		alloc_heap P((alloc_t)),
		alloc_local P((alloc_t)),
		alloc_global P((alloc_t));
word		globalString P((char *)),
		globalReal P((real)),
		heapReal P((real)),
		globalFunctor P((FunctorDef));
real		unpack_real P((Word));
volatile void	outOf P((Stack));
void		free_heap P((Void, alloc_t)),
		setReal P((word, real)),
		initAllocLocal P((void)),
		stopAllocLocal P((void));

		/* pl-arith.c */
word		pl_between P((Word, Word, Word, word)),
		pl_succ P((Word, Word)),
		pl_plus P((Word, Word, Word)),
		pl_compare P((Word, Word, Word)),
		pl_lessNumbers P((Word, Word)),
		pl_greaterNumbers P((Word, Word)),
		pl_lessEqualNumbers P((Word, Word)),
		pl_greaterEqualNumbers P((Word, Word)),
		pl_nonEqualNumbers P((Word, Word)),
		pl_equalNumbers P((Word, Word)),
		pl_is P((Word, Word)),
#if O_PROLOG_FUNCTIONS
  		pl_arithmetic_function P((Word)),
#endif
  		pl_current_arithmetic_function P((Word, word)),
		compareNumbers P((Word, Word, int)),
		evaluate P((Word));
void		initArith P((void));
#if O_COMPILE_ARITH
int		indexArithFunction P((FunctorDef, Module));
FunctorDef	functorArithFunction P((int));
bool		ar_func_n P((FI(code), int, Word *));
#endif O_COMPILE_ARITH

		/* pl-atom.c */
Atom		lookupAtom P((char *));
word		pl_current_atom P((Word, word)),
		pl_atom_hashstat P((Word, Word)),
		pl_complete_atom P((Word, Word, Word)),
		pl_atom_completions P((Word, Word));
char *		extendAtom P((char *, bool *));
bool		extendAlternatives P((char *));
void		initAtoms P((void));

		/* pl-bags.c */
word		pl_collect_bag P((Word, Word)),
		pl_record_bag P((Word, Word));

		/* pl-ext.c */
void		initBuildIns P((void));

		/* pl-dump.c */
word		pl_save_program P((Word, Word));

		/* pl-dwim.c */
word		pl_dwim_match P((Word, Word, Word)),
		pl_dwim_predicate P((Word, Word, word));
		
		/* pl-file.c */
bool		told P((void)),
		see P((word)),
		seen P((void)),
		seeString P((char *)),
		seeingString P((void)),
		seenString P((void)),
		tellString P((char *, long)),
		toldString P((void)),
		readLine P((char *, int)),
		unifyTime P((Word, long));
int		currentInputLine P((void));
Atom		currentStreamName P((void));
bool		put_character P((Char));
#if AIX
word		Putf();
#else 
word		Putf P((char *, ...));
#endif
word		vPutf P((char *, va_list)),
		pl_tty P((void)),
		pl_tty_fold P((Word, Word)),
		pl_put P((Word)),
		pl_get P((Word)),
		pl_get0 P((Word)),
		pl_put2 P((Word, Word)),
		pl_get2 P((Word, Word)),
		pl_get02 P((Word, Word)),
		pl_get_single_char P((Word)),
		pl_seeing P((Word)),
		pl_telling P((Word)),
		pl_seen P((void)),
		pl_told P((void)),
		pl_see P((Word)),
		pl_tell P((Word)),
		pl_append P((Word)),
		pl_ttyflush P((void)),
		pl_flush P((void)),
		pl_protocol P((Word)),
		pl_protocola P((Word)),
		pl_noprotocol P((void)),
		pl_protocolling P((Word)),
		pl_prompt P((Word, Word)),
		pl_tab P((Word)),
		pl_tab2 P((Word, Word)),
		pl_tmp_file P((Word, Word)),
		pl_time_file P((Word, Word)),
		pl_size_file P((Word, Word)),
		pl_access_file P((Word, Word)),
		pl_exists_file P((Word)),
		pl_exists_directory P((Word)),
		pl_delete_file P((Word)),
		pl_rename_file P((Word, Word)),
		pl_fileerrors P((Word, Word)),
		pl_absolute_file_name P((Word, Word)),
		pl_chdir P((Word)),
		pl_file_dir_name P((Word, Word)),
		pl_file_base_name P((Word, Word));
word		pl_open P((Word, Word, Word)),
		pl_open_null_stream P((Word)),
		pl_close P((Word)),
		pl_current_stream P((Word, Word, Word, word)),
		pl_flush_output P((Word)),
		pl_stream_position P((Word, Word, Word)),
		pl_set_input P((Word)),
		pl_set_output P((Word)),
		pl_current_input P((Word)),
		pl_current_output P((Word)),
		pl_character_count P((Word, Word)),
		pl_line_count P((Word, Word)),
		pl_line_position P((Word, Word)),
		pl_source_location P((Word, Word)),
		pl_wait_for_input P((Word, Word, Word));
Char		get_character P((void)),
		getSingleChar P((void));
FILE *		checkInput P((int));
void		initIO P((void)),
		dieIO P((void)),
		closeFiles P((void)),
		newLineInput P((void)),
		prompt P((bool));
int		streamNo P((Word, int));

		/* pl-glob.c */
bool		compilePattern P((char *)),
		matchPattern P((char *));
word		pl_expand_file_name P((Word, Word));
char *		ExpandOneFile P((char *));

		/* pl-gc.c */
void		garbageCollect P((LocalFrame)),
		resetGC P((void)),
		considerGarbageCollect P((Stack)),
		lockw P((Word)), unlockw P((Word)),
		lockp P((Word *)), unlockp P((Word *)),
		lockMark P((mark *)), unlockMark P((mark *));
word		pl_garbage_collect P((Word)),
		pl_collect_parms P((Word, Word));

		/* pl-flag.c */
void		initFlags P((void));
word		pl_flag P((Word, Word, Word)),
		pl_current_flag P((Word, word));

		/* pl-funct.c */
FunctorDef	lookupFunctorDef P((Atom name, int arity)),
		isCurrentFunctor P((Atom name, int arity));
void		initFunctors P((void));
word		pl_current_functor P((Word, Word, word));
int		atomIsFunctor P((Atom));
bool		atomIsProcedure P((Atom)),
		atomIsProcedureModule P((Atom, Module));

		/* pl-util.c */
char		*procedureName P((Procedure)),
		digitName P((int, bool));
int		digitValue P((int, char));
bool		wordToInteger P((word, long *)),
		wordToReal P((word, real *)),
		isUserSystemProcedure P((Procedure)),
		strsub P((char *, char *)),
		strprefix P((char *, char *)),
		strpostfix P((char *, char *));
void		systemMode P((bool));
word		notImplemented P((char *, int));

		/* pl-index.c */
int		cardinalityPattern P((ulong));
bool		reindexClause P((Clause)),
		indexPatternToTerm P((Procedure, Word));
struct index	getIndex P((Word, ulong, int));
Clause		findClause P((Clause, Word, const Definition, const bool *));

		/* pl-itf.h */
void		resetForeign P((void));

		/* pl-wam.c */
#if COUNTING
word		pl_count P((void));
#endif
#if O_COMPILE_OR
word		pl_alt P((Word, word));
#endif
word		pl_break P((void)),
		pl_break1 P((Word)),
		pl_abort P((void)),
		pl_metacut P((void));
bool		callGoal P((Module, word, bool)),
		prolog P((word)),
		interpret P((Module, word, bool)),
		unify P((Word, Word)),
		unify_atomic P((Word, word)),
		unifyFunctor P((Word, FunctorDef));

		/* pl-list.c */
word		pl_is_list P((Word)),
		pl_proper_list P((Word)),
		pl_length P((Word, Word)),
		pl_memberchk P((Word, Word)),
		pl_msort P((Word, Word)),
		pl_sort P((Word, Word));

		/* pl-proc.c */
Procedure	lookupProcedure P((FunctorDef, Module)),
		isCurrentProcedure P((FunctorDef, Module)),
		resolveProcedure P((FunctorDef, Module)),
		findProcedure P((Word)),
		findCreateProcedure P((Word));
bool		isDefinedProcedure P((Procedure)),
		assertProcedure P((Procedure, Clause, char)),
		abolishProcedure P((Procedure, Module)),
		retractClauseProcedure P((Procedure, Clause));
SourceFile	lookupSourceFile P((Atom)),
		isCurrentSourceFile P((Atom));
word		pl_current_predicate P((Word, Word, word)),
		pl_retract P((Word, word)),
		pl_abolish P((Word, Word)),
		pl_list_references P((Word)),
		pl_list_active_procedures P((void)),
		pl_predicate_attribute P((Word, Word, Word)),
		pl_index P((Word)),
		pl_source_file P((Word, Word)),
		pl_time_source_file P((Word, Word, word)),
		pl_start_consult P((Word)),
		pl_make_system_source_files P((void));
void		unallocClause P((Clause)),
		trapUndefined P((Procedure)),
		resetReferences P((void)),
		removeClausesProcedure P((Procedure)),
		resetProcedure P((Procedure)),
		reindexProcedure P((Procedure)),
		freeClause P((Clause));

		/* pl-prof.c */
word		pl_profile P((Word, Word)),
		pl_profile_count P((Word, Word, Word)),
		pl_reset_profiler P((void));
void		stopItimer P((void));

		/* pl-read.c */
word		pl_raw_read P((Word)),
		pl_read_variables P((Word, Word)),
		pl_read_variables3 P((Word, Word, Word)),
		pl_read P((Word)),
		pl_read2 P((Word, Word)),
		pl_read_clause P((Word)),
		pl_read_clause2 P((Word, Word));
void		resetRead P((void));

		/* pl-load.c */
char		*getExecutable P((void));
bool		getSymbols P((void));
void		resetLoader P((void));
word		pl_load_foreign P((Word, Word, Word, Word, Word));
word		pl_load_foreign1 P((Word));

		/* pl-modul.c */
Word		stripModule P((Word, Module *));
Module		lookupModule P((Atom)), isCurrentModule P((Atom));
void		initModules P((void));
bool		isPublicModule P((Module, Procedure)),
  		isSuperModule P((Module, Module));
word		pl_current_module P((Word, Word, word)),
		pl_default_module P((Word, Word, Word)),
		pl_strip_module P((Word, Word, Word)),
		pl_module P((Word, Word)),
		pl_set_source_module P((Word, Word)),
		pl_declare_module P((Word, Word)),
		pl_export P((Word)),
		pl_check_export P((void)),
		pl_context_module P((Word)),
		pl_import P((Word));

		/* pl-comp.c */
Clause		assert_term P((Word, char, Atom));
word		pl_assertz P((Word)), pl_asserta P((Word)),
		pl_assertz2 P((Word, Word)), pl_asserta2 P((Word, Word)),
		pl_record_clause P((Word, Word)),
		pl_clause P((Word, Word, Word, word));
bool		decompileHead P((Clause, Word)),
		decompile P((Clause, Word)),
		splitClause P((Word, Word *, Word *));
void		initWamTable P((void));

		/* pl-fmt.c */
word		pl_format P((Word, Word)),
		pl_format3 P((Word, Word, Word)),
		pl_format_predicate P((Word, Word));

		/* pl-op.c */
Operator	lookupOperator P((Atom, int)),
		isCurrentOperator P((Atom, int));
bool		isPrefixOperator P((Atom, int *, int *)),
		isPostfixOperator P((Atom, int *, int *)),
		isInfixOperator P((Atom, int *, int *)),
		operator P((Atom, int, int)),
		newOp P((char *, int, int));
word		pl_current_op P((Word, Word, Word, word)),
		pl_op1 P((Word, Word, Word));
void		initOperators P((void));

		/* pl-prims.c */
word		pl_nonvar P((Word)),
		pl_var P((Word)),
		pl_integer P((Word)),
		pl_float P((Word)),
		pl_number P((Word)),
		pl_arch P((Word, Word)),
		pl_atom P((Word)),
		pl_atomic P((Word)),
		pl_ground P((Word)),
		pl_unify P((Word, Word)),
		pl_notunify P((Word, Word)),
		pl_equal P((Word, Word)),
		pl_nonequal P((Word, Word)),
		pl_lessStandard P((Word, Word)),
		pl_lessEqualStandard P((Word, Word)),
		pl_greaterStandard P((Word, Word)),
		pl_greaterEqualStandard P((Word, Word)),
		pl_structural_equal P((Word, Word)),
		pl_structural_nonequal P((Word, Word)),
		pl_functor P((Word, Word, Word)),
		pl_arg P((Word, Word, Word)),
		pl_univ P((Word, Word)),
		pl_numbervars P((Word, Word, Word, Word)),
		pl_free_variables P((Word, Word)),
		pl_e_free_variables P((Word, Word)),
		pl_atom_length P((Word, Word)),
		pl_int_to_atom P((Word, Word, Word)),
		pl_format_number P((Word, Word, Word)),
		pl_apropos_match P((Word, Word)),
		pl_name P((Word, Word)),
		pl_concat P((Word, Word, Word)),
		pl_concat_atom P((Word, Word)),
#if O_STRING
		pl_string P((Word)),
		pl_string_length P((Word, Word)),
		pl_string_to_atom P((Word, Word)),
		pl_string_to_list P((Word, Word)),
		pl_substring P((Word, Word, Word, Word)),
		pl_write_on_string P((Word, Word)),
#endif
		pl_write_on_atom P((Word, Word)),
		pl_write_on_list P((Word, Word)),
		pl_term_to_atom P((Word, Word, Word)),
		pl_repeat P((word)),
		pl_fail P((void)),
		pl_halt P((void)),
		pl_statistics P((Word, Word)),
		pl_version P((Word)),
		pl_option P((Word, Word)),
		pl_please P((Word, Word, Word)),
		pl_style_check P((Word, Word)),
		pl_novice P((Word, Word)),
		stringToList P((char *)),
		pl_copy_term P((Word, Word)),
		pl_home P((Word));
int		compareStandard P((Word, Word)),
		lengthList P((Word)),
		numberVars P((Word, FunctorDef, int));
char 		*primitiveToString P((word, bool)),
		*formatInteger P((bool, int, int, bool, long)),
		*listToString P((word)),
		*toString P((word));

		/* pl-table.c */
Symbol		lookupHTable P((Table, Void)),
		firstHTable P((Table)),
		nextHTable P((Table, Symbol)),
		lookupLocalTable P((Table, Void));
Table		newHTable P((int)),
		newLocalTable P((int));
bool		addHTable P((Table, Void, Void)),
		addLocalTable P((Table, Void, Void)),
		deleteHTable P((Table, Void)),
		unifyStringWithList P((char *, Word));
void		clearHTable P((Table));

		/* pl-rec.c */
void		initRecords P((void));
word		getKey P((Word)),
		pl_recorda P((Word, Word, Word)),
		pl_recordz P((Word, Word, Word)),
		pl_recorded P((Word, Word, Word, word)),
		pl_current_key P((Word, word)),
		heapString P((char *)),
		copyTermToGlobal P((Record)),
		pl_erase P((Word));
bool		freeRecord P((Record)),
		unifyKey P((Word, word));
Record		copyTermToHeap P((Word));

		/* pl-setup.c */
void		setupProlog P((void));
#if unix
void		deliverSignal P((int, int, struct sigcontext *, char *));
handler_t	pl_signal P((int, handler_t));
#endif
word		pl_limit_stack P((Word, Word)),
		pl_trim_stacks P((void));

		/* pl-sys.c */
word		pl_shell P((Word, Word)),
#if LINK_THIEF
		pl_thief P((Word)),
#endif
		pl_grep P((Word, Word, Word, word)),
		pl_getenv P((Word, Word)),
		pl_setenv P((Word, Word)),
		pl_unsetenv P((Word)),
		pl_wildcard_match P((Word, Word)),
		pl_argv P((Word)),
		pl_convert_time P((Word, Word, Word, Word, Word, Word, Word, Word)),
		pl_get_time P((Word)),
		pl_sleep P((Word));

		/* pl-trace.c */
int		tracePort P((LocalFrame, int));
void		writeFrameGoal P((LocalFrame, int)),
		backTrace P((LocalFrame)),
		initTracer P((void));
word		pl_trace P((void)),
		pl_notrace P((void)),
		pl_tracing P((void)),
		pl_debug P((void)),
		pl_nodebug P((void)),
		pl_debugging P((void)),
		pl_skip_level P((Word, Word)),
		pl_spy P((Word)),
		pl_nospy P((Word)),
		pl_leash P((Word, Word)),
		pl_visible P((Word, Word)),
		pl_unknown P((Word, Word)),
		pl_prolog_current_frame P((Word)),
		pl_prolog_frame_attribute P((Word, Word, Word)),
		pl_trace_continuation P((Word));
void		interruptHandler P((void));

		/* pl-wic.c */
bool		loadWicFile P((char *, bool, bool));
word		pl_open_wic P((Word)),
		pl_close_wic P((void)),
		pl_add_clause_wic P((Word, Word)),
		pl_add_directive_wic P((Word)),
		pl_start_module_wic P((Word, Word)),
		pl_export_wic P((Word, Word)),
		pl_import_wic P((Word, Word, Word));
bool		compileFileList P((char *, int, char **));
bool		appendState P((char *));

		/* pl-write.c */
word		pl_nl P((void)),
		pl_display P((Word)),
		pl_displayq P((Word)),
		pl_write P((Word)),
		pl_writeq P((Word)),
		pl_print P((Word)),
		pl_display2 P((Word, Word)),
		pl_displayq2 P((Word, Word)),
		pl_write2 P((Word, Word)),
		pl_writeq2 P((Word, Word)),
		pl_print2 P((Word, Word)),
		pl_nl1 P((Word));

		/* pl-term.c */

void		resetTerm P((void));
word		pl_tty_get_capability P((Word, Word, Word)),
  		pl_tty_goto P((Word, Word)),
		pl_tty_put  P((Word, Word)),
  		pl_set_tty  P((Word, Word));
