/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Function type declarations for all common functions
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This header file defines all public functions of the SWI-Prolog  system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(__GNUC__) && __GNUC__ == 1
#define FI(type) int	/* type that promotes to int */
#else
#define FI(type) type
#endif

#define Put(c)			put_character((Char) (c))
#define Get0()			get_character()
#define unifyAtomic(p, w)	unify_atomic(p, (word)(w))

		/* pl-alloc.c */
char		*store_string(char *),
		*store_string_local(char *);
void		remove_string(char *s);
int		stringHashValue(char *, int size);
Void		alloc_heap(alloc_t),
		alloc_local(alloc_t),
		alloc_global(alloc_t);
word		globalString(char *),
		globalReal(real),
		heapReal(real),
		globalFunctor(FunctorDef);
Word		newTerm(void);
real		unpack_real(Word);
volatile void	outOf(Stack);
void		free_heap(Void, alloc_t),
		setReal(word, real),
		initAllocLocal(void),
		stopAllocLocal(void);

		/* pl-arith.c */
word		pl_between(Word, Word, Word, word),
		pl_succ(Word, Word),
		pl_plus(Word, Word, Word),
		pl_compare(Word, Word, Word),
		pl_lessNumbers(Word, Word),
		pl_greaterNumbers(Word, Word),
		pl_lessEqualNumbers(Word, Word),
		pl_greaterEqualNumbers(Word, Word),
		pl_nonEqualNumbers(Word, Word),
		pl_equalNumbers(Word, Word),
		pl_is(Word, Word),
#if O_PROLOG_FUNCTIONS
  		pl_arithmetic_function(Word),
#endif
  		pl_current_arithmetic_function(Word, word),
		compareNumbers(Word, Word, int),
		evaluate(Word);
void		initArith(void);
#if O_COMPILE_ARITH
int		indexArithFunction(FunctorDef, Module);
FunctorDef	functorArithFunction(int);
bool		ar_func_n(FI(code), int, Word *);
#endif /* O_COMPILE_ARITH */

		/* pl-atom.c */
Atom		lookupAtom(char *);
word		pl_current_atom(Word, word),
		pl_atom_hashstat(Word, Word),
		pl_complete_atom(Word, Word, Word),
		pl_atom_completions(Word, Word);
char *		extendAtom(char *, bool *);
bool		extendAlternatives(char *);
void		initAtoms(void);
char *		atom_generator(char *, int);

		/* pl-bags.c */
word		pl_collect_bag(Word, Word),
		pl_record_bag(Word, Word);

		/* pl-ext.c */
void		initBuildIns(void);

		/* pl-dump.c */
word		pl_save_program(Word, Word),
		pl_save(Word, Word),
		pl_restore(Word);

		/* pl-dwim.c */
word		pl_dwim_match(Word, Word, Word),
		pl_dwim_predicate(Word, Word, word);
		
		/* pl-file.c */
bool		told(void),
		see(word),
		seen(void),
		seeString(char *),
		seeingString(void),
		seenString(void),
		tellString(char *, long),
		toldString(void),
		readLine(char *, int),
		unifyTime(Word, long);
int		currentInputLine(void);
int		currentLinePosition(void);
Atom		currentStreamName(void);
bool		put_character(Char);
char *		PrologPrompt(void);
#if AIX
word		Putf();
#else 
word		Putf(char *, ...);
#endif
word		vPutf(char *, va_list),
		pl_tty(void),
		pl_tty_fold(Word, Word),
		pl_put(Word),
		pl_get(Word),
		pl_get0(Word),
		pl_put2(Word, Word),
		pl_get2(Word, Word),
		pl_get02(Word, Word),
		pl_get_single_char(Word),
		pl_seeing(Word),
		pl_telling(Word),
		pl_seen(void),
		pl_told(void),
		pl_see(Word),
		pl_tell(Word),
		pl_append(Word),
		pl_ttyflush(void),
		pl_flush(void),
		pl_protocol(Word),
		pl_protocola(Word),
		pl_noprotocol(void),
		pl_protocolling(Word),
		pl_prompt(Word, Word),
		pl_prompt1(Word),
		pl_tab(Word),
		pl_tab2(Word, Word),
		pl_tmp_file(Word, Word),
		pl_time_file(Word, Word),
		pl_size_file(Word, Word),
		pl_access_file(Word, Word),
		pl_exists_file(Word),
		pl_exists_directory(Word),
		pl_delete_file(Word),
		pl_rename_file(Word, Word),
		pl_same_file(Word, Word),
		pl_fileerrors(Word, Word),
		pl_absolute_file_name(Word, Word),
		pl_chdir(Word),
		pl_file_dir_name(Word, Word),
		pl_file_base_name(Word, Word);
word		pl_open(Word, Word, Word),
		pl_open_null_stream(Word),
		pl_close(Word),
		pl_current_stream(Word, Word, Word, word),
		pl_flush_output(Word),
		pl_stream_position(Word, Word, Word),
		pl_set_input(Word),
		pl_set_output(Word),
		pl_current_input(Word),
		pl_current_output(Word),
		pl_character_count(Word, Word),
		pl_line_count(Word, Word),
		pl_line_position(Word, Word),
		pl_source_location(Word, Word),
		pl_wait_for_input(Word, Word, Word);
Char		get_character(void),
		getSingleChar(void);
FILE *		checkInput(int);
void		initIO(void),
		dieIO(void),
		closeFiles(void),
		newLineInput(void);
int		streamNo(Word, int);

		/* pl-glob.c */
bool		compilePattern(char *),
		matchPattern(char *);
word		pl_expand_file_name(Word, Word);
char *		ExpandOneFile(char *);

		/* pl-gc.c */
void		garbageCollect(LocalFrame),
		resetGC(void),
		considerGarbageCollect(Stack),
		lockw(Word), unlockw(Word),
		lockp(void *), unlockp(void *),
		lockMark(mark *), unlockMark(mark *);
void		growStacks(LocalFrame, Code, int, int, int);
word		pl_garbage_collect(Word),
		pl_collect_parms(Word, Word);

		/* pl-flag.c */
void		initFlags(void);
word		pl_flag(Word, Word, Word),
		pl_current_flag(Word, word);

		/* pl-funct.c */
FunctorDef	lookupFunctorDef(Atom name, int arity),
		isCurrentFunctor(Atom name, int arity);
void		initFunctors(void);
word		pl_current_functor(Word, Word, word);
int		atomIsFunctor(Atom);
bool		atomIsProcedure(Atom),
		atomIsProcedureModule(Atom, Module);

		/* pl-util.c */
char		*procedureName(Procedure),
		digitName(int, bool);
int		digitValue(int, char);
bool		wordToInteger(word, long *),
		wordToReal(word, real *),
		isUserSystemProcedure(Procedure),
		strsub(char *, char *),
		strprefix(char *, char *),
		strpostfix(char *, char *);
void		systemMode(bool);
word		notImplemented(char *, int);

		/* pl-index.c */
int		cardinalityPattern(unsigned long);
bool		reindexClause(Clause),
		indexPatternToTerm(Procedure, Word);
struct index	getIndex(Word, unsigned long, int);
Clause		findClause(Clause, Word, const Definition, bool *);

		/* pl-itf.h */
void		resetForeign(void);

		/* pl-wam.c */
#if COUNTING
word		pl_count(void);
#endif
#if O_COMPILE_OR
word		pl_alt(Word, word);
#endif
word		pl_break(void),
		pl_break1(Word),
		pl_abort(void),
		pl_metacut(void);
bool		callGoal(Module, word, bool),
		prolog(word),
		interpret(Module, word, bool),
		unify(Word, Word),
		unify_atomic(Word, word),
		unifyFunctor(Word, FunctorDef);

		/* pl-list.c */
word		pl_is_list(Word),
		pl_proper_list(Word),
		pl_length(Word, Word),
		pl_memberchk(Word, Word),
		pl_msort(Word, Word),
		pl_sort(Word, Word);

		/* pl-proc.c */
Procedure	lookupProcedure(FunctorDef, Module),
		isCurrentProcedure(FunctorDef, Module),
		resolveProcedure(FunctorDef, Module),
		findProcedure(Word),
		findCreateProcedure(Word);
bool		isDefinedProcedure(Procedure),
		assertProcedure(Procedure, Clause, char),
		abolishProcedure(Procedure, Module),
		retractClauseProcedure(Procedure, Clause);
SourceFile	lookupSourceFile(Atom),
		isCurrentSourceFile(Atom);
word		pl_current_predicate(Word, Word, word),
		pl_retract(Word, word),
		pl_abolish(Word, Word),
		pl_list_references(Word),
		pl_list_active_procedures(void),
		pl_predicate_attribute(Word, Word, Word),
		pl_index(Word),
		pl_source_file(Word, Word),
		pl_time_source_file(Word, Word, word),
		pl_start_consult(Word),
		pl_make_system_source_files(void);
void		unallocClause(Clause),
		trapUndefined(Procedure),
		resetReferences(void),
		removeClausesProcedure(Procedure),
		resetProcedure(Procedure),
		reindexProcedure(Procedure),
		freeClause(Clause);

		/* pl-prof.c */
word		pl_profile(Word, Word),
		pl_profile_count(Word, Word, Word),
		pl_profile_box(Word, Word, Word, Word, Word),
		pl_reset_profiler(void);
void		stopItimer(void);

		/* pl-read.c */
word		charpToNumber(char *),
		pl_raw_read(Word),
		pl_read_variables(Word, Word),
		pl_read_variables3(Word, Word, Word),
		pl_read(Word),
		pl_read2(Word, Word),
		pl_read_clause(Word),
		pl_read_clause2(Word, Word);
void		resetRead(void);

		/* pl-load.c */
char		*getExecutable(void);
bool		getSymbols(void);
void		resetLoader(void);
word		pl_load_foreign(Word, Word, Word, Word, Word);
word		pl_load_foreign1(Word);

		/* pl-main.c */

bool		sysError(char *fm , ...),
		fatalError(char *fm , ...),
		warning(char *fm , ...),
		vsysError(char *fm , va_list args ),
		vfatalError(char *fm , va_list args ),
		vwarning(char *fm , va_list args );
int		startProlog(int, char **, char **);

		/* pl-modul.c */
Word		stripModule(Word, Module *);
Module		lookupModule(Atom), isCurrentModule(Atom);
void		initModules(void);
bool		isPublicModule(Module, Procedure),
  		isSuperModule(Module, Module);
word		pl_current_module(Word, Word, word),
		pl_default_module(Word, Word, Word),
		pl_strip_module(Word, Word, Word),
		pl_module(Word, Word),
		pl_set_source_module(Word, Word),
		pl_declare_module(Word, Word),
		pl_export(Word),
		pl_export_list(Word, Word),
		pl_check_export(void),
		pl_context_module(Word),
		pl_import(Word);

		/* pl-comp.c */
Clause		assert_term(Word, char, Atom);
word		pl_assertz(Word), pl_asserta(Word),
		pl_assertz2(Word, Word), pl_asserta2(Word, Word),
		pl_record_clause(Word, Word),
		pl_nth_clause(Word, Word, Word, word),
		pl_xr_member(Word, Word, word),
		pl_clause(Word, Word, Word, word);
bool		decompileHead(Clause, Word),
		decompile(Clause, Word),
		splitClause(Word, Word *, Word *);
void		initWamTable(void);

		/* pl-fmt.c */
word		pl_format(Word, Word),
		pl_format3(Word, Word, Word),
		pl_format_predicate(Word, Word);

		/* pl-op.c */
Operator	lookupOperator(Atom, int),
		isCurrentOperator(Atom, int);
bool		isPrefixOperator(Atom, int *, int *),
		isPostfixOperator(Atom, int *, int *),
		isInfixOperator(Atom, int *, int *),
		operator(Atom, int, int),
		newOp(char *, int, int);
word		pl_current_op(Word, Word, Word, word),
		pl_op1(Word, Word, Word);
void		initOperators(void);

		/* pl-prims.c */
word		pl_nonvar(Word),
		pl_var(Word),
		pl_integer(Word),
		pl_float(Word),
		pl_number(Word),
		pl_arch(Word, Word),
		pl_atom(Word),
		pl_atomic(Word),
		pl_ground(Word),
		pl_unify(Word, Word),
		pl_notunify(Word, Word),
		pl_equal(Word, Word),
		pl_nonequal(Word, Word),
		pl_lessStandard(Word, Word),
		pl_lessEqualStandard(Word, Word),
		pl_greaterStandard(Word, Word),
		pl_greaterEqualStandard(Word, Word),
		pl_structural_equal(Word, Word),
		pl_structural_nonequal(Word, Word),
		pl_functor(Word, Word, Word),
		pl_arg(Word, Word, Word, word),
		pl_univ(Word, Word),
		pl_numbervars(Word, Word, Word, Word),
		pl_free_variables(Word, Word),
		pl_e_free_variables(Word, Word),
		pl_atom_length(Word, Word),
		pl_int_to_atom(Word, Word, Word),
		pl_format_number(Word, Word, Word),
		pl_apropos_match(Word, Word),
		pl_name(Word, Word),
		pl_concat(Word, Word, Word),
		pl_concat_atom(Word, Word),
#if O_STRING
		pl_string(Word),
		pl_string_length(Word, Word),
		pl_string_to_atom(Word, Word),
		pl_string_to_list(Word, Word),
		pl_substring(Word, Word, Word, Word),
		pl_write_on_string(Word, Word),
#endif
		pl_write_on_atom(Word, Word),
		pl_write_on_list(Word, Word),
		pl_term_to_atom(Word, Word, Word),
		pl_repeat(word),
		pl_fail(void),
		pl_halt(void),
		pl_statistics(Word, Word),
		pl_version(Word),
		pl_option(Word, Word),
		pl_please(Word, Word, Word),
		pl_style_check(Word, Word),
		pl_novice(Word, Word),
		stringToList(char *),
		pl_copy_term(Word, Word),
		pl_home(Word);
int		compareStandard(Word, Word),
		lengthList(Word),
		numberVars(Word, FunctorDef, int);
char 		*primitiveToString(word, bool),
		*formatInteger(bool, int, int, bool, long),
		*listToString(word),
		*toString(word);

		/* pl-table.c */
Symbol		lookupHTable(Table, Void),
		firstHTable(Table),
		nextHTable(Table, Symbol),
		lookupLocalTable(Table, Void);
Table		newHTable(int),
		newLocalTable(int);
bool		addHTable(Table, Void, Void),
		addLocalTable(Table, Void, Void),
		deleteHTable(Table, Void),
		unifyStringWithList(char *, Word);
void		clearHTable(Table);

		/* pl-rec.c */
void		initRecords(void);
word		getKey(Word),
		pl_recorda(Word, Word, Word),
		pl_recordz(Word, Word, Word),
		pl_recorded(Word, Word, Word, word),
		pl_current_key(Word, word),
		heapString(char *),
		copyTermToGlobal(Record),
		pl_erase(Word);
bool		freeRecord(Record),
		unifyKey(Word, word);
Record		copyTermToHeap(Word);

		/* pl-setup.c */
void		setupProlog(void),
		deallocateStacks(void),
		initSignals(void);
bool		restoreStack(Stack);
#if unix || EMX
void		deliverSignal(int, int, SIGNAL_CONTEXT_TYPE, char *);
handler_t	pl_signal(int, handler_t);
#endif
word		pl_limit_stack(Word, Word),
		pl_trim_stacks(void),
		pl_stack_parameter(Word, Word, Word, Word);

		/* pl-sys.c */
word		pl_shell(Word, Word),
#if LINK_THIEF
		pl_thief(Word),
#endif
		pl_grep(Word, Word, Word, word),
		pl_getenv(Word, Word),
		pl_setenv(Word, Word),
		pl_unsetenv(Word),
		pl_wildcard_match(Word, Word),
		pl_argv(Word),
		pl_convert_time(Word, Word, Word, Word, Word, Word, Word, Word),
		pl_get_time(Word),
		pl_sleep(Word);

		/* pl-trace.c */
int		tracePort(LocalFrame, int);
void		writeFrameGoal(LocalFrame, int),
		backTrace(LocalFrame, int),
		initTracer(void);
word		pl_trace(void),
		pl_notrace(void),
		pl_tracing(void),
		pl_debug(void),
		pl_nodebug(void),
		pl_debugging(void),
		pl_skip_level(Word, Word),
		pl_spy(Word),
		pl_nospy(Word),
		pl_leash(Word, Word),
		pl_visible(Word, Word),
		pl_unknown(Word, Word),
		pl_prolog_current_frame(Word),
		pl_prolog_frame_attribute(Word, Word, Word),
		pl_trace_continuation(Word);
void		interruptHandler(int);

		/* pl-wic.c */
bool		loadWicFile(char *, bool, bool);
word		pl_open_wic(Word),
		pl_close_wic(void),
		pl_add_clause_wic(Word, Word),
		pl_add_directive_wic(Word),
		pl_start_module_wic(Word, Word),
		pl_export_wic(Word, Word),
		pl_import_wic(Word, Word, Word);
bool		compileFileList(char *, int, char **);
bool		appendState(char *);

		/* pl-write.c */
word		pl_nl(void),
		pl_display(Word),
		pl_displayq(Word),
		pl_write(Word),
		pl_writeq(Word),
		pl_print(Word),
		pl_dprint(Word, Word),
		pl_display2(Word, Word),
		pl_displayq2(Word, Word),
		pl_write2(Word, Word),
		pl_writeq2(Word, Word),
		pl_print2(Word, Word),
		pl_nl1(Word);

		/* pl-term.c */

void		resetTerm(void);
word		pl_tty_get_capability(Word, Word, Word),
  		pl_tty_goto(Word, Word),
		pl_tty_put(Word, Word),
  		pl_set_tty(Word, Word);
