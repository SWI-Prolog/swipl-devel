/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#define unifyAtomic(p, w)	unify_atomic(p, (word)(w))

/* pl-alloc.c */
void		free_heap(Void mem, size_t n);
volatile void	outOf(Stack s);
Void		alloc_global(int n);
Void		alloc_heap(size_t n);
word		globalFunctor(FunctorDef def);
word		globalString(char *s);
word		heapString(char *s);
Word		newTerm(void);
double		unpack_real(Word p);
word		globalReal(real f);
word		heapReal(real f);
void		freeHeapReal(word w);
void		initAllocLocal(void);
Void		alloc_local(size_t n);
void		stopAllocLocal(void);
char *		store_string_local(char *s);
char *		store_string(char *s);
void		remove_string(char *s);
int		unboundStringHashValue(char *t);
void *		xmalloc(size_t size);
void *		xrealloc(void *mem, size_t size);

/* pl-wam.c */
word		pl_count(void);
bool		unify(Word t1, Word t2, LocalFrame env);
bool		unify_atomic(Word p, word a);
bool		unifyFunctor(Word term, FunctorDef functor);
word		pl_alt(Word skip, word h);
void		TrailAssignment(Word p);
#ifdef __WIN32__
void		do_undo(mark *m);
#else
inline void	do_undo(mark *m);
#endif

/* pl-atom.c */
Atom		lookupAtom(char *s);
word		pl_atom_hashstat(Word i, Word n);
void		initAtoms(void);
word		pl_current_atom(Word a, word h);
word		pl_complete_atom(Word prefix, Word common, Word unique);
word		pl_atom_completions(Word prefix, Word alts);
char *		atom_generator(char *prefix, int state);

/* pl-arith.c */

#ifdef AVOID_0X80000000_BIT
word		fconsNum(long i);
long		fvalNum(word w);
#endif
word		pl_between(Word l, Word h, Word n, word b);
word		pl_succ(Word n1, Word n2);
word		pl_plus(Word a, Word b, Word c);
word		compareNumbers(Word n1, Word n2, int what);
word		pl_lessNumbers(Word n1, Word n2);
word		pl_greaterNumbers(Word n1, Word n2);
word		pl_lessEqualNumbers(Word n1, Word n2);
word		pl_greaterEqualNumbers(Word n1, Word n2);
word		pl_nonEqualNumbers(Word n1, Word n2);
word		pl_equalNumbers(Word n1, Word n2);
word		pl_is(Word v, Word e);
word		pl_arithmetic_function(Word descr);
word		pl_current_arithmetic_function(Word f, word h);
void		initArith(void);
int		indexArithFunction(FunctorDef fdef, Module m);
FunctorDef	functorArithFunction(int n);
bool		ar_func_n(code n, int argc, Word *stack);
word		evaluate(Word p);

/* pl-bag.c */
word		pl_record_bag(Word key, Word value);
int 		checkBags(void);
word		pl_collect_bag(Word bindings, Word bag);

/* pl-comp.c */
void		initWamTable(void);
bool		splitClause(Word term, Word *head, Word *body, Module *m);
Clause		assert_term(Word term, int where, Atom file);
word		pl_assertz(Word term);
word		pl_asserta(Word term);
word		pl_assertz2(Word term, Word ref);
word		pl_asserta2(Word term, Word ref);
word		pl_record_clause(Word term, Word file, Word ref);
word		pl_redefine_system_predicate(Word term);
bool		decompileHead(Clause clause, Word head);
bool		decompileArg1(Clause clause, Word arg1);
bool		decompile(Clause clause, Word term);
word		pl_clause(Word p, Word term, Word ref, word h);
word		pl_nth_clause(Word p, Word n, Word ref, word h);
word		pl_xr_member(Word ref, Word term, word h);
word		pl_wam_list(Word ref);

/* pl-dump.c */
word		saveProgram(Word new);
word		pl_save_program(Word new, Word args);
word		pl_save(Word file, Word restore);
word		pl_restore(Word file);
word		parseSaveProgramOptions(Word args,
			int *local, int *global, int *trail, int *argument,
			char **goal, char **toplevel, char **init_file,
			bool *tty);

/* pl-index.c */
int		cardinalityPattern(unsigned long pattern);
void		getIndex(Word argv, unsigned long pattern, int card,
			 struct index *);
ClauseRef	findClause(ClauseRef cl,
			   Word argv, Definition def, bool *deterministic);
bool		reindexClause(Clause clause);
bool		indexPatternToTerm(Procedure proc, Word value);
bool		hashDefinition(Definition def, int buckets);
word		pl_hash(Word pred);
void		addClauseToIndex(Definition def, Clause cl, int where);
void		delClauseFromIndex(ClauseIndex ci, Clause cl);
void		gcClauseIndex(ClauseIndex ci);
void		unallocClauseIndexTable(ClauseIndex ci);
void		markDirtyClauseIndex(ClauseIndex ci, Clause cl);

/* pl-dwim.c */
word		pl_dwim_match(Word a1, Word a2, Word mm);
word		pl_dwim_predicate(Word term, Word dwim, word h);

/* pl-ext.c */
void		initBuildIns(void);

/* pl-file.c */
void		initIO(void);
void		dieIO(void);
void		closeFiles(void);
void		newLineInput(void);
int		currentLinePosition(void);
Char		get_character(void);
Char		getSingleChar(void);
bool		LockStream(void);
bool		UnlockStream(void);
bool		put_character(Char c);
word		Putf(char *fm, ...);
word		vPutf(char *fm, va_list args);
bool		readLine(char *buffer);
int		currentInputLine(void);
bool		told(void);
bool		see(word f);
bool		seen(void);
bool		seeString(char *s);
bool		seeingString(void);
bool		seenString(void);
bool		tellString(char *s, int n);
bool		toldString(void);
Atom		currentStreamName(void);
word		pl_wait_for_input(Word streams, Word available, Word timeout);
word		pl_put(Word c);
word		pl_put2(Word stream, Word chr);
word		pl_get(Word chr);
word		pl_skip(Word chr);
word		pl_skip2(Word stream, Word chr);
word		pl_get2(Word stream, Word chr);
word		pl_tty(void);
word		pl_get_single_char(Word c);
word		pl_get0(Word c);
word		pl_get02(Word stream, Word c);
word		pl_seeing(Word f);
word		pl_telling(Word f);
word		pl_seen(void);
word		pl_told(void);
word		pl_see(Word f);
word		pl_tell(Word f);
word		pl_append(Word f);
word		pl_ttyflush(void);
word		pl_flush(void);
word		pl_protocol(Word file);
word		pl_protocola(Word file);
word		pl_noprotocol(void);
word		pl_protocolling(Word file);
word		pl_prompt(Word old, Word new);
void		prompt1(char *prompt);
word		pl_prompt1(Word prompt);
word		pl_tab(Word n);
char *		PrologPrompt(void);
word		pl_tab2(Word stream, Word n);
word		pl_open(Word file, Word mode, Word stream);
word		pl_open4(Word file, Word mode, Word stream, Word options);
word		pl_open_null_stream(Word stream);
int		streamNo(Word spec, int mode);
word		pl_close(Word stream);
word		pl_current_stream(Word file, Word mode, Word stream, word h);
word		pl_flush_output(Word stream);
word		pl_stream_position(Word stream, Word old, Word new);
word		pl_set_input(Word stream);
word		pl_set_output(Word stream);
word		pl_current_input(Word stream);
word		pl_current_output(Word stream);
word		pl_character_count(Word stream, Word count);
word		pl_line_count(Word stream, Word count);
word		pl_line_position(Word stream, Word count);
word		pl_source_location(Word file, Word line);
bool		unifyTime(Word t, long int time);
word		pl_time_file(Word name, Word t);
word		pl_size_file(Word name, Word len);
word		pl_access_file(Word name, Word mode);
word		pl_read_link(Word file, Word link, Word to);
word		pl_exists_file(Word name);
word		pl_exists_directory(Word name);
word		pl_tmp_file(Word base, Word name);
word		pl_delete_file(Word name);
word		pl_same_file(Word file1, Word file2);
word		pl_rename_file(Word old, Word new);
word		pl_fileerrors(Word old, Word new);
word		pl_absolute_file_name(Word name, Word expanded);
word		pl_is_absolute_file_name(Word name);
word		pl_chdir(Word dir);
word		pl_file_base_name(Word f, Word b);
word		pl_file_dir_name(Word f, Word b);
word		pl_prolog_to_os_filename(Word pl, Word os);
int		Get0(void);
int		Put(int c);
void		protocol(int c);
IOSTREAM *	PL_current_input();
#ifdef O_XOS
word		pl_make_fat_filemap(Word dir);
#endif

/* pl-flag.c */
void		initFlags(void);
word		pl_flag(Word name, Word old, Word new);
word		pl_current_flag(Word k, word h);

/* pl-fmt.c */
word		pl_format_predicate(Word chr, Word descr);
word		pl_format(Word fmt, Word args);
word		pl_format3(Word stream, Word fmt, Word args);
bool		format(char *fm, ...);

/* pl-funct.c */
FunctorDef	lookupFunctorDef(Atom atom, int arity);
FunctorDef	isCurrentFunctor(Atom atom, int arity);
void		initFunctors(void);
int 		checkFunctors(void);
word		pl_current_functor(Word name, Word arity, word h);

/* pl-gc.c */
void		considerGarbageCollect(Stack s);
void		garbageCollect(LocalFrame fr);
word		pl_garbage_collect(Word d);
void		resetGC(void);
void		lockw(Word p);
void		lockp(void *ptr);
void		lockMark(mark *m);
void		unlockw(Word p);
void		unlockp(Void ptr);
void		unlockMark(mark *m);
Word		findGRef(int n);
void		growStacks(LocalFrame fr, Code PC, int l, int g, int t);

/* pl-glob.c */
bool		compilePattern(char *p);
bool		matchPattern(char *s);
word		pl_wildcard_match(Word pattern, Word string);
word		pl_expand_file_name(Word f, Word l);

/* pl-itf.c */
void		resetForeign(void);
void		reinitForeign(int argc, char **argv);

/* pl-list.c */
word		pl_is_list(Word list);
word		pl_proper_list(Word list);
word		pl_length(Word list, Word l);
word		pl_memberchk(Word e, Word list);
word		pl_msort(Word list, Word sorted);
word		pl_sort(Word list, Word sorted);

/* pl-load.c */
bool		getSymbols(void);
void		resetLoader(void);
long		allocText(long int size);
word		pl_load_foreign(Word file, Word entry, Word options, Word libraries, Word size);
word		pl_load_foreign1(Word file);
word		pl_open_shared_object(Word file, Word plhandle, Word flags);
word		pl_close_shared_object(Word plhandle);
word		pl_call_shared_object_function(Word plhandle, Word name);
word		pl_load_shared_object(Word file, Word entry);

/* pl-modul.c */
Module		lookupModule(Atom name);
void		initModules(void);
Word		stripModule(Word term, Module *module);
bool		isPublicModule(Module module, Procedure proc);
word		pl_default_module(Word me, Word old, Word new);
word		pl_current_module(Word module, Word file, word h);
word		pl_strip_module(Word spec, Word module, Word term);
word		pl_module(Word old, Word new);
word		pl_set_source_module(Word old, Word new);
word		pl_declare_module(Word name, Word file);
word		pl_export_list(Word modulename, Word list);
word		pl_export(Word head);
word		pl_check_export(void);
word		pl_context_module(Word module);
word		pl_import(Word pred);

/* pl-op.c */
Operator	isCurrentOperator(Atom name, int type);
word		pl_current_op(Word prec, Word type, Word name, word h);
bool		isPrefixOperator(Atom atom, int *type, int *priority);
bool		isPostfixOperator(Atom atom, int *type, int *priority);
bool		isInfixOperator(Atom atom, int *type, int *priority);
word		pl_op1(Word priority, Word type, Word name);
bool		newOp(char *name, int type, int pri);
void		initOperators(void);
word		pl_reset_operators(void);

/* pl-os.c */
bool		initOs(void);
volatile void	Halt(int status);
char *		OsError(void);
real		CpuTime(void);
Void		Allocate(long int n);
long		Random(void);
Atom		TemporaryFile(char *id);
char *		canonisePath(char *path);
void		RemoveTemporaryFiles(void);
char *		OsPath(const char *unixpath);
char *		PrologPath(char *ospath, char *plpath);
long		LastModifiedFile(char *f);
bool		ExistsFile(char *path);
bool		AccessFile(char *path, int mode);
bool		ExistsDirectory(char *path);
long		SizeFile(char *path);
int		RemoveFile(const char *path);
bool		RenameFile(char *old, char *new);
bool		SameFile(char *f1, char *f2);
bool		OpenStream(int fd);
bool		MarkExecutable(char *name);
bool		expandVars(char *pattern, char *expanded);
char *		ExpandOneFile(char *spec);
char *		getwd(char *buf);
char *		AbsoluteFile(char *spec);
int		IsAbsolutePath(const char *spec);
char *		BaseName(char *f);
char *		DirName(char *f);
char *		ReadLink(char *f);
char *		DeRefLink(char *link);
bool		ChDir(char *path);
struct tm *	LocalTime(long int *t);
long		Time(void);
Char		GetChar(void);
void		ResetTty(void);
bool		PushTty(ttybuf *buf, int mode);
bool		PopTty(ttybuf *buf);
char *		Setenv(char *name, char *value);
char *		Unsetenv(char *name);
int		System(char *cmd);
char *		Symbols(void);
void		Pause(real time);

/* pl-prims.c */
word		pl_nonvar(Word k);
word		pl_var(Word k);
word		pl_integer(Word k);
word		pl_float(Word k);
word		pl_string(Word k);
word		pl_number(Word k);
word		pl_atom(Word k);
word		pl_atomic(Word k);
word		pl_ground(Word term);
word		pl_compound(Word term);
#ifdef O_HASHTERM
word		pl_hash_term(Word term, Word hval);
#endif
word		pl_unify(Word t1, Word t2);
word		pl_notunify(Word t1, Word t2);
word		pl_equal(Word t1, Word t2);
word		pl_nonequal(Word t1, Word t2);
int		compareStandard(Word t1, Word t2);
word		pl_compare(Word rel, Word t1, Word t2);
word		pl_lessStandard(Word t1, Word t2);
word		pl_lessEqualStandard(Word t1, Word t2);
word		pl_greaterStandard(Word t1, Word t2);
word		pl_greaterEqualStandard(Word t1, Word t2);
word		pl_structural_equal(Word t1, Word t2);
word		pl_structural_nonequal(Word t1, Word t2);
word		pl_functor(Word t, Word f, Word a);
word		pl_arg(Word n, Word term, Word arg, word b);
word		pl_setarg(Word n, Word term, Word arg);
int		lengthList(Word list);
word		pl_univ(Word t, Word l);
int		numberVars(Word t, FunctorDef functor, int n);
word		pl_numbervars(Word t, Word atom, Word start, Word end);
word		pl_free_variables(Word t, Word l);
word		pl_e_free_variables(Word t, Word l);
word		pl_copy_term(Word f, Word t);
bool		unifyStringWithList(char *s, Word l);
word		stringToList(char *s);
char *		listToString(word list);
char *		primitiveToString(word w, bool save);
char *		toString(word w);
word		pl_atom_length(Word w, Word n);
word		pl_int_to_atom(Word number, Word base, Word atom);
char *		formatInteger(bool split, int div, int radix, bool small, long int n);
word		pl_format_number(Word format, Word number, Word string);
word		pl_name(Word atom, Word string);
word		pl_atom_chars(Word atom, Word string);
word		pl_number_chars(Word number, Word string);
word		pl_atom_char(Word atom, Word chr);
word		pl_concat(Word a1, Word a2, Word a3);
word		pl_concat_atom(Word list, Word atom);
word		pl_apropos_match(Word a1, Word a2);
word		pl_string_length(Word str, Word l);
word		pl_string_to_atom(Word str, Word a);
word		pl_string_to_list(Word str, Word list);
word		pl_substring(Word str, Word offset, Word length, Word sub);
word		pl_write_on_atom(Word goal, Word atom);
word		pl_write_on_string(Word goal, Word string);
word		pl_write_on_list(Word goal, Word string);
word		pl_term_to_atom(Word term, Word atom, Word bindings, Word e);
word		pl_repeat(word h);
word		pl_fail(void);
word		pl_true(void);
word		pl_halt(Word code);
word		pl_statistics(Word k, Word value);
void		setFeature(Atom name, word value);
word		getFeature(Atom name);
word		pl_feature(Word key, Word value, word h);
word		pl_set_feature(Word key, Word value);
word		pl_option(Word key, Word old, Word new);
word		pl_please(Word key, Word old, Word new);
word		pl_style_check(Word old, Word new);
word		pl_novice(Word old, Word new);

/* pl-pro.c */
word		pl_break(void);
word		pl_break1(Word goal);
word		pl_notrace1(Word goal);
bool		callGoal(Module module, word goal, bool debug);
word		pl_abort(void);
bool		prolog(volatile word goal);
word		pl_metacut(void);
int 		trap_gdb(void);
word		checkData(Word p, int on_heap);

/* pl-proc.c */
Procedure	lookupProcedure(FunctorDef f, Module m);
void		resetProcedure(Procedure proc);
Procedure	isCurrentProcedure(FunctorDef f, Module m);
Procedure	lookupProcedureToDefine(FunctorDef def, Module m);
bool		isDefinedProcedure(Procedure proc);
Procedure	findProcedure(Word descr);
Procedure	findCreateProcedure(Word descr);
word		pl_current_predicate(Word name, Word functor, word h);
bool		assertProcedure(Procedure proc, Clause clause, int where);
bool		abolishProcedure(Procedure proc, Module module);
bool		retractClauseProcedure(Procedure proc, Clause clause);
void		freeClause(Clause c);
void		freeClauseRef(ClauseRef c);
ClauseRef	newClauseRef(Clause cl);
void		gcClausesDefinition(Definition def);
void		resetReferences(void);
Procedure	resolveProcedure(FunctorDef f, Module module);
Definition	trapUndefined(Definition def);
word		pl_retract(Word term, word h);
word		pl_retractall(Word head);
word		pl_abolish(Word atom, Word arity);
word		pl_get_clause_attribute(Word ref, Word att, Word value);
word		pl_get_predicate_attribute(Word pred, Word what, Word value);
word		pl_set_predicate_attribute(Word pred, Word what, Word value);
void		reindexDefinition(Definition def);
void		startConsult(SourceFile f);
word		pl_index(Word pred);
SourceFile	lookupSourceFile(Atom name);
SourceFile	isCurrentSourceFile(Atom name);
SourceFile	indexToSourceFile(int index);
void		addProcedureSourceFile(SourceFile sf, Procedure proc);
word		pl_make_system_source_files(void);
word		pl_source_file(Word descr, Word file);
word		pl_time_source_file(Word file, Word time, word h);
word		pl_start_consult(Word file);
Definition	findDefinition(FunctorDef f, Module m);
word		pl_default_predicate(Word d1, Word d2);
Definition	autoImport(FunctorDef f, Module m);
word		pl_require(Word pred);
word		pl_check_definition(Word spec);

/* pl-prof.c */
void		stopItimer(void);
word		pl_profile(Word old, Word new);
word		pl_profile_count(Word head, Word calls, Word prom);
word		pl_profile_box(Word head, Word calls, Word redos, Word exits, Word fails);
word		pl_reset_profiler(void);

/* pl-read.c */
void		resetRead(void);
int		syntaxerrors(int new);
word		pl_syntaxerrors(Word old, Word new);
word		charpToNumber(char *s);
word		pl_raw_read(Word term);
word		pl_read_variables(Word term, Word variables);
word		pl_read_variables3(Word stream, Word term, Word variables);
word		pl_read(Word term);
word		pl_read2(Word stream, Word term);
word		pl_read_clause(Word term);
word		pl_read_clause2(Word stream, Word term);

/* pl-rec.c */
void		initRecords(void);
Record		copyTermToHeap(Word term);
word		copyTermToGlobal(Record term);
bool		freeRecord(Record record);
bool		unifyKey(Word key, word val);
word		getKey(Word key);
word		pl_current_key(Word k, word h);
word		pl_recorda(Word key, Word term, Word ref);
word		pl_recordz(Word key, Word term, Word ref);
word		pl_recorded(Word key, Word term, Word ref, word h);
word		pl_erase(Word ref);

/* pl-setup.c */
void		setupProlog(void);
void		initSignals(void);
handler_t	pl_signal(int sig, handler_t func);
void		deliverSignal(int sig, int type, SignalContext scp, char *addr);
void		deallocateStacks(void);
bool		restoreStack(Stack s);
void		trimStacks(void);
word		pl_trim_stacks(void);
word		pl_limit_stack(Word s, Word l);
word		pl_stack_parameter(Word name, Word key, Word old, Word new);

/* pl-sys.c */
word		pl_shell(Word command, Word status);
word		pl_getenv(Word var, Word value);
word		pl_setenv(Word var, Word value);
word		pl_unsetenv(Word var);
word		pl_argv(Word list);
word		pl_grep(Word file, Word search, Word line, word h);
word		pl_convert_time(Word time, Word year, Word month, Word day, Word hour, Word minute, Word second, Word usec);
word		pl_get_time(Word t);
word		pl_sleep(Word time);

/* pl-table.c */
Table		newHTable(int size);
void		destroyHTable(Table ht);
Symbol		lookupHTable(Table ht, Void name);
bool		addHTable(Table ht, Void name, Void value);
bool		deleteHTable(Table ht, Void name);
Symbol		nextHTable(Table ht, Symbol s);
Symbol		firstHTable(Table ht);
void		clearHTable(Table ht);
Table		newLocalTable(int size);
Symbol		lookupLocalTable(Table ht, Void name);
bool		addLocalTable(Table ht, Void name, Void value);
void		initTables();

/* pl-trace.c */
int		tracePort(LocalFrame frame, int port);
void		writeFrameGoal(LocalFrame frame, int how);
void		backTrace(LocalFrame frame, int depth);
word		pl_trace_continuation(Word what);
void		interruptHandler(int sig);
void		initTracer(void);
word		pl_trace(void);
word		pl_notrace(void);
word		pl_tracing(void);
word		pl_debug(void);
word		pl_nodebug(void);
word		pl_debugging(void);
word		pl_skip_level(Word old, Word new);
word		pl_spy(Word p);
word		pl_nospy(Word p);
word		pl_leash(Word old, Word new);
word		pl_visible(Word old, Word new);
word		pl_debuglevel(Word old, Word new);
word		pl_unknown(Word old, Word new);
word		pl_prolog_current_frame(Word fr);
word		pl_prolog_frame_attribute(Word frame, Word what, Word value);

/* pl-util.c */
bool		wordToInteger(word w, long int *n);
bool		wordToReal(word w, real *f);
char		digitName(int n, bool small);
int		digitValue(int b, int c);
char *		procedureName(Procedure proc);
char *		predicateName(Definition def);
bool		isUserSystemProcedure(Procedure proc);
bool		isUserSystemPredicate(Definition def);
word		notImplemented(char *name, int arity);
bool		strprefix(char *string, char *prefix);
bool		strpostfix(char *string, char *postfix);
bool		stripostfix(char *string, char *postfix);
bool		strsub(char *string, char *sub);
void		systemMode(bool accept);
bool		scan_options(word list, int flags, OptSpec specs, ...);
#ifndef HAVE_STRICMP
int		stricmp(const char *s1, const char *s2);
#endif
#ifndef HAVE_STRLWR
char *		strlwr(char *s);
#endif

/* pl-wic.c */
bool		loadWicFile(char *file, bool toplevel, bool load_options);
word		pl_open_wic(Word name, Word options);
word		pl_close_wic(void);
word		pl_add_directive_wic(Word term);
word		pl_import_wic(Word module, Word head);
bool		compileFileList(char *out, int argc, char **argv);
bool		appendState(char *name);
void		qlfCleanup(void);

word		pl_qlf_put_states(void);
word		pl_qlf_start_module(Word name);
word		pl_qlf_start_sub_module(Word name);
word		pl_qlf_start_file(Word name);
word		pl_qlf_end_part(void);
word		pl_qlf_open(Word file);
word		pl_qlf_close(void);
word		pl_qlf_load(Word file, Word module);
word		pl_qlf_assert_clause(Word ref);
word		pl_qlf_info(Word file, Word cversion, Word fversion, Word info);

/* pl-write.c */
word		pl_nl(void);
word		pl_nl1(Word stream);
word		pl_display(Word term);
word		pl_displayq(Word term);
word		pl_display2(Word stream, Word term);
word		pl_displayq2(Word stream, Word term);
word		pl_write(Word term);
word		pl_writeq(Word term);
word		pl_print(Word term);
word		pl_dprint(Word term, Word g);
word		pl_write2(Word stream, Word term);
word		pl_writeq2(Word stream, Word term);
word		pl_print2(Word stream, Word term);

/* pl-term.c */
void		resetTerm(void);
word		pl_tty_get_capability(Word name, Word type, Word value);
word		pl_tty_goto(Word x, Word y);
word		pl_tty_put(Word a, Word affcnt);
word		pl_set_tty(Word old, Word new);

/* pl-main.c */
int		startProlog(int argc, char **argv, char **env);
bool		sysError(char *fm, ...);
bool		fatalError(char *fm, ...);
bool		warning(char *fm, ...);
bool		vsysError(char *fm, va_list args);
bool		vfatalError(char *fm, va_list args);
bool		vwarning(char *fm, va_list args);

/* pl-dde.c */

word		pl_open_dde_conversation(Word service, Word topic, Word hdl);
word		pl_close_dde_conversation(Word handle);
word		pl_dde_request(Word handle, Word item, Word value, Word tmo);
word		pl_dde_execute(Word handle, Word cmd, Word tmo);
word		pl_dde_register_service(Word service, Word onoff);

/* pl-dll.c */
word		pl_open_dll(Word name, Word handle);
word		pl_close_dll(Word handle);
word		pl_call_dll_function(Word handle, Word funcname);

/* pl-nt.c */

void		PlMessage(const char *buf, ...);
word		pl_window_title(Word old, Word new);
