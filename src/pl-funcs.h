/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

/* pl-alloc.c */
void		freeHeap(void *mem, size_t n);
word		outOfStack(Stack s, stack_overflow_action how);
volatile void	outOfCore(void);
Word		allocGlobal(int words);
Void		allocHeap(size_t n);
void		initMemAlloc(void);
void		cleanupMemAlloc(void);
word		globalFunctor(functor_t def);
int		sizeString(word w);
word		globalString(const char *s);
word		globalNString(long len, const char *s);
Word		newTerm(void);
void		doublecpy(void *to, void *from);
word		globalReal(real f);
word		globalLong(long i);
double		valReal(word w);
word		globalIndirect(word in);
int		equalIndirect(word r1, word r2);
word		globalIndirectFromCode(Code *PC);
word		makeRef(Word p);
#ifndef consPtr
word		consPtr(void *p, int ts);
#endif
char *		store_string(const char *s);
void		remove_string(char *s);
int		unboundStringHashValue(const char *t, unsigned int len);
#ifndef xmalloc
void *		xmalloc(size_t size);
void *		xrealloc(void *mem, size_t size);
#endif

/* pl-wam.c */
word		pl_count(void);
bool		unify_ptrs(Word t1, Word t2);
bool		can_unify(Word t1, Word t2);
void		TrailAssignment(Word p);
void		DoTrail(Word p);
void		do_undo(mark *m);
void		fix_term_ref_count(void);
word		pl_unify_with_occurs_check(term_t t1, term_t t2);

/* pl-atom.c */
word		lookupAtom(const char *s, unsigned int len);
word		pl_atom_hashstat(term_t i, term_t n);
void		initAtoms(void);
void		cleanupAtoms(void);
word		pl_current_atom2(term_t a, term_t refs, word h);
word		pl_current_atom(term_t a, word h);
word		pl_complete_atom(term_t prefix, term_t common, term_t unique);
word		pl_atom_completions(term_t prefix, term_t alts);
void		markAtom(atom_t a);
foreign_t	pl_garbage_collect_atoms(void);
void		resetAtoms(void);
#ifdef O_DEBUG_ATOMGC
word		pl_track_atom(term_t which, term_t stream);
void		_PL_debug_register_atom(atom_t a,
					const char *file, int line,
					const char *func);
void		_PL_debug_unregister_atom(atom_t a,
					  const char *file, int line,
					  const char *func);
#endif


/* pl-arith.c */

word		pl_between(term_t l, term_t h, term_t n, word b);
word		pl_succ(term_t n1, term_t n2);
word		pl_plus(term_t a, term_t b, term_t c);
int		ar_compare(Number n1, Number n2, int what);
word		pl_lessNumbers(term_t n1, term_t n2);
word		pl_greaterNumbers(term_t n1, term_t n2);
word		pl_lessEqualNumbers(term_t n1, term_t n2);
word		pl_greaterEqualNumbers(term_t n1, term_t n2);
word		pl_nonEqualNumbers(term_t n1, term_t n2);
word		pl_equalNumbers(term_t n1, term_t n2);
word		pl_is(term_t v, term_t e);
word		pl_arithmetic_function(term_t descr);
word		pl_current_arithmetic_function(term_t f, word h);
word		pl_prolog_arithmetic_function(term_t f, word h);
void		initArith(void);
void		cleanupArith(void);
int		indexArithFunction(functor_t fdef, Module m);
functor_t	functorArithFunction(int n);
bool		ar_func_n(code n, int argc, Number *stack);
int		valueExpression(term_t p, Number n ARG_LD);
int		toIntegerNumber(Number n);
void		canoniseNumber(Number n);

/* pl-bag.c */
word		pl_record_bag(term_t term);
int 		checkBags(void);
word		pl_collect_bag(term_t bindings, term_t bag);
foreign_t	pl_except_bag(term_t ex);

/* pl-comp.c */
void		initWamTable(void);
void		get_head_and_body_clause(term_t clause,
					 term_t head, term_t body, Module *m);
Clause		compileClause(Word head, Word body,
			      Procedure proc, Module module);
Clause		assert_term(term_t term, int where, SourceLoc loc);
void		unregisterAtomsClause(Clause clause);
word		pl_assertz(term_t term);
word		pl_asserta(term_t term);
word		pl_assertz2(term_t term, term_t ref);
word		pl_asserta2(term_t term, term_t ref);
word		pl_record_clause(term_t term, term_t file, term_t ref);
word		pl_redefine_system_predicate(term_t term);
bool		decompileHead(Clause clause, term_t head);
int		arg1Key(Clause clause, word *key);
bool		decompile(Clause clause, term_t term, term_t bindings);
word		pl_clause4(term_t p, term_t t, term_t ref, term_t b, word h);
word		pl_clause3(term_t p, term_t term, term_t ref, word h);
word		pl_clause2(term_t p, term_t term, word h);
word		pl_nth_clause(term_t p, term_t n, term_t ref, word h);
word		pl_xr_member(term_t ref, term_t term, word h);
void		wamListClause(Clause clause);
Code		wamListInstruction(IOSTREAM *out, Clause clause, Code bp);
word		pl_wam_list(term_t ref);
word		pl_fetch_vm(term_t ref, term_t offset, term_t noffset,
			    term_t instruction);
int		unify_definition(term_t head, Definition def,
				 term_t thehead, int flags);
word		pl_clause_term_position(term_t ref, term_t pc, term_t locterm);
word		pl_break_pc(term_t ref, term_t pc, term_t nextpc, control_t h);
word		pl_break_at(term_t ref, term_t pc, term_t set);
code		replacedBreak(Code PC);
void		clearBreakPointsClause(Clause clause);
word		pl_current_break(term_t ref, term_t pc, control_t h);

/* pl-dump.c */
word		saveProgram(term_t new);
word		pl_save_program(term_t new, term_t args);
word		pl_save(term_t file, term_t restore);
word		pl_restore(term_t file);
word		parseSaveProgramOptions(term_t args,
			int *local, int *global, int *trail, int *argument,
			char **goal, char **toplevel, char **init_file,
			bool *tty, bool *standalone);

/* pl-index.c */
int		cardinalityPattern(unsigned long pattern);
void		getIndex(Word argv, unsigned long pattern, int card,
			 struct index *
			 ARG_LD);
ClauseRef	firstClause(Word argv, LocalFrame fr, Definition def,
			    ClauseRef *next ARG_LD);
ClauseRef	findClause(ClauseRef cl, Word argv,
			   LocalFrame fr, Definition def, ClauseRef *next);
bool		reindexClause(Clause clause);
bool		unify_index_pattern(Procedure proc, term_t value);
bool		hashDefinition(Definition def, int buckets);
word		pl_hash(term_t pred);
void		addClauseToIndex(Definition def, Clause cl, int where);
void		delClauseFromIndex(ClauseIndex ci, Clause cl);
void		gcClauseIndex(ClauseIndex ci);
void		unallocClauseIndexTable(ClauseIndex ci);
void		markDirtyClauseIndex(ClauseIndex ci, Clause cl);

/* pl-dwim.c */
word		pl_dwim_match(term_t a1, term_t a2, term_t mm);
word		pl_dwim_predicate(term_t term, term_t dwim, word h);

/* pl-ext.c */
void		initBuildIns(void);
void		cleanupExtensions(void);

/* pl-error.c */

int		PL_error(const char *pred, int arity, const char *msg,
			 int id, ...);
char *		tostr(char *buf, const char *fmt, ...);
void		printMessage(atom_t severity, ...);
int		PL_get_nchars_ex(term_t t, unsigned int *len, char **s,
				 unsigned int flags);
int		PL_get_chars_ex(term_t t, char **s, unsigned int flags);
int		PL_get_atom_ex(term_t t, atom_t *a);
int		PL_get_integer_ex(term_t t, int *i);
int		PL_get_long_ex(term_t t, long *i);
int		PL_get_bool_ex(term_t t, int *i);
int		PL_unify_list_ex(term_t l, term_t h, term_t t);
int		PL_unify_nil_ex(term_t l);
int		PL_unify_bool_ex(term_t t, bool val);

/* pl-file.c */
void		initIO(void);
void		dieIO(void);
void		closeFiles(int all);
int		openFileDescriptors(unsigned char *buf, int size);
void		protocol(const char *s, int n);
bool		getInputStream(term_t t, IOSTREAM **s);
bool		getOutputStream(term_t t, IOSTREAM **s);
bool		streamStatus(IOSTREAM *s);
atom_t		fileNameStream(IOSTREAM *s);
int		getSingleChar(IOSTREAM *s);
bool		readLine(IOSTREAM *in, IOSTREAM *out, char *buffer);
bool		LockStream(void);
bool		UnlockStream(void);
IOSTREAM *	PL_current_input(void);
IOSTREAM *	PL_current_output(void);
word		pl_push_input_context(void);
word		pl_pop_input_context(void);
word		pl_told(void);
word		pl_flush_output(void);
word		pl_see(term_t f);
word		pl_seen(void);
word		pl_noprotocol(void);
bool		seeString(const char *s);
bool		seeingString(void);
bool		seenString(void);
bool		tellString(char **s, int *size);
bool		toldString(void);
word		pl_wait_for_input(term_t streams, term_t avail, term_t tmo);
word		pl_put(term_t c);
word		pl_put2(term_t stream, term_t chr);
word		pl_get(term_t chr);
word		pl_skip(term_t chr);
word		pl_skip2(term_t stream, term_t chr);
word		pl_get2(term_t stream, term_t chr);
word		pl_get_single_char(term_t c);
word		pl_get_byte(term_t c);
word		pl_get_byte2(term_t stream, term_t c);
word		pl_get_char(term_t c);
word		pl_get_char2(term_t stream, term_t c);
word		pl_seeing(term_t f);
word		pl_telling(term_t f);
word		pl_tell(term_t f);
word		pl_append(term_t f);
word		pl_ttyflush(void);
word		pl_protocol(term_t file);
word		pl_protocola(term_t file);
word		pl_protocolling(term_t file);
word		pl_prompt(term_t old, term_t new);
void		prompt1(char *prompt);
word		pl_prompt1(term_t prompt);
word		pl_tab(term_t n);
char *		PrologPrompt(void);
word		pl_tab2(term_t stream, term_t n);
word		pl_open4(term_t file, term_t mode, term_t stream, term_t opt);
word		pl_open(term_t file, term_t mode, term_t stream);
word		pl_open_null_stream(term_t stream);
int		streamNo(term_t spec, int mode);
word		pl_close(term_t stream);
word		pl_close2(term_t stream, term_t options);
void		release_stream_handle(term_t spec);
foreign_t	pl_stream_property(term_t stream, term_t property, word h);
word		pl_flush_output1(term_t stream);
word		pl_set_stream_position(term_t stream, term_t pos);
word		pl_seek(term_t stream,
			term_t offset, term_t method, term_t newloc);
word		pl_set_input(term_t stream);
word		pl_set_output(term_t stream);
foreign_t	pl_set_stream(term_t stream, term_t attr);
word		pl_current_input(term_t stream);
word		pl_current_output(term_t stream);
word		pl_character_count(term_t stream, term_t count);
word		pl_line_count(term_t stream, term_t count);
word		pl_line_position(term_t stream, term_t count);
word		pl_source_location(term_t file, term_t line);
word		pl_at_end_of_stream1(term_t stream);
word		pl_at_end_of_stream0();
word		pl_peek_byte2(term_t stream, term_t chr);
word		pl_peek_byte1(term_t chr);
word		pl_peek_code2(term_t stream, term_t chr);
word		pl_peek_code1(term_t chr);
word		pl_peek_char2(term_t stream, term_t chr);
word		pl_peek_char1(term_t chr);
bool		unifyTime(term_t t, long time);
word		pl_time_file(term_t name, term_t t);
word		pl_size_file(term_t name, term_t len);
word		pl_size_stream(term_t stream, term_t len);
word		pl_access_file(term_t name, term_t mode);
word		pl_read_link(term_t file, term_t link, term_t to);
word		pl_exists_file(term_t name);
word		pl_exists_directory(term_t name);
word		pl_tmp_file(term_t base, term_t name);
word		pl_delete_file(term_t name);
word		pl_delete_directory(term_t name);
word		pl_make_directory(term_t name);
word		pl_same_file(term_t file1, term_t file2);
word		pl_rename_file(term_t old, term_t new);
word		pl_fileerrors(term_t old, term_t new);
word		pl_absolute_file_name(term_t name, term_t expanded);
word		pl_is_absolute_file_name(term_t name);
word		pl_chdir(term_t dir);
word		pl_file_base_name(term_t f, term_t b);
word		pl_file_dir_name(term_t f, term_t b);
word		pl_file_name_extension(term_t base, term_t ext, term_t full);
word		pl_prolog_to_os_filename(term_t pl, term_t os);
foreign_t	pl_mark_executable(term_t path);
#ifdef __WIN32__
word		pl_make_fat_filemap(term_t dir);
#endif
word		pl_copy_stream_data3(term_t in, term_t out, term_t len);
word		pl_copy_stream_data2(term_t in, term_t out);
int		PL_get_char(term_t c, int *p);
int		PL_unify_char(term_t chr, int c, int mode);
int		PL_unify_stream_or_alias(term_t t, IOSTREAM *s);
void		pushOutputContext(void);
void		popOutputContext(void);

/* pl-flag.c */
void		initFlags(void);
word		pl_flag(term_t name, term_t old, term_t new);
word		pl_current_flag(term_t k, word h);
void		initFeatures(void);

/* pl-fli.c */
word		makeNum(long i);
void		finish_foreign_frame(ARG1_LD);
void		_PL_put_number(term_t t, Number n);
predicate_t	_PL_predicate(const char *name, int arity, const char *module,
			      predicate_t *bin);
void		initialiseForeign(int argc, char **argv);
void		cleanupInitialiseHooks(void);
char *		buffer_string(const char *s, int flags);
atom_t		codeToAtom(int code);
extern record_t PL_duplicate_record(record_t r);
int		PL_unify_termv(term_t t, va_list args);
term_t		wordToTermRef(Word p);
void		_PL_get_arg_ld(int index, term_t t, term_t a ARG_LD);

/* pl-fmt.c */
word		pl_format_predicate(term_t chr, term_t descr);
word		pl_current_format_predicate(term_t chr, term_t descr,
					    control_t h);
word		pl_format(term_t fmt, term_t args);
word		pl_format3(term_t s, term_t fmt, term_t args);

/* pl-funct.c */
functor_t	lookupFunctorDef(atom_t atom, int arity);
functor_t	isCurrentFunctor(atom_t atom, int arity);
void		initFunctors(void);
void		cleanupFunctors(void);
int 		checkFunctors(void);
word		pl_current_functor(term_t name, term_t arity, word h);

/* pl-gc.c */
void		considerGarbageCollect(Stack s);
void		garbageCollect(LocalFrame fr, Choice ch);
word		pl_garbage_collect(term_t d);
void		blockGC(void);
void		unblockGC(void);
Word		findGRef(int n);
int		growStacks(LocalFrame fr, Choice ch, Code PC,
			   int l, int g, int t);
void		clearUninitialisedVarsFrame(LocalFrame, Code);
word		check_foreign(void);	/* O_SECURE stuff */
void		markAtomsOnStacks(PL_local_data_t *ld);
#ifdef O_SECURE
word		checkStacks(LocalFrame fr, Choice ch);
#endif

/* pl-glob.c */
word		pl_wildcard_match(term_t pattern, term_t string);
word		pl_expand_file_name(term_t f, term_t l);

/* pl-itf.c */
void		resetForeign(void);

/* pl-list.c */
word		pl_is_list(term_t list);
word		pl_proper_list(term_t list);
word		pl_length(term_t list, term_t l);
word		pl_memberchk(term_t e, term_t list);
word		pl_msort(term_t list, term_t sorted);
word		pl_sort(term_t list, term_t sorted);

/* pl-load.c */
word		pl_open_shared_object(term_t file, term_t h, term_t flags);
word		pl_close_shared_object(term_t plhandle);
word		pl_call_shared_object_function(term_t plhandle, term_t name);
word		pl_load_shared_object(term_t file, term_t entry);

/* pl-modul.c */
Module		lookupModule(atom_t name);
Module		isCurrentModule(atom_t name);
void		initModules(void);
int		isSuperModule(Module s, Module m);
Word		stripModule(Word term, Module *module);
bool		isPublicModule(Module module, Procedure proc);
int		declareModule(atom_t name, SourceFile sf);
word		pl_default_module(term_t me, term_t old, term_t new);
word		pl_current_module(term_t module, term_t file, word h);
word		pl_strip_module(term_t spec, term_t module, term_t term);
word		pl_module(term_t old, term_t new);
word		pl_set_source_module(term_t old, term_t new);
word		pl_term_expansion_module(term_t name, word h);
word		pl_goal_expansion_module(term_t name, word h);
word		pl_declare_module(term_t name, term_t file);
word		pl_export_list(term_t modulename, term_t list);
word		pl_export(term_t head);
word		pl_check_export(void);
word		pl_context_module(term_t module);
word		pl_import(term_t pred);
#ifdef O_PROLOG_HOOK
word		pl_set_prolog_hook(term_t module, term_t old, term_t new);
#endif


/* pl-op.c */
int		currentOperator(Module m, atom_t name, int kind,
				int *type, int *priority);
int		priorityOperator(Module m, atom_t atom);
word		pl_current_op(term_t prec, term_t type, term_t name, word h);
word		pl_local_op(term_t prec, term_t type, term_t name, word h);
word		pl_op(term_t priority, term_t type, term_t name);
void		initOperators(void);
word		pl_builtin_op(term_t prec, term_t type, term_t name, word h);

/* pl-os.c */
bool		initOs(void);
void		cleanupOs(void);
char *		OsError(void);
real		CpuTime(void);
long		Random(void);
char *		canonisePath(char *path);
char *		OsPath(const char *plpath, char *ospath);
char *		PrologPath(const char *ospath, char *plpath);
long		LastModifiedFile(char *f);
bool		ExistsFile(const char *path);
bool		AccessFile(const char *path, int mode);
bool		ExistsDirectory(const char *path);
long		SizeFile(const char *path);
int		RemoveFile(const char *path);
void		RemoveTemporaryFiles(void);
bool		RenameFile(const char *old, const char *new);
bool		SameFile(const char *f1, const char *f2);
bool		OpenStream(int fd);
bool		MarkExecutable(const char *name);
bool		expandVars(const char *pattern, char *expanded);
char *		ExpandOneFile(const char *spec, char *file);
char *		getwd(char *buf);
char *		AbsoluteFile(const char *spec, char *path);
int		IsAbsolutePath(const char *spec);
char *		BaseName(const char *f);
char *		DirName(const char *f, char *buf);
char *		ReadLink(const char *f, char *buf);
char *		DeRefLink(const char *link, char *buf);
bool		ChDir(const char *path);
atom_t		TemporaryFile(const char *id);
int		hasConsole(void);
struct tm *	LocalTime(long int *t);
Char		GetChar(void);
char *		getenv3(const char *, char *buf, int buflen);
int		getenvl(const char *);
int		Setenv(char *name, char *value);
int		Unsetenv(char *name);
int		System(char *cmd);
char *		findExecutable(const char *module, char *buf);
void		Pause(real time);
#if __WIN32__
int		iswin32s(void);
#endif /*__WIN32__*/

/* pl-prims.c */
word		pl_nonvar(term_t k);
word		pl_var(term_t k);
word		pl_integer(term_t k);
word		pl_float(term_t k);
word		pl_string(term_t k);
word		pl_number(term_t k);
word		pl_atom(term_t k);
word		pl_atomic(term_t k);
word		pl_ground(term_t term);
word		pl_compound(term_t term);
word		pl_callable(term_t term);
word		pl_deterministic(void);
#ifdef O_HASHTERM
word		pl_hash_term(term_t term, term_t hval);
#endif
word		pl_unify(term_t t1, term_t t2);
word		pl_notunify(term_t t1, term_t t2);
word		pl_equal(term_t t1, term_t t2);
word		pl_nonequal(term_t t1, term_t t2);
int		compareStandard(Word t1, Word t2);
word		pl_compare(term_t rel, term_t t1, term_t t2);
word		pl_lessStandard(term_t t1, term_t t2);
word		pl_lessEqualStandard(term_t t1, term_t t2);
word		pl_greaterStandard(term_t t1, term_t t2);
word		pl_greaterEqualStandard(term_t t1, term_t t2);
word		pl_structural_equal(term_t t1, term_t t2);
word		pl_structural_nonequal(term_t t1, term_t t2);
word		pl_functor(term_t t, term_t f, term_t a);
word		pl_arg(term_t n, term_t t, term_t a, word b);
word		pl_setarg(term_t n, term_t term, term_t arg);
int		lengthList(term_t list, int errors);
word		pl_univ(term_t t, term_t l);
int		numberVars(term_t t, functor_t functor, int n);
word		pl_numbervars(term_t t, term_t atom,
			      term_t start, term_t end);
word		pl_free_variables(term_t t, term_t l);
word		pl_e_free_variables(term_t t, term_t l);
word		pl_copy_term(term_t f, term_t t);
word		stringToList(char *s);
word		pl_atom_length(term_t w, term_t n);
word		pl_int_to_atom(term_t number, term_t base,
			       term_t atom);
char *		formatInteger(bool split, int div, int radix,
			      bool small, long n, char *out);
word		pl_format_number(term_t format, term_t number,
				 term_t string);
word		pl_name(term_t atom, term_t string);
word		pl_atom_chars(term_t atom, term_t string);
word		pl_atom_codes(term_t atom, term_t string);
word		pl_number_chars(term_t number, term_t string);
word		pl_number_codes(term_t number, term_t string);
word		pl_char_code(term_t atom, term_t chr);
word		pl_atom_prefix(term_t atom, term_t prefix);
word		pl_atom_concat(term_t a1, term_t a2, term_t a3, control_t ctx);
word		pl_concat_atom(term_t list, term_t atom);
word		pl_concat_atom3(term_t list, term_t sep, term_t atom);
word		pl_apropos_match(term_t a1, term_t a2);
foreign_t	pl_sub_atom(term_t atom,
			    term_t before, term_t len, term_t after,
			    term_t sub, word h);
word		pl_string_length(term_t str, term_t l);
word		pl_string_concat(term_t a1, term_t a2, term_t a3, word h);
word		pl_string_to_atom(term_t str, term_t a);
word		pl_string_to_list(term_t str, term_t list);
word		pl_sub_string(term_t str,
			      term_t offset, term_t length, term_t after,
			      term_t sub, word h);
word		pl_write_on_string(term_t goal, term_t string);
word		pl_repeat(word h);
word		pl_fail(void);
word		pl_true(void);
word		pl_halt(term_t code);
word		pl_statistics(term_t k, term_t value);
word		pl_option(term_t key, term_t old, term_t new, control_t h);
int		set_pl_option(const char *name, const char *value);
word		pl_style_check(term_t old, term_t new);
word		pl_novice(term_t old, term_t new);

/* pl-feature.c */
void		defFeature(const char *name, int flags, ...);
word		pl_feature(term_t key, term_t value, word h);
word		pl_feature5(term_t key, term_t value,
			    term_t local, term_t access, term_t type,
			    word h);
word		pl_set_feature(term_t key, term_t value);
int		setDoubleQuotes(atom_t a, unsigned int *flagp);

/* pl-pro.c */
word		pl_break(void);
word		pl_break1(term_t goal);
word		pl_notrace1(term_t goal);
#ifdef O_LIMIT_DEPTH
word		pl_depth_limit(term_t limit, term_t olimit, term_t oreached);
word		pl_depth_limit_true(term_t limit,
				    term_t olimit, term_t oreached,
				    term_t res, term_t cut, word b);
word		pl_depth_limit_false(term_t limit,
				     term_t olimit, term_t oreached,
				     term_t res);
#endif /*O_LIMIT_DEPTH*/
int		callProlog(Module module, term_t goal, int flags, term_t *ex);
word		pl_abort(abort_type type);
bool		prologToplevel(atom_t toplevel);
word		pl_metacut(void);
int 		trap_gdb(void);
word		checkData(Word p);

/* pl-proc.c */
Procedure	lookupProcedure(functor_t f, Module m);
Procedure	isCurrentProcedure(functor_t f, Module m);
Procedure	lookupProcedureToDefine(functor_t def, Module m);
bool		hasClausesDefinition(Definition def);
bool		isDefinedProcedure(Procedure proc);
int		get_head_functor(term_t head, functor_t *fdef, int flags);
int		get_procedure(term_t descr, Procedure *proc, term_t he, int f);
word		pl_current_predicate(term_t name, term_t functor, word h);
foreign_t	pl_current_predicate1(term_t spec, word ctx);
ClauseRef	assertProcedure(Procedure proc, Clause clause, int where);
bool		abolishProcedure(Procedure proc, Module module);
bool		retractClauseProcedure(Procedure proc, Clause clause);
void		retractClauseDefinition(Definition def, Clause clause);
void		freeClause(Clause c);
void		freeClauseRef(ClauseRef c);
ClauseRef	newClauseRef(Clause cl);
void		gcClausesDefinition(Definition def);
void		resetReferences(void);
Procedure	resolveProcedure(functor_t f, Module module);
Definition	trapUndefined(Definition def);
word		pl_retract(term_t term, word h);
word		pl_retractall(term_t head);
word		pl_abolish(term_t atom, term_t arity);
word		pl_abolish1(term_t pred);
word		pl_get_clause_attribute(term_t ref, term_t att, term_t value);
word		pl_get_predicate_attribute(term_t pred, term_t k, term_t v);
word		pl_set_predicate_attribute(term_t pred, term_t k, term_t v);
void		reindexDefinition(Definition def);
void		redefineProcedure(Procedure proc, SourceFile sf);
void		startConsult(SourceFile f);
word		pl_index(term_t pred);
SourceFile	lookupSourceFile(atom_t name);
SourceFile	indexToSourceFile(int index);
void		cleanupSourceFiles(void);
void		addProcedureSourceFile(SourceFile sf, Procedure proc);
word		pl_make_system_source_files(void);
word		pl_source_file(term_t descr, term_t file, control_t h);
word		pl_time_source_file(term_t file, term_t t, control_t h);
word		pl_start_consult(term_t file);
word		pl_default_predicate(term_t d1, term_t d2);
Definition	autoImport(functor_t f, Module m);
word		pl_require(term_t pred);
word		pl_check_definition(term_t spec);
word		pl_clause_from_source(term_t file, term_t line, term_t clause);
foreign_t	pl_list_generations(term_t desc);
foreign_t	pl_check_procedure(term_t desc);
void		checkDefinition(Definition def);
Procedure	isStaticSystemProcedure(functor_t fd);


/* pl-prof.c */
void		stopItimer(void);
word		pl_profile(term_t old, term_t new);
word		pl_profile_count(term_t head, term_t calls, term_t prom);
word		pl_profile_box(term_t head,
			       term_t calls, term_t redos,
			       term_t exits, term_t fails);
word		pl_reset_profiler(void);
bool		stopProfiler(void);


/* pl-read.c */
void		resetRead(void);
int		get_number(const unsigned char *string,
			   unsigned char **end,
			   Number value);
word		pl_raw_read(term_t term);
word		pl_raw_read2(term_t stream, term_t term);
word		pl_read(term_t term);
word		pl_read2(term_t stream, term_t term);
word		pl_read_clause(term_t term);
word		pl_read_clause2(term_t stream, term_t term);
word		pl_read_term(term_t term, term_t pos);
word		pl_read_term3(term_t stream, term_t term, term_t pos);
word		pl_atom_to_term(term_t term, term_t atom, term_t bindings);
void		initCharConversion(void);
foreign_t	pl_char_conversion(term_t in, term_t out);
foreign_t	pl_current_char_conversion(term_t in, term_t out, word h);


/* pl-rec.c */
void		initRecords(void);
Record		compileTermToHeap(term_t term, int flags);
void		copyRecordToGlobal(term_t copy, Record term ARG_LD);
int		structuralEqualArg1OfRecord(term_t t, Record r ARG_LD);
bool		freeRecord(Record record);
bool		unifyKey(term_t key, word val);
int		getKeyEx(term_t key, word *k);
word		pl_current_key(term_t k, word h);
word		pl_recorda(term_t key, term_t term, term_t ref);
word		pl_recordz(term_t key, term_t term, term_t ref);
word		pl_recorded(term_t key, term_t term, term_t ref, word h);
word		pl_erase(term_t ref);
word		pl_term_complexity(term_t t, term_t mx, term_t count);
void		undo_while_saving_term(mark *m, Word term);

/* pl-rl.c */
void		install_rl(void);

/* pl-setup.c */
void		setupProlog(void);
foreign_t	pl_on_signal(term_t sig, term_t name, term_t old, term_t new);
handler_t	set_sighandler(int sig, handler_t func);
void		blockSignals(sigset_t *mask);
void		unblockSignals(sigset_t *mask);
void		unblockSignal(int sig);
void		blockSignal(int sig);
void		resetSignals(void);
void		cleanupSignals(void);
int		initPrologStacks(long local,
				 long global,
				 long trail,
				 long argument);
void		initPrologLocalData(void);
void		deallocateStacks(void);
bool		restoreStack(Stack s);
void		trimStacks(void);
void		resetStacks(void);
void		emptyStacks(void);
void		freeStacks(PL_local_data_t *ld);
void		freeLocalData(PL_local_data_t *ld);
word		pl_trim_stacks(void);
word		pl_limit_stack(term_t s, term_t l);
word		pl_stack_parameter(term_t s, term_t k, term_t o, term_t n);
void		ensureRoomStack(Stack s, int n);
int		_PL_get_signum(term_t sig, int *n);

/* pl-sys.c */
word		pl_shell(term_t command, term_t status);
word		pl_getenv(term_t var, term_t value);
word		pl_setenv(term_t var, term_t value);
word		pl_unsetenv(term_t var);
word		pl_convert_time(term_t time, term_t year,
				term_t month, term_t day,
				term_t hour, term_t minute,
				term_t second, term_t usec);
word		pl_convert_time2(term_t time, term_t string);
word		pl_get_time(term_t t);
word		pl_sleep(term_t time);
word		pl_get_pid(term_t pid);

/* pl-table.c */
void		initTables();
Table		newHTable(int size);
void		destroyHTable(Table ht);
Symbol		lookupHTable(Table ht, void *name);
bool		addHTable(Table ht, void *name, void *value);
void		deleteSymbolHTable(Table ht, Symbol s);
void		clearHTable(Table ht);
Table		copyHTable(Table org);
TableEnum	newTableEnum(Table ht);
void		freeTableEnum(TableEnum e);
Symbol		advanceTableEnum(TableEnum e);

/* pl-trace.c */
int		tracePort(LocalFrame frame, Choice bfr, int port, Code PC);
void		backTrace(LocalFrame frame, int depth);
void		initTracer(void);
void		resetTracer(void);
int		tracemode(int new, int *old);
int		debugmode(debug_type new, debug_type *old);
word		pl_trace(void);
word		pl_notrace(void);
word		pl_tracing(void);
word		pl_skip_level(term_t old, term_t new);
word		pl_spy(term_t p);
word		pl_nospy(term_t p);
word		pl_leash(term_t old, term_t new);
word		pl_visible(term_t old, term_t new);
word		pl_debuglevel(term_t old, term_t new);
word		pl_prolog_current_frame(term_t fr);
word		pl_prolog_frame_attribute(term_t fr, term_t key, term_t val);
void		callEventHook(int ev, ...);

/* pl-util.c */
char		digitName(int n, bool small);
int		digitValue(int b, int c);
char *		procedureName(Procedure proc);
char *		predicateName(Definition def);
word		notImplemented(char *name, int arity);
word		setBoolean(int *flag, term_t o, term_t n);
word		setInteger(int *val, term_t old, term_t new);
word		setLong(long *val, term_t old, term_t new);
bool		strprefix(const char *string, const char *prefix);
bool		strpostfix(const char *string, const char *postfix);
bool		stripostfix(const char *string, const char *postfix);
bool		scan_options(term_t list, int flags, atom_t name,
			     const opt_spec *specs, ...);
#ifndef HAVE_STRICMP
int		stricmp(const char *s1, const char *s2);
#endif
#ifndef HAVE_STRLWR
char *		strlwr(char *s);
#endif

/* pl-wic.c */
bool		loadWicFromStream(IOSTREAM *fd);
word		pl_open_wic(term_t name);
word		pl_close_wic(void);
word		pl_add_directive_wic(term_t term);
word		pl_import_wic(term_t module, term_t head);
bool		compileFileList(IOSTREAM *out, int argc, char **argv);
void		qlfCleanup(void);

word		pl_qlf_put_states(void);
word		pl_qlf_start_module(term_t name);
word		pl_qlf_start_sub_module(term_t name);
word		pl_qlf_start_file(term_t name);
word		pl_qlf_end_part(void);
word		pl_qlf_open(term_t file);
word		pl_qlf_close(void);
word		pl_qlf_load(term_t file, term_t module);
word		pl_qlf_assert_clause(term_t ref, term_t saveclass);
word		pl_qlf_info(term_t file, term_t cvers, term_t fvers, term_t i);

/* pl-write.c */
char *		varName(term_t var, char *buf);
word		pl_nl(void);
word		pl_nl1(term_t stream);
word		pl_write_canonical(term_t term);
word		pl_write_canonical2(term_t stream, term_t term);
word		pl_write_term(term_t term, term_t options);
word		pl_write_term3(term_t stream,
			       term_t term, term_t options);
word		pl_write(term_t term);
word		pl_writeq(term_t term);
word		pl_print(term_t term);
word		pl_write2(term_t stream, term_t term);
word		pl_writeq2(term_t stream, term_t term);
word		pl_print2(term_t stream, term_t term);

/* pl-term.c */
void		resetTerm(void);
word		pl_tty_get_capability(term_t name, term_t type, term_t value);
word		pl_tty_goto(term_t x, term_t y);
word		pl_tty_put(term_t a, term_t affcnt);
word		pl_tty_size(term_t r, term_t c);

/* pl-main.c */
int		startProlog(int argc, char **argv);
bool		sysError(const char *fm, ...);
bool		fatalError(const char *fm, ...);
bool		warning(const char *fm, ...);
bool		vfatalError(const char *fm, va_list args);
bool		vwarning(const char *fm, va_list args);

/* pl-dde.c */

word		pl_open_dde_conversation(term_t serv, term_t top, term_t hdl);
word		pl_close_dde_conversation(term_t handle);
word		pl_dde_request(term_t h, term_t it, term_t value, term_t tmo);
word		pl_dde_execute(term_t handle, term_t cmd, term_t tmo);
word		pl_dde_register_service(term_t service, term_t onoff);
word		pl_dde_poke(term_t h, term_t item, term_t data, term_t tmo);

/* pl-dll.c */
word		pl_open_dll(term_t name, term_t handle);
word		pl_close_dll(term_t handle);
word		pl_call_dll_function(term_t handle, term_t funcname);

/* pl-nt.c */

void		PlMessage(const char *buf, ...);
word		pl_window_title(term_t old, term_t new);
word		pl_win_exec(term_t command, term_t show);
word		pl_shell_execute(term_t op, term_t file);
foreign_t	pl_win_module_file(term_t module, term_t file);
#ifdef EMULATE_DLOPEN
void *		dlopen(const char *file, int flags);
const char *	dlerror(void);
void *		dlsym(void *handle, char *symbol);
int		dlclose(void *handle);
#endif /*EMULATE_DLOPEN*/
foreign_t	pl_get_registry_value(term_t Key, term_t Name, term_t Value);
void		getDefaultsFromRegistry(void);

/* pl-rc.c */
IOSTREAM *      SopenRC(void *rca,
			const char *name, const char *rcclass, int flags);
foreign_t	pl_rc_handle(term_t h);
foreign_t       pl_rc_open(term_t rc_h,
			   term_t name, term_t class,
			   term_t rw, term_t handle);
foreign_t       pl_rc_open_archive(term_t file, term_t handle);
foreign_t       pl_rc_close_archive(term_t rc_h);
foreign_t       pl_rc_save_archive(term_t rc_h, term_t to);
foreign_t       pl_rc_append_file(term_t rc_h,
				  term_t name, term_t class, term_t encoding,
				  term_t file);
foreign_t	pl_rc_members(term_t rc_h, term_t members);

/* pl-xterm.c */

foreign_t	pl_open_xterm(term_t title, term_t in, term_t out);

/* pl-ctype.c */

foreign_t	pl_char_type(term_t chr, term_t class, word h);
foreign_t	pl_code_type(term_t chr, term_t class, word h);
void		initCharTypes(void);
void		systemMode(bool accept);


/* pl-thread.c */
foreign_t	pl_with_mutex(term_t mutex, term_t goal);

