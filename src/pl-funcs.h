/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam,
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
COMMON(type) defines the symbol to be global  with regard to Prolog, but
not exported from the shared object.  On   GCC  based  platforms this is
achieved using the visibility attribute. Making  symbols local to Prolog
avoid the ELF  dynamic  linker  picking   the  wrong  symbol  for  other
libraries and avoids Prolog picking wrong   symbols. It also reduces ELF
symbol lookup and relocations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* pl-attvar.c */
COMMON(void)		assignAttVar(Word av, Word value ARG_LD);
COMMON(int)		saveWakeup(wakeup_state *state, int forceframe ARG_LD);
COMMON(void)		restoreWakeup(wakeup_state *state ARG_LD);
COMMON(int)		PL_get_attr__LD(term_t t, term_t a ARG_LD);
COMMON(int)		on_attvar_chain(Word avp);
COMMON(Word)		alloc_attvar(ARG1_LD);

/* pl-gvar.c */

COMMON(void)		destroyGlobalVars();
COMMON(void)		freezeGlobal(ARG1_LD);
COMMON(int)		gvar_value__LD(atom_t name, Word p ARG_LD);

/* pl-wam.c */
COMMON(word)		pl_count(void);
COMMON(void)		TrailAssignment__LD(Word p ARG_LD);
COMMON(void)		do_undo(mark *m);
COMMON(Definition)	getProcDefinition__LD(Definition def ARG_LD);
COMMON(Definition)	getProcDefinitionForThread(Definition def, unsigned int tid);
COMMON(void)		destroyLocalDefinition(Definition def, unsigned int tid);
COMMON(void)		fix_term_ref_count(void);
COMMON(fid_t)		PL_open_foreign_frame__LD(ARG1_LD);
COMMON(void)		PL_close_foreign_frame__LD(fid_t id ARG_LD);
COMMON(fid_t)		PL_open_signal_foreign_frame(int sync);
COMMON(int)		foreignWakeup(term_t ex ARG_LD);
COMMON(void)		updateAlerted(PL_local_data_t *ld);
COMMON(int)		raiseSignal(PL_local_data_t *ld, int sig);
COMMON(Module)		contextModule(LocalFrame fr);
COMMON(void)		setContextModule(LocalFrame fr, Module context);
COMMON(int)		existingChoice(Choice ch ARG_LD);

/* pl-stream.c */

COMMON(void)		unallocStream(IOSTREAM *s);

/* pl-supervisor.c */

COMMON(void)		freeCodesDefinition(Definition def, int linger);
COMMON(int)		createForeignSupervisor(Definition def, Func f);
COMMON(int)		createUndefSupervisor(Definition def);
COMMON(int)		createSupervisor(Definition def);
COMMON(size_t)		supervisorLength(Code base);
COMMON(void)		initSupervisors(void);

/* pl-atom.c */
#define checkAtoms()	checkAtoms_src(__FILE__, __LINE__)
COMMON(word)		lookupAtom(const char *s, size_t len);
COMMON(word)		lookupBlob(const char *s, size_t len,
				   PL_blob_t *type, int *new);
COMMON(word)		pl_atom_hashstat(term_t i, term_t n);
COMMON(void)		do_init_atoms(void);
COMMON(int)		resetListAtoms(void);
COMMON(void)		cleanupAtoms(void);
COMMON(void)		markAtom(atom_t a);
COMMON(foreign_t)	pl_garbage_collect_atoms(void);
COMMON(void)		resetAtoms(void);
COMMON(int)		checkAtoms_src(const char *file, int line);
#ifdef O_DEBUG_ATOMGC
COMMON(word)		pl_track_atom(term_t which, term_t stream);
COMMON(void)		_PL_debug_register_atom(atom_t a,
					const char *file, int line,
					const char *func);
COMMON(void)		_PL_debug_unregister_atom(atom_t a,
					  const char *file, int line,
					  const char *func);
#endif


/* pl-arith.c */

COMMON(int)		ar_compare(Number n1, Number n2, int what);
COMMON(int)		ar_compare_eq(Number n1, Number n2);
COMMON(int)		pl_ar_add(Number n1, Number n2, Number r);
COMMON(int)		ar_mul(Number n1, Number n2, Number r);
COMMON(word)		pl_current_arithmetic_function(term_t f, control_t h);
COMMON(void)		initArith(void);
COMMON(void)		cleanupArith(void);
COMMON(int)		indexArithFunction(functor_t fdef);
COMMON(functor_t)	functorArithFunction(unsigned int n);
COMMON(bool)		ar_func_n(int findex, int argc ARG_LD);
COMMON(int)		ar_add_ui(Number n, intptr_t add);
COMMON(int)		valueExpression(term_t p, Number n ARG_LD);
COMMON(int)		toIntegerNumber(Number n, int flags);
COMMON(int)		arithChar(Word p ARG_LD);
COMMON(int)		getCharExpression(Word p, Number r ARG_LD);
COMMON(Number)		growArithStack(ARG1_LD);
COMMON(void)		freeArithLocalData(PL_local_data_t *ld);
COMMON(int)		ar_sign_i(Number n1);
COMMON(int)		ar_signbit(Number n1);
COMMON(int)		check_float(double f);
COMMON(int)		PL_eval_expression_to_int64_ex(term_t t, int64_t *val);

/* pl-bag.c */
COMMON(void)		markAtomsFindall(PL_local_data_t *ld);

/* pl-comp.c */
COMMON(void)		initWamTable(void);
COMMON(void)		freeVarDefs(PL_local_data_t *ld);
COMMON(int)		get_head_and_body_clause(term_t clause,
					 term_t head, term_t body,
					 Module *m ARG_LD);
COMMON(Procedure)	lookupBodyProcedure(functor_t functor, Module tm);
COMMON(int)		compileClause(Clause *cp, Word head, Word body,
				      Procedure proc, Module module,
				      term_t warnings ARG_LD);
COMMON(Clause)		assert_term(term_t term, ClauseRef where, atom_t owner,
				    SourceLoc loc ARG_LD);
COMMON(void)		forAtomsInClause(Clause clause, void (func)(atom_t a));
COMMON(Code)		stepDynPC(Code PC, const code_info *ci);
COMMON(bool)		decompileHead(Clause clause, term_t head);
COMMON(Code)		skipArgs(Code PC, int skip);
COMMON(int)		argKey(Code PC, int skip, word *key);
COMMON(int)		arg1Key(Code PC, word *key);
COMMON(bool)		decompile(Clause clause, term_t term, term_t bindings);
COMMON(word)		pl_nth_clause(term_t p, term_t n, term_t ref,
				      control_t h);
COMMON(void)		wamListClause(Clause clause);
COMMON(Code)		wamListInstruction(IOSTREAM *out, Code relto, Code bp);
COMMON(int)		unify_definition(Module ctx, term_t head, Definition def,
					 term_t thehead, int flags);
COMMON(code)		replacedBreak(Code PC);
COMMON(code)		replacedBreakUnlocked(Code PC);
COMMON(int)		clearBreakPointsClause(Clause clause) WUNUSED;
COMMON(int)		unify_functor(term_t t, functor_t fd, int how);
COMMON(void)		vm_list(Code code);
COMMON(Module)		clauseBodyContext(const Clause cl);

/* pl-dump.c */
COMMON(word)		saveProgram(term_t new);
COMMON(word)		pl_save_program(term_t new, term_t args);
COMMON(word)		pl_save(term_t file, term_t restore);
COMMON(word)		pl_restore(term_t file);
COMMON(word)		parseSaveProgramOptions(term_t args,
			int *local, int *global, int *trail, int *argument,
			char **goal, char **toplevel, char **init_file,
			bool *tty, bool *standalone);

/* pl-index.c */
COMMON(word)		getIndexOfTerm(term_t t);
COMMON(ClauseRef)	firstClause(Word argv, LocalFrame fr, Definition def,
				    ClauseChoice next ARG_LD);
COMMON(ClauseRef)	nextClause__LD(ClauseChoice chp, Word argv, LocalFrame fr,
				       Definition def ARG_LD);
COMMON(int)		addClauseToIndexes(Definition def, Clause cl,
					   ClauseRef where);
COMMON(void)		delClauseFromIndex(Definition def, Clause cl);
COMMON(void)		cleanClauseIndexes(Definition def, gen_t active);
COMMON(void)		clearTriedIndexes(Definition def);
COMMON(void)		unallocClauseIndexTable(ClauseIndex ci);
COMMON(void)		deleteActiveClauseFromIndexes(Definition def, Clause cl);
COMMON(bool)		unify_index_pattern(Procedure proc, term_t value);
COMMON(void)		deleteIndexes(Definition def, int isnew);
COMMON(int)		checkClauseIndexSizes(Definition def, int nindexable);
COMMON(void)		checkClauseIndexes(Definition def);
COMMON(void)		listIndexGenerations(Definition def, gen_t gen);

/* pl-dwim.c */
COMMON(word)		pl_dwim_match(term_t a1, term_t a2, term_t mm);
COMMON(word)		pl_dwim_predicate(term_t term, term_t dwim,
					  control_t h);

/* pl-ext.c */
COMMON(void)		initBuildIns(void);
COMMON(void)		cleanupExtensions(void);
COMMON(void)            rememberExtensions(const char *module,
					   const PL_extension *e);

/* pl-flag.c */
COMMON(void)		initFlags(void);
COMMON(void)		cleanupFlags(void);
COMMON(word)		pl_current_flag(term_t k, control_t h);

/* pl-fli.c */
COMMON(word)		linkVal__LD(Word p ARG_LD);
COMMON(void)		bArgVar(Word ap, Word vp ARG_LD);
COMMON(int)		_PL_put_number__LD(term_t t, Number n ARG_LD);
COMMON(predicate_t)	_PL_predicate(const char *name, int arity,
				      const char *module, predicate_t *bin);
COMMON(void)		initialiseForeign(int argc, char **argv);
COMMON(void)		cleanupInitialiseHooks(void);
COMMON(atom_t)		codeToAtom(int code);
COMMON(int)		PL_unify_term__LD(term_t t ARG_LD, ...);
COMMON(int)		PL_unify_termv(term_t t, va_list args);
COMMON(int)		PL_unify_termv__LD(term_t t ARG_LD, va_list args);
COMMON(term_t)		pushWordAsTermRef__LD(Word p ARG_LD);
COMMON(void)		popTermRef__LD(ARG1_LD);
COMMON(void)		_PL_get_arg__LD(size_t index, term_t t, term_t a ARG_LD);
COMMON(term_t)		PL_new_term_ref__LD(ARG1_LD);
COMMON(term_t)		PL_new_term_ref_noshift__LD(ARG1_LD);
COMMON(term_t)		PL_new_term_refs__LD(int n ARG_LD);
COMMON(void)		PL_reset_term_refs__LD(term_t r ARG_LD);
COMMON(term_t)		PL_copy_term_ref__LD(term_t from ARG_LD);
COMMON(int)		PL_unify__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int)		PL_unify_output__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int)		PL_unify_integer__LD(term_t t1, intptr_t i ARG_LD);
COMMON(int)		PL_unify_int64__LD(term_t t1, int64_t ARG_LD);
COMMON(int)		PL_unify_int64_ex__LD(term_t t1, int64_t ARG_LD);
COMMON(int)		PL_unify_functor__LD(term_t t, functor_t f ARG_LD);
COMMON(int)		PL_get_atom__LD(term_t t1, atom_t *a ARG_LD);
COMMON(int)		PL_get_text_as_atom(term_t t, atom_t *a, int flags);
COMMON(int)		PL_put_variable__LD(term_t t1 ARG_LD);
COMMON(int)		PL_put_atom__LD(term_t t1, atom_t a ARG_LD);
COMMON(int)		PL_put_integer__LD(term_t t1, long i ARG_LD);
COMMON(int)		PL_put_intptr__LD(term_t t1, intptr_t i ARG_LD);
COMMON(int)		PL_is_atomic__LD(term_t t ARG_LD);
COMMON(int)		PL_is_functor__LD(term_t t, functor_t f ARG_LD);
COMMON(int)		PL_is_variable__LD(term_t t ARG_LD);
COMMON(int)		PL_strip_module__LD(term_t q, module_t *m,
					    term_t t, int flags ARG_LD) WUNUSED;
COMMON(int)		PL_strip_module_ex__LD(term_t raw, module_t *m,
					       term_t plain ARG_LD) WUNUSED;
COMMON(int)		PL_qualify(term_t raw, term_t qualified);
COMMON(int)		PL_get_integer__LD(term_t t, int *i ARG_LD);
COMMON(int)		PL_get_long__LD(term_t t, long *i ARG_LD);
COMMON(int)		PL_get_int64__LD(term_t t, int64_t *i ARG_LD);
COMMON(int)		PL_get_size_ex__LD(term_t t, size_t *i ARG_LD);
COMMON(int)		PL_get_number__LD(term_t t, Number n ARG_LD);
COMMON(int)		PL_get_pointer__LD(term_t t, void **ptr ARG_LD);
COMMON(int)		PL_put_term__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int)		PL_get_functor__LD(term_t t, functor_t *f ARG_LD);
COMMON(int)		PL_get_name_arity_sz__LD(term_t t, atom_t *name,
						 size_t *arity ARG_LD);
COMMON(int)		PL_get_uintptr(term_t t, size_t *i);
COMMON(int)		PL_unify_atom__LD(term_t t, atom_t a ARG_LD);
COMMON(int)		PL_unify_pointer__LD(term_t t, void *ptr ARG_LD);
COMMON(int)		PL_get_list__LD(term_t l, term_t h, term_t t ARG_LD);
COMMON(int)		PL_is_atom__LD(term_t t ARG_LD);
COMMON(int)		PL_unify_list__LD(term_t l, term_t h, term_t t ARG_LD);
COMMON(int)		PL_cons_list__LD(term_t l, term_t head, term_t tail
					 ARG_LD);
COMMON(int)		PL_cons_list_v(term_t list, size_t count, term_t elems);
COMMON(int)		PL_is_inf(term_t t);
COMMON(int)		PL_same_term__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int)		isUCSAtom(Atom a);
COMMON(atom_t)		lookupUCSAtom(const pl_wchar_t *s, size_t len);
COMMON(int)		charCode(word w);
COMMON(int)		isCallable(word w ARG_LD);

COMMON(void)		registerForeignLicenses(void);
COMMON(void)            bindExtensions(const char *module,
				       const PL_extension *ext);
COMMON(void)		initForeign(void);
COMMON(int)		PL_rethrow(void);
COMMON(int)		PL_pending__LD(int sig ARG_LD);
COMMON(int)		PL_clearsig__LD(int sig ARG_LD);
COMMON(void)		cleanupCodeToAtom(void);
COMMON(void)		PL_clear_foreign_exception(LocalFrame fr);
COMMON(except_class)    classify_exception__LD(term_t ex ARG_LD);
COMMON(except_class)    classify_exception_p__LD(Word p ARG_LD);

/* pl-fmt.c */
COMMON(word)		pl_format_predicate(term_t chr, term_t descr);
COMMON(word)		pl_current_format_predicate(term_t chr, term_t descr,
					    control_t h);
COMMON(word)		pl_format(term_t fmt, term_t args);
COMMON(word)		pl_format3(term_t s, term_t fmt, term_t args);

/* pl-funct.c */
COMMON(functor_t)	lookupFunctorDef(atom_t atom, size_t arity);
COMMON(functor_t)	isCurrentFunctor(atom_t atom, size_t arity);
COMMON(void)		initFunctors(void);
COMMON(void)		cleanupFunctors(void);
COMMON(int)		checkFunctors(void);
COMMON(word)		pl_current_functor(term_t name, term_t arity,
					   control_t h);

/* pl-gc.c */
COMMON(int)		considerGarbageCollect(Stack s);
COMMON(int)		garbageCollect(void);
COMMON(word)		pl_garbage_collect(term_t d);
COMMON(Word)		findGRef(int n);
COMMON(size_t)		nextStackSizeAbove(size_t n);
COMMON(int)		shiftTightStacks(void);
COMMON(int)		growStacks(size_t l, size_t g, size_t t);
COMMON(size_t)		nextStackSize(Stack s, size_t minfree);
COMMON(int)		makeMoreStackSpace(int overflow, int flags);
COMMON(int)		ensureGlobalSpace(size_t cells, int flags);
COMMON(int)		ensureTrailSpace(size_t cells);
COMMON(int)		growLocalSpace__LD(size_t bytes, int flags ARG_LD);
COMMON(void)		clearUninitialisedVarsFrame(LocalFrame, Code);
COMMON(void)		clearLocalVariablesFrame(LocalFrame fr);
COMMON(void)		setLTopInBody(void);
COMMON(word)		check_foreign(void);	/* DEBUG(CHK_SECURE...) stuff */
COMMON(void)		markAtomsOnStacks(PL_local_data_t *ld);
COMMON(void)		markPredicatesInEnvironments(PL_local_data_t *ld);
COMMON(QueryFrame)	queryOfFrame(LocalFrame fr);
COMMON(void)		mark_active_environment(struct bit_vector *active,
						LocalFrame fr, Code PC);
#if defined(O_DEBUG) || defined(SECURE_GC) || defined(O_MAINTENANCE)
word			checkStacks(void *vm_state);
COMMON(bool)		scan_global(int marked);
COMMON(char *)		print_addr(Word p, char *buf);
COMMON(char *)		print_val(word w, char *buf);
#endif

/* pl-itf.c */
COMMON(void)		resetForeign(void);

/* pl-load.c */
COMMON(word)		pl_load_shared_object(term_t file, term_t entry);
COMMON(void)		cleanupForeign(void);

/* pl-modul.c */
COMMON(Module)		lookupModule__LD(atom_t name ARG_LD);
COMMON(Module)		isCurrentModule__LD(atom_t name ARG_LD);
COMMON(void)		initModules(void);
COMMON(void)		cleanupModules(void);
COMMON(int)		addModuleSourceFile(SourceFile sf, Module m);
COMMON(int)		setSuperModule(Module m, Module s);
COMMON(int)		isSuperModule(Module s, Module m);
COMMON(void)		clearSupersModule(Module m);
COMMON(int)		addSuperModule(Module m, Module s, int where);
COMMON(int)		getUnknownModule(Module m);
COMMON(Word)		stripModule(Word term, Module *module, int flags ARG_LD);
COMMON(Word)		stripModuleName(Word term, atom_t *name ARG_LD);
COMMON(bool)		isPublicModule(Module module, Procedure proc);
COMMON(int)		exportProcedure(Module module, Procedure proc);
COMMON(int)		declareModule(atom_t name, atom_t class, atom_t super,
				      SourceFile sf, int line,
				      int rdef);
COMMON(word)		pl_context_module(term_t module);
COMMON(int)		atomToImportStrength(atom_t a);
COMMON(word)		pl_import(term_t pred);
#ifdef O_PROLOG_HOOK
COMMON(word)		pl_set_prolog_hook(term_t module, term_t old, term_t new);
#endif


/* pl-op.c */
COMMON(int)		currentOperator(Module m, atom_t name, int kind,
				int *type, int *priority);
COMMON(int)		priorityOperator(Module m, atom_t atom);
COMMON(void)		initOperators(void);

/* pl-os.c */
COMMON(bool)		initOs(void);
COMMON(void)		cleanupOs(void);
COMMON(char *)		OsError(void);
COMMON(void)		setRandom(unsigned int *seed);
COMMON(uint64_t)	_PL_Random(void);
COMMON(char *)		canonicalisePath(char *path);
COMMON(char *)		OsPath(const char *plpath, char *ospath);
COMMON(char *)		PrologPath(const char *ospath, char *plpath, size_t len);
COMMON(void)		setOSPrologFlags(void);
COMMON(void)		RemoveTemporaryFiles(void);
COMMON(bool)		OpenStream(int fd);
COMMON(char *)		expandVars(const char *pattern, char *expanded, int len);
COMMON(char *)		getwd(char *buf);
COMMON(char *)		AbsoluteFile(const char *spec, char *path);
COMMON(int)		IsAbsolutePath(const char *spec);
COMMON(char *)		BaseName(const char *f);
COMMON(char *)		DirName(const char *f, char *buf);
COMMON(bool)		ChDir(const char *path);
COMMON(atom_t)		TemporaryFile(const char *id, int *fdp);
COMMON(int)		DeleteTemporaryFile(atom_t name);
COMMON(int)		hasConsole(void);
COMMON(struct tm *)	PL_localtime_r(const time_t *t, struct tm *r);
COMMON(char *)		PL_asctime_r(const struct tm *tm, char *buf);
COMMON(Char)		GetChar(void);
COMMON(size_t)		getenv3(const char *, char *buf, size_t buflen);
COMMON(char *)		Getenv(const char *, char *buf, size_t buflen);
COMMON(int)		Setenv(char *name, char *value);
COMMON(int)		Unsetenv(char *name);
COMMON(int)		System(char *cmd);
COMMON(char *)		findExecutable(const char *module, char *buf, size_t len);
COMMON(int)		Pause(double time);

/* pl-prims.c */
COMMON(int)		unify_ptrs(Word t1, Word t2, int flags ARG_LD);
COMMON(void)		unify_vp(Word vp, Word val ARG_LD);
COMMON(bool)		can_unify(Word t1, Word t2, term_t ex);
COMMON(int)		compareStandard(Word t1, Word t2, int eq ARG_LD);
COMMON(int)		compareAtoms(atom_t a1, atom_t a2);
COMMON(intptr_t)	skip_list(Word l, Word *tailp ARG_LD);
COMMON(intptr_t)	lengthList(term_t list, int errors);
COMMON(int)		is_acyclic(Word p ARG_LD);
COMMON(intptr_t)	numberVars(term_t t, nv_options *opts, intptr_t n ARG_LD);
COMMON(int)		duplicate_term(term_t in, term_t copy ARG_LD);
COMMON(word)		stringToList(char *s);
COMMON(foreign_t)	pl_sub_atom(term_t atom,
				    term_t before, term_t len, term_t after,
				    term_t sub, control_t h);
COMMON(word)		pl_sub_string(term_t str,
			      term_t offset, term_t length, term_t after,
			      term_t sub, control_t h);
COMMON(word)		pl_repeat(control_t h);
COMMON(word)		pl_fail(void);
COMMON(word)		pl_true(void);
COMMON(word)		pl_halt(term_t code);
COMMON(int)		pl_statistics_ld(term_t k, term_t value,
				 PL_local_data_t *ld ARG_LD);
COMMON(int)		set_pl_option(const char *name, const char *value);
COMMON(word)		pl_novice(term_t old, term_t new);
COMMON(Word)		ground__LD(Word p ARG_LD);
COMMON(int)		PL_factorize_term(term_t term,
					  term_t template, term_t factors);
COMMON(int)		PL_var_occurs_in(term_t var, term_t value);
COMMON(void)		raiseInferenceLimitException(void);

/* pl-prologflag.c */
COMMON(void)		setPrologFlag(const char *name, int flags, ...);
COMMON(int)		set_prolog_flag(term_t key, term_t value, int flags);
COMMON(word)		pl_prolog_flag(term_t key, term_t value, control_t h);
COMMON(word)		pl_prolog_flag5(term_t key, term_t value,
					term_t local, term_t access, term_t type,
					control_t h);
COMMON(int)		setDoubleQuotes(atom_t a, unsigned int *flagp);
COMMON(int)		setBackQuotes(atom_t a, unsigned int *flagp);
COMMON(void)		initPrologFlags(void);
COMMON(void)		cleanupPrologFlags(void);

/* pl-pro.c */
COMMON(word)		pl_break(void);
COMMON(int)		currentBreakLevel(void);
COMMON(int)		callProlog(Module module, term_t goal, int flags, term_t *ex);
COMMON(int)		abortProlog(void);
COMMON(bool)		prologToplevel(atom_t toplevel);
COMMON(int)		query_loop(atom_t goal, int loop);
COMMON(word)		pl_metacut(void);
COMMON(int)		trap_gdb(void);
COMMON(word)		checkData(Word p);
COMMON(word)		checkDataEx(Word p, int flags);
COMMON(int)		getAccessLevelMask(atom_t a, access_level_t *val);
COMMON(atom_t)		accessLevel(void);

/* pl-proc.c */
COMMON(Procedure)	lookupProcedure(functor_t f, Module m) WUNUSED;
COMMON(void)		unallocProcedure(Procedure proc);
COMMON(Procedure)	isCurrentProcedure__LD(functor_t f, Module m ARG_LD);
COMMON(int)		importDefinitionModule(Module m,
					       Definition def, int flags);
COMMON(Procedure)	lookupProcedureToDefine(functor_t def, Module m);
COMMON(ClauseRef)	hasClausesDefinition(Definition def);
COMMON(Procedure)	getDefinitionProc(Definition def);
COMMON(bool)		isDefinedProcedure(Procedure proc);
COMMON(void)		shareDefinition(Definition def);
COMMON(int)		unshareDefinition(Definition def);
COMMON(void)		lingerDefinition(Definition def);
COMMON(int)		get_head_functor(term_t head, functor_t *fdef,
				 int flags ARG_LD);
COMMON(int)		get_functor(term_t descr, functor_t *fdef,
				    Module *m, term_t h, int how);
COMMON(int)		get_procedure(term_t descr, Procedure *proc,
				      term_t he, int f);
COMMON(int)		checkModifySystemProc(functor_t f);
COMMON(int)		overruleImportedProcedure(Procedure proc, Module target);
COMMON(word)		pl_current_predicate(term_t name, term_t functor, control_t h);
COMMON(foreign_t)	pl_current_predicate1(term_t spec, control_t ctx);
COMMON(void)		clear_meta_declaration(Definition def);
COMMON(void)		setMetapredicateMask(Definition def, arg_info *args);
COMMON(int)		isTransparentMetamask(Definition def, arg_info *args);
COMMON(ClauseRef)	assertProcedure(Procedure proc, Clause clause,
					ClauseRef where ARG_LD);
COMMON(bool)		abolishProcedure(Procedure proc, Module module);
COMMON(bool)		retractClauseDefinition(Definition def, Clause clause);
COMMON(void)		unallocClause(Clause c);
COMMON(void)		freeClause(Clause c);
COMMON(void)		lingerClauseRef(ClauseRef c);
COMMON(ClauseRef)	newClauseRef(Clause cl, word key);
COMMON(void)		gcClauseRefs(void);
COMMON(size_t)		removeClausesPredicate(Definition def,
					       int sfindex, int fromfile);
COMMON(void)		reconsultFinalizePredicate(sf_reload *rl, Definition def,
						   p_reload *r ARG_LD);
COMMON(void)		destroyDefinition(Definition def);
COMMON(Procedure)	resolveProcedure__LD(functor_t f, Module module ARG_LD);
COMMON(Definition)	trapUndefined(Definition undef ARG_LD);
COMMON(word)		pl_retractall(term_t head);
COMMON(word)		pl_abolish(term_t atom, term_t arity);
COMMON(word)		pl_abolish1(term_t pred);
COMMON(word)		pl_get_predicate_attribute(term_t pred, term_t k, term_t v);
COMMON(word)		pl_set_predicate_attribute(term_t pred, term_t k, term_t v);
COMMON(int)		redefineProcedure(Procedure proc, SourceFile sf,
					  unsigned int suppress);
COMMON(word)		pl_index(term_t pred);
COMMON(word)		pl_default_predicate(term_t d1, term_t d2);
COMMON(Definition)	autoImport(functor_t f, Module m);
COMMON(word)		pl_require(term_t pred);
COMMON(word)		pl_check_definition(term_t spec);
COMMON(foreign_t)	pl_list_generations(term_t desc);
COMMON(foreign_t)	pl_check_procedure(term_t desc);
COMMON(void)		checkDefinition(Definition def);
COMMON(Procedure)	isStaticSystemProcedure(functor_t fd);
COMMON(foreign_t)	pl_garbage_collect_clauses(void);
COMMON(int)		setDynamicDefinition(Definition def, bool isdyn);
COMMON(int)		setThreadLocalDefinition(Definition def, bool isdyn);
COMMON(int)		setAttrDefinition(Definition def, unsigned attr, int val);
COMMON(int)		PL_meta_predicate(predicate_t def, const char*);

/* pl-srcfile.c */

COMMON(int)		startConsult(SourceFile f);
COMMON(int)		endConsult(SourceFile f);
COMMON(size_t)		highSourceFileIndex(void);
COMMON(SourceFile)	lookupSourceFile(atom_t name, int create);
COMMON(SourceFile)	indexToSourceFile(int index);
COMMON(void)		cleanupSourceFiles(void);
COMMON(void)		unlinkSourceFileModule(SourceFile sf, Module m);
COMMON(void)		addProcedureSourceFile(SourceFile sf, Procedure proc);
COMMON(int)		hasProcedureSourceFile(SourceFile sf, Procedure proc);
COMMON(int)		reloadHasClauses(SourceFile sf, Procedure proc ARG_LD);
COMMON(int)		isDefinedProcedureSource(Procedure proc);
COMMON(ClauseRef)	assertProcedureSource(SourceFile sf, Procedure proc,
					      Clause clause ARG_LD);
COMMON(int)		setAttrProcedureSource(SourceFile sf, Procedure proc,
					       unsigned attr, int val ARG_LD);
COMMON(int)		setMetapredicateSource(SourceFile sf, Procedure proc,
					       arg_info *args ARG_LD);
COMMON(int)		exportProcedureSource(SourceFile sf, Module module,
					      Procedure proc);
COMMON(void)		registerReloadModule(SourceFile sf, Module module);

/* pl-read.c */
COMMON(void)		resetRead(void);
COMMON(int)		f_is_prolog_var_start(wint_t c);
COMMON(int)		f_is_prolog_atom_start(wint_t c);
COMMON(int)		f_is_prolog_identifier_continue(wint_t c);
COMMON(int)		f_is_prolog_symbol(wint_t c);
COMMON(int)		unicode_separator(pl_wchar_t c);
COMMON(int)		atom_varnameW(const pl_wchar_t *s, size_t len);
COMMON(int)		atom_is_named_var(atom_t name);
COMMON(strnumstat)	str_number(const unsigned char *string,
				   unsigned char **end,
				   Number value, bool escape);
COMMON(const char *)	str_number_error(strnumstat rc);
COMMON(word)		pl_raw_read(term_t term);
COMMON(word)		pl_raw_read2(term_t stream, term_t term);
COMMON(word)		pl_read(term_t term);
COMMON(word)		pl_read2(term_t stream, term_t term);
COMMON(void)		initCharConversion(void);
COMMON(foreign_t)	pl_char_conversion(term_t in, term_t out);
COMMON(foreign_t)	pl_current_char_conversion(term_t in, term_t out, control_t h);
COMMON(int)		read_clause(IOSTREAM *s, term_t term, term_t options ARG_LD);

/* pl-rec.c */
COMMON(void)		initRecords(void);
COMMON(void)		cleanupRecords(void);
COMMON(Record)		compileTermToHeap__LD(term_t term,
					      void* (*allocate)(void *ctx, size_t size),
					      void* ctx,
					      int flags ARG_LD);
COMMON(int)		copyRecordToGlobal(term_t copy, Record term,
					   int flags ARG_LD);
COMMON(int)		variantRecords(const Record r1, const Record r2);
COMMON(bool)		freeRecord(Record record);
COMMON(void)		unallocRecordRef(RecordRef r);
COMMON(bool)		unifyKey(term_t key, word val);
COMMON(int)		getKeyEx(term_t key, word *k ARG_LD);
COMMON(word)		pl_term_complexity(term_t t, term_t mx, term_t count);
COMMON(void)		markAtomsRecord(Record record);

/* pl-rl.c */
COMMON(void)		install_rl(void);

/* pl-setup.c */
COMMON(int)		setupProlog(void);
COMMON(int)		endCritical__LD(ARG1_LD);
COMMON(void)		dispatch_signal(int sig, int sync);
COMMON(handler_t)	set_sighandler(int sig, handler_t func);
COMMON(void)		blockSignals(sigset_t *mask);
COMMON(void)		allSignalMask(sigset_t *set);
COMMON(void)		unblockSignals(sigset_t *mask);
COMMON(void)		unblockSignal(int sig);
COMMON(void)		blockSignal(int sig);
COMMON(void)		resetSignals(void);
COMMON(void)		cleanupSignals(void);
COMMON(int)		handleSignals(ARG1_LD);

COMMON(int)		initPrologStacks(size_t local,
					 size_t global,
					 size_t trail);
COMMON(void)		initPrologLocalData(ARG1_LD);
COMMON(void)		deallocateStacks(void);
COMMON(bool)		restoreStack(Stack s);
COMMON(void)		trimStacks(int resize ARG_LD);
COMMON(void)		emptyStacks(void);
COMMON(void)		freeStacks(ARG1_LD);
COMMON(void)		freePrologLocalData(PL_local_data_t *ld);
COMMON(int)		ensure_room_stack(Stack s, size_t n, int ex);
COMMON(int)		trim_stack(Stack s);
COMMON(void *)		stack_malloc(size_t size);
COMMON(void *)		stack_realloc(void *old, size_t size);
COMMON(void)		stack_free(void *mem);
COMMON(const char *)	signal_name(int sig);

/* pl-sys.c */
COMMON(word)		pl_shell(term_t command, term_t status);
COMMON(word)		pl_getenv(term_t var, term_t value);
COMMON(word)		pl_setenv(term_t var, term_t value);
COMMON(word)		pl_unsetenv(term_t var);
COMMON(word)		pl_get_time(term_t t);
COMMON(word)		pl_sleep(term_t time);
COMMON(word)		pl_get_pid(term_t pid);

/* pl-trace.c */
COMMON(int)		isDebugFrame(LocalFrame FR);
COMMON(int)		tracePort(LocalFrame frame, Choice bfr,
				  int port, Code PC ARG_LD);
COMMON(void)		initTracer(void);
COMMON(void)		resetTracer(void);
COMMON(int)		tracemode(int new, int *old);
COMMON(int)		debugmode(debug_type new, debug_type *old);
COMMON(int)		trace_if_space(void);
COMMON(word)		pl_trace(void);
COMMON(word)		pl_notrace(void);
COMMON(word)		pl_tracing(void);
COMMON(word)		pl_spy(term_t p);
COMMON(word)		pl_nospy(term_t p);
COMMON(word)		pl_leash(term_t old, term_t new);
COMMON(word)		pl_visible(term_t old, term_t new);
COMMON(word)		pl_debuglevel(term_t old, term_t new);
COMMON(word)		pl_prolog_current_frame(term_t fr);
COMMON(int)		PL_call_event_hook(pl_event_type ev, ...) WUNUSED;
COMMON(int)		PL_call_event_hook_va(pl_event_type ev, va_list args);
COMMON(int)		delayEvents(void);
COMMON(int)		sendDelayedEvents(int noerror) WUNUSED;
COMMON(void)		PL_put_frame(term_t t, LocalFrame fr);
COMMON(void)		PL_put_choice(term_t t, Choice ch);

/* pl-util.c */
COMMON(char *)		procedureName(Procedure proc);
COMMON(char *)		predicateName(Definition def);
COMMON(char *)		functorName(functor_t f);
COMMON(char *)		keyName(word key);
COMMON(char *)		sourceFileName(SourceFile sf);
COMMON(char *)		generationName(gen_t gen);
COMMON(int)		clauseNo(Definition def, Clause clause, gen_t gen);
COMMON(int)		notImplemented(char *name, int arity);
COMMON(word)		setBoolean(int *flag, term_t o, term_t n);
COMMON(word)		setInteger(int *val, term_t old, term_t new);
COMMON(const char *)	atom_summary(atom_t name, unsigned int maxlen);

/* pl-wic.c */
COMMON(bool)		loadWicFromStream(IOSTREAM *fd);
COMMON(bool)		compileFileList(IOSTREAM *out, int argc, char **argv);
COMMON(void)		qlfCleanup(void);

COMMON(void)		wicPutNum(int64_t n, IOSTREAM *fd);
COMMON(int64_t)		wicGetNum(IOSTREAM *fd);
COMMON(void)		wicPutStringW(const pl_wchar_t *w, size_t len,
				      IOSTREAM *fd);
COMMON(pl_wchar_t*)	wicGetStringUTF8(IOSTREAM *fd, size_t *length,
					 pl_wchar_t *buf, size_t bufsize);

/* pl-write.c */
COMMON(char *)		var_name_ptr__LD(Word p, char *name ARG_LD);
COMMON(char *)		varName(term_t var, char *buf);
COMMON(word)		pl_write_canonical(term_t term);
COMMON(word)		pl_write_canonical2(term_t stream, term_t term);
COMMON(word)		pl_write_term(term_t term, term_t options);
COMMON(word)		pl_write_term3(term_t stream,
			       term_t term, term_t options);
COMMON(word)		pl_write(term_t term);
COMMON(word)		pl_writeln(term_t term);
COMMON(word)		pl_writeq(term_t term);
COMMON(word)		pl_print(term_t term);
COMMON(word)		pl_write2(term_t stream, term_t term);
COMMON(word)		pl_writeln2(term_t stream, term_t term);
COMMON(word)		pl_writeq2(term_t stream, term_t term);
COMMON(word)		pl_print2(term_t stream, term_t term);
COMMON(int)		writeAttributeMask(atom_t name);
COMMON(int)		writeUCSAtom(IOSTREAM *fd, atom_t atom, int flags);
COMMON(int)		writeReservedSymbol(IOSTREAM *fd, atom_t atom, int flags);
COMMON(int)		writeAtomToStream(IOSTREAM *s, atom_t atom);
COMMON(char *)		format_float(double f, char *buf);
COMMON(int)		unquoted_atom(atom_t a);
COMMON(strnumstat)	make_nan(double *f);
COMMON(double)		NaN_value(double f);

/* pl-term.c */
COMMON(void)		cleanupTerm(void);

/* pl-init.c */
COMMON(int)		startProlog(int argc, char **argv);
COMMON(bool)		sysError(const char *fm, ...);
COMMON(void)		fatalError(const char *fm, ...) NORETURN;
COMMON(bool)		warning(const char *fm, ...);
COMMON(void)		vfatalError(const char *fm, va_list args) NORETURN;
COMMON(bool)		vwarning(const char *fm, va_list args);
COMMON(int)		cleanupProlog(int status, int reclaim);
COMMON(int)		run_on_halt(OnHalt *handlers, int rval);
COMMON(int)		setTraditional(void);

/* pl-dll.c */
COMMON(word)		pl_open_dll(term_t name, term_t handle);
COMMON(word)		pl_close_dll(term_t handle);
COMMON(word)		pl_call_dll_function(term_t handle, term_t funcname);

/* pl-nt.c */

COMMON(void)		PlMessage(const char *buf, ...);
COMMON(word)		pl_window_title(term_t old, term_t new);
COMMON(word)		pl_win_exec(term_t command, term_t show);
COMMON(foreign_t)	pl_win_module_file(term_t module, term_t file);
#ifdef EMULATE_DLOPEN
COMMON(void *)		dlopen(const char *file, int flags);
COMMON(const)  char *	dlerror(void);
COMMON(void *)		dlsym(void *handle, char *symbol);
COMMON(int)		dlclose(void *handle);
#endif /*EMULATE_DLOPEN*/
COMMON(int)		ms_snprintf(char *buffer, size_t count,
				    const char *fmt, ...);
COMMON(void)		getDefaultsFromRegistry(void);
COMMON(const char*)	WinError(void);

/* pl-rc.c */
COMMON(IOSTREAM *)       SopenRC(void *rca,
			const char *name, const char *rcclass, int flags);
COMMON(foreign_t)	pl_rc_handle(term_t h);
COMMON(foreign_t)        pl_rc_open(term_t rc_h,
			   term_t name, term_t class,
			   term_t rw, term_t handle);
COMMON(foreign_t)        pl_rc_open_archive(term_t file, term_t handle);
COMMON(foreign_t)        pl_rc_close_archive(term_t rc_h);
COMMON(foreign_t)        pl_rc_save_archive(term_t rc_h, term_t to);
COMMON(foreign_t)        pl_rc_append_file(term_t rc_h,
				  term_t name, term_t class, term_t encoding,
				  term_t file);
COMMON(foreign_t)	pl_rc_members(term_t rc_h, term_t members);

/* pl-xterm.c */

COMMON(foreign_t)	pl_open_xterm(term_t title,
				      term_t in, term_t out, term_t err,
				      term_t argv);

/* pl-ctype.c */

COMMON(IOENC)		initEncoding(void);
COMMON(void)		initCharTypes(void);
COMMON(access_level_t)	setAccessLevel(access_level_t new_level);


/* pl-thread.c */
COMMON(foreign_t)	pl_with_mutex(term_t mutex, term_t goal);
COMMON(foreign_t)	pl_thread_self(term_t self);
#ifdef O_PLMT
COMMON(int)		unify_thread_id(term_t id, PL_thread_info_t *info);
#endif
COMMON(int)		enableThreads(int enable);


/* pl-mutex.c */
COMMON(void)		initMutexes(void);


/* pl-gmp.c */
COMMON(int)	PL_unify_number__LD(term_t t, Number n ARG_LD);
COMMON(int)	PL_put_number__LD(term_t t, Number n ARG_LD);
COMMON(void)	get_number(word w, Number n  ARG_LD);
COMMON(int)	PL_get_number(term_t t, Number n);
COMMON(int)	put_uint64(Word at, uint64_t l, int flags ARG_LD);
COMMON(int)	put_number(Word at, Number n, int flags ARG_LD);
COMMON(int)	promoteToFloatNumber(Number n);
COMMON(int)	make_same_type_numbers(Number n1, Number n2) WUNUSED;
COMMON(int)     promoteNumber(Number n1, numtype type) WUNUSED;
COMMON(int)	cmpNumbers(Number n1, Number n2);
COMMON(void)	cpNumber(Number to, Number from);

/* pl-cont.c */
COMMON(Code)	push_continuation(term_t cont, LocalFrame pfr, Code pcret
				  ARG_LD);
COMMON(Code)	shift(term_t ball ARG_LD);

/* pl-version.h */
COMMON(void)	setGITVersion(void);
