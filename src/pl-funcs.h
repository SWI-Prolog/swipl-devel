/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam,
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
COMMON(int)		foreignWakeup(term_t *ex ARG_LD);
COMMON(void)		updateAlerted(PL_local_data_t *ld);
COMMON(int)		raiseSignal(PL_local_data_t *ld, int sig);
COMMON(Module)		contextModule(LocalFrame fr);
COMMON(void)		setContextModule(LocalFrame fr, Module context);

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
COMMON(word)		lookupAtom(const char *s, size_t len);
COMMON(word)		lookupBlob(const char *s, size_t len,
				   PL_blob_t *type, int *new);
COMMON(word)		pl_atom_hashstat(term_t i, term_t n);
COMMON(void)		initAtoms(void);
COMMON(void)		cleanupAtoms(void);
COMMON(void)		markAtom(atom_t a);
COMMON(foreign_t)	pl_garbage_collect_atoms(void);
COMMON(void)		resetAtoms(void);
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
COMMON(Number)		allocArithStack(ARG1_LD);
COMMON(void)		pushArithStack(Number n ARG_LD);
COMMON(void)		resetArithStack(ARG1_LD);
COMMON(Number)		argvArithStack(int n ARG_LD);
COMMON(void)		popArgvArithStack(int n ARG_LD);
COMMON(void)		freeArithLocalData(PL_local_data_t *ld);
COMMON(int)		ar_sign_i(Number n1);
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
COMMON(int)		compileClause(Clause *cp, Word head, Word body,
				      Procedure proc, Module module,
				      term_t warnings ARG_LD);
COMMON(Clause)		assert_term(term_t term, int where, atom_t owner,
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
COMMON(void)		clearBreakPointsClause(Clause clause);
COMMON(int)		unify_functor(term_t t, functor_t fd, int how);

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
COMMON(ClauseRef)	nextClause(ClauseChoice chp, Word argv, LocalFrame fr,
				   Definition def);
COMMON(void)		addClauseToIndexes(Definition def, Clause cl, int where);
COMMON(void)		delClauseFromIndex(Definition def, Clause cl);
COMMON(void)		cleanClauseIndexes(Definition def);
COMMON(void)		clearTriedIndexes(Definition def);
COMMON(void)		unallocClauseIndexes(Definition def);
COMMON(void)		unallocClauseIndexTable(ClauseIndex ci);
COMMON(void)		deleteActiveClauseFromIndexes(Definition def, Clause cl);
COMMON(bool)		unify_index_pattern(Procedure proc, term_t value);

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
COMMON(int)		_PL_put_number__LD(term_t t, Number n ARG_LD);
COMMON(predicate_t)	_PL_predicate(const char *name, int arity,
				      const char *module, predicate_t *bin);
COMMON(void)		initialiseForeign(int argc, char **argv);
COMMON(void)		cleanupInitialiseHooks(void);
COMMON(atom_t)		codeToAtom(int code);
COMMON(extern)		record_t PL_duplicate_record(record_t r);
COMMON(int)		PL_unify_termv(term_t t, va_list args);
COMMON(term_t)		pushWordAsTermRef__LD(Word p ARG_LD);
COMMON(void)		popTermRef__LD(ARG1_LD);
COMMON(void)		_PL_get_arg__LD(int index, term_t t, term_t a ARG_LD);
COMMON(term_t)		PL_new_term_ref__LD(ARG1_LD);
COMMON(term_t)		PL_new_term_ref_noshift__LD(ARG1_LD);
COMMON(term_t)		PL_new_term_refs__LD(int n ARG_LD);
COMMON(int)		PL_unify__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int)		PL_unify_integer__LD(term_t t1, intptr_t i ARG_LD);
COMMON(int)		PL_unify_int64__LD(term_t t1, int64_t ARG_LD);
COMMON(int)		PL_unify_int64_ex__LD(term_t t1, int64_t ARG_LD);
COMMON(int)		PL_get_atom__LD(term_t t1, atom_t *a ARG_LD);
COMMON(int)		PL_put_variable__LD(term_t t1 ARG_LD);
COMMON(int)		PL_put_atom__LD(term_t t1, atom_t a ARG_LD);
COMMON(int)		PL_put_integer__LD(term_t t1, long i ARG_LD);
COMMON(int)		PL_put_intptr__LD(term_t t1, intptr_t i ARG_LD);
COMMON(int)		PL_is_atomic__LD(term_t t ARG_LD);
COMMON(int)		PL_is_functor__LD(term_t t, functor_t f ARG_LD);
COMMON(int)		PL_is_variable__LD(term_t t ARG_LD);
COMMON(int)		PL_strip_module__LD(term_t q, module_t *m,
					    term_t t ARG_LD);
COMMON(int)		PL_strip_module_ex__LD(term_t raw, module_t *m,
					       term_t plain ARG_LD);
COMMON(int)		PL_qualify(term_t raw, term_t qualified);
COMMON(int)		PL_get_integer__LD(term_t t, int *i ARG_LD);
COMMON(int)		PL_get_long__LD(term_t t, long *i ARG_LD);
COMMON(int)		PL_get_int64__LD(term_t t, int64_t *i ARG_LD);
COMMON(int)		PL_get_pointer__LD(term_t t, void **ptr ARG_LD);
COMMON(int)		PL_put_term__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int)		PL_get_functor__LD(term_t t, functor_t *f ARG_LD);
COMMON(int)		PL_get_uintptr(term_t t, size_t *i);
COMMON(int)		PL_unify_atom__LD(term_t t, atom_t a ARG_LD);
COMMON(int)		PL_unify_pointer__LD(term_t t, void *ptr ARG_LD);
COMMON(int)		PL_get_list__LD(term_t l, term_t h, term_t t ARG_LD);
COMMON(int)		PL_is_atom__LD(term_t t ARG_LD);
COMMON(int)		PL_unify_list__LD(term_t l, term_t h, term_t t ARG_LD);
COMMON(int)		PL_cons_list__LD(term_t l, term_t head, term_t tail
					 ARG_LD);
COMMON(int)		PL_is_inf(term_t t);
COMMON(int)		PL_same_term__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int)		isUCSAtom(Atom a);
COMMON(atom_t)		lookupUCSAtom(const pl_wchar_t *s, size_t len);
COMMON(int)		charCode(word w);

COMMON(void)		registerForeignLicenses(void);
COMMON(void)            bindExtensions(const char *module,
				       const PL_extension *ext);
COMMON(void)		initForeign(void);
COMMON(int)		PL_rethrow(void);
COMMON(int)		PL_pending__LD(int sig ARG_LD);
COMMON(int)		PL_clearsig__LD(int sig ARG_LD);
COMMON(void)		cleanupCodeToAtom(void);
COMMON(void)		PL_clear_foreign_exception(LocalFrame fr);

/* pl-fmt.c */
COMMON(word)		pl_format_predicate(term_t chr, term_t descr);
COMMON(word)		pl_current_format_predicate(term_t chr, term_t descr,
					    control_t h);
COMMON(word)		pl_format(term_t fmt, term_t args);
COMMON(word)		pl_format3(term_t s, term_t fmt, term_t args);

/* pl-funct.c */
COMMON(functor_t)	lookupFunctorDef(atom_t atom, unsigned int arity);
COMMON(functor_t)	isCurrentFunctor(atom_t atom, unsigned int arity);
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
COMMON(int)		ensureLocalSpace(size_t bytes, int flags);
COMMON(void)		clearUninitialisedVarsFrame(LocalFrame, Code);
COMMON(void)		setLTopInBody(void);
COMMON(word)		check_foreign(void);	/* DEBUG(CHK_SECURE...) stuff */
COMMON(void)		markAtomsOnStacks(PL_local_data_t *ld);
COMMON(void)		markPredicatesInEnvironments(PL_local_data_t *ld);
COMMON(QueryFrame)	queryOfFrame(LocalFrame fr);
#if defined(O_DEBUG) || defined(SECURE_GC) || defined(O_MAINTENANCE)
word			checkStacks(void *vm_state);
COMMON(bool)		scan_global(int marked);
#endif

/* pl-itf.c */
COMMON(void)		resetForeign(void);

/* pl-load.c */
COMMON(word)		pl_load_shared_object(term_t file, term_t entry);
COMMON(void)		cleanupForeign(void);

/* pl-modul.c */
COMMON(Module)		lookupModule(atom_t name);
COMMON(Module)		isCurrentModule(atom_t name);
COMMON(void)		initModules(void);
COMMON(void)		cleanupModules(void);
COMMON(int)		addModuleSourceFile(SourceFile sf, Module m);
COMMON(int)		setSuperModule(Module m, Module s);
COMMON(int)		isSuperModule(Module s, Module m);
COMMON(void)		clearSupersModule(Module m);
COMMON(int)		addSuperModule(Module m, Module s, int where);
COMMON(int)		getUnknownModule(Module m);
COMMON(Word)		stripModule(Word term, Module *module ARG_LD);
COMMON(bool)		isPublicModule(Module module, Procedure proc);
COMMON(int)		declareModule(atom_t name, atom_t class, atom_t super,
				      SourceFile sf, int line,
				      int rdef);
COMMON(word)		pl_module(term_t old, term_t new);
COMMON(word)		pl_set_source_module(term_t old, term_t new);
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
COMMON(char *)		findExecutable(const char *module, char *buf);
COMMON(int)		Pause(double time);

/* pl-prims.c */
COMMON(bool)		unify_ptrs(Word t1, Word t2, int flags ARG_LD);
COMMON(bool)		can_unify(Word t1, Word t2, term_t *ex);
COMMON(int)		compareStandard(Word t1, Word t2, int eq ARG_LD);
COMMON(intptr_t)	skip_list(Word l, Word *tailp ARG_LD);
COMMON(intptr_t)	lengthList(term_t list, int errors);
COMMON(int)		is_acyclic(Word p ARG_LD);
COMMON(int)		numberVars(term_t t, nv_options *opts, int n ARG_LD);
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
COMMON(int)		ground__LD(Word p ARG_LD);
COMMON(int)		PL_factorize_term(term_t term,
					  term_t template, term_t factors);
COMMON(int)		PL_var_occurs_in(term_t var, term_t value);

/* pl-prologflag.c */
COMMON(void)		setPrologFlag(const char *name, int flags, ...);
COMMON(word)		pl_prolog_flag(term_t key, term_t value, control_t h);
COMMON(word)		pl_prolog_flag5(term_t key, term_t value,
					term_t local, term_t access, term_t type,
					control_t h);
COMMON(int)		setDoubleQuotes(atom_t a, unsigned int *flagp);
COMMON(void)		initPrologFlags(void);
COMMON(void)		cleanupPrologFlags(void);

/* pl-pro.c */
COMMON(word)		pl_break(void);
COMMON(word)		pl_notrace1(term_t goal);
COMMON(int)		currentBreakLevel(void);
COMMON(int)		callProlog(Module module, term_t goal, int flags, term_t *ex);
COMMON(int)		abortProlog(void);
COMMON(bool)		prologToplevel(atom_t toplevel);
COMMON(int)		query_loop(atom_t goal, int loop);
COMMON(word)		pl_metacut(void);
COMMON(int)		trap_gdb(void);
COMMON(word)		checkData(Word p);
COMMON(int)		getAccessLevelMask(atom_t a, access_level_t *val);
COMMON(atom_t)		accessLevel(void);

/* pl-proc.c */
COMMON(Procedure)	lookupProcedure(functor_t f, Module m);
COMMON(void)		unallocProcedure(Procedure proc);
COMMON(Procedure)	isCurrentProcedure(functor_t f, Module m);
COMMON(int)		importDefinitionModule(Module m,
					       Definition def, int flags);
COMMON(Procedure)	lookupProcedureToDefine(functor_t def, Module m);
COMMON(ClauseRef)	hasClausesDefinition(Definition def);
COMMON(bool)		isDefinedProcedure(Procedure proc);
COMMON(void)		shareDefinition(Definition def);
COMMON(int)		unshareDefinition(Definition def);
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
COMMON(ClauseRef)	assertProcedure(Procedure proc, Clause clause,
					int where ARG_LD);
COMMON(bool)		abolishProcedure(Procedure proc, Module module);
COMMON(bool)		retractClauseDefinition(Definition def, Clause clause);
COMMON(void)		freeClause(Clause c);
COMMON(void)		unallocClause(Clause c);
COMMON(void)		freeClauseRef(ClauseRef c);
COMMON(void)		freeClauseList(ClauseRef cref);
COMMON(ClauseRef)	newClauseRef(Clause cl, word key);
COMMON(void)		gcClausesDefinition(Definition def);
COMMON(void)		gcClausesDefinitionAndUnlock(Definition def);
COMMON(void)		destroyDefinition(Definition def);
COMMON(void)		resetReferences(void);
COMMON(Procedure)	resolveProcedure(functor_t f, Module module);
COMMON(Definition)	trapUndefined(Definition undef ARG_LD);
COMMON(word)		pl_retractall(term_t head);
COMMON(word)		pl_abolish(term_t atom, term_t arity);
COMMON(word)		pl_abolish1(term_t pred);
COMMON(word)		pl_get_predicate_attribute(term_t pred, term_t k, term_t v);
COMMON(word)		pl_set_predicate_attribute(term_t pred, term_t k, term_t v);
COMMON(int)		redefineProcedure(Procedure proc, SourceFile sf,
					  unsigned int suppress);
COMMON(void)		startConsult(SourceFile f);
COMMON(word)		pl_index(term_t pred);
COMMON(SourceFile)	lookupSourceFile(atom_t name, int create);
COMMON(SourceFile)	indexToSourceFile(int index);
COMMON(void)		cleanupSourceFiles(void);
COMMON(void)		addProcedureSourceFile(SourceFile sf, Procedure proc);
COMMON(word)		pl_make_system_source_files(void);
COMMON(word)		pl_source_file(term_t descr, term_t file, control_t h);
COMMON(word)		pl_default_predicate(term_t d1, term_t d2);
COMMON(Definition)	autoImport(functor_t f, Module m);
COMMON(word)		pl_require(term_t pred);
COMMON(word)		pl_check_definition(term_t spec);
COMMON(foreign_t)	pl_list_generations(term_t desc);
COMMON(foreign_t)	pl_check_procedure(term_t desc);
COMMON(void)		checkDefinition(Definition def);
COMMON(Procedure)	isStaticSystemProcedure(functor_t fd);
COMMON(foreign_t)	pl_garbage_collect_clauses(void);
COMMON(int)		setDynamicProcedure(Procedure proc, bool isdyn);
COMMON(int)		PL_meta_predicate(predicate_t def, const char*);


/* pl-read.c */
COMMON(void)		resetRead(void);
COMMON(int)		f_is_prolog_var_start(wint_t c);
COMMON(int)		f_is_prolog_atom_start(wint_t c);
COMMON(int)		f_is_prolog_identifier_continue(wint_t c);
COMMON(int)		f_is_prolog_symbol(wint_t c);
COMMON(int)		unicode_separator(pl_wchar_t c);
COMMON(int)		unquoted_atomW(const pl_wchar_t *s, size_t len,
				       IOSTREAM *fd);
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
COMMON(void)		resumeAfterException(void);
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
COMMON(int)		callEventHook(int ev, ...);
COMMON(void)		PL_put_frame(term_t t, LocalFrame fr);
COMMON(void)		PL_put_choice(term_t t, Choice ch);

/* pl-util.c */
COMMON(char *)		procedureName(Procedure proc);
COMMON(char *)		predicateName(Definition def);
COMMON(char *)		functorName(functor_t f);
COMMON(char *)		keyName(word key);
COMMON(int)		clauseNo(Definition def, Clause clause);
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
COMMON(word)		pl_writeq2(term_t stream, term_t term);
COMMON(word)		pl_print2(term_t stream, term_t term);
COMMON(int)		writeAttributeMask(atom_t name);
COMMON(int)		writeUCSAtom(IOSTREAM *fd, atom_t atom, int flags);
COMMON(int)		writeAtomToStream(IOSTREAM *s, atom_t atom);
COMMON(char *)		format_float(double f, char *buf);

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

COMMON(foreign_t)	pl_open_xterm(term_t title, term_t in, term_t out, term_t err);

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


/* pl-gmp.c */
COMMON(int)	PL_unify_number__LD(term_t t, Number n ARG_LD);
COMMON(int)	PL_put_number__LD(term_t t, Number n ARG_LD);
COMMON(void)	get_number(word w, Number n  ARG_LD);
COMMON(int)	PL_get_number(term_t t, Number n);
COMMON(int)	put_number(Word at, Number n, int flags ARG_LD);
COMMON(int)	promoteToFloatNumber(Number n);
COMMON(void)	make_same_type_numbers(Number n1, Number n2);
COMMON(void)    promoteNumber(Number n1, numtype type);
COMMON(int)	cmpNumbers(Number n1, Number n2);
COMMON(void)	cpNumber(Number to, Number from);

/* pl-version.h */
COMMON(void)	setGITVersion(void);
