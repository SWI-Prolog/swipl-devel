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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
COMMON(type) defines the symbol to be global  with regard to Prolog, but
not exported from the shared object.  On   GCC  based  platforms this is
achieved using the visibility attribute. Making  symbols local to Prolog
avoid the ELF  dynamic  linker  picking   the  wrong  symbol  for  other
libraries and avoids Prolog picking wrong   symbols. It also reduces ELF
symbol lookup and relocations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define COMMON(type) type SO_LOCAL


/* pl-alloc.c */
COMMON(void) 		freeHeap__LD(void *mem, size_t n ARG_LD);
COMMON(word) 		outOfStack(Stack s, stack_overflow_action how);
COMMON(volatile void)	outOfCore(void);
COMMON(Word) 		allocGlobal__LD(int words ARG_LD);
COMMON(Word) 		allocGlobalNoShift__LD(int words ARG_LD);
COMMON(void *) 		allocHeap__LD(size_t n ARG_LD);
COMMON(void) 		initMemAlloc(void);
COMMON(void) 		cleanupMemAlloc(void);
COMMON(void) 		mergeAllocPool(AllocPool to, AllocPool from);
COMMON(word) 		globalFunctor(functor_t def);
COMMON(int) 		sizeString(word w);
COMMON(word) 		globalString(const char *s);
COMMON(word) 		globalNString(long len, const char *s);
COMMON(Word) 		newTerm(void);
COMMON(void) 		doublecpy(void *to, void *from);
COMMON(word) 		globalReal(real f);
COMMON(word) 		globalLong(long i ARG_LD);
COMMON(double) 		valReal__LD(word w ARG_LD);
COMMON(word) 		globalIndirect(word in);
COMMON(int) 		equalIndirect(word r1, word r2);
COMMON(word) 		globalIndirectFromCode(Code *PC);
COMMON(word) 		makeRef(Word p);
#ifndef consPtr
COMMON(word) 		consPtr(void *p, int ts);
#endif
COMMON(char *) 		store_string(const char *s);
COMMON(void) 		remove_string(char *s);
COMMON(int) 		unboundStringHashValue(const char *t, unsigned int l);
#ifndef xmalloc
COMMON(void *) 		xmalloc(size_t size);
COMMON(void *) 		xrealloc(void *mem, size_t size);
#endif

/* pl-attvar.c */
COMMON(int) 		assignAttVar(Word av, Word value ARG_LD);

/* pl-gvar.c */

COMMON(void) 		destroyGlobalVars();
COMMON(void)		freezeGlobal(ARG1_LD);


/* pl-wam.c */
COMMON(word) 		pl_count(void);
COMMON(bool) 		unify_ptrs(Word t1, Word t2 ARG_LD);
COMMON(bool) 		can_unify(Word t1, Word t2);
COMMON(void) 		TrailAssignment(Word p);
COMMON(void) 		do_undo(mark *m);
COMMON(Definition) 	getProcDefinition(Procedure proc);
COMMON(void) 		fix_term_ref_count(void);
COMMON(word) 		pl_unify_with_occurs_check(term_t t1, term_t t2);
COMMON(fid_t) 		PL_open_signal_foreign_frame(void);

/* pl-atom.c */
COMMON(word) 		lookupAtom(const char *s, unsigned int len);
COMMON(word) 		lookupBlob(const char *s, unsigned int len,
				   PL_blob_t *type, int *new);
COMMON(word) 		pl_atom_hashstat(term_t i, term_t n);
COMMON(void) 		initAtoms(void);
COMMON(void) 		cleanupAtoms(void);
COMMON(word) 		pl_complete_atom(term_t prefix, term_t common,
					 term_t unique);
COMMON(word) 		pl_atom_completions(term_t prefix, term_t alts);
COMMON(void) 		markAtom(atom_t a);
COMMON(foreign_t) 	pl_garbage_collect_atoms(void);
COMMON(void) 		resetAtoms(void);
#ifdef O_DEBUG_ATOMGC
COMMON(word) 		pl_track_atom(term_t which, term_t stream);
COMMON(void) 		_PL_debug_register_atom(atom_t a,
					const char *file, int line,
					const char *func);
COMMON(void) 		_PL_debug_unregister_atom(atom_t a,
					  const char *file, int line,
					  const char *func);
#endif


/* pl-arith.c */

COMMON(word) 		pl_between(term_t l, term_t h, term_t n,
				   control_t ctx);
COMMON(word) 		pl_succ(term_t n1, term_t n2);
COMMON(word) 		pl_plus(term_t a, term_t b, term_t c);
COMMON(int) 		ar_compare(Number n1, Number n2, int what);
COMMON(word) 		pl_current_arithmetic_function(term_t f, control_t h);
COMMON(void) 		initArith(void);
COMMON(void) 		cleanupArith(void);
COMMON(int) 		indexArithFunction(functor_t fdef, Module m);
COMMON(functor_t) 	functorArithFunction(int n);
COMMON(bool) 		ar_func_n(code n, int argc, Number *stack);
COMMON(int) 		valueExpression(term_t p, Number n ARG_LD);
COMMON(int) 		toIntegerNumber(Number n);
COMMON(void) 		canoniseNumber(Number n);
COMMON(int) 		arithChar(Word p ARG_LD);

/* pl-bag.c */
COMMON(void) 		resetBags(void);
COMMON(int)  		checkBags(void);

/* pl-comp.c */
COMMON(void) 		initWamTable(void);
COMMON(void) 		get_head_and_body_clause(term_t clause,
					 term_t head, term_t body,
					 Module *m ARG_LD);
COMMON(Clause) 		compileClause(Word head, Word body,
			      Procedure proc, Module module ARG_LD);
COMMON(Clause) 		assert_term(term_t term, int where,
				    SourceLoc loc ARG_LD);
COMMON(void) 		unregisterAtomsClause(Clause clause);
COMMON(word) 		pl_redefine_system_predicate(term_t term);
COMMON(bool) 		decompileHead(Clause clause, term_t head);
COMMON(int) 		arg1Key(Clause clause, word *key);
COMMON(bool) 		decompile(Clause clause, term_t term, term_t bindings);
COMMON(word) 		pl_clause4(term_t p, term_t t, term_t ref, term_t b,
				   control_t h);
COMMON(word) 		pl_clause3(term_t p, term_t term, term_t ref,
				   control_t h);
COMMON(word) 		pl_clause2(term_t p, term_t term, control_t h);
COMMON(word) 		pl_nth_clause(term_t p, term_t n, term_t ref,
				      control_t h);
COMMON(word) 		pl_xr_member(term_t ref, term_t term, control_t h);
COMMON(void) 		wamListClause(Clause clause);
COMMON(Code) 		wamListInstruction(IOSTREAM *out, Clause clause,
					   Code bp);
COMMON(word) 		pl_wam_list(term_t ref);
COMMON(word) 		pl_fetch_vm(term_t ref, term_t offset, term_t noffset,
				    term_t instruction);
COMMON(int) 		unify_definition(term_t head, Definition def,
				 term_t thehead, int flags);
COMMON(word) 		pl_clause_term_position(term_t ref, term_t pc,
						term_t locterm);
COMMON(word) 		pl_break_pc(term_t ref, term_t pc, term_t nextpc,
				    control_t h);
COMMON(word) 		pl_break_at(term_t ref, term_t pc, term_t set);
COMMON(code) 		replacedBreak(Code PC);
COMMON(void) 		clearBreakPointsClause(Clause clause);
COMMON(word) 		pl_current_break(term_t ref, term_t pc, control_t h);
COMMON(int) 		get_clause_ptr_ex(term_t ref, Clause *cl);

/* pl-dump.c */
COMMON(word) 		saveProgram(term_t new);
COMMON(word) 		pl_save_program(term_t new, term_t args);
COMMON(word) 		pl_save(term_t file, term_t restore);
COMMON(word) 		pl_restore(term_t file);
COMMON(word) 		parseSaveProgramOptions(term_t args,
			int *local, int *global, int *trail, int *argument,
			char **goal, char **toplevel, char **init_file,
			bool *tty, bool *standalone);

/* pl-index.c */
COMMON(int) 		cardinalityPattern(unsigned long pattern);
COMMON(void) 		getIndex(Word argv, unsigned long pattern, int card,
			 struct index *
			 ARG_LD);
COMMON(word) 		getIndexOfTerm(term_t t);
COMMON(ClauseRef) 	firstClause(Word argv, LocalFrame fr, Definition def,
			    ClauseRef *next ARG_LD);
COMMON(ClauseRef) 	findClause(ClauseRef cl, Word argv, LocalFrame fr,
			   Definition def, ClauseRef *next ARG_LD);
COMMON(bool) 		reindexClause(Clause clause, Definition def);
COMMON(bool) 		unify_index_pattern(Procedure proc, term_t value);
COMMON(bool) 		hashDefinition(Definition def, int buckets);
COMMON(word) 		pl_hash(term_t pred);
COMMON(void) 		addClauseToIndex(Definition def, Clause cl,
					 int where ARG_LD);
COMMON(void) 		delClauseFromIndex(Definition def, Clause cl);
COMMON(void) 		gcClauseIndex(ClauseIndex ci ARG_LD);
COMMON(void) 		unallocClauseIndexTable(ClauseIndex ci);
COMMON(void) 		markDirtyClauseIndex(ClauseIndex ci, Clause cl);

/* pl-dwim.c */
COMMON(word) 		pl_dwim_match(term_t a1, term_t a2, term_t mm);
COMMON(word) 		pl_dwim_predicate(term_t term, term_t dwim,
					  control_t h);

/* pl-ext.c */
COMMON(void) 		initBuildIns(void);
COMMON(void) 		cleanupExtensions(void);

/* pl-error.c */

COMMON(int) 		PL_error(const char *pred, int arity, const char *msg,
			 int id, ...);
COMMON(char *) 		tostr(char *buf, const char *fmt, ...);
COMMON(void) 		printMessage(atom_t severity, ...);
COMMON(int) 		PL_get_nchars_ex(term_t t, unsigned int *len, char **s,
				 unsigned int flags);
COMMON(int) 		PL_get_chars_ex(term_t t, char **s, unsigned int flags);
COMMON(int) 		PL_get_atom_ex(term_t t, atom_t *a);
COMMON(int) 		PL_get_integer_ex(term_t t, int *i);
COMMON(int) 		PL_get_long_ex(term_t t, long *i);
COMMON(int) 		PL_get_bool_ex(term_t t, int *i);
COMMON(int) 		PL_get_float_ex(term_t t, double *f);
COMMON(int) 		PL_get_char_ex(term_t t, int *p, int eof);
COMMON(int) 		PL_unify_list_ex(term_t l, term_t h, term_t t);
COMMON(int) 		PL_unify_nil_ex(term_t l);
COMMON(int) 		PL_get_list_ex(term_t l, term_t h, term_t t);
COMMON(int) 		PL_get_nil_ex(term_t l);
COMMON(int) 		PL_unify_bool_ex(term_t t, bool val);
COMMON(int) 		PL_get_arg_ex(int n, term_t term, term_t arg);


/* pl-file.c */
COMMON(void) 		initIO(void);
COMMON(void) 		dieIO(void);
COMMON(void) 		closeFiles(int all);
COMMON(int) 		openFileDescriptors(unsigned char *buf, int size);
COMMON(void) 		protocol(const char *s, int n);
COMMON(bool) 		getInputStream__LD(term_t t, IOSTREAM **s ARG_LD);
COMMON(bool) 		getOutputStream(term_t t, IOSTREAM **s);
COMMON(bool) 		streamStatus(IOSTREAM *s);
COMMON(atom_t) 		fileNameStream(IOSTREAM *s);
COMMON(int) 		getSingleChar(IOSTREAM *s);
COMMON(bool) 		readLine(IOSTREAM *in, IOSTREAM *out, char *buffer);
COMMON(bool) 		LockStream(void);
COMMON(bool) 		UnlockStream(void);
COMMON(IOSTREAM *) 	PL_current_input(void);
COMMON(IOSTREAM *) 	PL_current_output(void);
COMMON(word) 		pl_push_input_context(void);
COMMON(word) 		pl_pop_input_context(void);
COMMON(word) 		pl_told(void);
COMMON(word) 		pl_flush_output(void);
COMMON(word) 		pl_see(term_t f);
COMMON(word) 		pl_seen(void);
COMMON(word) 		pl_noprotocol(void);
COMMON(bool) 		seeString(const char *s);
COMMON(bool) 		seeingString(void);
COMMON(bool) 		seenString(void);
COMMON(bool) 		tellString(char **s, int *size);
COMMON(bool) 		toldString(void);
COMMON(word) 		pl_wait_for_input(term_t streams, term_t avail,
					  term_t tmo);
COMMON(word) 		pl_put(term_t c);
COMMON(word) 		pl_put2(term_t stream, term_t chr);
COMMON(word) 		pl_get(term_t chr);
COMMON(word) 		pl_skip(term_t chr);
COMMON(word) 		pl_skip2(term_t stream, term_t chr);
COMMON(word) 		pl_get2(term_t stream, term_t chr);
COMMON(word) 		pl_get_single_char(term_t c);
COMMON(word) 		pl_get_byte(term_t c);
COMMON(word) 		pl_get_byte2(term_t stream, term_t c);
COMMON(word) 		pl_get_char(term_t c);
COMMON(word) 		pl_get_char2(term_t stream, term_t c);
COMMON(word) 		pl_seeing(term_t f);
COMMON(word) 		pl_telling(term_t f);
COMMON(word) 		pl_tell(term_t f);
COMMON(word) 		pl_append(term_t f);
COMMON(word) 		pl_ttyflush(void);
COMMON(word) 		pl_protocol(term_t file);
COMMON(word) 		pl_protocola(term_t file);
COMMON(word) 		pl_protocolling(term_t file);
COMMON(word) 		pl_prompt(term_t old, term_t new);
COMMON(void) 		prompt1(char *prompt);
COMMON(word) 		pl_prompt1(term_t prompt);
COMMON(word) 		pl_tab(term_t n);
COMMON(char *) 		PrologPrompt(void);
COMMON(word) 		pl_tab2(term_t stream, term_t n);
COMMON(word) 		pl_open4(term_t file, term_t mode, term_t stream,
				 term_t opt);
COMMON(word) 		pl_open(term_t file, term_t mode, term_t stream);
COMMON(word) 		pl_open_null_stream(term_t stream);
COMMON(int) 		streamNo(term_t spec, int mode);
COMMON(word) 		pl_close(term_t stream);
COMMON(word) 		pl_close2(term_t stream, term_t options);
COMMON(void) 		release_stream_handle(term_t spec);
COMMON(foreign_t) 	pl_stream_property(term_t stream, term_t property,
				   control_t h);
COMMON(word) 		pl_flush_output1(term_t stream);
COMMON(word) 		pl_set_stream_position(term_t stream, term_t pos);
COMMON(word) 		pl_seek(term_t stream,
			term_t offset, term_t method, term_t newloc);
COMMON(word) 		pl_set_input(term_t stream);
COMMON(word) 		pl_set_output(term_t stream);
COMMON(foreign_t) 	pl_set_stream(term_t stream, term_t attr);
COMMON(word) 		pl_current_input(term_t stream);
COMMON(word) 		pl_current_output(term_t stream);
COMMON(word) 		pl_character_count(term_t stream, term_t count);
COMMON(word) 		pl_line_count(term_t stream, term_t count);
COMMON(word) 		pl_line_position(term_t stream, term_t count);
COMMON(word) 		pl_source_location(term_t file, term_t line);
COMMON(word) 		pl_at_end_of_stream1(term_t stream);
COMMON(word) 		pl_at_end_of_stream0();
COMMON(word) 		pl_peek_byte2(term_t stream, term_t chr);
COMMON(word) 		pl_peek_byte1(term_t chr);
COMMON(word) 		pl_peek_code2(term_t stream, term_t chr);
COMMON(word) 		pl_peek_code1(term_t chr);
COMMON(word) 		pl_peek_char2(term_t stream, term_t chr);
COMMON(word) 		pl_peek_char1(term_t chr);
COMMON(bool) 		unifyTime(term_t t, long time);
COMMON(word) 		pl_time_file(term_t name, term_t t);
COMMON(word) 		pl_size_file(term_t name, term_t len);
COMMON(word) 		pl_size_stream(term_t stream, term_t len);
COMMON(word) 		pl_access_file(term_t name, term_t mode);
COMMON(word) 		pl_read_link(term_t file, term_t link, term_t to);
COMMON(word) 		pl_exists_file(term_t name);
COMMON(word) 		pl_exists_directory(term_t name);
COMMON(word) 		pl_tmp_file(term_t base, term_t name);
COMMON(word) 		pl_delete_file(term_t name);
COMMON(word) 		pl_delete_directory(term_t name);
COMMON(word) 		pl_make_directory(term_t name);
COMMON(word) 		pl_same_file(term_t file1, term_t file2);
COMMON(word) 		pl_rename_file(term_t old, term_t new);
COMMON(word) 		pl_fileerrors(term_t old, term_t new);
COMMON(word) 		pl_absolute_file_name(term_t name, term_t expanded);
COMMON(word) 		pl_is_absolute_file_name(term_t name);
COMMON(word) 		pl_working_directory(term_t from, term_t to);
COMMON(word) 		pl_file_base_name(term_t f, term_t b);
COMMON(word) 		pl_file_dir_name(term_t f, term_t b);
COMMON(word) 		pl_file_name_extension(term_t base, term_t ext, term_t full);
COMMON(word) 		pl_prolog_to_os_filename(term_t pl, term_t os);
COMMON(foreign_t) 	pl_mark_executable(term_t path);
#ifdef __WIN32__
COMMON(word) 		pl_make_fat_filemap(term_t dir);
#endif
COMMON(word) 		pl_copy_stream_data3(term_t in, term_t out, term_t len);
COMMON(word) 		pl_copy_stream_data2(term_t in, term_t out);
COMMON(int) 		PL_get_char(term_t c, int *p, int eof);
COMMON(int) 		PL_unify_char(term_t chr, int c, int mode);
COMMON(int) 		PL_unify_stream_or_alias(term_t t, IOSTREAM *s);
COMMON(void) 		pushOutputContext(void);
COMMON(void) 		popOutputContext(void);

/* pl-flag.c */
COMMON(void) 		initFlags(void);
COMMON(word) 		pl_current_flag(term_t k, control_t h);
COMMON(void) 		initFeatureTable(void);
COMMON(void) 		initFeatures(void);

/* pl-fli.c */
COMMON(word) 		linkVal__LD(Word p ARG_LD);
COMMON(word) 		makeNum__LD(long i ARG_LD);
COMMON(void) 		_PL_put_number__LD(term_t t, Number n ARG_LD);
COMMON(predicate_t) 	_PL_predicate(const char *name, int arity,
				      const char *module, predicate_t *bin);
COMMON(void) 		initialiseForeign(int argc, char **argv);
COMMON(void) 		cleanupInitialiseHooks(void);
COMMON(char *) 		buffer_string(const char *s, int flags);
COMMON(atom_t) 		codeToAtom(int code);
COMMON(extern)  	record_t PL_duplicate_record(record_t r);
COMMON(int) 		PL_unify_termv(term_t t, va_list args);
COMMON(term_t) 		wordToTermRef(Word p);
COMMON(void) 		_PL_get_arg__LD(int index, term_t t, term_t a ARG_LD);
COMMON(term_t) 		PL_new_term_ref__LD(ARG1_LD);
COMMON(term_t) 		PL_new_term_refs__LD(int n ARG_LD);
COMMON(int) 		PL_unify__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int) 		PL_unify_integer__LD(term_t t1, long i ARG_LD);
COMMON(int) 		PL_get_atom__LD(term_t t1, atom_t *a ARG_LD);
COMMON(void) 		PL_put_atom__LD(term_t t1, atom_t a ARG_LD);
COMMON(void) 		PL_put_integer__LD(term_t t1, long i ARG_LD);
COMMON(int) 		PL_is_atomic__LD(term_t t ARG_LD);
COMMON(int) 		PL_is_functor__LD(term_t t, functor_t f ARG_LD);
COMMON(int) 		PL_is_variable__LD(term_t t ARG_LD);
COMMON(int) 		PL_strip_module__LD(term_t q, module_t *m, term_t t ARG_LD);
COMMON(int) 		PL_get_integer__LD(term_t t, int *i ARG_LD);
COMMON(int) 		PL_get_long__LD(term_t t, long *i ARG_LD);
COMMON(int) 		PL_get_pointer__LD(term_t t, void **ptr ARG_LD);
COMMON(void) 		PL_put_term__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int) 		PL_get_functor__LD(term_t t, functor_t *f ARG_LD);
COMMON(int) 		PL_unify_atom__LD(term_t t, atom_t a ARG_LD);
COMMON(int) 		PL_unify_pointer__LD(term_t t, void *ptr ARG_LD);
COMMON(int) 		PL_get_list__LD(term_t l, term_t h, term_t t ARG_LD);
COMMON(int) 		PL_is_atom__LD(term_t t ARG_LD);
COMMON(int) 		PL_unify_list__LD(term_t l, term_t h, term_t t ARG_LD);
COMMON(void) 		PL_cons_list__LD(term_t l, term_t head, term_t tail
					 ARG_LD);
COMMON(int)		PL_is_inf(term_t t);

COMMON(void) 		registerForeignLicenses(void);

/* pl-fmt.c */
COMMON(word) 		pl_format_predicate(term_t chr, term_t descr);
COMMON(word) 		pl_current_format_predicate(term_t chr, term_t descr,
					    control_t h);
COMMON(word) 		pl_format(term_t fmt, term_t args);
COMMON(word) 		pl_format3(term_t s, term_t fmt, term_t args);

/* pl-funct.c */
COMMON(functor_t) 	lookupFunctorDef(atom_t atom, unsigned int arity);
COMMON(functor_t) 	isCurrentFunctor(atom_t atom, unsigned int arity);
COMMON(void) 		initFunctors(void);
COMMON(void) 		cleanupFunctors(void);
COMMON(int)  		checkFunctors(void);
COMMON(word) 		pl_current_functor(term_t name, term_t arity,
					   control_t h);

/* pl-gc.c */
COMMON(void) 		considerGarbageCollect(Stack s);
COMMON(void) 		garbageCollect(LocalFrame fr, Choice ch);
COMMON(word) 		pl_garbage_collect(term_t d);
COMMON(void) 		blockGC(ARG1_LD);
COMMON(void) 		unblockGC(ARG1_LD);
COMMON(Word) 		findGRef(int n);
COMMON(int) 		growStacks(LocalFrame fr, Choice ch, Code PC,
			   long lminfree, long gminfree, long tminfree);
COMMON(void) 		clearUninitialisedVarsFrame(LocalFrame, Code);
COMMON(word) 		check_foreign(void);	/* O_SECURE stuff */
COMMON(void) 		markAtomsOnStacks(PL_local_data_t *ld);
COMMON(void) 		markPredicatesInEnvironments(PL_local_data_t *ld);
#if defined(O_SECURE) || defined(SECURE_GC)
COMMON(word) 		checkStacks(LocalFrame fr, Choice ch);
COMMON(bool)		scan_global(int marked);
#endif

/* pl-glob.c */
COMMON(word) 		pl_wildcard_match(term_t pattern, term_t string);
COMMON(word) 		pl_expand_file_name(term_t f, term_t l);

/* pl-itf.c */
COMMON(void) 		resetForeign(void);

/* pl-list.c */
COMMON(word) 		pl_length(term_t list, term_t l);
COMMON(word) 		pl_memberchk(term_t e, term_t list);

/* pl-load.c */
COMMON(word) 		pl_open_shared_object(term_t file, term_t h, term_t flags);
COMMON(word) 		pl_close_shared_object(term_t plhandle);
COMMON(word) 		pl_call_shared_object_function(term_t plhandle, term_t name);
COMMON(word) 		pl_load_shared_object(term_t file, term_t entry);

/* pl-modul.c */
COMMON(Module) 		lookupModule(atom_t name);
COMMON(Module) 		isCurrentModule(atom_t name);
COMMON(void) 		initModules(void);
COMMON(int) 		isSuperModule(Module s, Module m);
COMMON(Word) 		stripModule(Word term, Module *module ARG_LD);
COMMON(bool) 		isPublicModule(Module module, Procedure proc);
COMMON(int) 		declareModule(atom_t name, SourceFile sf, int line);
COMMON(word) 		pl_current_module(term_t module, term_t file, control_t h);
COMMON(word) 		pl_strip_module(term_t spec, term_t module, term_t term);
COMMON(word) 		pl_module(term_t old, term_t new);
COMMON(word) 		pl_set_source_module(term_t old, term_t new);
COMMON(word) 		pl_declare_module(term_t name, term_t file, term_t line);
COMMON(word) 		pl_export_list(term_t modulename, term_t list);
COMMON(word) 		pl_export(term_t head);
COMMON(word) 		pl_check_export(void);
COMMON(word) 		pl_context_module(term_t module);
COMMON(word) 		pl_import(term_t pred);
#ifdef O_PROLOG_HOOK
COMMON(word) 		pl_set_prolog_hook(term_t module, term_t old, term_t new);
#endif


/* pl-op.c */
COMMON(int) 		currentOperator(Module m, atom_t name, int kind,
				int *type, int *priority);
COMMON(int) 		priorityOperator(Module m, atom_t atom);
COMMON(word) 		pl_current_op(term_t prec, term_t type, term_t name,
			      control_t h);
COMMON(word) 		pl_local_op(term_t prec, term_t type, term_t name,
			    control_t h);
COMMON(word) 		pl_op(term_t priority, term_t type, term_t name);
COMMON(void) 		initOperators(void);
COMMON(word) 		pl_builtin_op(term_t prec, term_t type, term_t name,
			      control_t h);

/* pl-os.c */
COMMON(bool) 		initOs(void);
COMMON(void) 		cleanupOs(void);
COMMON(char *) 		OsError(void);
COMMON(long) 		Random(void);
COMMON(char *) 		canonisePath(char *path);
COMMON(char *) 		OsPath(const char *plpath, char *ospath);
COMMON(char *) 		PrologPath(const char *ospath, char *plpath);
COMMON(long) 		LastModifiedFile(char *f);
COMMON(bool) 		ExistsFile(const char *path);
COMMON(bool) 		AccessFile(const char *path, int mode);
COMMON(bool) 		ExistsDirectory(const char *path);
COMMON(long) 		SizeFile(const char *path);
COMMON(int) 		RemoveFile(const char *path);
COMMON(void) 		RemoveTemporaryFiles(void);
COMMON(bool) 		RenameFile(const char *old, const char *new);
COMMON(bool) 		SameFile(const char *f1, const char *f2);
COMMON(bool) 		OpenStream(int fd);
COMMON(bool) 		MarkExecutable(const char *name);
COMMON(bool) 		expandVars(const char *pattern, char *expanded);
COMMON(char *) 		ExpandOneFile(const char *spec, char *file);
COMMON(char *) 		getwd(char *buf);
COMMON(char *) 		AbsoluteFile(const char *spec, char *path);
COMMON(int) 		IsAbsolutePath(const char *spec);
COMMON(char *) 		BaseName(const char *f);
COMMON(char *) 		DirName(const char *f, char *buf);
COMMON(char *) 		ReadLink(const char *f, char *buf);
COMMON(char *) 		DeRefLink(const char *link, char *buf);
COMMON(bool) 		ChDir(const char *path);
COMMON(atom_t) 		TemporaryFile(const char *id);
COMMON(int) 		hasConsole(void);
COMMON(struct)  tm *	LocalTime(long int *t, struct tm *r);
COMMON(Char) 		GetChar(void);
COMMON(char *) 		getenv3(const char *, char *buf, unsigned int buflen);
COMMON(int) 		getenvl(const char *);
COMMON(int) 		Setenv(char *name, char *value);
COMMON(int) 		Unsetenv(char *name);
COMMON(int) 		System(char *cmd);
COMMON(char *) 		findExecutable(const char *module, char *buf);
COMMON(int) 		Pause(real time);
#if __WIN32__
COMMON(int) 		iswin32s(void);
#endif /*__WIN32__*/

/* pl-prims.c */
COMMON(word) 		pl_notunify(term_t t1, term_t t2);
COMMON(int) 		compareStandard(Word t1, Word t2, int eq ARG_LD);
COMMON(int) 		lengthList(term_t list, int errors);
COMMON(word) 		pl_univ(term_t t, term_t l);
COMMON(int) 		numberVars(term_t t, functor_t functor,
				   av_action on_av, int n ARG_LD);
COMMON(word) 		pl_e_free_variables(term_t t, term_t l);
COMMON(word) 		stringToList(char *s);
COMMON(word) 		pl_atom_length(term_t w, term_t n);
COMMON(word) 		pl_int_to_atom(term_t number, term_t base,
			       term_t atom);
COMMON(char *) 		formatInteger(bool split, int div, int radix,
			      bool small, long n, char *out);
COMMON(word) 		pl_format_number(term_t format, term_t number,
				 term_t string);
COMMON(word) 		pl_name(term_t atom, term_t string);
COMMON(word) 		pl_atom_chars(term_t atom, term_t string);
COMMON(word) 		pl_atom_codes(term_t atom, term_t string);
COMMON(word) 		pl_number_chars(term_t number, term_t string);
COMMON(word) 		pl_number_codes(term_t number, term_t string);
COMMON(word) 		pl_char_code(term_t atom, term_t chr);
COMMON(word) 		pl_atom_prefix(term_t atom, term_t prefix);
COMMON(word) 		pl_atom_concat(term_t a1, term_t a2, term_t a3, control_t ctx);
COMMON(word) 		pl_concat_atom(term_t list, term_t atom);
COMMON(word) 		pl_concat_atom3(term_t list, term_t sep, term_t atom);
COMMON(word) 		pl_apropos_match(term_t a1, term_t a2);
COMMON(foreign_t) 	pl_sub_atom(term_t atom,
			    term_t before, term_t len, term_t after,
			    term_t sub, control_t h);
COMMON(word) 		pl_string_length(term_t str, term_t l);
COMMON(word) 		pl_string_concat(term_t a1, term_t a2, term_t a3, control_t h);
COMMON(word) 		pl_string_to_atom(term_t str, term_t a);
COMMON(word) 		pl_string_to_list(term_t str, term_t list);
COMMON(word) 		pl_sub_string(term_t str,
			      term_t offset, term_t length, term_t after,
			      term_t sub, control_t h);
COMMON(word) 		pl_write_on_string(term_t goal, term_t string);
COMMON(word) 		pl_repeat(control_t h);
COMMON(word) 		pl_fail(void);
COMMON(word) 		pl_true(void);
COMMON(word) 		pl_halt(term_t code);
COMMON(int) 		pl_statistics_ld(term_t k, term_t value,
				 PL_local_data_t *ld ARG_LD);
COMMON(int) 		set_pl_option(const char *name, const char *value);
COMMON(word) 		pl_novice(term_t old, term_t new);

/* pl-feature.c */
COMMON(void) 		defFeature(const char *name, int flags, ...);
COMMON(word) 		pl_feature(term_t key, term_t value, control_t h);
COMMON(word) 		pl_feature5(term_t key, term_t value,
				    term_t local, term_t access, term_t type,
				    control_t h);
COMMON(word) 		pl_set_feature(term_t key, term_t value);
COMMON(int) 		setDoubleQuotes(atom_t a, unsigned int *flagp);

/* pl-pro.c */
COMMON(word) 		pl_break(void);
COMMON(word) 		pl_break1(term_t goal);
COMMON(word) 		pl_notrace1(term_t goal);
#ifdef O_LIMIT_DEPTH
COMMON(word) 		pl_depth_limit(term_t limit, term_t olimit, term_t oreached);
COMMON(word) 		pl_depth_limit_true(term_t limit,
				    term_t olimit, term_t oreached,
				    term_t res, term_t cut, control_t b);
#endif /*O_LIMIT_DEPTH*/
COMMON(int) 		callProlog(Module module, term_t goal, int flags, term_t *ex);
COMMON(word) 		pl_abort(abort_type type);
COMMON(bool) 		prologToplevel(atom_t toplevel);
COMMON(word) 		pl_metacut(void);
COMMON(int)  		trap_gdb(void);
COMMON(word) 		checkData(Word p);

/* pl-proc.c */
COMMON(Procedure) 	lookupProcedure(functor_t f, Module m);
COMMON(Procedure) 	isCurrentProcedure(functor_t f, Module m);
COMMON(int)		importDefinitionModule(Module m, Definition def);
COMMON(Procedure) 	lookupProcedureToDefine(functor_t def, Module m);
COMMON(ClauseRef) 	hasClausesDefinition(Definition def);
COMMON(bool) 		isDefinedProcedure(Procedure proc);
COMMON(int) 		get_head_functor(term_t head, functor_t *fdef,
				 int flags ARG_LD);
COMMON(int) 		get_procedure(term_t descr, Procedure *proc, term_t he, int f);
COMMON(word) 		pl_current_predicate(term_t name, term_t functor, control_t h);
COMMON(foreign_t) 	pl_current_predicate1(term_t spec, control_t ctx);
COMMON(ClauseRef) 	assertProcedure(Procedure proc, Clause clause,
				int where ARG_LD);
COMMON(bool) 		abolishProcedure(Procedure proc, Module module);
COMMON(bool) 		retractClauseProcedure(Procedure proc, Clause clause ARG_LD);
COMMON(void) 		freeClause(Clause c ARG_LD);
COMMON(void) 		freeClauseRef(ClauseRef c ARG_LD);
COMMON(ClauseRef) 	newClauseRef(Clause cl ARG_LD);
COMMON(void) 		gcClausesDefinition(Definition def);
COMMON(void) 		gcClausesDefinitionAndUnlock(Definition def);
COMMON(void) 		destroyDefinition(Definition def);
COMMON(void) 		resetReferences(void);
COMMON(Procedure) 	resolveProcedure(functor_t f, Module module);
COMMON(Definition) 	trapUndefined(LocalFrame fr, Code PC, Procedure proc ARG_LD);
COMMON(word) 		pl_retract(term_t term, control_t h);
COMMON(word) 		pl_retractall(term_t head);
COMMON(word) 		pl_abolish(term_t atom, term_t arity);
COMMON(word) 		pl_abolish1(term_t pred);
COMMON(word) 		pl_get_clause_attribute(term_t ref, term_t att, term_t value);
COMMON(word) 		pl_get_predicate_attribute(term_t pred, term_t k, term_t v);
COMMON(word) 		pl_set_predicate_attribute(term_t pred, term_t k, term_t v);
COMMON(void) 		reindexDefinition(Definition def);
COMMON(void) 		redefineProcedure(Procedure proc, SourceFile sf);
COMMON(void) 		startConsult(SourceFile f);
COMMON(void) 		indexDefinition(Definition def, unsigned long pattern);
COMMON(word) 		pl_index(term_t pred);
COMMON(SourceFile) 	lookupSourceFile(atom_t name);
COMMON(SourceFile) 	indexToSourceFile(int index);
COMMON(void) 		cleanupSourceFiles(void);
COMMON(void) 		addProcedureSourceFile(SourceFile sf, Procedure proc);
COMMON(word) 		pl_make_system_source_files(void);
COMMON(word) 		pl_source_file(term_t descr, term_t file, control_t h);
COMMON(word) 		pl_time_source_file(term_t file, term_t t, control_t h);
COMMON(word) 		pl_start_consult(term_t file);
COMMON(word) 		pl_default_predicate(term_t d1, term_t d2);
COMMON(Definition) 	autoImport(functor_t f, Module m);
COMMON(word) 		pl_require(term_t pred);
COMMON(word) 		pl_check_definition(term_t spec);
COMMON(word) 		pl_clause_from_source(term_t file, term_t line, term_t clause);
COMMON(foreign_t) 	pl_list_generations(term_t desc);
COMMON(foreign_t) 	pl_check_procedure(term_t desc);
COMMON(void) 		checkDefinition(Definition def);
COMMON(Procedure) 	isStaticSystemProcedure(functor_t fd);
COMMON(foreign_t) 	pl_garbage_collect_clauses(void);
COMMON(int) 		setDynamicProcedure(Procedure proc, bool isdyn);


/* pl-prof.c */
COMMON(void) 		stopItimer(void);
COMMON(bool) 		resetProfiler(void);
COMMON(struct)  call_node* profCall(Definition def ARG_LD);
COMMON(void) 		profExit(struct call_node *node ARG_LD);
COMMON(void) 		profRedo(struct call_node *node ARG_LD);

/* pl-read.c */
COMMON(void) 		resetRead(void);
COMMON(int) 		get_number(const unsigned char *string,
			   unsigned char **end,
			   Number value, bool escape);
COMMON(word) 		pl_raw_read(term_t term);
COMMON(word) 		pl_raw_read2(term_t stream, term_t term);
COMMON(word) 		pl_read(term_t term);
COMMON(word) 		pl_read2(term_t stream, term_t term);
COMMON(word) 		pl_read_term(term_t term, term_t pos);
COMMON(word) 		pl_read_term3(term_t stream, term_t term, term_t pos);
COMMON(word) 		pl_atom_to_term(term_t term, term_t atom, term_t bindings);
COMMON(void) 		initCharConversion(void);
COMMON(foreign_t) 	pl_char_conversion(term_t in, term_t out);
COMMON(foreign_t) 	pl_current_char_conversion(term_t in, term_t out, control_t h);
COMMON(int) 		read_clause(IOSTREAM *s, term_t term ARG_LD);

/* pl-rec.c */
COMMON(void) 		initRecords(void);
COMMON(Record) 		compileTermToHeap__LD(term_t term, int flags ARG_LD);
COMMON(void) 		copyRecordToGlobal(term_t copy, Record term ARG_LD);
COMMON(int) 		structuralEqualArg1OfRecord(term_t t, Record r ARG_LD);
COMMON(bool) 		freeRecord__LD(Record record ARG_LD);
COMMON(bool) 		unifyKey(term_t key, word val);
COMMON(int) 		getKeyEx(term_t key, word *k ARG_LD);
COMMON(word) 		pl_current_key(term_t k, control_t h);
COMMON(word) 		pl_recorda(term_t key, term_t term, term_t ref);
COMMON(word) 		pl_recordz(term_t key, term_t term, term_t ref);
COMMON(word) 		pl_recorded(term_t key, term_t term, term_t ref, control_t h);
COMMON(word) 		pl_erase(term_t ref);
COMMON(word) 		pl_term_complexity(term_t t, term_t mx, term_t count);
COMMON(void) 		undo_while_saving_term(mark *m, Word term);

/* pl-rl.c */
COMMON(void) 		install_rl(void);

/* pl-setup.c */
COMMON(void) 		setupProlog(void);
COMMON(foreign_t) 	pl_on_signal(term_t sig, term_t name, term_t old, term_t new);
COMMON(handler_t) 	set_sighandler(int sig, handler_t func);
COMMON(void) 		blockSignals(sigset_t *mask);
COMMON(void) 		allSignalMask(sigset_t *set);
COMMON(void) 		unblockSignals(sigset_t *mask);
COMMON(void) 		unblockSignal(int sig);
COMMON(void) 		blockSignal(int sig);
COMMON(void) 		resetSignals(void);
COMMON(void) 		cleanupSignals(void);
COMMON(int) 		initPrologStacks(long local,
				 long global,
				 long trail,
				 long argument);
COMMON(void) 		initPrologLocalData(void);
COMMON(void) 		deallocateStacks(void);
COMMON(bool) 		restoreStack(Stack s);
COMMON(void) 		trimStacks(ARG1_LD);
COMMON(void) 		resetStacks(void);
COMMON(void) 		emptyStacks(void);
COMMON(void) 		freeStacks(PL_local_data_t *ld);
COMMON(void) 		freeLocalData(PL_local_data_t *ld);
COMMON(word) 		pl_trim_stacks(void);
COMMON(word) 		pl_limit_stack(term_t s, term_t l);
COMMON(word) 		pl_stack_parameter(term_t s, term_t k, term_t o, term_t n);
COMMON(void) 		ensureRoomStack(Stack s, int n);
COMMON(int) 		_PL_get_signum(term_t sig, int *n);

/* pl-sys.c */
COMMON(word) 		pl_shell(term_t command, term_t status);
COMMON(word) 		pl_getenv(term_t var, term_t value);
COMMON(word) 		pl_setenv(term_t var, term_t value);
COMMON(word) 		pl_unsetenv(term_t var);
COMMON(word) 		pl_convert_time(term_t time, term_t year,
				term_t month, term_t day,
				term_t hour, term_t minute,
				term_t second, term_t usec);
COMMON(word) 		pl_convert_time2(term_t time, term_t string);
COMMON(word) 		pl_get_time(term_t t);
COMMON(word) 		pl_sleep(term_t time);
COMMON(word) 		pl_get_pid(term_t pid);

/* pl-table.c */
COMMON(void) 		initTables();
COMMON(Table) 		newHTable(int size);
COMMON(void) 		destroyHTable(Table ht);
COMMON(Symbol) 		lookupHTable(Table ht, void *name);
COMMON(Symbol) 		addHTable(Table ht, void *name, void *value);
COMMON(void) 		deleteSymbolHTable(Table ht, Symbol s);
COMMON(void) 		clearHTable(Table ht);
COMMON(Table) 		copyHTable(Table org);
COMMON(TableEnum) 	newTableEnum(Table ht);
COMMON(void) 		freeTableEnum(TableEnum e);
COMMON(Symbol) 		advanceTableEnum(TableEnum e);

/* pl-trace.c */
COMMON(int) 		tracePort(LocalFrame frame, Choice bfr,
			  int port, Code PC ARG_LD);
COMMON(void) 		backTrace(LocalFrame frame, int depth);
COMMON(void) 		initTracer(void);
COMMON(void) 		resetTracer(void);
COMMON(int) 		tracemode(int new, int *old);
COMMON(int) 		debugmode(debug_type new, debug_type *old);
COMMON(word) 		pl_trace(void);
COMMON(word) 		pl_notrace(void);
COMMON(word) 		pl_tracing(void);
COMMON(word) 		pl_skip_level(term_t old, term_t new);
COMMON(word) 		pl_spy(term_t p);
COMMON(word) 		pl_nospy(term_t p);
COMMON(word) 		pl_leash(term_t old, term_t new);
COMMON(word) 		pl_visible(term_t old, term_t new);
COMMON(word) 		pl_debuglevel(term_t old, term_t new);
COMMON(word) 		pl_prolog_current_frame(term_t fr);
COMMON(word) 		pl_prolog_frame_attribute(term_t fr, term_t key, term_t val);
COMMON(foreign_t) 	pl_prolog_choice_attribute(term_t fr, term_t key, term_t val);
COMMON(void) 		callEventHook(int ev, ...);

/* pl-util.c */
COMMON(char) 		digitName(int n, bool small);
COMMON(int) 		digitValue(int b, int c);
COMMON(char *) 		procedureName(Procedure proc);
COMMON(char *) 		predicateName(Definition def);
COMMON(word) 		notImplemented(char *name, int arity);
COMMON(word) 		setBoolean(int *flag, term_t o, term_t n);
COMMON(word) 		setInteger(int *val, term_t old, term_t new);
COMMON(word) 		setLong(long *val, term_t old, term_t new);
COMMON(bool) 		strprefix(const char *string, const char *prefix);
COMMON(bool) 		strpostfix(const char *string, const char *postfix);
COMMON(bool) 		stripostfix(const char *string, const char *postfix);
COMMON(bool) 		scan_options(term_t list, int flags, atom_t name,
				     const opt_spec *specs, ...);
COMMON(const char *)  atom_summary(atom_t name, unsigned int maxlen);
#ifndef HAVE_STRICMP
COMMON(int) 		stricmp(const char *s1, const char *s2);
#endif
#ifndef HAVE_STRLWR
COMMON(char *) 		strlwr(char *s);
#endif

/* pl-wic.c */
COMMON(bool) 		loadWicFromStream(IOSTREAM *fd);
COMMON(word) 		pl_open_wic(term_t name);
COMMON(word) 		pl_close_wic(void);
COMMON(word) 		pl_add_directive_wic(term_t term);
COMMON(word) 		pl_import_wic(term_t module, term_t head);
COMMON(bool) 		compileFileList(IOSTREAM *out, int argc, char **argv);
COMMON(void) 		qlfCleanup(void);

COMMON(word) 		pl_qlf_put_states(void);
COMMON(word) 		pl_qlf_start_module(term_t name);
COMMON(word) 		pl_qlf_start_sub_module(term_t name);
COMMON(word) 		pl_qlf_start_file(term_t name);
COMMON(word) 		pl_qlf_end_part(void);
COMMON(word) 		pl_qlf_open(term_t file);
COMMON(word) 		pl_qlf_close(void);
COMMON(word) 		pl_qlf_load(term_t file, term_t module);
COMMON(word) 		pl_qlf_assert_clause(term_t ref, term_t saveclass);

/* pl-write.c */
COMMON(char *) 		varName(term_t var, char *buf);
COMMON(word) 		pl_nl(void);
COMMON(word) 		pl_nl1(term_t stream);
COMMON(word) 		pl_write_canonical(term_t term);
COMMON(word) 		pl_write_canonical2(term_t stream, term_t term);
COMMON(word) 		pl_write_term(term_t term, term_t options);
COMMON(word) 		pl_write_term3(term_t stream,
			       term_t term, term_t options);
COMMON(word) 		pl_write(term_t term);
COMMON(word) 		pl_writeln(term_t term);
COMMON(word) 		pl_writeq(term_t term);
COMMON(word) 		pl_print(term_t term);
COMMON(word) 		pl_write2(term_t stream, term_t term);
COMMON(word) 		pl_writeq2(term_t stream, term_t term);
COMMON(word) 		pl_print2(term_t stream, term_t term);
COMMON(int) 		writeAttributeMask(atom_t name);

/* pl-term.c */
COMMON(void) 		resetTerm(void);
COMMON(word) 		pl_tty_get_capability(term_t name, term_t type, term_t value);
COMMON(word) 		pl_tty_goto(term_t x, term_t y);
COMMON(word) 		pl_tty_put(term_t a, term_t affcnt);
COMMON(word) 		pl_tty_size(term_t r, term_t c);

/* pl-main.c */
COMMON(int) 		startProlog(int argc, char **argv);
COMMON(bool) 		sysError(const char *fm, ...);
COMMON(bool) 		fatalError(const char *fm, ...);
COMMON(bool) 		warning(const char *fm, ...);
COMMON(bool) 		vfatalError(const char *fm, va_list args);
COMMON(bool) 		vwarning(const char *fm, va_list args);

/* pl-dde.c */

COMMON(word) 		pl_open_dde_conversation(term_t serv, term_t top, term_t hdl);
COMMON(word) 		pl_close_dde_conversation(term_t handle);
COMMON(word) 		pl_dde_request(term_t h, term_t it, term_t value, term_t tmo);
COMMON(word) 		pl_dde_execute(term_t handle, term_t cmd, term_t tmo);
COMMON(word) 		pl_dde_register_service(term_t service, term_t onoff);
COMMON(word) 		pl_dde_poke(term_t h, term_t item, term_t data, term_t tmo);

/* pl-dll.c */
COMMON(word) 		pl_open_dll(term_t name, term_t handle);
COMMON(word) 		pl_close_dll(term_t handle);
COMMON(word) 		pl_call_dll_function(term_t handle, term_t funcname);

/* pl-nt.c */

COMMON(void) 		PlMessage(const char *buf, ...);
COMMON(word) 		pl_window_title(term_t old, term_t new);
COMMON(word) 		pl_win_exec(term_t command, term_t show);
COMMON(foreign_t) 	pl_win_module_file(term_t module, term_t file);
#ifdef EMULATE_DLOPEN
COMMON(void *) 		dlopen(const char *file, int flags);
COMMON(const)  char *	dlerror(void);
COMMON(void *) 		dlsym(void *handle, char *symbol);
COMMON(int) 		dlclose(void *handle);
#endif /*EMULATE_DLOPEN*/
COMMON(foreign_t) 	pl_get_registry_value(term_t Key, term_t Name, term_t Value);
COMMON(void) 		getDefaultsFromRegistry(void);

/* pl-rc.c */
COMMON(IOSTREAM *)       SopenRC(void *rca,
			const char *name, const char *rcclass, int flags);
COMMON(foreign_t) 	pl_rc_handle(term_t h);
COMMON(foreign_t)        pl_rc_open(term_t rc_h,
			   term_t name, term_t class,
			   term_t rw, term_t handle);
COMMON(foreign_t)        pl_rc_open_archive(term_t file, term_t handle);
COMMON(foreign_t)        pl_rc_close_archive(term_t rc_h);
COMMON(foreign_t)        pl_rc_save_archive(term_t rc_h, term_t to);
COMMON(foreign_t)        pl_rc_append_file(term_t rc_h,
				  term_t name, term_t class, term_t encoding,
				  term_t file);
COMMON(foreign_t) 	pl_rc_members(term_t rc_h, term_t members);

/* pl-xterm.c */

COMMON(foreign_t) 	pl_open_xterm(term_t title, term_t in, term_t out, term_t err);

/* pl-ctype.c */

COMMON(foreign_t) 	pl_char_type(term_t chr, term_t class, control_t h);
COMMON(foreign_t) 	pl_code_type(term_t chr, term_t class, control_t h);
COMMON(foreign_t) 	pl_downcase_atom(term_t in, term_t out);
COMMON(foreign_t) 	pl_upcase_atom(term_t in, term_t out);
COMMON(void) 		initCharTypes(void);
COMMON(bool) 		systemMode(bool accept);


/* pl-thread.c */
COMMON(foreign_t) 	pl_with_mutex(term_t mutex, term_t goal);
COMMON(foreign_t) 	pl_thread_self(term_t self);
COMMON(int) 		enableThreads(int enable);


