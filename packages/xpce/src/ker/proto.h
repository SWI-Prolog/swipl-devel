
/* alloc.c */
Any		alloc(register int n);
void		checkFreeChains(void);
void		unalloc(register int n, Any p);
void		initAlloc(void);
void		allocRange(void *low, int size);
status		listWastedCorePce(Pce pce, Bool ppcells);
char *		save_string(const char *s);
void		free_string(char *s);

/* assoc.c */
PceITFSymbol	newSymbol(Any obj, Name name);
void		deleteAssoc(Any obj);
void		newAssoc(Name name, Any obj);
Any		getObjectAssoc(Name name);
Name		getNameAssoc(Any obj);
status		renameAssoc(Name old, Name new);
status		forSomeAssoc(Code code);
void		initAssoc(int handles);

/* behaviour.c */
status		initialiseBehaviour(Behaviour b, Name name, Any ctx);
status		makeClassBehaviour(Class class);

/* class.c */
Class		typeClass(Name name);
Class		nameToTypeClass(Name name);
Class		defineClass(Name name, Name super, StringObj summary, SendFunc makefunction);
status		defineClasses(struct class_definition *classes);
status		realiseClass(Class class);
status		realiseBootClass(Class class);
Class		bootClass(Name name, Name super_name, int size, int slots, SendFunc newF, int argc, ...);
void		lookupBootClass(Class class, Func func, int argc, ...);
Class		getConvertClass(Class class_class, Any obj);
status		initClass(Class class);
status		prepareClass(Class class);
void		makeBuiltInClasses(VoidFunc *f);
status		instanceVariableClass(Class class, Variable var);
status		sendMethodClass(Class class, SendMethod m);
status		getMethodClass(Class class, GetMethod m);
status		setChangedFunctionClass(Class class, SendFunc func);
status		setTraceFunctionClass(Class class, VoidFunc func);
status		setInEventAreaFunctionClass(Class class, SendFunc func);
status		isPceSlot(Class class, int n);
status		allPceSlotsClass(Class class);
void		termClass(Class class, char *name, int argc, ...);
status		sendMethod(Class class, Name name, Name group, int argc, ...);
status		storeMethod(Class class, Name name, SendFunc function);
status		getMethod(Class class, Name name, Name group, char *rtype, int argc, ...);
status		cloneStyleClass(Class class, Name style);
status		cloneStyleVariableClass(Class class, Name slot, Name style);
status		saveStyleVariableClass(Class class, Name slot, Name style);
status		saveStyleClass(Class class, Name style);
status		setCloneFunctionClass(Class class, SendFunc function);
status		setRedrawFunctionClass(Class class, SendFunc function);
status		setLoadStoreFunctionClass(Class class, SendFunc load, SendFunc store);
status		solidClass(Class class, Bool val);
status		sourceClass(Class class, SendFunc f, char *file, char *rcs);
void		localClass(Class class, Name name, Name group, char *type, Name access, char *doc);
void		superClass(Class class, Name name, Name group, char *type, Name access, Name wrapper, char *doc);
status		delegateClass(Class class, Name name);
status		prependDelegateClass(Class class, Name name);
Variable	getInstanceVariableClass(Class class, Any which);
Any		getResolveSendMethodClass(Class class, Name name);
Any		getResolveGetMethodClass(Class class, Name name);
Int		getNoCreatedClass(Class class);
Int		getNoFreedClass(Class class);
Chain		getSubClassesClass(Class class);
status		createdClass(Class class, Any instance, Name how);
status		freedClass(Class class, Any instance);
status		isAClass(Class class, Class super);
int		numberTreeClass(Class class, int n);
status		makeClassClass(Class class);

/* conversion.c */
status		toString(Any obj, String s);
char *		toCharp(Any obj);
Int		toInteger(Any obj);
Real		toReal(Any obj);
Bool		toBool(Any obj);
Name		toName(Any obj);
Type		toType(Any obj);
char *		pcePP(Any obj);
Any		expandFunction(Any obj);

/* csymbol.c */
Name		getCSymbolFilePce(Pce pce);
char *		c_function_name_from_address(long int addr, int *perc);
Name		getCFunctionNamePce(Pce pce, Int address);

/* debug.c */
void		catchErrorSignals(Bool yes);
void		pcePrintStack(int depth);
status		confirmTerminal(char *question, char *def);

/* declarations.c */
void		initClassDefs(void);
void		initTypeAliases(void);

/* error.c */
Error		getConvertError(Class class, Name id);
status		makeClassError(Class class);
status		errorPce(Any obj, Name id, ...);

/* gc.c */
void		pushAnswerObject(Any obj);
void		deleteAnswerObject(Any obj);
void		_markAnswerStack(AnswerMark *mark);
void		_rewindAnswerStack(AnswerMark *mark, Any obj);
void		initAnswerStack(void);
void		resetAnswerStack(void);
Int		countAnswerStack(void);

/* getmethod.c */
GetMethod	createGetMethod(Name name, Type rtype, Vector types, StringObj doc, Func action);
status		initialiseGetMethod(GetMethod m, Name name, Type rtype, Vector types, Function msg, StringObj doc, SourceLocation loc, Name group);
Any		getGetGetMethod(GetMethod m, Any receiver, int argc, const Any argv []);
status		makeClassGetMethod(Class class);

/* glob.c */

/* global.c */
void		initGlobals(void);
Any		findGlobal(Name name);

/* goodies.c */
int		isqrt(long a);
int		distance(int x1, int y1, int x2, int y2);
int		rfloat(float f);
char *		strcpyskip(char *t, char *f);
status		substr(register char *str, register char *sb);
status		prefixstr(char *s1, char *s2);
status		substr_ignore_case(register char *str, register char *sb);
status		prefixstr_ignore_case(char *s1, char *s2);
status		streq_ignore_case(char *s1, char *s2);
char *		upcasestr(char *s);
char *		downcasestr(char *s);
void		checkSummaryCharp(Name classname, Name name, char *s);
Name		characterName(Any chr);
status		writef_arguments(char *fm, va_list args, int *argc, Any *argv);
void		writef(char *fm, ...);
status		swritefv(char *buf, CharArray format, int argc, Any *argv);
status		str_writefv(String s, CharArray format, int argc, Any *argv);
Int		scanstr(char *str, char *fmt, Any *r);
status		sysPce(char *fm, ...);
void		msleep(int time);
void		msleep(int time);
int		getdtablesize(void);
int		pceAssert(int expr, char *text, char *file, int line);

/* passing.c */
status		sendImplementation(Any implementation, Any receiver, int argc, Any *argv);
Any		resolveSendMethodObject(Any obj, Class class, Name sel, Any *receiver, DelegateList dlist);
Any		resolveGetMethodObject(Any obj, Class class, Name sel, Any *receiver);
status		vm_send(Any receiver, Name selector, Class class, int argc, const Any argv []);
status		sendv(Any receiver, Name selector, int argc, Any *argv);
status		simpleSendv(Any receiver, Name selector, int argc, Any *argv);
status		qadSendv(Any r, Name selector, int ac, Any *av);
Any		vm_get(Any receiver, Name selector, Class class, int argc, const Any argv []);
Any		getv(Any receiver, Name selector, int argc, Any *argv);
Any		qadGetv(Any r, Name selector, int ac, Any *av);
status		errorTypeMismatch(Any rec, Any impl, int arg, Type type);
status		send(Any receiver, Name selector, ...);
Any		get(Any receiver, Name selector, ...);

/* method.c */
status		createMethod(Method m, Name name, Vector types, StringObj doc, Func action);
status		initialiseMethod(Method m, Name name, Vector types, Code msg, StringObj doc, SourceLocation loc, Name group);
Type		getArgumentTypeMethod(Method m, Int n);
Any		invokeMethod(Method m, Name c, Any receiver, int argc, const Any argv []);
Method		getInheritedFromMethod(Method m);
Method		getMethodFromFunction(Any f);
status		makeClassMethod(Class class);

/* name.c */
void		trapGdb(void);
void		initNamesPass1(void);
void		initNamesPass2(void);
void		checkNames(int prt);
status		initialiseName(Name n, CharArray value);
status		forNamePce(Pce pce, Code code);
Name		getCapitaliseName(Name n);
Name		getLabelNameName(Name n);
Name		getDeleteSuffixName(Name n, Name suffix);
Name		getExternalName(Name n);
Name		StringToName(String s);
Name		CtoName(const char *text);
Name		CtoKeyword(const char *s);
char *		saveStringName(Name n);
status		makeClassName(Class class);

/* object.c */
Any		allocObject(Class class, int funcs);
status		initialiseObject(Instance obj, int argc, const Any argv []);
Any		createObjectv(Name assoc, Class class, int argc, const Any argv []);
Any		newObjectv(Class class, int argc, const Any argv []);
Any		tempObjectv(Class class, int argc, const Any argv []);
status		considerPreserveObject(Any obj);
Any		answerObjectv(Class class, int argc, Any *argv);
Any		newObject(Class class, ...);
Any		tempObject(Class class, ...);
Any		globalObject(Name assoc, Class class, ...);
Any		answerObject(Class class, ...);
status		freeObject(Any obj);
status		createdObject(Any obj, Name how);
status		succeedObject(Any obj, ...);
status		failObject(Any obj, ...);
Any		getFailObject(Any obj);
status		virtualObject(Any obj);
Any		getVirtualObject(Any obj);
Int		getReferencesObject(Any obj);
Int		getCodeReferencesObject(Any obj);
Name		getFlagsObject(Any obj);
status		protectObject(Any obj);
status		doneObject(Any obj);
status		lockObject(Any obj, Bool val);
status		inspectObject(Any obj, Bool val);
Bool		getInspectObject(Any obj);
Name		getClassNameObject(Any obj);
Class		getClassObject(Any obj);
Any		getSelfObject(Any obj);
status		setSlotInstance(Any obj, Variable var, Any value);
status		nameReferenceObject(Any obj, Name name);
status		constraintObject(Any obj, Constraint c);
status		deleteConstraintObject(Any obj, Constraint c);
status		attachHyperObject(Any obj, Hyper h, Any to);
status		deleteHyperObject(Any obj, Hyper h);
status		attributeObject(Any obj, Any name, Any value);
status		deleteAttributeObject(Any obj, Any att);
Any		getAttributeObject(Any obj, Name name);
status		hasSendMethodObject(Any obj, Name selector);
status		hasGetMethodObject(Any obj, Name selector);
Chain		getAllConstraintsObject(Any obj, Bool create);
Chain		getAllHypersObject(Any obj, Bool create);
Chain		getAllAttributesObject(Any obj, Bool create);
Chain		getAllSendMethodsObject(Any obj, Bool create);
Chain		getAllGetMethodsObject(Any obj, Bool create);
Any		getCloneObject(Any obj);
status		clonePceSlots(Any org, Any Clone);
Any		getClone2Object(Any obj);
Int		getArityObject(Any obj);
Name		getFunctorObject(Any obj);
Any		getArgObject(Any obj, Int arg);
Any		getSlotObject(Any obj, Any which);
status		slotObject(Any obj, Any which, Any value);
status		equalObject(Any o1, Any o2);
status		sameReferenceObject(Any o1, Any o2);
status		sendSuperObject(Any obj, Name selector, int argc, const Any argv []);
Any		getGetSuperObject(Any obj, Name selector, int argc, const Any argv []);
Any		getFindHyperObject(Any obj, Name hname, Code cond);
Any		getHyperedObject(Any obj, Name hname, Code cond);
status		freeHypersObject(Any obj, Name hname, Code cond);
void		addRefObject(Any from, Any to);
void		delRefObject(Any from, Any to);
void		assignField(Instance instance, Any *field, Any value);
status		changedObject(Any obj, ...);
status		changedFieldObject(Any obj, Any *field);
Any		getResourceValueObject(Any obj, Name name);
status		obtainResourcesObject(Any obj);
status		convertLoadedObjectObject(Any obj, Int oldversion, Int currentversion);
Any		getConvertObject(Class class, Any x);
status		CheckObject(Any obj, Bool recursive);
status		errorObjectv(Any obj, Error e, int argc, Any *argv);
Name		getManIdObject(Any obj);
status		makeClassObject(Class class);

/* programobject.c */
status		initialiseProgramObject(Any obj);
status		initialiseNewSlotProgramObject(ProgramObject obj, Variable var);
ulong		nameToTraceFlag(Name name);
ulong		nameToBreakFlag(Name name);
status		systemProgramObject(ProgramObject obj, Bool val);
status		evalTraceConditionProgramObject(ProgramObject obj, Goal g);
status		evalBreakConditionProgramObject(ProgramObject obj, Goal g);
void		setDFlagProgramObject(Any obj, ulong mask);
void		clearDFlagProgramObject(Any obj, ulong mask);
status		makeClassProgramObject(Class class);

/* save.c */
Int		isSavedObject(Any obj);
status		saveInFileObject(Any obj, FileObj file);
status		storeObject(Any obj, FileObj file);
status		storeSlotsObject(Any obj, FileObj file);
long		loadWord(FILE *fd);
char *		loadCharp(FILE *fd);
void		restoreMessage(Any msg);
Any		getObjectFile(FileObj f);
Any		loadObject(FILE *fd);
status		loadSlotsObject(Any obj, FILE *fd, ClassDef def);

/* self.c */
status		formatPcev(Pce pce, CharArray fmt, int argc, Any *argv);
Name		getOsErrorPce(Pce pce);
status		catchedErrorPce(Pce pce, Name id);
Name		getEnvironmentVariablePce(Pce pce, Name name);
status		tracePce(Pce pce, Name val);
status		exceptionPcev(Pce pce, Name name, int argc, Any *argv);
status		exceptionPce(Pce pce, Name kind, ...);
int		getdtablesize(void);
char *		getlogin(void);
int		_dosemu_gethostname(char *buf, int len);
Name		getHostnamePce(Pce pce);
status		catchErrorSignalsPce(Pce pce, Bool val);
status		resetPce(Pce pce);
Any		getObjectFromReferencePce(Pce pce, Any ref);
status		makeClassPce(Class class);
status		pceReInitialise(int argc, char **argv);
status		pceInitialise(int handles, int argc, char **argv);

/* sendmethod.c */
SendMethod	createSendMethod(Name name, Vector types, StringObj doc, SendFunc action);
status		sendSendMethod(SendMethod m, Any receiver, int argc, const Any argv []);
status		makeClassSendMethod(Class class);

/* sourcelocation.c */
status		initialiseSourceLocation(SourceLocation loc, Name file, Int line);
status		makeClassSourceLocation(Class class);

/* timer.c */
status		intervalTimer(Timer tm, Real interval);
status		executeTimer(Timer tm);
status		statusTimer(Timer tm, Name stat);
status		delayTimer(Timer tm);
status		startTimer(Timer tm, Name mode);
status		stopTimer(Timer tm);
status		makeClassTimer(Class class);

/* trace.c */
void		resetDebugger(void);
void		initDebugger(void);
status		parentGoal(ProgramObject obj, Any rec, Name sel);
void		doTraceEnter(Goal g);
void		doTraceReturn(Goal g, status rval);
void		doTraceAnswer(Goal g, Any rval);
void		writeGoal(Goal g, Name port);
void		traceBackPce(Int depth, Name mode);
int		getModeGoal(Any obj);

/* type.c */
status		initialiseType(Type t, Name name, Name kind, Any context, Chain supers);
Type		getLookupType(Class class, Name name);
Type		createType(Name name, Name kind, Any context);
Name		getNameType(Type t);
void		superType(Type t, Type t2);
status		vectorType(Type t, Bool val);
status		mayBeDefaultType(Type t);
status		isClassType(Type t);
status		specialisedType(Type t1, Type t2);
status		includesType(Type t1, Type t2);
Chain		getValueSetType(Type t, Any ctx);
status		validateType(Type t, Any val, Any ctx);
Any		checkType(Any val, Type t, Any ctx);
status		makeClassType(Class class);
Type		nameToType(Name name);
Type		CtoType(char *s);
void		resetTypes(void);
void		initTypes(void);
Type		defineType(char *name, char *def);

/* variable.c */
Variable	createVariable(Name name, Type type, Name access);
status		cloneStyleVariable(Variable var, Name style);
status		saveStyleVariable(Variable var, Name style);
status		sendAccessVariable(Variable var);
status		getAccessVariable(Variable var);
status		allocValueVariable(Variable var, Any value);
Any		getAllocValueVariable(Variable var);
status		initFunctionVariable(Variable var, Any f);
status		initialValueVariable(Variable var, Any value);
status		sendVariable(Variable var, Any rec, int argc, const Any argv []);
Any		getGetVariable(Variable var, Any rec, int argc, const Any argv []);
Name		getGroupVariable(Variable v);
Name		getAccessArrowVariable(Variable v);
status		makeClassVariable(Class class);
status		makeClassDelegateVariable(Class class);

/* vmi.c */
status		initialiseVmi(Vmi vmi, Name name);
status		makeClassVmi(Class class);

/* xref.c */
WsRef		getXrefObject(Any obj, DisplayObj d);
WsRef		getExistingXrefObject(Any obj, DisplayObj d);
status		registerXrefObject(Any obj, DisplayObj d, WsRef xref);
Xref		unregisterXrefObject(Any obj, DisplayObj d);

/* error.c */
Error		getConvertError(Class class, Name id);
status		makeClassError(Class class);
status		errorPce(Any obj, Name id, ...);
