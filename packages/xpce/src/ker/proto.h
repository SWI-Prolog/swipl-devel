
/* ../src/ker/alloc.c */
Any		alloc(int n);
void		unalloc(int n, Any p);
void		initAlloc(void);
void		allocRange(void *low, int size);
void		checkFreeChains(void);
status		listWastedCorePce(Pce pce, Bool ppcells);
char *		save_string(const char *s);
void		free_string(char *s);

/* ../src/ker/assoc.c */
PceITFSymbol	newSymbol(Any obj, Name name);
void		deleteAssoc(Any obj);
void		newAssoc(Name name, Any obj);
Any		getObjectAssoc(Name name);
Name		getNameAssoc(Any obj);
status		renameAssoc(Name old, Name new);
status		forSomeAssoc(Code code);
void		initAssoc(int handles);

/* ../src/ker/behaviour.c */
status		initialiseBehaviour(Behaviour b, Name name, Any ctx);
status		makeClassBehaviour(Class class);

/* ../src/ker/class.c */
Class		typeClass(Name name);
Class		nameToTypeClass(Name name);
Class		defineClass(Name name, Name super, StringObj summary, SendFunc makefunction);
status		defineClasses(struct class_definition *classes);
status		realiseClass(Class class);
void		bindNewMethodsClass(Class class);
status		realiseBootClass(Class class);
Class		bootClass(Name name, Name super_name, int size, int slots, SendFunc newF, int argc, ...);
void		lookupBootClass(Class class, Func f, int argc, ...);
Class		getConvertClass(Class class_class, Any obj);
status		initClass(Class class);
status		prepareClass(Class class);
status		instanceVariableClass(Class class, Variable var);
void		fixSendFunctionClass(Class class, Name selector);
void		fixGetFunctionClass(Class class, Name selector);
status		sendMethodClass(Class class, SendMethod m);
status		getMethodClass(Class class, GetMethod m);
status		setChangedFunctionClass(Class class, SendFunc func);
status		setInEventAreaFunctionClass(Class class, SendFunc func);
status		isPceSlot(Class class, int n);
status		allPceSlotsClass(Class class);
void		termClass(Class class, char *name, int argc, ...);
status		sendMethodv(Class class, Name name, Name group, int argc, va_list args);
status		sendMethod(Class class, Name name, Name group, int argc, ...);
status		storeMethod(Class class, Name name, SendFunc function);
status		getMethodv(Class class, Name name, Name group, const char *rtype, int argc, va_list args);
status		getMethod(Class class, Name name, Name group, const char *rtype, int argc, ...);
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
status		declareClass(Class class, const classdecl *decls);
status		delegateClass(Class class, Name name);
status		prependDelegateClass(Class class, Name name);
Variable	getInstanceVariableClass(Class class, Any which);
void		resetMessageResolve(void);
Any		getResolveSendMethodClass(Class class, Name name);
Any		getResolveGetMethodClass(Class class, Name name);
Int		getNoCreatedClass(Class class, Bool subtoo);
Int		getNoFreedClass(Class class, Bool subtoo);
status		createdClass(Class class, Any instance, Name how);
status		freedClass(Class class, Any instance);
status		isAClass(Class class, Class super);
int		numberTreeClass(Class class, int n);
status		featureClass(Class class, Name name, Any value);
Any		getFeatureClass(Class class, Name name);
Chain		getSendMethodsClass(Class class);
status		makeClassClass(Class class);

/* ../src/ker/conversion.c */
status		toString(Any obj, String s);
char *		toCharp(Any obj);
Int		toInteger(Any obj);
Real		toReal(Any obj);
Bool		toBool(Any obj);
Name		toName(Any obj);
Type		toType(Any obj);
char *		pcePP(Any obj);
char *		pcePP(Any obj);
Any		expandFunction(Any obj);

/* ../src/ker/debug.c */
void		catchErrorSignals(Bool yes);
status		confirmTerminal(char *question, char *def);

/* ../src/ker/declarations.c */
void		initClassDefs(void);
void		initTypeAliases(void);

/* ../src/ker/error.c */
Error		getConvertError(Class class, Name id);
status		makeClassError(Class class);
status		errorPce(Any obj, Name id, ...);
status		errorTypeMismatch(Any rec, Any impl, int arg, Type type, Any val);

/* ../src/ker/gc.c */
void		pushAnswerObject(Any obj);
void		deleteAnswerObject(Any obj);
export void	_rewindAnswerStack(AnswerMark *mark, Any obj);
void		initAnswerStack(void);
void		resetAnswerStack(void);
Int		countAnswerStack(void);

/* ../src/ker/getmethod.c */
GetMethod	createGetMethod(Name name, Type rtype, Vector types, StringObj doc, Func action);
status		initialiseGetMethod(GetMethod m, Name name, Type rtype, Vector types, Function msg, StringObj doc, SourceLocation loc, Name group);
status		makeClassGetMethod(Class class);

/* ../src/ker/glob.c */
int		IAmAGlobalFunctionToMakeMeLoad(void);

/* ../src/ker/global.c */
Any		findGlobal(Name name);

/* ../src/ker/goodies.c */
long		rdouble(double f);
int		isqrt(long a);
int		distance(int x1, int y1, int x2, int y2);
int		rfloat(double f);
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
char *		swritefv(char *buf, int *szp, CharArray format, int argc, const Any argv []);
status		str_writefv(String s, CharArray format, int argc, Any *argv);
Int		scanstr(char *str, char *fmt, Any *r);
status		sysPce(char *fm, ...);
void		initMClock(void);
unsigned long	mclock(void);
void		initMClock(void);
unsigned long	mclock(void);
void		msleep(int time);
void		msleep(int time);
void		msleep(int time);
int		getdtablesize(void);
int		pceAssert(int expr, char *text, char *file, int line);
void		at_pce_exit(atexit_function f, int flags);
void		run_pce_exit_hooks(int rval);

/* ../src/ker/passing.c */
status		vm_send(Any receiver, Name selector, Class class, int argc, const Any argv []);
Any		vm_get(Any receiver, Name selector, Class class, int argc, const Any argv []);
status		sendSendMethod(SendMethod sm, Any receiver, int argc, const Any argv []);
Any		getGetGetMethod(GetMethod gm, Any receiver, int argc, const Any argv []);
status		sendv(Any receiver, Name selector, int argc, Any *argv);
status		qadSendv(Any r, Name selector, int ac, Any *av);
Any		getv(Any receiver, Name selector, int argc, Any *argv);
Any		qadGetv(Any r, Name selector, int ac, Any *av);
status		send(Any receiver, Name selector, ...);
Any		get(Any receiver, Name selector, ...);
Any		resolveSendMethodObject(Any obj, Class class, Name sel, Any *receiver);
Any		resolveGetMethodObject(Any obj, Class class, Name sel, Any *receiver);

/* ../src/ker/method.c */
status		createMethod(Method m, Name name, Vector types, StringObj doc, Func action);
status		initialiseMethod(Method m, Name name, Vector types, Code msg, StringObj doc, SourceLocation loc, Name group);
Type		getArgumentTypeMethod(Method m, Int n);
Method		getInheritedFromMethod(Method m);
Method		getMethodFromFunction(Any f);
status		makeClassMethod(Class class);

/* ../src/ker/name.c */
void		trapGdb(void);
void		initNamesPass1(void);
void		initNamesPass2(void);
void		checkNames(int prt);
status		initialiseName(Name n, CharArray value);
status		forNamePce(Pce pce, Code code);
Name		getCapitaliseName(Name n);
Name		GetLabelNameName(Name n);
Name		getDeleteSuffixName(Name n, Name suffix);
Name		StringToName(String s);
Name		CtoKeyword(const char *s);
char *		saveStringName(Name n);
status		makeClassName(Class class);

/* ../src/ker/object.c */
void		unreferencedObject(Any obj);
void		addRefObject(Any from, Any to);
void		delRefObject(Any from, Any to);
void		assignField(Instance instance, Any *field, Any value);
void		unallocInstanceProtoClass(Class class);
Any		allocObject(Class class, int funcs);
status		initialiseObject(Instance obj, int argc, const Any argv []);
Any		createObjectv(Name assoc, Class class, int argc, const Any argv []);
Any		newObjectv(Class class, int argc, const Any argv []);
status		considerPreserveObject(Any obj);
Any		answerObjectv(Class class, int argc, const Any *argv);
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
status		virtualObject1(Any obj, Any a1);
status		virtualObject2(Any obj, Any a1, Any a2);
Any		getVirtualObject(Any obj);
Any		getVirtualObject1(Any obj, Any a1);
Any		getVirtualObject2(Any obj, Any a1, Any a2);
Int		getReferencesObject(Any obj);
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
status		attributeObject(Any obj, Any name, Any value);
status		deleteAttributeObject(Any obj, Any att);
Any		getAttributeObject(Any obj, Name name);
status		updateConstraintsObject(Any obj);
Tuple		getSendMethodObject(Any obj, Name selector);
Tuple		getGetMethodObject(Any obj, Name selector);
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
Any		getFindHyperObject(Any obj, Name hname, Code cond);
Any		getHyperedObject(Any obj, Name hname, Code cond);
status		freeHypersObject(Any obj, Name hname, Code cond);
status		changedObject(Any obj, ...);
status		changedFieldObject(Any obj, Any *field);
Any		getClassVariableValueObject(Any obj, Name name);
status		obtainClassVariablesObject(Any obj);
status		convertLoadedObjectObject(Any obj, Int oldversion, Int currentversion);
Any		getConvertObject(Any ctx, Any x);
status		CheckObject(Any obj, Bool recursive);
status		errorObjectv(Any obj, Error e, int argc, Any *argv);
Name		getManIdObject(Any obj);
status		makeClassObject(Class class);

/* ../src/ker/programobject.c */
status		initialiseProgramObject(Any obj);
status		initialiseNewSlotProgramObject(ProgramObject obj, Variable var);
void		setDFlagProgramObject(Any obj, unsigned long mask);
void		clearDFlagProgramObject(Any obj, unsigned long mask);
status		makeClassProgramObject(Class class);

/* ../src/ker/save.c */
Int		isSavedObject(Any obj);
status		saveInFileObject(Any obj, FileObj file);
status		storeObject(Any obj, FileObj file);
status		storeSlotsObject(Any obj, FileObj file);
long		loadWord(IOSTREAM *fd);
char *		loadCharp(IOSTREAM *fd);
void		restoreMessage(Any msg);
status		checkObjectMagic(IOSTREAM *fd);
Any		getObjectSourceSink(SourceSink f);
Any		loadObject(IOSTREAM *fd);
status		loadSlotsObject(Any obj, IOSTREAM *fd, ClassDef def);

/* ../src/ker/self.c */
status		formatPcev(Pce pce, CharArray fmt, int argc, Any *argv);
Name		getOsErrorPce(Pce pce);
status		catchErrorPce(Pce pce, Any ids);
status		catchPopPce(Pce pce);
status		catchedErrorPce(Pce pce, Name id);
Name		getEnvironmentVariablePce(Pce pce, Name name);
status		debuggingPce(Pce pce, Bool val);
status		exceptionPcev(Pce pce, Name name, int argc, Any *argv);
status		exceptionPce(Pce pce, Name kind, ...);
int		getdtablesize(void);
int		getdtablesize(void);
Name		getHostnamePce(Pce pce);
status		catchErrorSignalsPce(Pce pce, Bool val);
status		resetPce(Pce pce);
Any		getObjectFromReferencePce(Pce pce, Any ref);
status		makeClassPce(Class class);
export status	pceInitialise(int handles, const char *home, int argc, char **argv);

/* ../src/ker/sendmethod.c */
SendMethod	createSendMethod(Name name, Vector types, StringObj doc, SendFunc action);
status		makeClassSendMethod(Class class);

/* ../src/ker/srclocation.c */
status		initialiseSourceLocation(SourceLocation loc, Name file, Int line);
status		makeClassSourceLocation(Class class);

/* ../src/ker/timer.c */
status		intervalTimer(Timer tm, Real interval);
status		executeTimer(Timer tm);
status		statusTimer(Timer tm, Name stat);
status		startTimer(Timer tm, Name mode);
status		stopTimer(Timer tm);
status		makeClassTimer(Class class);

/* ../src/ker/trace.c */
void		resetDebugger(void);
void		initDebugger(void);
void		writeErrorGoal(void);
int		pceDebugging(Name subject);

/* ../src/ker/type.c */
status		initialiseType(Type t, Name name, Name kind, Any context, Chain supers);
Type		getLookupType(Class class, Name name);
Type		createType(Name name, Name kind, Any context);
Name		getNameType(Type t);
void		superType(Type t, Type t2);
status		vectorType(Type t, Bool val);
status		isClassType(Type t);
status		specialisedType(Type t1, Type t2);
status		specialisedType(Type t1, Type t2);
status		equalType(Type t1, Type t2);
status		includesType(Type t1, Type t2);
Chain		getValueSetType(Type t, Any ctx);
Any		getTranslateType(Type t, Any val, Any ctx);
status		validateType(Type t, const Any val, const Any ctx);
status		makeClassType(Class class);
Type		nameToType(Name name);
void		resetTypes(void);
void		initTypes(void);
Type		defineType(char *name, char *def);

/* ../src/ker/variable.c */
Variable	createVariable(Name name, Type type, Name access);
status		cloneStyleVariable(Variable var, Name style);
status		saveStyleVariable(Variable var, Name style);
status		sendAccessVariable(Variable var);
status		getAccessVariable(Variable var);
status		initialValueVariable(Variable var, Any value);
status		sendVariable(Variable var, Any rec, Any val);
Any		getGetVariable(Variable var, Any rec);
Name		getGroupVariable(Variable v);
status		makeClassVariable(Class class);

/* ../src/ker/xref.c */
WsRef		getXrefObject(Any obj, DisplayObj d);
WsRef		getExistingXrefObject(Any obj, DisplayObj d);
status		registerXrefObject(Any obj, DisplayObj d, WsRef xref);
Xref		unregisterXrefObject(Any obj, DisplayObj d);
void		closeAllXrefs(void);

/* ../src/ker/classvar.c */
Any		getValueClassVariable(ClassVariable cv);
ClassVariable	getClassVariableClass(Class class, Name name);
status		classVariableValueClass(Class cl, Name name, Any val);
Any		getClassVariableValueClass(Class cl, Name name);
status		attach_class_variable(Class cl, Name name, const char *type, const char *def, const char *doc);
status		refine_class_variable(Class cl, const char *name_s, const char *def);
status		makeClassClassVariable(Class class);
status		loadDefaultsPce(Pce pce, SourceSink from);
