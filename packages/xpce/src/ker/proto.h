#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/ker/alloc.c */
COMMON(Any)	alloc(unsigned int n);
COMMON(void)	unalloc(unsigned int n, Any p);
COMMON(void)	initAlloc(void);
COMMON(void)	allocRange(void *low, int size);
COMMON(void)	checkFreeChains(void);
COMMON(status)	listWastedCorePce(Pce pce, Bool ppcells);
COMMON(char *)	save_string(const char *s);
COMMON(void)	free_string(char *s);

/* /staff/jan/src/pl/packages/xpce/src/ker/assoc.c */
COMMON(PceITFSymbol) newSymbol(Any obj, Name name);
COMMON(void)	deleteAssoc(Any obj);
COMMON(void)	newAssoc(Name name, Any obj);
COMMON(Any)	getObjectAssoc(Name name);
COMMON(Name)	getNameAssoc(Any obj);
COMMON(status)	renameAssoc(Name old, Name new);
COMMON(status)	forSomeAssoc(Code code);
COMMON(void)	initAssoc(int handles);

/* /staff/jan/src/pl/packages/xpce/src/ker/behaviour.c */
COMMON(status)	initialiseBehaviour(Behaviour b, Name name, Any ctx);
COMMON(status)	makeClassBehaviour(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/class.c */
COMMON(Class)	typeClass(Name name);
COMMON(Class)	nameToTypeClass(Name name);
COMMON(Class)	defineClass(Name name, Name super, StringObj summary, SendFunc makefunction);
COMMON(status)	defineClasses(struct class_definition *classes);
COMMON(status)	realiseClass(Class class);
COMMON(void)	bindNewMethodsClass(Class class);
COMMON(status)	realiseBootClass(Class class);
COMMON(Class)	bootClass(Name name, Name super_name, int size, int slots, SendFunc newF, int argc, ...);
COMMON(void)	lookupBootClass(Class class, Func f, int argc, ...);
COMMON(Class)	getConvertClass(Class class_class, Any obj);
COMMON(status)	initClass(Class class);
COMMON(status)	instanceVariableClass(Class class, Variable var);
COMMON(void)	fixSendFunctionClass(Class class, Name selector);
COMMON(void)	fixGetFunctionClass(Class class, Name selector);
COMMON(status)	sendMethodClass(Class class, SendMethod m);
COMMON(status)	getMethodClass(Class class, GetMethod m);
COMMON(status)	setChangedFunctionClass(Class class, SendFunc func);
COMMON(status)	setInEventAreaFunctionClass(Class class, SendFunc func);
COMMON(status)	isPceSlot(Class class, int n);
COMMON(status)	allPceSlotsClass(Class class);
COMMON(void)	termClass(Class class, char *name, int argc, ...);
COMMON(status)	sendMethodv(Class class, Name name, Name group, int argc, va_list args);
COMMON(status)	sendMethod(Class class, Name name, Name group, int argc, ...);
COMMON(status)	storeMethod(Class class, Name name, SendFunc function);
COMMON(status)	getMethodv(Class class, Name name, Name group, const char *rtype, int argc, va_list args);
COMMON(status)	getMethod(Class class, Name name, Name group, const char *rtype, int argc, ...);
COMMON(status)	cloneStyleClass(Class class, Name style);
COMMON(status)	cloneStyleVariableClass(Class class, Name slot, Name style);
COMMON(status)	saveStyleVariableClass(Class class, Name slot, Name style);
COMMON(status)	saveStyleClass(Class class, Name style);
COMMON(status)	setCloneFunctionClass(Class class, SendFunc function);
COMMON(status)	setRedrawFunctionClass(Class class, SendFunc function);
COMMON(status)	setLoadStoreFunctionClass(Class class, SendFunc load, SendFunc store);
COMMON(status)	solidClass(Class class, Bool val);
COMMON(status)	sourceClass(Class class, SendFunc f, char *file, char *rcs);
COMMON(void)	localClass(Class class, Name name, Name group, char *type, Name access, char *doc);
COMMON(status)	declareClass(Class class, const classdecl *decls);
COMMON(status)	delegateClass(Class class, Name name);
COMMON(status)	prependDelegateClass(Class class, Name name);
COMMON(Variable) getInstanceVariableClass(Class class, Any which);
COMMON(void)	resetMessageResolve(void);
COMMON(Any)	getResolveSendMethodClass(Class class, Name name);
COMMON(Any)	getResolveGetMethodClass(Class class, Name name);
COMMON(Int)	getNoCreatedClass(Class class, Bool subtoo);
COMMON(Int)	getNoFreedClass(Class class, Bool subtoo);
COMMON(status)	createdClass(Class class, Any instance, Name how);
COMMON(status)	freedClass(Class class, Any instance);
COMMON(status)	isAClass(Class class, Class super);
COMMON(int)	numberTreeClass(Class class, int n);
COMMON(status)	featureClass(Class class, Name name, Any value);
COMMON(Any)	getFeatureClass(Class class, Name name);
COMMON(Chain)	getSendMethodsClass(Class class);
COMMON(status)	makeClassClass(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/conversion.c */
COMMON(status)	toString(Any obj, String s);
COMMON(char *)	toCharp(Any obj);
COMMON(Int)	toInteger(Any obj);
COMMON(Real)	toReal(Any obj);
COMMON(Bool)	toBool(Any obj);
COMMON(Name)	toName(Any obj);
COMMON(Type)	toType(Any obj);
COMMON(char *)	pcePP(Any obj);
COMMON(char *)	pcePP(Any obj);
COMMON(Any)	expandFunction(Any obj);

/* /staff/jan/src/pl/packages/xpce/src/ker/debug.c */
COMMON(void)	catchErrorSignals(Bool yes);
COMMON(status)	confirmTerminal(char *question, char *def);

/* /staff/jan/src/pl/packages/xpce/src/ker/declarations.c */
COMMON(void)	initClassDefs(void);
COMMON(void)	initTypeAliases(void);

/* /staff/jan/src/pl/packages/xpce/src/ker/error.c */
COMMON(Error)	getConvertError(Class class, Name id);
COMMON(status)	makeClassError(Class class);
COMMON(status)	errorPce(Any obj, Name id, ...);
COMMON(status)	errorTypeMismatch(Any rec, Any impl, int arg, Type type, Any val);

/* /staff/jan/src/pl/packages/xpce/src/ker/gc.c */
COMMON(void)	pushAnswerObject(Any obj);
COMMON(void)	deleteAnswerObject(Any obj);
COMMON(export void) _rewindAnswerStack(AnswerMark *mark, Any obj);
COMMON(void)	initAnswerStack(void);
COMMON(void)	resetAnswerStack(void);
COMMON(Int)	countAnswerStack(void);

/* /staff/jan/src/pl/packages/xpce/src/ker/getmethod.c */
COMMON(GetMethod) createGetMethod(Name name, Type rtype, Vector types, StringObj doc, Func action);
COMMON(status)	initialiseGetMethod(GetMethod m, Name name, Type rtype, Vector types, Function msg, StringObj doc, SourceLocation loc, Name group);
COMMON(status)	makeClassGetMethod(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/glob.c */
COMMON(int)	IAmAGlobalFunctionToMakeMeLoad(void);

/* /staff/jan/src/pl/packages/xpce/src/ker/global.c */
COMMON(Any)	findGlobal(Name name);

/* /staff/jan/src/pl/packages/xpce/src/ker/goodies.c */
COMMON(long)	rdouble(double f);
COMMON(int)	isqrt(long a);
COMMON(int)	distance(int x1, int y1, int x2, int y2);
COMMON(int)	rfloat(double f);
COMMON(char *)	strcpyskip(char *t, char *f);
COMMON(status)	substr(register char *str, register char *sb);
COMMON(status)	prefixstr(char *s1, char *s2);
COMMON(status)	substr_ignore_case(register char *str, register char *sb);
COMMON(status)	prefixstr_ignore_case(char *s1, char *s2);
COMMON(status)	streq_ignore_case(char *s1, char *s2);
COMMON(char *)	upcasestr(char *s);
COMMON(char *)	downcasestr(char *s);
COMMON(void)	checkSummaryCharp(Name classname, Name name, char *s);
COMMON(Name)	characterName(Any chr);
COMMON(status)	writef_arguments(char *fm, va_list args, int *argc, Any *argv);
COMMON(void)	writef(char *fm, ...);
COMMON(char *)	swritefv(char *buf, int *szp, CharArray format, int argc, const Any argv []);
COMMON(status)	str_writefv(String s, CharArray format, int argc, Any *argv);
COMMON(Int)	scanstr(char *str, char *fmt, Any *r);
COMMON(status)	sysPce(char *fm, ...);
COMMON(void)	initMClock(void);
COMMON(unsigned long) mclock(void);
COMMON(void)	initMClock(void);
COMMON(unsigned long) mclock(void);
COMMON(void)	msleep(int time);
COMMON(void)	msleep(int time);
COMMON(void)	msleep(int time);
COMMON(int)	getdtablesize(void);
COMMON(int)	pceAssert(int expr, char *text, char *file, int line);
COMMON(void)	at_pce_exit(atexit_function f, int flags);
COMMON(void)	run_pce_exit_hooks(int rval);

/* /staff/jan/src/pl/packages/xpce/src/ker/passing.c */
COMMON(status)	vm_send(Any receiver, Name selector, Class class, int argc, const Any argv []);
COMMON(Any)	vm_get(Any receiver, Name selector, Class class, int argc, const Any argv []);
COMMON(status)	sendSendMethod(SendMethod sm, Any receiver, int argc, const Any argv []);
COMMON(Any)	getGetGetMethod(GetMethod gm, Any receiver, int argc, const Any argv []);
COMMON(status)	sendv(Any receiver, Name selector, int argc, Any *argv);
COMMON(status)	qadSendv(Any r, Name selector, int ac, Any *av);
COMMON(Any)	getv(Any receiver, Name selector, int argc, Any *argv);
COMMON(Any)	qadGetv(Any r, Name selector, int ac, Any *av);
COMMON(status)	send(Any receiver, Name selector, ...);
COMMON(Any)	get(Any receiver, Name selector, ...);
COMMON(Any)	resolveSendMethodObject(Any obj, Class class, Name sel, Any *receiver);
COMMON(Any)	resolveGetMethodObject(Any obj, Class class, Name sel, Any *receiver);

/* /staff/jan/src/pl/packages/xpce/src/ker/method.c */
COMMON(status)	createMethod(Method m, Name name, Vector types, StringObj doc, Func action);
COMMON(status)	initialiseMethod(Method m, Name name, Vector types, Code msg, StringObj doc, SourceLocation loc, Name group);
COMMON(Type)	getArgumentTypeMethod(Method m, Int n);
COMMON(Method)	getInheritedFromMethod(Method m);
COMMON(Method)	getMethodFromFunction(Any f);
COMMON(status)	makeClassMethod(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/name.c */
COMMON(void)	trapGdb(void);
COMMON(void)	initNamesPass1(void);
COMMON(void)	initNamesPass2(void);
COMMON(void)	checkNames(int prt);
COMMON(status)	initialiseName(Name n, CharArray value);
COMMON(status)	forNamePce(Pce pce, Code code);
COMMON(Name)	getCapitaliseName(Name n);
COMMON(Name)	GetLabelNameName(Name n);
COMMON(Name)	getDeleteSuffixName(Name n, Name suffix);
COMMON(Name)	StringToName(String s);
COMMON(Name)	CtoKeyword(const char *s);
COMMON(char *)	saveStringName(Name n);
COMMON(status)	makeClassName(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/object.c */
COMMON(void)	unreferencedObject(Any obj);
COMMON(void)	addRefObject(Any from, Any to);
COMMON(void)	delRefObject(Any from, Any to);
COMMON(void)	assignField(Instance instance, Any *field, Any value);
COMMON(void)	unallocInstanceProtoClass(Class class);
COMMON(Any)	allocObject(Class class, int funcs);
COMMON(status)	initialiseObject(Instance obj);
COMMON(Any)	createObjectv(Name assoc, Class class, int argc, const Any argv []);
COMMON(Any)	newObjectv(Class class, int argc, const Any argv []);
COMMON(status)	considerPreserveObject(Any obj);
COMMON(Any)	answerObjectv(Class class, int argc, const Any *argv);
COMMON(Any)	newObject(Class class, ...);
COMMON(Any)	tempObject(Class class, ...);
COMMON(Any)	globalObject(Name assoc, Class class, ...);
COMMON(Any)	answerObject(Class class, ...);
COMMON(status)	freeObject(Any obj);
COMMON(status)	createdObject(Any obj, Name how);
COMMON(status)	succeedObject(Any obj, ...);
COMMON(status)	failObject(Any obj, ...);
COMMON(Any)	getFailObject(Any obj);
COMMON(status)	virtualObject(Any obj);
COMMON(status)	virtualObject1(Any obj, Any a1);
COMMON(status)	virtualObject2(Any obj, Any a1, Any a2);
COMMON(Any)	getVirtualObject(Any obj);
COMMON(Any)	getVirtualObject1(Any obj, Any a1);
COMMON(Any)	getVirtualObject2(Any obj, Any a1, Any a2);
COMMON(Int)	getReferencesObject(Any obj);
COMMON(Name)	getFlagsObject(Any obj);
COMMON(status)	protectObject(Any obj);
COMMON(status)	doneObject(Any obj);
COMMON(status)	lockObject(Any obj, Bool val);
COMMON(status)	inspectObject(Any obj, Bool val);
COMMON(Bool)	getInspectObject(Any obj);
COMMON(Name)	getClassNameObject(Any obj);
COMMON(Class)	getClassObject(Any obj);
COMMON(Any)	getSelfObject(Any obj);
COMMON(status)	setSlotInstance(Any obj, Variable var, Any value);
COMMON(status)	nameReferenceObject(Any obj, Name name);
COMMON(status)	constraintObject(Any obj, Constraint c);
COMMON(status)	deleteConstraintObject(Any obj, Constraint c);
COMMON(status)	attachHyperObject(Any obj, Hyper h, Any to);
COMMON(status)	attributeObject(Any obj, Any name, Any value);
COMMON(status)	deleteAttributeObject(Any obj, Any att);
COMMON(Any)	getAttributeObject(Any obj, Name name);
COMMON(status)	updateConstraintsObject(Any obj);
COMMON(Tuple)	getSendMethodObject(Any obj, Name selector);
COMMON(Tuple)	getGetMethodObject(Any obj, Name selector);
COMMON(status)	hasSendMethodObject(Any obj, Name selector);
COMMON(status)	hasGetMethodObject(Any obj, Name selector);
COMMON(Any)	getCreateContextObject(Any obj, Code cond);
COMMON(Chain)	getAllConstraintsObject(Any obj, Bool create);
COMMON(Chain)	getAllHypersObject(Any obj, Bool create);
COMMON(Chain)	getAllAttributesObject(Any obj, Bool create);
COMMON(Chain)	getAllSendMethodsObject(Any obj, Bool create);
COMMON(Chain)	getAllGetMethodsObject(Any obj, Bool create);
COMMON(Any)	getCloneObject(Any obj);
COMMON(status)	clonePceSlots(Any org, Any Clone);
COMMON(Any)	getClone2Object(Any obj);
COMMON(Int)	getArityObject(Any obj);
COMMON(Name)	getFunctorObject(Any obj);
COMMON(Any)	getArgObject(Any obj, Int arg);
COMMON(Any)	getSlotObject(Any obj, Any which);
COMMON(status)	slotObject(Any obj, Any which, Any value);
COMMON(status)	equalObject(Any o1, Any o2);
COMMON(status)	sameReferenceObject(Any o1, Any o2);
COMMON(status)	sendSuperObject(Any obj, Name selector, int argc, const Any argv []);
COMMON(Any)	getFindHyperObject(Any obj, Name hname, Code cond);
COMMON(Any)	getHyperedObject(Any obj, Name hname, Code cond);
COMMON(status)	freeHypersObject(Any obj, Name hname, Code cond);
COMMON(status)	changedObject(Any obj, ...);
COMMON(status)	changedFieldObject(Any obj, Any *field);
COMMON(Any)	getClassVariableValueObject(Any obj, Name name);
COMMON(status)	obtainClassVariablesObject(Any obj);
COMMON(status)	convertLoadedObjectObject(Any obj, Int oldversion, Int currentversion);
COMMON(Any)	getConvertObject(Any ctx, Any x);
COMMON(status)	CheckObject(Any obj, Bool recursive);
COMMON(status)	errorObjectv(Any obj, Error e, int argc, Any *argv);
COMMON(Name)	getManIdObject(Any obj);
COMMON(status)	makeClassObject(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/programobject.c */
COMMON(status)	initialiseProgramObject(Any obj);
COMMON(status)	initialiseNewSlotProgramObject(ProgramObject obj, Variable var);
COMMON(void)	setDFlagProgramObject(Any obj, unsigned long mask);
COMMON(void)	clearDFlagProgramObject(Any obj, unsigned long mask);
COMMON(status)	makeClassProgramObject(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/save.c */
COMMON(Int)	isSavedObject(Any obj);
COMMON(status)	saveInFileObject(Any obj, FileObj file);
COMMON(status)	storeObject(Any obj, FileObj file);
COMMON(status)	storeSlotsObject(Any obj, FileObj file);
COMMON(long)	loadWord(IOSTREAM *fd);
COMMON(char *)	loadCharp(IOSTREAM *fd);
COMMON(void)	restoreMessage(Any msg);
COMMON(status)	checkObjectMagic(IOSTREAM *fd);
COMMON(Any)	getObjectSourceSink(SourceSink f);
COMMON(Any)	loadObject(IOSTREAM *fd);
COMMON(status)	loadSlotsObject(Any obj, IOSTREAM *fd, ClassDef def);

/* /staff/jan/src/pl/packages/xpce/src/ker/self.c */
COMMON(status)	formatPcev(Pce pce, CharArray fmt, int argc, Any *argv);
COMMON(Name)	getOsErrorPce(Pce pce);
COMMON(status)	catchErrorPce(Pce pce, Any ids);
COMMON(status)	catchPopPce(Pce pce);
COMMON(status)	catchedErrorPce(Pce pce, Name id);
COMMON(Name)	getEnvironmentVariablePce(Pce pce, Name name);
COMMON(status)	debuggingPce(Pce pce, Bool val);
COMMON(status)	exceptionPcev(Pce pce, Name name, int argc, Any *argv);
COMMON(status)	exceptionPce(Pce pce, Name kind, ...);
COMMON(int)	getdtablesize(void);
COMMON(int)	getdtablesize(void);
COMMON(Name)	getHostnamePce(Pce pce);
COMMON(status)	catchErrorSignalsPce(Pce pce, Bool val);
COMMON(status)	resetPce(Pce pce);
COMMON(Any)	getObjectFromReferencePce(Pce pce, Any ref);
COMMON(status)	makeClassPce(Class class);
COMMON(export status) pceInitialise(int handles, const char *home, int argc, char **argv);

/* /staff/jan/src/pl/packages/xpce/src/ker/sendmethod.c */
COMMON(SendMethod) createSendMethod(Name name, Vector types, StringObj doc, SendFunc action);
COMMON(status)	makeClassSendMethod(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/srclocation.c */
COMMON(status)	initialiseSourceLocation(SourceLocation loc, Name file, Int line);
COMMON(status)	makeClassSourceLocation(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/timer.c */
COMMON(status)	intervalTimer(Timer tm, Real interval);
COMMON(status)	executeTimer(Timer tm);
COMMON(status)	statusTimer(Timer tm, Name stat);
COMMON(status)	startTimer(Timer tm, Name mode);
COMMON(status)	stopTimer(Timer tm);
COMMON(status)	makeClassTimer(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/trace.c */
COMMON(void)	resetDebugger(void);
COMMON(void)	initDebugger(void);
COMMON(void)	writeErrorGoal(void);
COMMON(int)	pceDebugging(Name subject);

/* /staff/jan/src/pl/packages/xpce/src/ker/type.c */
COMMON(status)	initialiseType(Type t, Name name, Name kind, Any context, Chain supers);
COMMON(Type)	getLookupType(Class class, Name name);
COMMON(Type)	createType(Name name, Name kind, Any context);
COMMON(Name)	getNameType(Type t);
COMMON(void)	superType(Type t, Type t2);
COMMON(status)	vectorType(Type t, Bool val);
COMMON(status)	isClassType(Type t);
COMMON(status)	specialisedType(Type t1, Type t2);
COMMON(status)	specialisedType(Type t1, Type t2);
COMMON(status)	equalType(Type t1, Type t2);
COMMON(status)	includesType(Type t1, Type t2);
COMMON(Chain)	getValueSetType(Type t, Any ctx);
COMMON(Any)	getTranslateType(Type t, Any val, Any ctx);
COMMON(status)	validateType(Type t, const Any val, const Any ctx);
COMMON(status)	makeClassType(Class class);
COMMON(Type)	nameToType(Name name);
COMMON(void)	resetTypes(void);
COMMON(void)	initTypes(void);
COMMON(Type)	defineType(char *name, char *def);

/* /staff/jan/src/pl/packages/xpce/src/ker/variable.c */
COMMON(Variable) createVariable(Name name, Type type, Name access);
COMMON(status)	cloneStyleVariable(Variable var, Name style);
COMMON(status)	saveStyleVariable(Variable var, Name style);
COMMON(status)	sendAccessVariable(Variable var);
COMMON(status)	getAccessVariable(Variable var);
COMMON(status)	initialValueVariable(Variable var, Any value);
COMMON(status)	sendVariable(Variable var, Any rec, Any val);
COMMON(Any)	getGetVariable(Variable var, Any rec);
COMMON(Name)	getGroupVariable(Variable v);
COMMON(status)	makeClassVariable(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ker/xref.c */
COMMON(WsRef)	getXrefObject(Any obj, DisplayObj d);
COMMON(WsRef)	getExistingXrefObject(Any obj, DisplayObj d);
COMMON(status)	registerXrefObject(Any obj, DisplayObj d, WsRef xref);
COMMON(Xref)	unregisterXrefObject(Any obj, DisplayObj d);
COMMON(void)	closeAllXrefs(void);

/* /staff/jan/src/pl/packages/xpce/src/ker/classvar.c */
COMMON(Any)	getValueClassVariable(ClassVariable cv);
COMMON(ClassVariable) getClassVariableClass(Class class, Name name);
COMMON(status)	classVariableValueClass(Class cl, Name name, Any val);
COMMON(Any)	getClassVariableValueClass(Class cl, Name name);
COMMON(status)	attach_class_variable(Class cl, Name name, const char *type, const char *def, const char *doc);
COMMON(status)	refine_class_variable(Class cl, const char *name_s, const char *def);
COMMON(status)	makeClassClassVariable(Class class);
COMMON(status)	loadDefaultsPce(Pce pce, SourceSink from);
