#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/itf/c.c */
COMMON(status)	makeClassC(Class class);
COMMON(void)	XPCE_initialise(void);
COMMON(XPCE_Object) XPCE_to_string(char *text);
COMMON(XPCE_Object) XPCE_to_tmp_char_array(char *text);
COMMON(void)	XPCE_done_tmp_char_array(XPCE_Object ca);
COMMON(XPCE_Object) XPCE_to_name(char *text);
COMMON(XPCE_Object) XPCE_to_integer(long value);
COMMON(XPCE_Object) XPCE_to_real(float value);
COMMON(XPCE_Object) XPCE_to_pointer(void *ptr);
COMMON(XPCE_Object) XPCE_to_object(XPCE_Object name);
COMMON(XPCE_Object) XPCE_to_class(XPCE_Object name);
COMMON(char *)	XPCE_charp_of(XPCE_Object string);
COMMON(long)	XPCE_int_of(XPCE_Object integer);
COMMON(float)	XPCE_float_of(XPCE_Object real);
COMMON(void *)	XPCE_pointer_of(XPCE_Object cptr);
COMMON(XPCE_status) XPCE_sendv(XPCE_Object receiver, XPCE_Object selector, int argc, const XPCE_Object argv []);
COMMON(XPCE_Object) XPCE_getv(XPCE_Object receiver, XPCE_Object selector, int argc, const XPCE_Object argv []);
COMMON(XPCE_Object) XPCE_newv(XPCE_Object class, const XPCE_Object name, int argc, const XPCE_Object argv []);
COMMON(XPCE_status) XPCE_free(XPCE_Object object);
COMMON(XPCE_status) XPCE_send(XPCE_Object receiver, XPCE_Object selector, ...);
COMMON(XPCE_Object) XPCE_get(XPCE_Object receiver, XPCE_Object selector, ...);
COMMON(XPCE_Object) XPCE_new(XPCE_Object class, const XPCE_Object name, ...);
COMMON(XPCE_Object) XPCE_CHost(void);
COMMON(XPCE_Object) XPCE_callv(XPCE_Procedure function, int argc, const XPCE_Object argv []);
COMMON(XPCE_Object) XPCE_funcallv(XPCE_Function function, int argc, const XPCE_Object argv []);
COMMON(XPCE_Object) XPCE_call(XPCE_Procedure function, ...);
COMMON(XPCE_Object) XPCE_funcall(XPCE_Function function, ...);
COMMON(XPCE_Object) XPCE_defclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary, XPCE_Procedure makefunc);
COMMON(XPCE_Object) XPCE_defcxxclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary, XPCE_Procedure makefunc);
COMMON(XPCE_Object) XPCE_makeclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary);
COMMON(XPCE_Variable) XPCE_defvar(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Object type, XPCE_Object access, XPCE_Object initial);
COMMON(XPCE_status) XPCE_defsendmethodv(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Procedure implementation, int argc, const XPCE_Object types []);
COMMON(XPCE_status) XPCE_defgetmethodv(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Object return_type, XPCE_Function implementation, int argc, const XPCE_Object types []);
COMMON(XPCE_status) XPCE_store(XPCE_Object in, XPCE_Variable var, XPCE_Object value);
COMMON(XPCE_Object) XPCE_fetch(XPCE_Object in, XPCE_Variable var);
COMMON(XPCE_Object) XPCE_chain_head(XPCE_Object chain);
COMMON(XPCE_Object) XPCE_next_cell(XPCE_Object cell);
COMMON(XPCE_Object) XPCE_cell_value(XPCE_Object cell);
COMMON(void)	initCGlobals(void);
COMMON(XPCE_Object) XPCE_callCPlusPlusMethodv(XPCE_Procedure function, void *obj, int argc, const XPCE_Object argv []);
COMMON(XPCE_Object) XPCE_funcallCPlusPlusMethodv(XPCE_Function function, void *obj, int argc, const XPCE_Object argv []);
COMMON(XPCE_Object) XPCE_callCPlusPlusv(XPCE_Procedure f, int argc, const XPCE_Object argv []);
COMMON(XPCE_Object) XPCE_funcallCPlusPlusv(XPCE_Function f, int argc, const XPCE_Object argv []);

/* /staff/jan/src/pl/packages/xpce/src/itf/host.c */
COMMON(status)	initialiseHost(Host h, Name which);
COMMON(status)	makeClassHost(Class class);
COMMON(Host)	HostObject(void);

/* /staff/jan/src/pl/packages/xpce/src/itf/interface.c */
COMMON(Any)	cToPceInteger(long int i);
COMMON(Any)	cToPceReal(double f);
COMMON(Any)	cToPceString(Name assoc, const char *s, unsigned int len, int translate);
COMMON(Any)	cToPceName(const char *text);
COMMON(Any)	cToPceName_n(const char *text, unsigned int len);
COMMON(Any)	cToPcePointer(void *ptr);
COMMON(void *)	pcePointerToC(PceObject obj);
COMMON(Any)	cToPceAssoc(const char *s);
COMMON(PceObject) pceObjectFromName(PceName name);
COMMON(Any)	cToPceReference(unsigned long val);
COMMON(int)	pceExistsReference(unsigned long ref);
COMMON(char *)	pcePPReference(PceObject ref);
COMMON(int)	pceExistsAssoc(PceName assoc);
COMMON(PceObject) cToPceTmpCharArray(const char *s);
COMMON(void)	donePceTmpCharArray(Any ca);
COMMON(export void) _markAnswerStack(AnswerMark *mark);
COMMON(status)	pceInstanceOf(Any obj, Any classspec);
COMMON(PceClass) nameToExistingClass(PceName Name);
COMMON(PceClass) pceClassOfObject(PceObject obj);
COMMON(int)	pceReferencesOfObject(PceObject obj);
COMMON(int)	pceFreeObject(PceObject obj);
COMMON(void)	pceSendMethod(PceClass class, const char *name, const char *group, int argc, ...);
COMMON(void)	pceGetMethod(PceClass class, const char *name, const char *group, const char *rtype, int argc, ...);
COMMON(int)	pceToCReference(Any obj, PceCValue *rval);
COMMON(int)	pceToC(Any obj, PceCValue *rval);
COMMON(char *)	pceStringToC(Any val);
COMMON(char *)	pceCharArrayToC(Any val, unsigned int *len);
COMMON(int)	pceObject(Any obj);
COMMON(int)	pceGetMethodInfo(PceMethod m, pce_method_info *info);
COMMON(PceITFSymbol) getITFSymbolName(Name name);
COMMON(PceITFSymbol) pceLookupHandle(int n, hostHandle handle);
COMMON(void)	pceRegisterName(int n, hostHandle handle, Name name);
COMMON(void)	pceRegisterAssoc(int n, hostHandle handle, Any obj);
COMMON(Any)	pceNew(Name assoc, Any class, int argc, Any *argv);
COMMON(status)	pceSend(Any receiver, Name classname, Name selector, int argc, Any *argv);
COMMON(Any)	pceGet(Any receiver, Name classname, Name selector, int argc, Any *argv);
COMMON(int)	pceDispatch(int fd, int time);
COMMON(void)	pceRedraw(int sync);
COMMON(int)	pceExecuteMode(void);
COMMON(void)	pceReset(void);
COMMON(void)	pceWriteCurrentGoal(void);
COMMON(void)	pceWriteErrorGoal(void);
COMMON(void)	pceRegisterCallbacks(pce_callback_functions *fs);
COMMON(int)	hostSend(PceObject host, PceName selector, int argc, PceObject argv []);
COMMON(PceObject) hostGet(PceObject host, PceName selector, int argc, PceObject argv []);
COMMON(int)	hostQuery(int what, PceCValue *value);
COMMON(int)	hostAction(int what, ...);
COMMON(void)	Cprintf(const char *fmt, ...);
COMMON(void)	Cvprintf(const char *fmt, va_list args);
COMMON(int)	Cputchar(int chr);
COMMON(void)	Cflush(void);
COMMON(char *)	Cgetline(char *line, int size);
COMMON(int)	pceSetProfileHooks(pce_profile_hooks *hooks);
COMMON(void *)	pceMalloc(int size);
COMMON(void *)	pceRealloc(void *ptr, int size);
COMMON(void)	pceFree(void *ptr);
COMMON(void *)	pceAlloc(int bytes);
COMMON(void)	pceUnAlloc(int bytes, void *p);
COMMON(int)	pceEnumElements(PceObject collection, int (*enumfunc)(PceObject,void *), void *closure);

/* /staff/jan/src/pl/packages/xpce/src/itf/cpointer.c */
COMMON(CPointer) CtoCPointer(void *ptr);
COMMON(status)	makeClassCPointer(Class class);

/* /staff/jan/src/pl/packages/xpce/src/itf/asfile.c */
COMMON(int)	pceOpen(Any obj, int flags);
COMMON(int)	pceClose(int handle);
COMMON(int)	pceWrite(int handle, const char *buf, int size);
COMMON(long)	pceSeek(int handle, long offset, int whence);
COMMON(int)	pceRead(int handle, char *buf, int size);
COMMON(const char *) pceOsError(void);

/* /staff/jan/src/pl/packages/xpce/src/itf/console.c */
COMMON(void)	Stub__vCprintf(const char *fmt, va_list args);
COMMON(int)	Stub__Cputchar(int chr);
COMMON(char *)	Stub__Cgetline(char *line, int size);
COMMON(void)	Stub__Cflush(void);

/* /staff/jan/src/pl/packages/xpce/src/itf/stub.c */
COMMON(int)	Stub__HostActionv(int action, va_list args);
COMMON(int)	Stub__HostQuery(int what, PceCValue *value);
COMMON(int)	Stub__HostSend(PceObject prolog, PceName sel, int argc, PceObject *argv);
COMMON(PceObject) Stub__HostGet(PceObject prolog, PceName sel, int argc, PceObject *argv);
COMMON(int)	Stub__HostCall(PceGoal goal);

/* /staff/jan/src/pl/packages/xpce/src/itf/xmalloc.c */
COMMON(void *)	xmalloc(size_t nbytes);
COMMON(void *)	xrealloc(void *ptr, size_t nbytes);

/* /staff/jan/src/pl/packages/xpce/src/itf/iostream.c */
COMMON(IOSTREAM *) Sopen_FILE(FILE *fd, int flags);
COMMON(IOSTREAM *) Sopen_object(Any obj, const char *mode);

/* /staff/jan/src/pl/packages/xpce/src/itf/srcsink.c */
COMMON(status)	initialiseSourceSink(SourceSink ss);
COMMON(status)	checkErrorSourceSink(SourceSink ss, IOSTREAM *fd);
COMMON(status)	makeClassSourceSink(Class class);

/* /staff/jan/src/pl/packages/xpce/src/itf/rc.c */
COMMON(status)	makeClassRC(Class class);

/* /staff/jan/src/pl/packages/xpce/src/itf/hostdata.c */
COMMON(HostData) CtoHostData(Class class, void *h, int flags);
COMMON(void)	setHostDataHandle(HostData hd, void *h);
COMMON(void *)	getHostDataHandle(HostData hd);
COMMON(int)	freeHostData(HostData hd);
COMMON(void)	makeAnyHostData(HostData hd);
COMMON(status)	makeClassHostData(Class class);
