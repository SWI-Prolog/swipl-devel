
/* ../src/itf/c.c */
status		makeClassC(Class class);
void		XPCE_initialise(void);
XPCE_Object	XPCE_to_string(char *text);
XPCE_Object	XPCE_to_tmp_char_array(char *text);
void		XPCE_done_tmp_char_array(XPCE_Object ca);
XPCE_Object	XPCE_to_name(char *text);
XPCE_Object	XPCE_to_integer(long value);
XPCE_Object	XPCE_to_real(float value);
XPCE_Object	XPCE_to_pointer(void *ptr);
XPCE_Object	XPCE_to_object(XPCE_Object name);
XPCE_Object	XPCE_to_class(XPCE_Object name);
char *		XPCE_charp_of(XPCE_Object string);
long		XPCE_int_of(XPCE_Object integer);
float		XPCE_float_of(XPCE_Object real);
void *		XPCE_pointer_of(XPCE_Object cptr);
XPCE_status	XPCE_sendv(XPCE_Object receiver, XPCE_Object selector, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_getv(XPCE_Object receiver, XPCE_Object selector, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_newv(XPCE_Object class, const XPCE_Object name, int argc, const XPCE_Object argv []);
XPCE_status	XPCE_free(XPCE_Object object);
XPCE_status	XPCE_send(XPCE_Object receiver, XPCE_Object selector, ...);
XPCE_Object	XPCE_get(XPCE_Object receiver, XPCE_Object selector, ...);
XPCE_Object	XPCE_new(XPCE_Object class, const XPCE_Object name, ...);
XPCE_Object	XPCE_CHost(void);
XPCE_Object	XPCE_callv(XPCE_Procedure function, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_funcallv(XPCE_Function function, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_call(XPCE_Procedure function, ...);
XPCE_Object	XPCE_funcall(XPCE_Function function, ...);
XPCE_Object	XPCE_defclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary, XPCE_Procedure makefunc);
XPCE_Object	XPCE_defcxxclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary, XPCE_Procedure makefunc);
XPCE_Object	XPCE_makeclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary);
XPCE_Variable	XPCE_defvar(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Object type, XPCE_Object access, XPCE_Object initial);
XPCE_status	XPCE_defsendmethodv(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Procedure implementation, int argc, const XPCE_Object types []);
XPCE_status	XPCE_defgetmethodv(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Object return_type, XPCE_Function implementation, int argc, const XPCE_Object types []);
XPCE_status	XPCE_store(XPCE_Object in, XPCE_Variable var, XPCE_Object value);
XPCE_Object	XPCE_fetch(XPCE_Object in, XPCE_Variable var);
XPCE_Object	XPCE_chain_head(XPCE_Object chain);
XPCE_Object	XPCE_next_cell(XPCE_Object cell);
XPCE_Object	XPCE_cell_value(XPCE_Object cell);
void		initCGlobals(void);
XPCE_Object	XPCE_callCPlusPlusMethodv(XPCE_Procedure function, void *obj, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_funcallCPlusPlusMethodv(XPCE_Function function, void *obj, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_callCPlusPlusv(XPCE_Procedure f, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_funcallCPlusPlusv(XPCE_Function f, int argc, const XPCE_Object argv []);

/* ../src/itf/host.c */
status		initialiseHost(Host h, Name which);
status		makeClassHost(Class class);
Host		HostObject(void);

/* ../src/itf/interface.c */
Any		cToPceInteger(long int i);
Any		cToPceReal(double f);
Any		cToPceString(Name assoc, char *s, int translate);
Any		cToPceName(const char *text);
Any		cToPcePointer(void *ptr);
void *		pcePointerToC(PceObject obj);
Any		cToPceAssoc(const char *s);
PceObject	pceObjectFromName(PceName name);
Any		cToPceReference(ulong val);
int		pceExistsReference(ulong ref);
char *		pcePPReference(PceObject ref);
int		pceExistsAssoc(PceName assoc);
PceObject	cToPceTmpCharArray(const char *s);
void		donePceTmpCharArray(Any ca);
export void	_markAnswerStack(AnswerMark *mark);
status		pceInstanceOf(Any obj, Any classspec);
PceClass	nameToExistingClass(PceName Name);
PceClass	pceClassOfObject(PceObject obj);
int		pceReferencesOfObject(PceObject obj);
int		pceFreeObject(PceObject obj);
void		pceSendMethod(PceClass class, const char *name, const char *group, int argc, ...);
void		pceGetMethod(PceClass class, const char *name, const char *group, const char *rtype, int argc, ...);
int		pceToCReference(Any obj, PceCValue *rval);
int		pceToC(Any obj, PceCValue *rval);
char *		pceStringToC(Any val);
char *		pceCharArrayToC(Any val);
int		pceObject(Any obj);
int		pceGetMethodInfo(PceMethod m, pce_method_info *info);
PceITFSymbol	getITFSymbolName(Name name);
PceITFSymbol	pceLookupHandle(int n, hostHandle handle);
void		pceRegisterName(int n, hostHandle handle, Name name);
void		pceRegisterAssoc(int n, hostHandle handle, Any obj);
Any		pceNew(Name assoc, Any class, int argc, Any *argv);
status		pceSend(Any receiver, Name classname, Name selector, int argc, Any *argv);
Any		pceGet(Any receiver, Name classname, Name selector, int argc, Any *argv);
int		pceDispatch(int fd, int time);
void		pceRedraw(int sync);
int		pceExecuteMode(void);
void		pceReset(void);
void		pceWriteCurrentGoal(void);
void		pceWriteErrorGoal(void);
void		pceRegisterCallbacks(pce_callback_functions *fs);
int		hostSend(PceObject host, PceName selector, int argc, PceObject argv []);
PceObject	hostGet(PceObject host, PceName selector, int argc, PceObject argv []);
int		hostQuery(int what, PceCValue *value);
int		hostAction(int what, ...);
void		Cprintf(const char *fmt, ...);
void		Cvprintf(const char *fmt, va_list args);
int		Cputchar(int chr);
void		Cflush(void);
char *		Cgetline(char *line, int size);
void *		pceMalloc(int size);
void *		pceRealloc(void *ptr, int size);
void		pceFree(void *ptr);
void *		pceAlloc(int bytes);
void		pceUnAlloc(int bytes, void *p);
int		pceEnumElements(PceObject collection, int (*enumfunc)(PceObject,void *), void *closure);

/* ../src/itf/cpointer.c */
CPointer	CtoCPointer(void *ptr);
status		makeClassCPointer(Class class);

/* ../src/itf/asfile.c */
int		pceOpen(Any obj, int flags);
int		pceClose(int handle);
int		pceWrite(int handle, const char *buf, int size);
long		pceSeek(int handle, long offset, int whence);
int		pceRead(int handle, char *buf, int size);
const char *	pceOsError(void);

/* ../src/itf/console.c */
void		Stub__vCprintf(const char *fmt, va_list args);
int		Stub__Cputchar(int chr);
char *		Stub__Cgetline(char *line, int size);
void		Stub__Cflush(void);

/* ../src/itf/stub.c */
int		Stub__HostActionv(int action, va_list args);
int		Stub__HostQuery(int what, PceCValue *value);
int		Stub__HostSend(PceObject prolog, PceName sel, int argc, PceObject *argv);
PceObject	Stub__HostGet(PceObject prolog, PceName sel, int argc, PceObject *argv);
int		Stub__HostCall(PceGoal goal);

/* ../src/itf/xmalloc.c */
void *		xmalloc(size_t nbytes);
void *		xrealloc(void *ptr, size_t nbytes);

/* ../src/itf/iostream.c */
IOSTREAM *	Sopen_FILE(FILE *fd, int flags);
IOSTREAM *	Sopen_object(Any obj, const char *mode);

/* ../src/itf/srcsink.c */
status		initialiseSourceSink(SourceSink ss);
status		checkErrorSourceSink(SourceSink ss, IOSTREAM *fd);
status		makeClassSourceSink(Class class);

/* ../src/itf/rc.c */
status		makeClassRC(Class class);

/* ../src/itf/hostdata.c */
HostData	CtoHostData(Class class, void *h, int flags);
void		setHostDataHandle(HostData hd, void *h);
void *		getHostDataHandle(HostData hd);
int		freeHostData(HostData hd);
status		makeClassHostData(Class class);

/* ../src/itf/cpp.cxx */
PceStatus	callCPlusPlusProc(Any f, int ac, const Any av []);
Any		callCPlusPlusFunc(Any f, int ac, const Any av []);
PceStatus	callCPlusPlusMethodProc(Any obj, Any f, int ac, const Any av []);
Any		callCPlusPlusMethodFunc(Any obj, Any f, int ac, const Any av []);
PceStatus	callCPlusPlusPceMethodProc(Any obj, Any f, int ac, const Any av []);
Any		callCPlusPlusPceMethodFunc(Any obj, Any f, int ac, const Any av []);
