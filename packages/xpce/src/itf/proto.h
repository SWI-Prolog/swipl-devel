
/* c.c */
status		makeClassC(Class class);
void		XPCE_initialise(void);
XPCE_Object	XPCE_to_string(char *text);
XPCE_Object	XPCE_to_tmp_char_array(char *text);
void		XPCE_done_tmp_char_array(XPCE_Object ca);
XPCE_Object	XPCE_to_name(char *text);
XPCE_Object	XPCE_to_integer(long value);
XPCE_Object	XPCE_to_real(float value);
XPCE_Object	XPCE_to_object(XPCE_Object name);
XPCE_Object	XPCE_to_class(XPCE_Object name);
char *		XPCE_charp_of(XPCE_Object string);
long		XPCE_int_of(XPCE_Object integer);
float		XPCE_float_of(XPCE_Object real);
XPCE_status	XPCE_sendv(XPCE_Object receiver, XPCE_Object selector, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_getv(XPCE_Object receiver, XPCE_Object selector, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_newv(XPCE_Object class, const XPCE_Object name, int argc, const XPCE_Object argv []);
XPCE_status	XPCE_free(XPCE_Object object);
XPCE_status	XPCE_send_superv(XPCE_Object receiver, XPCE_Object selector, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_get_superv(XPCE_Object receiver, XPCE_Object selector, int argc, const XPCE_Object argv []);
XPCE_status	XPCE_send(XPCE_Object receiver, XPCE_Object selector, ...);
XPCE_Object	XPCE_get(XPCE_Object receiver, XPCE_Object selector, ...);
XPCE_Object	XPCE_new(XPCE_Object class, const XPCE_Object name, ...);
XPCE_Object	XPCE_CHost(void);
XPCE_Object	XPCE_callv(XPCE_Procedure function, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_funcallv(XPCE_Function function, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_call(XPCE_Procedure function, ...);
XPCE_Object	XPCE_funcall(XPCE_Function function, ...);
XPCE_Object	XPCE_defclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary, XPCE_Procedure makefunc);
XPCE_Object	XPCE_makeclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary);
XPCE_Variable	XPCE_defvar(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Object type, XPCE_Object access, XPCE_Object initial);
XPCE_status	XPCE_defsendmethodv(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Procedure implementation, int argc, const XPCE_Object types []);
XPCE_status	XPCE_defgetmethodv(XPCE_Object class, XPCE_Object name, XPCE_Object group, XPCE_Object summary, XPCE_Object return_type, XPCE_Function implementation, int argc, const XPCE_Object types []);
XPCE_status	XPCE_store(XPCE_Object in, XPCE_Variable var, XPCE_Object value);
XPCE_Object	XPCE_fetch(XPCE_Object in, XPCE_Variable var);
XPCE_Object	XPCE_chain_head(XPCE_Object chain);
XPCE_Object	XPCE_next_cell(XPCE_Object cell);
XPCE_Object	XPCE_cell_value(XPCE_Object cell);
XPCE_Object	XPCE_callCPlusPlusMethodv(XPCE_Procedure function, void *obj, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_funcallCPlusPlusMethodv(XPCE_Function function, void *obj, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_callCPlusPlusv(XPCE_Procedure f, int argc, const XPCE_Object argv []);
XPCE_Object	XPCE_funcallCPlusPlusv(XPCE_Function f, int argc, const XPCE_Object argv []);

/* host.c */
status		initialiseHost(Host h, Name which);
status		makeClassHost(Class class);
Host		HostObject(void);

/* interface.c */
Any		cToPceInteger(long int i);
Any		cToPceReal(double f);
Any		cToPceString(char *assoc, char *s);
Any		cToPceName(char *s);
Any		cToPcePointer(void *ptr);
void *		pcePointerToC(PceObject obj);
Any		cToPceAssoc(char *s);
Any		cToPceReference(ulong val);
int		pceExistsReference(long int ref);
int		pceExistsAssoc(char *assoc);
PceObject	cToPceTmpCharArray(char *s);
void		donePceTmpCharArray(Any ca);
status		pceInstanceOf(Any obj, Any classspec);
int		pceToCReference(Any obj, PceCValue *rval);
int		pceToC(Any obj, PceCValue *rval);
char *		pceStringToC(Any val);
char *		pceCharArrayToC(Any val);
int		pceObject(Any obj);
PceITFSymbol	getITFSymbolName(Name name);
PceITFSymbol	pceLookupHandle(int n, hostHandle handle);
void		pceRegisterName(int n, hostHandle handle, Name name);
void		pceRegisterAssoc(int n, hostHandle handle, Any obj);
status		pceSend(Any receiver, Name selector, int argc, Any *argv);
Any		pceGet(Any receiver, Name selector, int argc, Any *argv);
Any		pceNew(char *assoc, Any class, int argc, Any *argv);
int		pceDispatch(int fd, int time);
void		pceRedraw(void);
char *		getHostSymbolTable(void);
void		pceReset(void);
void		pceTrace(int on);
void		pceTraceBack(int depth);
void		pceWriteCurrentGoal(void);
void		pceWriteErrorGoal(void);
void		pceRegisterCallbacks(pce_callback_functions *fs);
int		hostSend(PceObject host, PceName selector, int argc, PceObject argv []);
PceObject	hostGet(PceObject host, PceName selector, int argc, PceObject argv []);
int		hostCallProc(PceObject handle, PceObject receiver, int argc, PceObject argv []);
PceObject	hostCallFunc(PceObject handle, PceObject receiver, int argc, PceObject argv []);
int		hostQuery(int what, PceCValue *value);
int		hostAction(int what, ...);
void		Cprintf(const char *fmt, ...);
void		Cvprintf(const char *fmt, va_list args);
int		Cputchar(int chr);
char *		Cgetline(char *line, int size);

/* cpointer.c */
CPointer	CtoCPointer(void *ptr);
status		makeClassCPointer(Class class);

/* asfile.c */
int		pceOpen(Any obj, int flags);
int		pceClose(int handle);
int		pceWrite(int handle, const char *buf, int size);
long		pceSeek(int handle, long offset, int whence);
int		pceRead(int handle, char *buf, int size);
const char *	pceOsError(void);

/* console.c */
void		Stub__vCprintf(const char *fmt, va_list args);
int		Stub__Cputchar(int chr);
char *		Stub__Cgetline(char *line, int size);

/* stub.c */
void		on_exit(OnExitFunction f, char *s);
int		Stub__HostActionv(int action, va_list args);
int		Stub__HostQuery(int what, PceCValue *value);
int		Stub__HostSend(PceObject prolog, PceName sel, int argc, PceObject *argv);
PceObject	Stub__HostGet(PceObject prolog, PceName sel, int argc, PceObject *argv);
int		Stub__HostCallProc(PceObject handle, PceObject rec, int argc, PceObject *argv);
PceObject	Stub__HostCallFunc(PceObject handle, PceObject rec, int argc, PceObject *argv);

/* xmalloc.c */
void *		xmalloc(size_t nbytes);
void *		xrealloc(void *ptr, size_t nbytes);

/* main.c */
int		main(int argc, char *argv []);

/* cpp.cxx */
PceStatus	callCPlusPlusProc(Any f, int ac, const Any av []);
Any		callCPlusPlusFunc(Any f, int ac, const Any av []);
PceStatus	callCPlusPlusMethodProc(Any obj, Any f, int ac, const Any av []);
Any		callCPlusPlusMethodFunc(Any obj, Any f, int ac, const Any av []);
PceStatus	callCPlusPlusPceMethodProc(Any obj, Any f, int ac, const Any av []);
Any		callCPlusPlusPceMethodFunc(Any obj, Any f, int ac, const Any av []);
void		initCPlusPlusGlobals(void);
