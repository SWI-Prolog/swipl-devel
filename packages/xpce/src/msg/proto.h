#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/msg/and.c */
COMMON(status)	makeClassAnd(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/assign.c */
COMMON(status)	makeClassAssign(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/binding.c */
COMMON(status)	makeClassBinding(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/block.c */
COMMON(status)	makeClassBlock(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/code.c */
COMMON(status)	initialiseCode(Code c);
COMMON(status)	userForwardReceiverCodev(Code c, Any receiver, int argc, const Any argv []);
COMMON(status)	forwardReceiverCodev(Code c, Any receiver, int argc, const Any argv []);
COMMON(status)	forwardCode(Code c, ...);
COMMON(status)	forwardReceiverCode(Code c, Any rec, ...);
COMMON(Vector)	createCodeVectorv(int argc, const Any argv []);
COMMON(void)	doneCodeVector(Vector v);
COMMON(status)	makeClassCodeVector(Class class);
COMMON(status)	makeClassCode(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/create.c */
COMMON(status)	makeClassCreate(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/equal.c */
COMMON(status)	makeClassEqual(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/function.c */
COMMON(status)	initialiseFunction(Function f);
COMMON(Any)	getForwardFunctionv(Function f, int argc, const Any argv []);
COMMON(Any)	getForwardReceiverFunctionv(Function f, Any receiver, int argc, const Any argv []);
COMMON(Any)	getForwardFunction(Function f, ...);
COMMON(Any)	getForwardReceiverFunction(Function f, Any receiver, ...);
COMMON(Any)	getSendMethodFunction(Function f, Name selector);
COMMON(Any)	getGetMethodFunction(Function f, Name selector);
COMMON(status)	makeClassFunction(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/if.c */
COMMON(status)	makeClassIf(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/message.c */
COMMON(status)	ExecuteMessage(Message msg);
COMMON(status)	makeClassMessage(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/nonequal.c */
COMMON(status)	makeClassNonEqual(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/not.c */
COMMON(status)	makeClassNot(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/obtain.c */
COMMON(status)	makeClassObtain(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/or.c */
COMMON(status)	makeClassOr(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/progn.c */
COMMON(status)	makeClassProgn(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/quote.c */
COMMON(status)	makeClassQuoteFunction(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/var.c */
COMMON(status)	makeClassVar(Class class);
COMMON(void)	resetVars(void);
COMMON(void)	popVarEnvironment(void);
COMMON(status)	assignVar(Var v, Any value, Name scope);

/* /staff/jan/src/pl/packages/xpce/src/msg/when.c */
COMMON(status)	makeClassWhen(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/while.c */
COMMON(status)	makeClassWhile(Class class);

/* /staff/jan/src/pl/packages/xpce/src/msg/nameref.c */
COMMON(status)	makeClassAssoc(Class class);
