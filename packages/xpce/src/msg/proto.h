
/* and.c */
status		makeClassAnd(Class class);

/* assign.c */
status		makeClassAssign(Class class);

/* binding.c */
status		makeClassBinding(Class class);

/* block.c */
status		makeClassBlock(Class class);

/* code.c */
status		initialiseCode(Code c);
status		forwardReceiverCodev(Code c, Any receiver, int argc, const Any argv []);
status		forwardCode(Code c, ...);
status		forwardReceiverCode(Code c, Any rec, ...);
Vector		createCodeVectorv(int argc, const Any argv []);
void		doneCodeVector(Vector v);
status		makeClassCodeVector(Class class);
status		makeClassCode(Class class);

/* create.c */
status		makeClassCreate(Class class);

/* equal.c */
status		makeClassEqual(Class class);

/* function.c */
status		initialiseFunction(Function f);
Any		getForwardFunctionv(Function f, int argc, Any *argv);
Any		getForwardReceiverFunctionv(Function f, Any receiver, int argc, Any *argv);
Any		getForwardFunction(Function f, ...);
Any		getForwardReceiverFunction(Function f, Any receiver, ...);
Any		getSendMethodFunction(Function f, Name selector);
Any		getGetMethodFunction(Function f, Name selector);
status		makeClassFunction(Class class);

/* if.c */
status		makeClassIf(Class class);

/* message.c */
status		ExecuteMessage(Message msg);
status		makeClassMessage(Class class);

/* nonequal.c */
status		makeClassNonEqual(Class class);

/* not.c */
status		makeClassNot(Class class);

/* obtain.c */
status		makeClassObtain(Class class);

/* or.c */
status		makeClassOr(Class class);

/* progn.c */
status		makeClassProgn(Class class);

/* quote.c */
status		makeClassQuoteFunction(Class class);

/* var.c */
Any		getValueVar(Var v);
status		makeClassVar(Class class);
void		resetVars(void);
void		popVarEnvironment(void);
status		assignVar(Var v, Any value, Name scope);

/* when.c */
status		makeClassWhen(Class class);

/* while.c */
status		makeClassWhile(Class class);
