
/* msg/and.c */
status		makeClassAnd(Class class);

/* msg/assign.c */
status		makeClassAssign(Class class);

/* msg/binding.c */
status		makeClassBinding(Class class);

/* msg/block.c */
status		makeClassBlock(Class class);

/* msg/code.c */
status		initialiseCode(Code c);
status		forwardReceiverCodev(Code c, Any receiver, int argc, const Any argv []);
status		forwardCode(Code c, ...);
status		forwardReceiverCode(Code c, Any rec, ...);
Vector		createCodeVectorv(int argc, const Any argv []);
void		doneCodeVector(Vector v);
status		makeClassCodeVector(Class class);
status		makeClassCode(Class class);

/* msg/create.c */
status		makeClassCreate(Class class);

/* msg/equal.c */
status		makeClassEqual(Class class);

/* msg/function.c */
status		initialiseFunction(Function f);
Any		getForwardFunctionv(Function f, int argc, const Any argv []);
Any		getForwardReceiverFunctionv(Function f, Any receiver, int argc, const Any argv []);
Any		getForwardFunction(Function f, ...);
Any		getForwardReceiverFunction(Function f, Any receiver, ...);
Any		getSendMethodFunction(Function f, Name selector);
Any		getGetMethodFunction(Function f, Name selector);
status		makeClassFunction(Class class);

/* msg/if.c */
status		makeClassIf(Class class);

/* msg/message.c */
status		ExecuteMessage(Message msg);
status		makeClassMessage(Class class);

/* msg/nonequal.c */
status		makeClassNonEqual(Class class);

/* msg/not.c */
status		makeClassNot(Class class);

/* msg/obtain.c */
status		makeClassObtain(Class class);

/* msg/or.c */
status		makeClassOr(Class class);

/* msg/progn.c */
status		makeClassProgn(Class class);

/* msg/quote.c */
status		makeClassQuoteFunction(Class class);

/* msg/var.c */
status		makeClassVar(Class class);
void		resetVars(void);
void		popVarEnvironment(void);
status		assignVar(Var v, Any value, Name scope);

/* msg/when.c */
status		makeClassWhen(Class class);

/* msg/while.c */
status		makeClassWhile(Class class);

/* msg/nameref.c */
status		makeClassAssoc(Class class);
