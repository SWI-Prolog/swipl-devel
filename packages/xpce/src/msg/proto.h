
/* ../src/msg/and.c */
status		makeClassAnd(Class class);

/* ../src/msg/assign.c */
status		makeClassAssign(Class class);

/* ../src/msg/binding.c */
status		makeClassBinding(Class class);

/* ../src/msg/block.c */
status		makeClassBlock(Class class);

/* ../src/msg/code.c */
status		initialiseCode(Code c);
status		forwardReceiverCodev(Code c, Any receiver, int argc, const Any argv []);
status		forwardCode(Code c, ...);
status		forwardReceiverCode(Code c, Any rec, ...);
Vector		createCodeVectorv(int argc, const Any argv []);
void		doneCodeVector(Vector v);
status		makeClassCodeVector(Class class);
status		makeClassCode(Class class);

/* ../src/msg/create.c */
status		makeClassCreate(Class class);

/* ../src/msg/equal.c */
status		makeClassEqual(Class class);

/* ../src/msg/function.c */
status		initialiseFunction(Function f);
Any		getForwardFunctionv(Function f, int argc, const Any argv []);
Any		getForwardReceiverFunctionv(Function f, Any receiver, int argc, const Any argv []);
Any		getForwardFunction(Function f, ...);
Any		getForwardReceiverFunction(Function f, Any receiver, ...);
Any		getSendMethodFunction(Function f, Name selector);
Any		getGetMethodFunction(Function f, Name selector);
status		makeClassFunction(Class class);

/* ../src/msg/if.c */
status		makeClassIf(Class class);

/* ../src/msg/message.c */
status		ExecuteMessage(Message msg);
status		makeClassMessage(Class class);

/* ../src/msg/nonequal.c */
status		makeClassNonEqual(Class class);

/* ../src/msg/not.c */
status		makeClassNot(Class class);

/* ../src/msg/obtain.c */
status		makeClassObtain(Class class);

/* ../src/msg/or.c */
status		makeClassOr(Class class);

/* ../src/msg/progn.c */
status		makeClassProgn(Class class);

/* ../src/msg/quote.c */
status		makeClassQuoteFunction(Class class);

/* ../src/msg/var.c */
status		makeClassVar(Class class);
void		resetVars(void);
void		popVarEnvironment(void);
status		assignVar(Var v, Any value, Name scope);

/* ../src/msg/when.c */
status		makeClassWhen(Class class);

/* ../src/msg/while.c */
status		makeClassWhile(Class class);

/* ../src/msg/nameref.c */
status		makeClassAssoc(Class class);
