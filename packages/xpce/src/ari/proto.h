
/* ari/equation.c */
status		makeClassBinaryCondition(Class class);
Int		getVarEquation(Equation e, Var var);
status		makeClassEquation(Class class);
status		makeClassLess(Class class);
status		makeClassLessEqual(Class class);
status		makeClassGreater(Class class);
status		makeClassGreaterEqual(Class class);

/* ari/expression.c */
PseudoFloat	getPseudoFloatExpression(Any e);
Int		getVarInBinaryExpression(BinaryExpression e, Var var);
status		makeClassBinaryExpression(Class class);
status		makeClassDivide(Class class);
status		makeClassTimes(Class class);
status		makeClassPlus(Class class);
status		makeClassMinus(Class class);
Int		getValueExpression(Expression e, ...);
