
/* ../src/ari/equation.c */
status		makeClassBinaryCondition(Class class);
status		evaluateEquation(Equation e, Var var, NumericValue result);
status		makeClassEquation(Class class);
status		makeClassLess(Class class);
status		makeClassLessEqual(Class class);
status		makeClassGreater(Class class);
status		makeClassGreaterEqual(Class class);

/* ../src/ari/expression.c */
PseudoFloat	getPseudoFloatExpression(Any e);
void		promoteToRealNumericValue(NumericValue n);
status		ar_add(NumericValue n1, NumericValue n2, NumericValue r);
int		ar_minus(NumericValue n1, NumericValue n2, NumericValue r);
int		ar_divide(NumericValue n1, NumericValue n2, NumericValue r);
int		ar_times(NumericValue n1, NumericValue n2, NumericValue r);
status		evaluateExpression(Any e, NumericValue v);
Any		ar_result(NumericValue n);
Int		ar_int_result(Any e, NumericValue n);
Int		getVarInBinaryExpression(BinaryExpression e, Var var);
status		makeClassBinaryExpression(Class class);
status		makeClassDivide(Class class);
status		makeClassTimes(Class class);
status		makeClassPlus(Class class);
status		makeClassMinus(Class class);
Int		getValueExpression(Expression e, ...);
