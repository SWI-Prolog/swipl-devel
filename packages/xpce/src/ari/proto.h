#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/ari/equation.c */
COMMON(status)	makeClassBinaryCondition(Class class);
COMMON(status)	evaluateEquation(Equation e, Var var, NumericValue result);
COMMON(status)	makeClassEquation(Class class);
COMMON(status)	makeClassLess(Class class);
COMMON(status)	makeClassLessEqual(Class class);
COMMON(status)	makeClassGreater(Class class);
COMMON(status)	makeClassGreaterEqual(Class class);

/* /staff/jan/src/pl/packages/xpce/src/ari/expression.c */
COMMON(PseudoFloat) getPseudoFloatExpression(Any e);
COMMON(void)	promoteToRealNumericValue(NumericValue n);
COMMON(status)	ar_add(NumericValue n1, NumericValue n2, NumericValue r);
COMMON(int)	ar_minus(NumericValue n1, NumericValue n2, NumericValue r);
COMMON(int)	ar_divide(NumericValue n1, NumericValue n2, NumericValue r);
COMMON(int)	ar_times(NumericValue n1, NumericValue n2, NumericValue r);
COMMON(status)	evaluateExpression(Any e, NumericValue v);
COMMON(Any)	ar_result(NumericValue n);
COMMON(Int)	ar_int_result(Any e, NumericValue n);
COMMON(Int)	getVarInBinaryExpression(BinaryExpression e, Var var);
COMMON(status)	makeClassBinaryExpression(Class class);
COMMON(status)	makeClassDivide(Class class);
COMMON(status)	makeClassTimes(Class class);
COMMON(status)	makeClassPlus(Class class);
COMMON(status)	makeClassMinus(Class class);
COMMON(Int)	getValueExpression(Expression e, ...);
