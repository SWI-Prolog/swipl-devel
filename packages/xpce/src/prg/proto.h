#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/prg/operator.c */
COMMON(status)	makeClassOperator(Class class);

/* /staff/jan/src/pl/packages/xpce/src/prg/parser.c */
COMMON(status)	makeClassParser(Class class);

/* /staff/jan/src/pl/packages/xpce/src/prg/tokeniser.c */
COMMON(Tokeniser) getOpenTokeniser(Tokeniser t, Any source);
COMMON(Int)	getPeekTokeniser(Tokeniser t);
COMMON(status)	symbolTokeniser(Tokeniser t, Name symb);
COMMON(status)	makeClassTokeniser(Class class);
