
/* operator.c */
status		makeClassOperator(Class class);

/* parser.c */
status		makeClassParser(Class class);

/* tokeniser.c */
Int		getPeekTokeniser(Tokeniser t);
status		symbolTokeniser(Tokeniser t, Name symb);
status		makeClassTokeniser(Class class);
