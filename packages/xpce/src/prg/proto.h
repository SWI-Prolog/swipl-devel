
/* operator.c */
status		makeClassOperator(Class class);

/* parser.c */
status		makeClassParser(Class class);

/* tokeniser.c */
Int		getLineTokeniser(Tokeniser t);
Int		getCaretTokeniser(Tokeniser t);
Tokeniser	getOpenTokeniser(Tokeniser t, Any source);
Int		getPeekTokeniser(Tokeniser t);
status		symbolTokeniser(Tokeniser t, Name symb);
status		makeClassTokeniser(Class class);
