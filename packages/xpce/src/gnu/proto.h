
/* gregex.c */
int		re_set_syntax(int syntax);
char *		re_compile_pattern(char *pattern, int size, struct re_pattern_buffer *bufp);
void		re_compile_fastmap(struct re_pattern_buffer *bufp);
int		re_search(struct re_pattern_buffer *pbufp, char *string, int size, int startpos, int range, struct re_registers *regs);
int		re_search_2(struct re_pattern_buffer *pbufp, char *string1, int size1, char *string2, int size2, int startpos, register int range, struct re_registers *regs, int mstop);
int		re_match(struct re_pattern_buffer *pbufp, char *string, int size, int pos, struct re_registers *regs);
int		re_match_2(struct re_pattern_buffer *pbufp, unsigned char *string1, int size1, unsigned char *string2, int size2, int pos, struct re_registers *regs, int mstop);
char *		re_comp(char *s);
int		re_exec(char *s);
int 		main(int argc, char **argv);
int 		print_buf(struct re_pattern_buffer *bufp);
int 		printchar(int c);
int 		error(char *string);

/* getdate.c */
int		yyerror(char *s);
int		yylex(void);
time_t		get_date(char *p, struct timeb *now);
int 		main(int ac, char *av []);
int		yyparse(void);
