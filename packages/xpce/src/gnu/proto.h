
/* ../src/gnu/gregex.c */
void		print_fastmap(char *fastmap);
void		print_partial_compiled_pattern(unsigned char *start, unsigned char *end);
void		print_compiled_pattern(struct re_pattern_buffer *bufp);
void		print_double_string(const char *where, const char *string1, int size1, const char *string2, int size2);
reg_syntax_t	re_set_syntax(reg_syntax_t syntax);
int		re_compile_fastmap(struct re_pattern_buffer *bufp);
void		re_set_registers(struct re_pattern_buffer *bufp, struct re_registers *regs, unsigned num_regs, regoff_t *starts, regoff_t *ends);
int		re_search(struct re_pattern_buffer *bufp, const char *string, int size, int startpos, int range, struct re_registers *regs);
int		re_search_2(struct re_pattern_buffer *bufp, const char *string1, int size1, const char *string2, int size2, int startpos, int range, struct re_registers *regs, int stop);
int		re_match(struct re_pattern_buffer *bufp, const char *string, int size, int pos, struct re_registers *regs);
int		re_match_2(struct re_pattern_buffer *bufp, const char *string1, int size1, const char *string2, int size2, int pos, struct re_registers *regs, int stop);
const char *	re_compile_pattern(const char *pattern, int length, struct re_pattern_buffer *bufp);
char *		re_comp(const char *s);
int		re_exec(const char *s);
int		regcomp(regex_t *preg, const char *pattern, int cflags);
int		regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch [], int eflags);
size_t		regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
void		regfree(regex_t *preg);

/* ../src/gnu/getdate.c */
YYPARSE_RETURN_TYPEyyparse(int YYPARSE_PARAM_ARG);
time_t		get_date(const char *p, const time_t *now);
int		main(int ac, char *av []);
