#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/gnu/gregex.c */
COMMON(void)	print_fastmap(char *fastmap);
COMMON(void)	print_partial_compiled_pattern(unsigned char *start, unsigned char *end);
COMMON(void)	print_compiled_pattern(struct re_pattern_buffer *bufp);
COMMON(void)	print_double_string(const char *where, const char *string1, int size1, const char *string2, int size2);
COMMON(reg_syntax_t) re_set_syntax(reg_syntax_t syntax);
COMMON(int)	re_compile_fastmap(struct re_pattern_buffer *bufp);
COMMON(void)	re_set_registers(struct re_pattern_buffer *bufp, struct re_registers *regs, unsigned num_regs, regoff_t *starts, regoff_t *ends);
COMMON(int)	re_search(struct re_pattern_buffer *bufp, const char *string, int size, int startpos, int range, struct re_registers *regs);
COMMON(int)	re_search_2(struct re_pattern_buffer *bufp, const char *string1, int size1, const char *string2, int size2, int startpos, int range, struct re_registers *regs, int stop);
COMMON(int)	re_match(struct re_pattern_buffer *bufp, const char *string, int size, int pos, struct re_registers *regs);
COMMON(int)	re_match_2(struct re_pattern_buffer *bufp, const char *string1, int size1, const char *string2, int size2, int pos, struct re_registers *regs, int stop);
COMMON(const char *) re_compile_pattern(const char *pattern, int length, struct re_pattern_buffer *bufp);
COMMON(char *)	re_comp(const char *s);
COMMON(int)	re_exec(const char *s);
COMMON(int)	regcomp(regex_t *preg, const char *pattern, int cflags);
COMMON(int)	regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch [], int eflags);
COMMON(size_t)	regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
COMMON(void)	regfree(regex_t *preg);

/* /staff/jan/src/pl/packages/xpce/src/gnu/getdate.c */
COMMON(YYPARSE_RETURN_TYPE) yyparse(int YYPARSE_PARAM_ARG);
COMMON(time_t)	get_date(const char *p, const time_t *now);
COMMON(int)	main(int ac, char *av []);
