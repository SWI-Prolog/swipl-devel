
/* history.c */
void		rlc_init_history(int auto_add, int size);
void		rlc_add_history(const char *line);
int		rlc_at_head_history(void);
const char *	rlc_bwd_history(void);
const char *	rlc_fwd_history(void);
