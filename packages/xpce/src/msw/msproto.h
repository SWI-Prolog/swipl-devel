/* Additional prototypes for the MS-Windows interface.
*/

void	r_msarc(int x, int y, int w, int h,
		int sx, int sy,
		int ex, int ey,
		Name close,
		Any fill);
void	ws_check_intr(void);
int	ws_free_file_descriptors(void);
int	ws_getpid(void);
void	ws_initialise(int argc, char **argv);
void	ws_sleep(int time);
void	ws_invalidate_window(PceWindow sw, Area a);
void	ws_scroll_window(PceWindow sw, int dx, int dy);
void	ws_redraw_window(PceWindow sw, IArea a, int clear);
int	ws_emulate_three_buttons(int time);
int	ws_mousebuttons(void);
int	iswin32s(void);
char *	ws_os(void);
