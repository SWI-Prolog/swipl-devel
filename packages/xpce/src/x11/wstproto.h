
/* ../src/x11/xcolour.c */
status		ws_create_colour(Colour c, DisplayObj d);
void		ws_uncreate_colour(Colour c, DisplayObj d);
status		ws_colour_name(DisplayObj d, Name name);
Colour		ws_pixel_to_colour(DisplayObj d, unsigned long pixel);
void		ws_colour_cube(ColourMap cm, int size);
void		ws_colour_map_colours(ColourMap cm);
status		ws_create_colour_map(ColourMap cm, DisplayObj d);
status		ws_uncreate_colour_map(ColourMap cm, DisplayObj d);

/* ../src/x11/xcursor.c */
void		ws_init_cursor_font(void);
Int		ws_cursor_font_index(Name name);
status		ws_create_cursor(CursorObj c, DisplayObj d);
void		ws_destroy_cursor(CursorObj c, DisplayObj d);

/* ../src/x11/xdisplay.c */
void		ws_flush_display(DisplayObj d);
void		ws_synchronise_display(DisplayObj d);
void		ws_bell_display(DisplayObj d, int volume);
void		ws_get_size_display(DisplayObj d, int *w, int *h);
Name		ws_get_visual_type_display(DisplayObj d);
int		ws_depth_display(DisplayObj d);
int		ws_resolution_display(DisplayObj d, int *rx, int *ry);
void		ws_activate_screen_saver(DisplayObj d);
void		ws_deactivate_screen_saver(DisplayObj d);
void		ws_init_display(DisplayObj d);
status		ws_legal_display_name(char *s);
status		ws_opened_display(DisplayObj d);
void		ws_open_display(DisplayObj d);
void		ws_quit_display(DisplayObj d);
status		ws_init_graphics_display(DisplayObj d);
void		ws_foreground_display(DisplayObj d, Colour c);
void		ws_background_display(DisplayObj d, Colour c);
void		ws_draw_in_display(DisplayObj d, Graphical gr, Bool invert, Bool subtoo);
void		ws_grab_server(DisplayObj d);
void		ws_ungrab_server(DisplayObj d);
Int		ws_display_connection_number(DisplayObj d);
status		ws_events_queued_display(DisplayObj d);
status		ws_set_cutbuffer(DisplayObj d, int n, String s);
StringObj	ws_get_cutbuffer(DisplayObj d, int n);
unsigned long	ws_get_selection_timeout(void);
void		ws_set_selection_timeout(unsigned long time);
Any		ws_get_selection(DisplayObj d, Name which, Name target);
void		ws_disown_selection(DisplayObj d, Name selection);
status		ws_own_selection(DisplayObj d, Name selection, Name type);
Name		ws_window_manager(DisplayObj d);
void		ws_synchronous(DisplayObj d);
void		ws_asynchronous(DisplayObj d);
status		ws_postscript_display(DisplayObj d);
Image		ws_grab_image_display(DisplayObj d, int x, int y, int width, int height);

/* ../src/x11/xdraw.c */
void		resetDraw(void);
void		d_offset(int x, int y);
void		r_offset(int x, int y);
void		r_filloffset(Point offset, int x0, int y0, fill_state *state);
void		r_fillrestore(fill_state *state);
DisplayObj	d_display(DisplayObj d);
void		d_ensure_display(void);
void		d_flush(void);
void		d_window(PceWindow sw, int x, int y, int w, int h, int clear, int limit);
void		d_image(Image i, int x, int y, int w, int h);
void		d_screen(DisplayObj d);
void		d_frame(FrameObj fr, int x, int y, int w, int h);
void		d_clip(int x, int y, int w, int h);
void		d_done(void);
void		d_clip_done(void);
void		intersection_iarea(IArea a, IArea b);
void		r_clear(int x, int y, int w, int h);
void		r_complement(int x, int y, int w, int h);
void		r_and(int x, int y, int w, int h, Image pattern);
void		r_thickness(int pen);
int		r_transformed(int val);
void		r_dash(Name name);
void		d_pen(Pen pen);
void		r_fillpattern(Any fill, Name which);
void		r_arcmode(Name mode);
void		r_fix_colours(Any fg, Any bg, ColourContext ctx);
void		r_unfix_colours(ColourContext ctx);
Any		r_default_colour(Any c);
Any		r_colour(Any c);
Any		r_background(Any c);
void		r_swap_background_and_foreground(void);
Bool		r_subwindow_mode(Bool val);
void		r_invert_mode(Bool val);
void		r_translate(int x, int y, int *ox, int *oy);
void		r_box(int x, int y, int w, int h, int r, Any fill);
void		r_shadow_box(int x, int y, int w, int h, int r, int shadow, Image fill);
Any		r_elevation_shadow(Elevation e);
void		r_3d_segments(int n, ISegment s, Elevation e, int light);
void		r_3d_box(int x, int y, int w, int h, int radius, Elevation e, int up);
void		r_3d_line(int x1, int y1, int x2, int y2, Elevation e, int up);
void		r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Elevation e, int up, int map);
void		r_3d_diamond(int x, int y, int w, int h, Elevation e, int up);
void		r_arc(int x, int y, int w, int h, int s, int e, Any fill);
void		r_ellipse(int x, int y, int w, int h, Any fill);
void		r_3d_ellipse(int x, int y, int w, int h, Elevation z, int up);
void		r_line(int x1, int y1, int x2, int y2);
void		r_polygon(IPoint pts, int n, int close);
void		r_path(Chain points, int ox, int oy, int radius, int closed, Image fill);
void		r_op_image(Image image, int sx, int sy, int x, int y, int w, int h, Name op);
void		r_image(Image image, int sx, int sy, int x, int y, int w, int h, Bool transparent);
void		r_fill(int x, int y, int w, int h, Any pattern);
void		r_fill_polygon(IPoint pts, int n);
void		r_caret(int cx, int cy, FontObj font);
void		r_fill_triangle(int x1, int y1, int x2, int y2, int x3, int y3);
void		r_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Any fill);
void		r_pixel(int x, int y, Any val);
void		r_complement_pixel(int x, int y);
void		d_modify(void);
int		r_get_mono_pixel(int x, int y);
unsigned long	r_get_pixel(int x, int y);
int		s_has_char(FontObj f, unsigned int c);
void		f_domain(FontObj f, Name which, int *x, int *y);
int		s_default_char(FontObj font);
int		s_ascent(FontObj f);
int		s_descent(FontObj f);
int		s_height(FontObj f);
int		c_width(unsigned int c, FontObj font);
String		str_bits_as_font(String s, FontObj f, int *shift);
int		str_width(String s, int from, int to, FontObj f);
int		str_advance(String s, int from, int to, FontObj f);
void		s_print8(char8 *s, int l, int x, int y, FontObj f);
void		s_print16(char16 *s, int l, int x, int y, FontObj f);
void		s_print(String s, int x, int y, FontObj f);
void		s_print_aligned(String s, int x, int y, FontObj f);
void		str_size(String s, FontObj font, int *width, int *height);
void		str_string(String s, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
void		str_selected_string(String s, FontObj font, int f, int t, Style style, int x, int y, int w, int h, Name hadjust, Name vadjust);
void		ps_string(String s, FontObj font, int x, int y, int w, Name format, int flags);
void		str_label(String s, int acc, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);

/* ../src/x11/xevent.c */
void		resetDispatch(void);
status		ws_dispatch(Int FD, Int timeout);
void		ws_discard_input(const char *msg);
Any		ws_event_in_subwindow(EventObj ev, Any root);

/* ../src/x11/xfont.c */
status		ws_create_font(FontObj f, DisplayObj d);
void		ws_destroy_font(FontObj f, DisplayObj d);
status		ws_system_fonts(DisplayObj d);

/* ../src/x11/xframe.c */
status		ws_created_frame(FrameObj fr);
void		ws_uncreate_frame(FrameObj fr);
status		ws_create_frame(FrameObj fr);
void		ws_realise_frame(FrameObj fr);
PceWindow	ws_window_holding_point_frame(FrameObj fr);
void		ws_raise_frame(FrameObj fr);
void		ws_lower_frame(FrameObj fr);
status		ws_attach_wm_prototols_frame(FrameObj fr);
status		setDndAwareFrame(FrameObj fr);
void		ws_frame_cursor(FrameObj fr, CursorObj cursor);
void		ws_grab_frame_pointer(FrameObj fr, Bool grab, CursorObj cursor);
status		ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h);
void		ws_x_geometry_frame(FrameObj fr, Name spec);
void		ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h);
void		ws_border_frame(FrameObj fr, int b);
void		ws_busy_cursor_frame(FrameObj fr, CursorObj c);
void		ws_frame_background(FrameObj fr, Any c);
void		ws_set_icon_frame(FrameObj fr);
void		ws_set_icon_label_frame(FrameObj fr);
void		ws_set_icon_position_frame(FrameObj fr, int x, int y);
status		ws_get_icon_position_frame(FrameObj fr, int *x, int *y);
void		ws_enable_modal(FrameObj fr, Bool val);
void		ws_status_frame(FrameObj fr, Name status);
void		ws_topmost_frame(FrameObj fr, Bool topmost);
void		ws_set_label_frame(FrameObj fr);
Image		ws_image_of_frame(FrameObj fr);
void		ws_transient_frame(FrameObj fr, FrameObj fr2);
status		ws_postscript_frame(FrameObj fr);

/* ../src/x11/ximage.c */
void		ws_init_image(Image image);
void		ws_destroy_image(Image image);
status		ws_store_image(Image image, FileObj file);
status		loadXImage(Image image, IOSTREAM *fd);
status		loadPNMImage(Image image, IOSTREAM *fd);
status		ws_load_old_image(Image image, IOSTREAM *fd);
status		ws_load_image_file(Image image);
Image		ws_std_xpm_image(Name name, Image *global, char **data);
status		ws_save_image_file(Image image, SourceSink into, Name fmt);
status		ws_open_image(Image image, DisplayObj d);
void		ws_close_image(Image image, DisplayObj d);
status		ws_resize_image(Image image, Int w, Int h);
Image		ws_scale_image(Image image, int w, int h);
Image		ws_rotate_image(Image image, int angle);
Image		ws_monochrome_image(Image image);
void		ws_postscript_image(Image image, Int depth);
status		loadXliImage(Image image, FileObj file, Int bright);
void		ws_create_image_from_x11_data(Image image, unsigned char *data, int w, int h);
ColourMap	ws_colour_map_for_image(Image image);
void		ws_system_images(void);

/* ../src/x11/xstream.c */
void		ws_close_input_stream(Stream s);
void		ws_close_output_stream(Stream s);
void		ws_close_stream(Stream s);
void		ws_input_stream(Stream s);
void		ws_no_input_stream(Stream s);
void		ws_listen_socket(Socket s);
status		ws_write_stream_data(Stream s, void *data, int len);
int		ws_read_stream_data(Stream s, void *data, int len);
StringObj	ws_read_line_stream(Stream s, Int timeout);
void		ws_done_process(Process p);

/* ../src/x11/xtimer.c */
void		ws_status_timer(Timer tm, Name status);

/* ../src/x11/xwindow.c */
status		ws_created_window(PceWindow sw);
void		ws_uncreate_window(PceWindow sw);
status		ws_create_window(PceWindow sw, PceWindow parent);
void		ws_manage_window(PceWindow sw);
void		ws_unmanage_window(PceWindow sw);
void		ws_reassociate_ws_window(PceWindow from, PceWindow to);
void		ws_geometry_window(PceWindow sw, int x, int y, int w, int h, int pen);
void		ws_grab_pointer_window(PceWindow sw, Bool val);
void		ws_grab_keyboard_window(PceWindow sw, Bool val);
void		ws_grab_pointer_window(PceWindow sw, Bool val);
void		ws_grab_keyboard_window(PceWindow sw, Bool val);
void		ws_ungrab_all(void);
void		ws_flash_area_window(PceWindow sw, int x, int y, int w, int h, int msecs);
void		ws_flash_window(PceWindow sw, int msecs);
void		ws_move_pointer(PceWindow sw, int x, int y);
void		ws_window_cursor(PceWindow sw, CursorObj cursor);
void		ws_window_background(PceWindow sw, Any c);
void		ws_raise_window(PceWindow sw);
void		ws_lower_window(PceWindow sw);

/* ../src/x11/x11.c */
void		ws_initialise(int argc, char **argv);
int		ws_version(void);
int		ws_revision(void);
status		ws_show_console(Name how);
status		ws_console_label(CharArray label);
Int		ws_default_scrollbar_width(void);

/* ../src/x11/xmenu.c */
status		ws_draw_scrollbar_arrow(ScrollBar s, int x, int y, int w, int h, Name which, int up);
int		ws_arrow_height_scrollbar(ScrollBar s);
status		ws_draw_sb_thumb(int x, int y, int w, int h);
Colour		ws_3d_grey(void);
status		ws_draw_button_face(DialogItem di, int x, int y, int w, int h, int up, int defb, int focus);
int		ws_combo_box_width(void);
int		ws_stepper_width(void);
status		ws_entry_field(int x, int y, int w, int h, int flags);
status		ws_draw_checkbox(int x, int y, int w, int h, int b, int flags);
status		ws_checkbox_size(int flags, int *w, int *h);
int		ws_message_box(Any msg, int flags);

/* ../src/gra/graphstate.c */
void		g_save(void);
void		g_restore(void);
int		g_level(void);
