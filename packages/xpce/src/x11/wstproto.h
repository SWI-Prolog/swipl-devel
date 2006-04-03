#define COMMON(type) SO_LOCAL type

/* ../src/x11/xcolour.c */
COMMON(status)	ws_create_colour(Colour c, DisplayObj d);
COMMON(void)	ws_uncreate_colour(Colour c, DisplayObj d);
COMMON(status)	ws_colour_name(DisplayObj d, Name name);
COMMON(Colour)	ws_pixel_to_colour(DisplayObj d, unsigned long pixel);
COMMON(void)	ws_colour_cube(ColourMap cm, int size);
COMMON(void)	ws_colour_map_colours(ColourMap cm);
COMMON(status)	ws_create_colour_map(ColourMap cm, DisplayObj d);
COMMON(status)	ws_uncreate_colour_map(ColourMap cm, DisplayObj d);
COMMON(status)	ws_unlink_colour_map(ColourMap cm);

/* ../src/x11/xcursor.c */
COMMON(void)	ws_init_cursor_font(void);
COMMON(Int)	ws_cursor_font_index(Name name);
COMMON(status)	ws_create_cursor(CursorObj c, DisplayObj d);
COMMON(void)	ws_destroy_cursor(CursorObj c, DisplayObj d);

/* ../src/x11/xdisplay.c */
COMMON(void)	ws_flush_display(DisplayObj d);
COMMON(void)	ws_synchronise_display(DisplayObj d);
COMMON(void)	ws_bell_display(DisplayObj d, int volume);
COMMON(void)	ws_get_size_display(DisplayObj d, int *w, int *h);
COMMON(Name)	ws_get_visual_type_display(DisplayObj d);
COMMON(int)	ws_depth_display(DisplayObj d);
COMMON(int)	ws_resolution_display(DisplayObj d, int *rx, int *ry);
COMMON(void)	ws_activate_screen_saver(DisplayObj d);
COMMON(void)	ws_deactivate_screen_saver(DisplayObj d);
COMMON(void)	ws_init_display(DisplayObj d);
COMMON(status)	ws_legal_display_name(char *s);
COMMON(status)	ws_opened_display(DisplayObj d);
COMMON(void)	ws_open_display(DisplayObj d);
COMMON(void)	ws_quit_display(DisplayObj d);
COMMON(status)	ws_init_graphics_display(DisplayObj d);
COMMON(status)	ws_init_monitors_display(DisplayObj d);
COMMON(void)	ws_foreground_display(DisplayObj d, Colour c);
COMMON(void)	ws_background_display(DisplayObj d, Colour c);
COMMON(void)	ws_draw_in_display(DisplayObj d, Graphical gr, Bool invert, Bool subtoo);
COMMON(void)	ws_grab_server(DisplayObj d);
COMMON(void)	ws_ungrab_server(DisplayObj d);
COMMON(Int)	ws_display_connection_number(DisplayObj d);
COMMON(status)	ws_events_queued_display(DisplayObj d);
COMMON(status)	ws_set_cutbuffer(DisplayObj d, int n, String s);
COMMON(StringObj) ws_get_cutbuffer(DisplayObj d, int n);
COMMON(unsigned long)ws_get_selection_timeout(void);
COMMON(void)	ws_set_selection_timeout(unsigned long time);
COMMON(Any)	ws_get_selection(DisplayObj d, Name which, Name target);
COMMON(void)	ws_disown_selection(DisplayObj d, Name selection);
COMMON(status)	ws_own_selection(DisplayObj d, Name selection, Name type);
COMMON(Name)	ws_window_manager(DisplayObj d);
COMMON(void)	ws_synchronous(DisplayObj d);
COMMON(void)	ws_asynchronous(DisplayObj d);
COMMON(status)	ws_postscript_display(DisplayObj d, int iscolor);
COMMON(Image)	ws_grab_image_display(DisplayObj d, int x, int y, int width, int height);

/* ../src/x11/xdraw.c */
COMMON(void)	resetDraw(void);
COMMON(void)	d_offset(int x, int y);
COMMON(void)	r_offset(int x, int y);
COMMON(void)	r_filloffset(Point offset, int x0, int y0, fill_state *state);
COMMON(void)	r_fillrestore(fill_state *state);
COMMON(DisplayObj) d_display(DisplayObj d);
COMMON(void)	d_ensure_display(void);
COMMON(void)	d_flush(void);
COMMON(void)	d_window(PceWindow sw, int x, int y, int w, int h, int clear, int limit);
COMMON(void)	d_image(Image i, int x, int y, int w, int h);
COMMON(void)	d_screen(DisplayObj d);
COMMON(void)	d_frame(FrameObj fr, int x, int y, int w, int h);
COMMON(void)	d_clip(int x, int y, int w, int h);
COMMON(void)	d_done(void);
COMMON(void)	d_clip_done(void);
COMMON(void)	intersection_iarea(IArea a, IArea b);
COMMON(void)	r_clear(int x, int y, int w, int h);
COMMON(void)	r_complement(int x, int y, int w, int h);
COMMON(void)	r_and(int x, int y, int w, int h, Image pattern);
COMMON(void)	r_thickness(int pen);
COMMON(int)	r_transformed(int val);
COMMON(void)	r_dash(Name name);
COMMON(void)	d_pen(Pen pen);
COMMON(void)	r_fillpattern(Any fill, Name which);
COMMON(void)	r_arcmode(Name mode);
COMMON(void)	r_fix_colours(Any fg, Any bg, ColourContext ctx);
COMMON(void)	r_unfix_colours(ColourContext ctx);
COMMON(Any)	r_default_colour(Any c);
COMMON(Any)	r_colour(Any c);
COMMON(Any)	r_background(Any c);
COMMON(void)	r_swap_background_and_foreground(void);
COMMON(Bool)	r_subwindow_mode(Bool val);
COMMON(void)	r_invert_mode(Bool val);
COMMON(void)	r_translate(int x, int y, int *ox, int *oy);
COMMON(void)	r_box(int x, int y, int w, int h, int r, Any fill);
COMMON(void)	r_shadow_box(int x, int y, int w, int h, int r, int shadow, Image fill);
COMMON(Any)	r_elevation_shadow(Elevation e);
COMMON(void)	r_3d_segments(int n, ISegment s, Elevation e, int light);
COMMON(void)	r_3d_box(int x, int y, int w, int h, int radius, Elevation e, int up);
COMMON(void)	r_3d_line(int x1, int y1, int x2, int y2, Elevation e, int up);
COMMON(void)	r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Elevation e, int up, int map);
COMMON(void)	r_3d_diamond(int x, int y, int w, int h, Elevation e, int up);
COMMON(void)	r_arc(int x, int y, int w, int h, int s, int e, Any fill);
COMMON(void)	r_ellipse(int x, int y, int w, int h, Any fill);
COMMON(void)	r_3d_ellipse(int x, int y, int w, int h, Elevation z, int up);
COMMON(void)	r_line(int x1, int y1, int x2, int y2);
COMMON(void)	r_polygon(IPoint pts, int n, int close);
COMMON(void)	r_path(Chain points, int ox, int oy, int radius, int closed, Image fill);
COMMON(void)	r_op_image(Image image, int sx, int sy, int x, int y, int w, int h, Name op);
COMMON(void)	r_image(Image image, int sx, int sy, int x, int y, int w, int h, Bool transparent);
COMMON(void)	r_fill(int x, int y, int w, int h, Any pattern);
COMMON(void)	r_fill_polygon(IPoint pts, int n);
COMMON(void)	r_caret(int cx, int cy, FontObj font);
COMMON(void)	r_fill_triangle(int x1, int y1, int x2, int y2, int x3, int y3);
COMMON(void)	r_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Any fill);
COMMON(void)	r_pixel(int x, int y, Any val);
COMMON(void)	r_complement_pixel(int x, int y);
COMMON(void)	d_modify(void);
COMMON(int)	r_get_mono_pixel(int x, int y);
COMMON(unsigned long) r_get_pixel(int x, int y);
COMMON(int)	s_has_char(FontObj f, unsigned int c);
COMMON(void)	f_domain(FontObj f, Name which, int *x, int *y);
COMMON(int)	s_default_char(FontObj font);
COMMON(int)	s_ascent(FontObj f);
COMMON(int)	s_descent(FontObj f);
COMMON(int)	s_height(FontObj f);
COMMON(int)	c_width(wint_t c, FontObj font);
COMMON(int)	str_width(String s, int from, int to, FontObj f);
COMMON(int)	str_advance(String s, int from, int to, FontObj f);
COMMON(void)	s_printA(charA *s, int l, int x, int y, FontObj f);
COMMON(void)	s_printW(charW *s, int l, int x, int y, FontObj f);
COMMON(void)	s_print(String s, int x, int y, FontObj f);
COMMON(void)	s_print_aligned(String s, int x, int y, FontObj f);
COMMON(void)	str_size(String s, FontObj font, int *width, int *height);
COMMON(void)	str_string(String s, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
COMMON(void)	str_selected_string(String s, FontObj font, int f, int t, Style style, int x, int y, int w, int h, Name hadjust, Name vadjust);
COMMON(void)	ps_string(String s, FontObj font, int x, int y, int w, Name format, int flags);
COMMON(void)	str_label(String s, int acc, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);

/* ../src/x11/xevent.c */
COMMON(void)	resetDispatch(void);
COMMON(status)	ws_dispatch(Int FD, Any timeout);
COMMON(void)	ws_discard_input(const char *msg);
COMMON(Any)	ws_event_in_subwindow(EventObj ev, Any root);
COMMON(int)	ws_wait_for_key(int maxwait);

/* ../src/x11/xfont.c */
COMMON(status)	ws_create_font(FontObj f, DisplayObj d);
COMMON(void)	ws_destroy_font(FontObj f, DisplayObj d);
COMMON(status)	ws_system_fonts(DisplayObj d);

/* ../src/x11/xframe.c */
COMMON(status)	ws_created_frame(FrameObj fr);
COMMON(void)	ws_uncreate_frame(FrameObj fr);
COMMON(status)	ws_create_frame(FrameObj fr);
COMMON(void)	ws_realise_frame(FrameObj fr);
COMMON(PceWindow)ws_window_holding_point_frame(FrameObj fr);
COMMON(void)	ws_raise_frame(FrameObj fr);
COMMON(void)	ws_lower_frame(FrameObj fr);
COMMON(status)	ws_attach_wm_prototols_frame(FrameObj fr);
COMMON(status)	setDndAwareFrame(FrameObj fr);
COMMON(void)	ws_frame_cursor(FrameObj fr, CursorObj cursor);
COMMON(void)	ws_grab_frame_pointer(FrameObj fr, Bool grab, CursorObj cursor);
COMMON(status)	ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h);
COMMON(void)	ws_x_geometry_frame(FrameObj fr, Name spec);
COMMON(void)	ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h);
COMMON(void)	ws_border_frame(FrameObj fr, int b);
COMMON(void)	ws_busy_cursor_frame(FrameObj fr, CursorObj c);
COMMON(void)	ws_frame_background(FrameObj fr, Any c);
COMMON(void)	ws_set_icon_frame(FrameObj fr);
COMMON(void)	ws_set_icon_label_frame(FrameObj fr);
COMMON(void)	ws_set_icon_position_frame(FrameObj fr, int x, int y);
COMMON(status)	ws_get_icon_position_frame(FrameObj fr, int *x, int *y);
COMMON(void)	ws_enable_modal(FrameObj fr, Bool val);
COMMON(void)	ws_status_frame(FrameObj fr, Name status);
COMMON(void)	ws_topmost_frame(FrameObj fr, Bool topmost);
COMMON(void)	ws_set_label_frame(FrameObj fr);
COMMON(Image)	ws_image_of_frame(FrameObj fr);
COMMON(void)	ws_transient_frame(FrameObj fr, FrameObj fr2);
COMMON(status)	ws_postscript_frame(FrameObj fr, int iscolor);
COMMON(Int)	ws_frame_thread(FrameObj fr);
COMMON(int)	ws_enable_frame(FrameObj fr, int enable);

/* ../src/x11/ximage.c */
COMMON(void)	ws_init_image(Image image);
COMMON(void)	ws_destroy_image(Image image);
COMMON(status)	ws_store_image(Image image, FileObj file);
COMMON(status)	loadXImage(Image image, IOSTREAM *fd);
COMMON(status)	loadPNMImage(Image image, IOSTREAM *fd);
COMMON(status)	ws_load_old_image(Image image, IOSTREAM *fd);
COMMON(status)	ws_load_image_file(Image image);
COMMON(status)	ws_create_image_from_xpm_data(Image image, char **data, DisplayObj d);
COMMON(status)	ws_save_image_file(Image image, SourceSink into, Name fmt);
COMMON(status)	ws_open_image(Image image, DisplayObj d);
COMMON(void)	ws_close_image(Image image, DisplayObj d);
COMMON(status)	ws_resize_image(Image image, Int w, Int h);
COMMON(Image)	ws_scale_image(Image image, int w, int h);
COMMON(Image)	ws_rotate_image(Image image, float angle);
COMMON(Image)	ws_monochrome_image(Image image);
COMMON(void)	ws_postscript_image(Image image, Int depth, int iscolor);
COMMON(status)	loadXliImage(Image image, FileObj file, Int bright);
COMMON(void)	ws_create_image_from_x11_data(Image image, unsigned char *data, int w, int h);
COMMON(ColourMap)ws_colour_map_for_image(Image image);
COMMON(void)	ws_system_images(void);
COMMON(void)	ws_prepare_image_mask(Image image);

/* ../src/x11/xstream.c */
COMMON(void)	ws_close_input_stream(Stream s);
COMMON(void)	ws_close_output_stream(Stream s);
COMMON(void)	ws_close_stream(Stream s);
COMMON(void)	ws_input_stream(Stream s);
COMMON(void)	ws_no_input_stream(Stream s);
COMMON(void)	ws_listen_socket(Socket s);
COMMON(status)	ws_write_stream_data(Stream s, void *data, int len);
COMMON(int)	ws_read_stream_data(Stream s, void *data, int len, Real timeout);
COMMON(void)	ws_done_process(Process p);

/* ../src/x11/xtimer.c */
COMMON(void)	ws_status_timer(Timer tm, Name status);

/* ../src/x11/xwindow.c */
COMMON(status)	ws_created_window(PceWindow sw);
COMMON(void)	ws_uncreate_window(PceWindow sw);
COMMON(status)	ws_create_window(PceWindow sw, PceWindow parent);
COMMON(void)	ws_manage_window(PceWindow sw);
COMMON(void)	ws_unmanage_window(PceWindow sw);
COMMON(void)	ws_reassociate_ws_window(PceWindow from, PceWindow to);
COMMON(void)	ws_geometry_window(PceWindow sw, int x, int y, int w, int h, int pen);
COMMON(void)	ws_topmost_window(PceWindow sw, Bool topmost);
COMMON(void)	ws_grab_pointer_window(PceWindow sw, Bool val);
COMMON(void)	ws_grab_keyboard_window(PceWindow sw, Bool val);
COMMON(void)	ws_grab_pointer_window(PceWindow sw, Bool val);
COMMON(void)	ws_grab_keyboard_window(PceWindow sw, Bool val);
COMMON(void)	ws_ungrab_all(void);
COMMON(void)	ws_flash_area_window(PceWindow sw, int x, int y, int w, int h, int msecs);
COMMON(void)	ws_flash_window(PceWindow sw, int msecs);
COMMON(void)	ws_move_pointer(PceWindow sw, int x, int y);
COMMON(void)	ws_window_cursor(PceWindow sw, CursorObj cursor);
COMMON(void)	ws_window_background(PceWindow sw, Any c);
COMMON(void)	ws_raise_window(PceWindow sw);
COMMON(void)	ws_lower_window(PceWindow sw);
COMMON(int)	ws_enable_window(PceWindow sw, int enable);
COMMON(Int)	ws_window_thread(PceWindow sw);
COMMON(int)	ws_delayed_redraw_window(PceWindow sw);

/* ../src/x11/x11.c */
COMMON(void)	ws_initialise(int argc, char **argv);
COMMON(int)	ws_version(void);
COMMON(int)	ws_revision(void);
COMMON(status)	ws_show_console(Name how);
COMMON(status)	ws_console_label(CharArray label);
COMMON(Int)	ws_default_scrollbar_width(void);
COMMON(char *)	ws_user(void);

/* ../src/x11/xmenu.c */
COMMON(status)	ws_draw_scrollbar_arrow(ScrollBar s, int x, int y, int w, int h, Name which, int up);
COMMON(int)	ws_arrow_height_scrollbar(ScrollBar s);
COMMON(status)	ws_draw_sb_thumb(int x, int y, int w, int h);
COMMON(Colour)	ws_3d_grey(void);
COMMON(status)	ws_draw_button_face(DialogItem di, int x, int y, int w, int h, int up, int defb, int focus);
COMMON(int)	ws_combo_box_width(void);
COMMON(int)	ws_stepper_width(void);
COMMON(int)	ws_entry_field_margin();
COMMON(status)	ws_entry_field(int x, int y, int w, int h, int flags);
COMMON(status)	ws_draw_checkbox(int x, int y, int w, int h, int b, int flags);
COMMON(status)	ws_checkbox_size(int flags, int *w, int *h);
COMMON(int)	ws_message_box(Any msg, int flags);

/* ../src/gra/graphstate.c */
COMMON(void)	g_save(void);
COMMON(void)	g_restore(void);
COMMON(int)	g_level(void);
