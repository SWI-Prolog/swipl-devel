
/* ../src/win/browser.c */
status		makeClassBrowser(Class class);

/* ../src/win/decorate.c */
status		requestComputeScrollbarsWindowDecorator(WindowDecorator dw);
status		makeClassWindowDecorator(Class class);

/* ../src/win/dialog.c */
status		makeClassDialog(Class class);

/* ../src/win/display.c */
status		openDisplay(DisplayObj d);
Bool		getOpenDisplay(Any d);
status		drawInDisplay(DisplayObj d, Graphical gr, Point pos, Bool invert, Bool subtoo);
status		grabServerDisplay(DisplayObj d, Bool val);
status		dispatchDisplay(DisplayObj d);
status		flushDisplay(DisplayObj d);
status		synchroniseDisplay(DisplayObj d);
status		bellDisplay(DisplayObj d, Int vol);
Size		getSizeDisplay(DisplayObj d);
Int		getWidthDisplay(DisplayObj d);
Int		getHeightDisplay(DisplayObj d);
status		looseSelectionDisplay(DisplayObj d, Name which);
status		confirmDisplay(DisplayObj d, CharArray fmt, int argc, Any *argv);
status		informDisplay(DisplayObj d, CharArray fmt, int argc, Any *argv);
status		busyCursorDisplay(DisplayObj d, CursorObj c, Bool block_events);
status		inspectDisplay(DisplayObj d, Graphical gr, EventObj ev);
status		makeClassDisplay(Class class);

/* ../src/win/displaymgr.c */
status		appendDisplayManager(DisplayManager dm, DisplayObj d);
DisplayObj	getMemberDisplayManager(DisplayManager dm, Name address);
DisplayObj	CurrentDisplay(Any obj);
status		RedrawDisplayManager(DisplayManager dm);
status		dispatchDisplayManager(DisplayManager dm, Int fd, Int timeout);
DisplayManager	TheDisplayManager(void);
status		makeClassDisplayManager(Class class);

/* ../src/win/frame.c */
Any		getConfirmFrame(FrameObj fr, Point pos, Bool grab, Bool normalise);
Any		getConfirmCenteredFrame(FrameObj fr, Point pos, Bool grab);
status		createdFrame(FrameObj fr);
status		exposeFrame(FrameObj fr);
status		hideFrame(FrameObj fr);
status		forwardColourMapChangeFrame(FrameObj fr);
status		busyCursorFrame(FrameObj fr, CursorObj c, Bool block_events);
Name		getIconLabelFrame(FrameObj fr);
TileObj		getTileFrame(FrameObj fr);
status		AppendFrame(FrameObj fr, PceWindow sw);
status		DeleteFrame(FrameObj fr, PceWindow sw);
PceWindow	getKeyboardFocusFrame(FrameObj fr);
FrameObj	blockedByModalFrame(FrameObj fr);
status		eventFrame(FrameObj fr, EventObj ev);
status		makeClassFrame(Class class);

/* ../src/win/picture.c */
status		makeClassPicture(Class class);

/* ../src/win/setup.c */

/* ../src/win/tile.c */
status		unrelateTile(TileObj t);
TileObj		getRootTile(TileObj t);
status		distribute_stretches(stretch *s, int n, int w);
void		sum_stretches(stretch *sp, int len, stretch *r);
void		join_stretches(stretch *stretches, int len, stretch *r);
status		setTile(TileObj t, Int x, Int y, Int w, Int h);
status		enforceTile(TileObj t, Bool val);
Bool		getCanResizeTile(TileObj t);
TileObj		getSubTileToResizeTile(TileObj t, Point pos);
status		makeClassTile(Class class);

/* ../src/win/view.c */
status		makeClassView(Class class);

/* ../src/win/window.c */
status		initialiseWindow(PceWindow sw, Name label, Size size, DisplayObj display);
status		createdWindow(PceWindow sw);
status		grabPointerWindow(PceWindow sw, Bool val);
status		grabKeyboardWindow(PceWindow sw, Bool val);
status		unlinkWindow(PceWindow sw);
PceWindow	userWindow(PceWindow sw);
status		updatePositionWindow(PceWindow sw);
status		resizeWindow(PceWindow sw);
void		offset_window(PceWindow sw, int *x, int *y);
void		compute_window(PceWindow sw, int *x, int *y, int *w, int *h);
status		frame_offset_window(Any obj, FrameObj *fr, int *X, int *Y);
void		offset_windows(PceWindow w1, Any w2, int *X, int *Y);
int		is_service_window(PceWindow sw);
status		typedWindow(PceWindow sw, EventId id, Bool delegate);
status		inputFocusWindow(PceWindow sw, Bool val);
status		keyboardFocusWindow(PceWindow sw, Graphical gr);
status		focusWindow(PceWindow sw, Graphical gr, Recogniser recogniser, CursorObj cursor, Name button);
status		computeWindow(PceWindow sw);
void		changed_window(PceWindow sw, int x, int y, int w, int h, int clear);
void		unlink_changes_data_window(PceWindow sw);
status		redrawWindow(PceWindow sw, Area a);
status		RedrawWindow(PceWindow sw);
status		RedrawAreaWindow(PceWindow sw, IArea a, int clear);
status		changedUnionWindow(PceWindow sw, Int ox, Int oy, Int ow, Int oh);
status		pointerWindow(PceWindow sw, Point pos);
status		focusCursorWindow(PceWindow sw, CursorObj cursor);
status		updateCursorWindow(PceWindow sw);
status		geometryWindow(PceWindow sw, Int X, Int Y, Int W, Int H);
status		requestGeometryWindow(PceWindow sw, Int X, Int Y, Int W, Int H);
status		get_display_position_window(PceWindow sw, int *X, int *Y);
PceWindow	getUserWindow(PceWindow sw);
status		frameWindow(PceWindow sw, FrameObj frame);
TileObj		getTileWindow(PceWindow sw);
FrameObj	getFrameWindow(PceWindow sw, Bool create);
status		flushWindow(PceWindow sw);
status		flashWindow(PceWindow sw, Area a, Int time);
status		makeClassWindow(Class class);

/* ../src/win/application.c */
void		resetApplications(void);
status		makeClassApplication(Class class);
