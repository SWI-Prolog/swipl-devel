#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/win/browser.c */
COMMON(status)	makeClassBrowser(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/decorate.c */
COMMON(status)	requestComputeScrollbarsWindowDecorator(WindowDecorator dw);
COMMON(status)	makeClassWindowDecorator(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/dialog.c */
COMMON(status)	makeClassDialog(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/display.c */
COMMON(status)	openDisplay(DisplayObj d);
COMMON(Bool)	getOpenDisplay(Any d);
COMMON(status)	drawInDisplay(DisplayObj d, Graphical gr, Point pos, Bool invert, Bool subtoo);
COMMON(status)	grabServerDisplay(DisplayObj d, Bool val);
COMMON(status)	dispatchDisplay(DisplayObj d);
COMMON(status)	flushDisplay(DisplayObj d);
COMMON(status)	synchroniseDisplay(DisplayObj d);
COMMON(status)	bellDisplay(DisplayObj d, Int vol);
COMMON(Size)	getSizeDisplay(DisplayObj d);
COMMON(Int)	getWidthDisplay(DisplayObj d);
COMMON(Int)	getHeightDisplay(DisplayObj d);
COMMON(status)	looseSelectionDisplay(DisplayObj d, Name which);
COMMON(status)	confirmDisplay(DisplayObj d, CharArray fmt, int argc, Any *argv);
COMMON(status)	informDisplay(DisplayObj d, CharArray fmt, int argc, Any *argv);
COMMON(status)	busyCursorDisplay(DisplayObj d, CursorObj c, Bool block_events);
COMMON(status)	inspectDisplay(DisplayObj d, Graphical gr, EventObj ev);
COMMON(status)	makeClassDisplay(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/displaymgr.c */
COMMON(status)	appendDisplayManager(DisplayManager dm, DisplayObj d);
COMMON(DisplayObj) getMemberDisplayManager(DisplayManager dm, Name address);
COMMON(DisplayObj) CurrentDisplay(Any obj);
COMMON(status)	RedrawDisplayManager(DisplayManager dm);
COMMON(status)	dispatchDisplayManager(DisplayManager dm, Int fd, Int timeout);
COMMON(DisplayManager) TheDisplayManager(void);
COMMON(status)	makeClassDisplayManager(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/frame.c */
COMMON(Any)	getConfirmFrame(FrameObj fr, Point pos, Bool grab, Bool normalise);
COMMON(Any)	getConfirmCenteredFrame(FrameObj fr, Point pos, Bool grab);
COMMON(status)	createdFrame(FrameObj fr);
COMMON(status)	exposeFrame(FrameObj fr);
COMMON(status)	hideFrame(FrameObj fr);
COMMON(status)	forwardColourMapChangeFrame(FrameObj fr);
COMMON(status)	busyCursorFrame(FrameObj fr, CursorObj c, Bool block_events);
COMMON(Name)	getIconLabelFrame(FrameObj fr);
COMMON(TileObj)	getTileFrame(FrameObj fr);
COMMON(status)	AppendFrame(FrameObj fr, PceWindow sw);
COMMON(status)	DeleteFrame(FrameObj fr, PceWindow sw);
COMMON(PceWindow) getKeyboardFocusFrame(FrameObj fr);
COMMON(status)	redrawFrame(FrameObj fr, Area a);
COMMON(FrameObj) blockedByModalFrame(FrameObj fr);
COMMON(status)	eventFrame(FrameObj fr, EventObj ev);
COMMON(status)	makeClassFrame(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/picture.c */
COMMON(status)	makeClassPicture(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/tileadjust.c */
COMMON(status)	makeClassTileAdjuster(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/setup.c */

/* /staff/jan/src/pl/packages/xpce/src/win/tile.c */
COMMON(status)	unrelateTile(TileObj t);
COMMON(TileObj)	getRootTile(TileObj t);
COMMON(status)	distribute_stretches(stretch *s, int n, int w);
COMMON(void)	sum_stretches(stretch *sp, int len, stretch *r);
COMMON(void)	join_stretches(stretch *stretches, int len, stretch *r);
COMMON(status)	setTile(TileObj t, Int x, Int y, Int w, Int h);
COMMON(status)	enforceTile(TileObj t, Bool val);
COMMON(Bool)	getCanResizeTile(TileObj t);
COMMON(status)	updateAdjusterPositionTile(TileObj t);
COMMON(TileObj)	getSubTileToResizeTile(TileObj t, Point pos);
COMMON(status)	makeClassTile(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/view.c */
COMMON(status)	makeClassView(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/window.c */
COMMON(status)	initialiseWindow(PceWindow sw, Name label, Size size, DisplayObj display);
COMMON(status)	createdWindow(PceWindow sw);
COMMON(status)	grabPointerWindow(PceWindow sw, Bool val);
COMMON(status)	grabKeyboardWindow(PceWindow sw, Bool val);
COMMON(status)	unlinkWindow(PceWindow sw);
COMMON(PceWindow) userWindow(PceWindow sw);
COMMON(status)	updatePositionWindow(PceWindow sw);
COMMON(status)	resizeWindow(PceWindow sw);
COMMON(void)	offset_window(PceWindow sw, int *x, int *y);
COMMON(void)	compute_window(PceWindow sw, int *x, int *y, int *w, int *h);
COMMON(status)	frame_offset_window(Any obj, FrameObj *fr, int *X, int *Y);
COMMON(void)	offset_windows(PceWindow w1, Any w2, int *X, int *Y);
COMMON(int)	is_service_window(PceWindow sw);
COMMON(status)	postEventWindow(PceWindow sw, EventObj ev);
COMMON(status)	typedWindow(PceWindow sw, EventId id, Bool delegate);
COMMON(status)	inputFocusWindow(PceWindow sw, Bool val);
COMMON(status)	keyboardFocusWindow(PceWindow sw, Graphical gr);
COMMON(status)	focusWindow(PceWindow sw, Graphical gr, Recogniser recogniser, CursorObj cursor, Name button);
COMMON(status)	computeWindow(PceWindow sw);
COMMON(void)	changed_window(PceWindow sw, int x, int y, int w, int h, int clear);
COMMON(void)	unlink_changes_data_window(PceWindow sw);
COMMON(status)	redrawWindow(PceWindow sw, Area a);
COMMON(status)	RedrawWindow(PceWindow sw);
COMMON(status)	RedrawAreaWindow(PceWindow sw, IArea a, int clear);
COMMON(status)	changedUnionWindow(PceWindow sw, Int ox, Int oy, Int ow, Int oh);
COMMON(status)	pointerWindow(PceWindow sw, Point pos);
COMMON(status)	focusCursorWindow(PceWindow sw, CursorObj cursor);
COMMON(status)	updateCursorWindow(PceWindow sw);
COMMON(status)	geometryWindow(PceWindow sw, Int X, Int Y, Int W, Int H);
COMMON(status)	requestGeometryWindow(PceWindow sw, Int X, Int Y, Int W, Int H);
COMMON(status)	get_display_position_window(PceWindow sw, int *X, int *Y);
COMMON(PceWindow) getUserWindow(PceWindow sw);
COMMON(status)	frameWindow(PceWindow sw, FrameObj frame);
COMMON(TileObj)	getTileWindow(PceWindow sw);
COMMON(FrameObj) getFrameWindow(PceWindow sw, Bool create);
COMMON(status)	flushWindow(PceWindow sw);
COMMON(status)	flashWindow(PceWindow sw, Area a, Int time);
COMMON(status)	makeClassWindow(Class class);

/* /staff/jan/src/pl/packages/xpce/src/win/application.c */
COMMON(void)	resetApplications(void);
COMMON(status)	makeClassApplication(Class class);
