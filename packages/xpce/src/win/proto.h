
/* win/browser.c */
status		requestGeometryBrowser(Browser b, Int x, Int y, Int w, Int h);
status		makeClassBrowser(Class class);

/* win/decorate.c */
status		requestComputeScrollbarsWindowDecorator(WindowDecorator dw);
status		makeClassWindowDecorator(Class class);

/* win/dialog.c */
status		displayDialog(Dialog d, Graphical item, Point pos);
status		makeClassDialog(Class class);

/* win/display.c */
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
status		loadResourceFileDisplay(DisplayObj d, FileObj f);
status		makeClassDisplay(Class class);

/* win/displaymgr.c */
status		appendDisplayManager(DisplayManager dm, DisplayObj d);
DisplayObj	getMemberDisplayManager(DisplayManager dm, Name address);
DisplayObj	CurrentDisplay(Any obj);
status		RedrawDisplayManager(DisplayManager dm);
status		dispatchDisplayManager(DisplayManager dm, Int fd, Int timeout);
DisplayManager	TheDisplayManager(void);
status		makeClassDisplayManager(Class class);

/* win/frame.c */
Any		getConfirmFrame(FrameObj fr, Point pos, Bool grab, Bool normalise);
Any		getConfirmCenteredFrame(FrameObj fr, Point pos, Bool grab);
status		resizeFrame(FrameObj fr);
status		createdFrame(FrameObj fr);
status		fitFrame(FrameObj fr);
status		statusFrame(FrameObj fr, Name stat);
status		exposeFrame(FrameObj fr);
status		hideFrame(FrameObj fr);
status		forwardColourMapChangeFrame(FrameObj fr);
status		busyCursorFrame(FrameObj fr, CursorObj c, Bool block_events);
Name		getIconLabelFrame(FrameObj fr);
TileObj		getTileFrame(FrameObj fr);
status		AppendFrame(FrameObj fr, PceWindow sw);
status		DeleteFrame(FrameObj fr, PceWindow sw);
PceWindow	getKeyboardFocusFrame(FrameObj fr);
status		eventFrame(FrameObj fr, EventObj ev);
status		makeClassFrame(Class class);

/* win/picture.c */
status		makeClassPicture(Class class);

/* win/resource.c */
char *		resourceName(Name name);
Resource	getSubResource(Resource r, Class class);
status		resourceClass(Class class, Resource r);
Resource	getResourceClass(Class class, Name name);
status		resourceValueClass(Class cl, Name name, Any val);
Any		getResourceValueClass(Class cl, Name name);
status		attach_resource(Class cl, char *name, char *type, char *def, char *doc);
status		refine_resource(Class cl, char *name_s, char *def);
status		variable_resource(Class cl, Name name, char *def);
status		makeClassResource(Class class);
status		load_resource_file(FileObj f);
StringObj	ws_get_resource_value(DisplayObj d, Name cc, Name cn, Name rc, Name rn, int accept_default);

/* win/setup.c */

/* win/tile.c */
status		unrelateTile(TileObj t);
TileObj		getRootTile(TileObj t);
status		setTile(TileObj t, Int x, Int y, Int w, Int h);
status		enforceTile(TileObj t, Bool val);
TileObj		getSubTileToResizeTile(TileObj t, Point pos);
status		makeClassTile(Class class);

/* win/view.c */
status		requestGeometryView(View v, Int x, Int y, Int w, Int h);
status		makeClassView(Class class);

/* win/window.c */
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
status		typedWindow(PceWindow sw, EventId id, Bool delegate);
status		inputFocusWindow(PceWindow sw, Bool val);
status		keyboardFocusWindow(PceWindow sw, Graphical gr);
status		focusWindow(PceWindow sw, Graphical gr, Recogniser recogniser, CursorObj cursor, Name button);
void		changed_window(PceWindow sw, int x, int y, int w, int h, int clear);
status		redrawWindow(PceWindow sw, Area a);
status		RedrawWindow(PceWindow sw);
status		RedrawAreaWindow(PceWindow sw, IArea a, int clear);
status		changedUnionWindow(PceWindow sw, Int ox, Int oy, Int ow, Int oh);
status		bubbleScrollBarWindow(PceWindow sw, ScrollBar sb);
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
