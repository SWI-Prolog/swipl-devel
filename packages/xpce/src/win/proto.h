
/* browser.c */
status		requestGeometryBrowser(Browser b, Int x, Int y, Int w, Int h);
status		makeClassBrowser(Class class);

/* decorate.c */
status		requestComputeScrollbarsWindowDecorator(WindowDecorator dw);
status		makeClassWindowDecorator(Class class);

/* dialog.c */
status		displayDialog(Dialog d, Graphical item, Point pos);
status		makeClassDialog(Class class);

/* display.c */
status		openDisplay(DisplayObj d);
Bool		getOpenDisplay(Any d);
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

/* displaymanager.c */
status		appendDisplayManager(DisplayManager dm, DisplayObj d);
DisplayObj	getMemberDisplayManager(DisplayManager dm, Name address);
DisplayObj	CurrentDisplay(Any obj);
status		RedrawDisplayManager(DisplayManager dm);
status		dispatchDisplayManager(DisplayManager dm, Int fd, Int timeout);
DisplayManager	TheDisplayManager(void);
status		makeClassDisplayManager(Class class);

/* frame.c */
Any		getConfirmFrame(FrameObj fr, Point pos, Bool grab, Bool normalise);
Any		getConfirmCenteredFrame(FrameObj fr, Point pos, Bool grab);
status		resizeFrame(FrameObj fr);
status		createdFrame(FrameObj fr);
status		fitFrame(FrameObj fr);
status		statusFrame(FrameObj fr, Name stat);
status		exposeFrame(FrameObj fr);
status		hideFrame(FrameObj fr);
status		busyCursorFrame(FrameObj fr, CursorObj c, Bool block_events);
Name		getIconLabelFrame(FrameObj fr);
TileObj		getTileFrame(FrameObj fr);
status		AppendFrame(FrameObj fr, PceWindow sw);
status		DeleteFrame(FrameObj fr, PceWindow sw);
PceWindow	getKeyboardFocusFrame(FrameObj fr);
status		eventFrame(FrameObj fr, EventObj ev);
status		makeClassFrame(Class class);

/* picture.c */
status		makeClassPicture(Class class);

/* resource.c */
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

/* setup.c */

/* tile.c */
status		unrelateTile(TileObj t);
status		leftTile(TileObj t, Any obj);
status		rightTile(TileObj t, Any obj);
status		aboveTile(TileObj t, Any obj);
status		belowTile(TileObj t, Any obj);
TileObj		getRootTile(TileObj t);
status		setTile(TileObj t, Int x, Int y, Int w, Int h);
status		enforceTile(TileObj t);
status		makeClassTile(Class class);

/* view.c */
status		requestGeometryView(View v, Int x, Int y, Int w, Int h);
status		makeClassView(Class class);

/* window.c */
status		initialiseWindow(PceWindow sw, Name label, Size size, DisplayObj display);
status		createdWindow(PceWindow sw);
status		grabPointerWindow(PceWindow sw, Bool val);
status		grabKeyboardWindow(PceWindow sw, Bool val);
status		unlinkWindow(PceWindow sw);
status		updatePositionWindow(PceWindow sw);
status		resizeWindow(PceWindow sw);
void		offset_window(PceWindow sw, int *x, int *y);
void		compute_window(PceWindow sw, int *x, int *y, int *w, int *h);
status		frame_offset_window(PceWindow w, FrameObj *fr, int *X, int *Y);
void		offset_windows(PceWindow w1, PceWindow w2, int *X, int *Y);
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
FrameObj	getFrameWindow(PceWindow sw);
status		flushWindow(PceWindow sw);
status		makeClassWindow(Class class);
