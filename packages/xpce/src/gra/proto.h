#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/gra/arc.c */
COMMON(void)	points_arc(Arc a, int *sx, int *sy, int *ex, int *ey);
COMMON(status)	makeClassArc(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/arrow.c */
COMMON(status)	makeClassArrow(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/bitmap.c */
COMMON(status)	updateSolidBitmap(BitmapObj bm);
COMMON(status)	makeClassBitmap(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/box.c */
COMMON(status)	makeClassBox(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/circle.c */
COMMON(status)	makeClassCircle(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/colour.c */
COMMON(Int)	getRedColour(Colour c);
COMMON(Int)	getGreenColour(Colour c);
COMMON(Int)	getBlueColour(Colour c);
COMMON(Colour)	getHiliteColour(Colour c, Real h);
COMMON(Colour)	getReduceColour(Colour c, Real re);
COMMON(status)	makeClassColour(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/connection.c */
COMMON(status)	updateHideExposeConnection(Connection c);
COMMON(status)	updateDeviceConnection(Connection c);
COMMON(status)	makeClassConnection(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/cursor.c */
COMMON(status)	makeClassCursor(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/device.c */
COMMON(status)	initialiseDevice(Device dev);
COMMON(status)	unlinkDevice(Device dev);
COMMON(CursorObj) getDisplayedCursorDevice(Device dev);
COMMON(Chain)	getPointedObjectsDevice(Device dev, Any pos, Chain ch);
COMMON(status)	inspectDevice(Device dev, EventObj ev);
COMMON(status)	eventDevice(Any obj, EventObj ev);
COMMON(Button)	getDefaultButtonDevice(Device d);
COMMON(status)	advanceDevice(Device dev, Graphical gr, Bool propagate, Name direction);
COMMON(status)	requestComputeDevice(Device dev, Any val);
COMMON(status)	computeGraphicalsDevice(Device dev);
COMMON(status)	computeLayoutDevice(Device dev);
COMMON(status)	computeDevice(Any obj);
COMMON(status)	updateBoundingBoxDevice(Device dev, Int *od);
COMMON(status)	computeBoundingBoxDevice(Device dev);
COMMON(status)	EnterRedrawAreaDevice(Device dev, Area a, DeviceDrawContext ctx);
COMMON(void)	ExitRedrawAreaDevice(Device dev, Area a, DeviceDrawContext ctx);
COMMON(status)	RedrawAreaDevice(Device dev, Area a);
COMMON(status)	flashDevice(Device dev, Area a, Int time);
COMMON(status)	clearDevice(Device dev, Name how);
COMMON(status)	displayDevice(Any Dev, Any Gr, Point pos);
COMMON(status)	appendDevice(Device dev, Graphical gr);
COMMON(status)	subGraphical(Graphical gr, Graphical sub);
COMMON(status)	eraseDevice(Device dev, Graphical gr);
COMMON(status)	displayedGraphicalDevice(Device dev, Graphical gr, Bool val);
COMMON(status)	exposeDevice(Device dev, Graphical gr, Graphical gr2);
COMMON(status)	hideDevice(Device dev, Graphical gr, Graphical gr2);
COMMON(status)	swapGraphicalsDevice(Device dev, Graphical gr, Graphical gr2);
COMMON(status)	layoutDialogDevice(Device d, Size gap, Size bb, Size border);
COMMON(status)	appendDialogItemDevice(Device d, Graphical item, Name where);
COMMON(Graphical) getMemberDevice(Device dev, Name name);
COMMON(status)	updateConnectionsDevice(Device dev, Int level);
COMMON(status)	geometryDevice(Device dev, Int x, Int y, Int w, Int h);
COMMON(status)	makeClassDevice(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/ellipse.c */
COMMON(status)	makeClassEllipse(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/figure.c */
COMMON(status)	initialiseFigure(Figure f);
COMMON(Any)	RedrawBoxFigure(Figure f, Area area);
COMMON(status)	makeClassFigure(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/font.c */
COMMON(status)	replaceFont(FontObj f, DisplayObj d);
COMMON(status)	makeBuiltinFonts(void);
COMMON(Int)	getWidthFont(FontObj f, CharArray txt);
COMMON(Int)	getAdvanceFont(FontObj f, CharArray txt);
COMMON(Int)	getExFont(FontObj f);
COMMON(Int)	getHeightFont(FontObj f);
COMMON(Int)	getAscentFont(FontObj f);
COMMON(Int)	getDescentFont(FontObj f);
COMMON(Bool)	getFixedWidthFont(FontObj f);
COMMON(Bool)	getB16Font(FontObj f);
COMMON(status)	makeClassFont(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/format.c */
COMMON(status)	makeClassFormat(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/graphical.c */
COMMON(status)	initialiseGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	unlinkGraphical(Graphical gr);
COMMON(status)	copyGraphical(Any obj1, Any obj2);
COMMON(status)	DeviceGraphical(Any obj, Device dev);
COMMON(status)	deviceGraphical(Any obj, Device dev);
COMMON(status)	reparentGraphical(Graphical gr);
COMMON(status)	DisplayedGraphical(Any obj, Bool val);
COMMON(status)	displayedGraphical(Any obj, Bool val);
COMMON(Bool)	getIsDisplayedGraphical(Graphical gr, Device dev);
COMMON(status)	initialiseDeviceGraphical(Any obj, int *x, int *y, int *w, int *h);
COMMON(status)	initialiseRedrawAreaGraphical(Any obj, Area a, int *x, int *y, int *w, int *h, IArea redraw);
COMMON(status)	offsetDeviceGraphical(Any obj, int *x, int *y);
COMMON(Area)	getAbsoluteAreaGraphical(Graphical gr, Device device);
COMMON(Graphical) getRootGraphical(Graphical gr);
COMMON(PceWindow) getWindowGraphical(Graphical gr);
COMMON(FrameObj) getFrameGraphical(Graphical gr);
COMMON(DisplayObj) getDisplayGraphical(Graphical gr);
COMMON(Application) getApplicationGraphical(Graphical gr);
COMMON(Device)	getCommonDeviceGraphical(Graphical gr1, Graphical gr2);
COMMON(int)	get_extension_margin_graphical(Graphical gr);
COMMON(status)	changedAreaGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	changedImageGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	changedEntireImageGraphical(Any obj);
COMMON(status)	redrawGraphical(Graphical gr, Area a);
COMMON(status)	requestComputeGraphical(Any obj, Any val);
COMMON(status)	ComputeGraphical(Any obj);
COMMON(status)	RedrawArea(Any obj, Area area);
COMMON(status)	paintSelectedGraphical(Graphical gr);
COMMON(status)	RedrawAreaGraphical(Any obj, Area area);
COMMON(status)	flushGraphical(Any gr);
COMMON(status)	synchroniseGraphical(Graphical gr, Bool always);
COMMON(status)	exposeGraphical(Any obj1, Any obj2);
COMMON(status)	setGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	doSetGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	requestGeometryGraphical(Any gr, Int x, Int y, Int w, Int h);
COMMON(status)	geometryGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	xGraphical(Graphical gr, Int x);
COMMON(status)	yGraphical(Graphical gr, Int y);
COMMON(status)	heightGraphical(Graphical gr, Int h);
COMMON(status)	positionGraphical(Graphical gr, Point pos);
COMMON(status)	centerGraphical(Graphical gr, Point pos);
COMMON(status)	relativeMoveGraphical(Graphical gr, Point pos);
COMMON(status)	init_resize_graphical(Any obj, Real xfactor, Real yfactor, Point origin, float *xf, float *yf, int *ox, int *oy);
COMMON(Area)	getAreaGraphical(Graphical gr);
COMMON(Int)	getXGraphical(Graphical gr);
COMMON(Int)	getYGraphical(Graphical gr);
COMMON(Int)	getWidthGraphical(Graphical gr);
COMMON(Int)	getHeightGraphical(Graphical gr);
COMMON(Int)	getLeftSideGraphical(Graphical gr);
COMMON(Int)	getRightSideGraphical(Graphical gr);
COMMON(Int)	getBottomSideGraphical(Graphical gr);
COMMON(Point)	getPositionGraphical(Graphical gr);
COMMON(status)	get_absolute_xy_graphical(Graphical gr, Device *dev, Int *X, Int *Y);
COMMON(Int)	getAbsoluteXGraphical(Any gr, Device dev);
COMMON(Int)	getAbsoluteYGraphical(Any gr, Device dev);
COMMON(Point)	getDisplayPositionGraphical(Graphical gr);
COMMON(Size)	getSizeGraphical(Graphical gr);
COMMON(status)	appendDialogItemNetworkDevice(Device dev, Graphical gr1);
COMMON(status)	aboveGraphical(Graphical gr1, Graphical gr2);
COMMON(status)	belowGraphical(Graphical gr1, Graphical gr2);
COMMON(status)	rightGraphical(Graphical gr1, Graphical gr2);
COMMON(status)	leftGraphical(Graphical gr1, Graphical gr2);
COMMON(status)	referenceGraphical(Graphical gr, Point ref);
COMMON(status)	penGraphical(Graphical gr, Int pen);
COMMON(status)	shadowGraphical(Graphical gr, Int s);
COMMON(status)	fillPatternGraphical(Graphical gr, Image pattern);
COMMON(status)	fillOffsetGraphical(Graphical gr, Point pattern);
COMMON(status)	colourGraphical(Graphical gr, Any c);
COMMON(Any)	getDisplayColourGraphical(Graphical gr);
COMMON(Handle)	getHandleGraphical(Graphical gr, Name name);
COMMON(Point)	getHandlePositionGraphical(Graphical gr, Name name, Device dev);
COMMON(Chain)	getHandlesGraphical(Graphical gr, Point pos, Name kind, Int distance);
COMMON(status)	activeGraphical(Graphical gr, Bool val);
COMMON(status)	focusCursorGraphical(Graphical gr, CursorObj cursor);
COMMON(status)	focusGraphical(Graphical gr, Recogniser recogniser, CursorObj cursor, Name button);
COMMON(status)	updateConnectionsGraphical(Graphical gr, Int level);
COMMON(status)	connectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
COMMON(status)	attachConnectionGraphical(Graphical gr, Connection c);
COMMON(status)	detachConnectionGraphical(Graphical gr, Connection c);
COMMON(status)	connectedGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
COMMON(status)	disconnectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
COMMON(status)	eventGraphical(Any obj, EventObj ev);
COMMON(Bool)	getKeyboardFocusGraphical(Graphical gr);
COMMON(status)	generateEventGraphical(Graphical gr, Name name);
COMMON(status)	inEventAreaGraphical(Graphical gr, Int xc, Int yc);
COMMON(Chain)	getAllRecognisersGraphical(Any obj, Bool create);
COMMON(status)	assignGraphical(Any obj, Name slot, Any value);
COMMON(status)	flashGraphical(Graphical gr, Area a, Int time);
COMMON(status)	alertGraphical(Graphical gr);
COMMON(Node)	getNodeGraphical(Graphical gr);
COMMON(status)	pointerGraphical(Graphical gr, Point pos);
COMMON(Any)	getMasterGraphical(Graphical gr);
COMMON(status)	nameGraphical(Graphical gr, Name name);
COMMON(Any)	getContainedInGraphical(Graphical gr);
COMMON(status)	initialiseNewSlotGraphical(Graphical gr, Variable new);
COMMON(status)	clipGraphical(Graphical gr, Area a);
COMMON(status)	unclipGraphical(Graphical gr);
COMMON(status)	makeClassGraphical(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/handle.c */
COMMON(status)	getXYHandle(Handle h, Graphical gr, Device dev, Int *X, Int *Y);
COMMON(Int)	getXHandle(Handle h, Graphical gr, Device dev);
COMMON(Int)	getYHandle(Handle h, Graphical gr, Device dev);
COMMON(status)	makeClassHandle(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/image.c */
COMMON(status)	initialiseImage(Image image, SourceSink data, Int w, Int h, Name kind);
COMMON(Image)	getConvertImage(Class class, Any obj);
COMMON(status)	XopenImage(Image image, DisplayObj d);
COMMON(status)	XcloseImage(Image image, DisplayObj d);
COMMON(status)	loadImage(Image image, SourceSink file, CharArray path);
COMMON(status)	fillImage(Image image, Any pattern, Area area);
COMMON(Image)	getMonochromeImage(Image image);
COMMON(status)	makeClassImage(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/joint.c */
COMMON(status)	initialiseJoint(Joint jt, Int x, Int y, Int w, Int h, Name arrows);
COMMON(status)	copyJoint(Joint jt1, Joint jt2);
COMMON(status)	setArrowsJoint(Joint jt, Graphical first, Graphical second);
COMMON(status)	makeClassJoint(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/line.c */
COMMON(status)	initialiseLine(Line ln, Int xa, Int ya, Int xb, Int yb, Name arrows);
COMMON(status)	adjustFirstArrowLine(Line ln);
COMMON(status)	adjustSecondArrowLine(Line ln);
COMMON(status)	computeLine(Line ln);
COMMON(status)	copyLine(Line l1, Line l2);
COMMON(status)	paintSelectedLine(Line ln);
COMMON(status)	pointsLine(Line ln, Int sx, Int sy, Int ex, Int ey);
COMMON(int)	distanceLineToPoint(int x1, int y1, int x2, int y2, int px, int py, int extended);
COMMON(Point)	getIntersectionLine(Line l1, Line l2);
COMMON(Real)	getAngleLine(Line ln, Point p);
COMMON(status)	makeClassLine(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/link.c */
COMMON(status)	makeClassLink(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/listbrowser.c */
COMMON(status)	requestGeometryListBrowser(ListBrowser lb, Int x, Int y, Int w, Int h);
COMMON(Size)	getSizeListBrowser(ListBrowser lb);
COMMON(status)	executeSearchListBrowser(ListBrowser lb);
COMMON(status)	typedListBrowser(ListBrowser lb, EventId id);
COMMON(DictItem) getDictItemListBrowser(ListBrowser lb, EventObj ev);
COMMON(Any)	selectBrowserGesture(void);
COMMON(status)	forwardListBrowser(ListBrowser lb, Name action);
COMMON(status)	selectedListBrowser(ListBrowser lb, DictItem di);
COMMON(status)	selectionListBrowser(ListBrowser lb, Any obj);
COMMON(Any)	getSelectionListBrowser(ListBrowser lb);
COMMON(status)	scrollToListBrowser(ListBrowser lb, Int index);
COMMON(status)	normaliseListBrowser(ListBrowser lb, DictItem di);
COMMON(status)	backgroundListBrowser(ListBrowser lb, Any bg);
COMMON(DictItem) getMemberListBrowser(ListBrowser lb, Any key);
COMMON(Chain)	getContainsListBrowser(ListBrowser lb);
COMMON(status)	makeClassListBrowser(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/node.c */
COMMON(status)	updateDisplayedTree(Tree t);
COMMON(status)	relateImageNode(Node n, Node n2);
COMMON(status)	forAllNode(Node n, Code msg);
COMMON(status)	forSomeNode(Node n, Code msg);
COMMON(Node)	getFindNodeNode(Node n, Graphical gr);
COMMON(status)	makeClassNode(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/path.c */
COMMON(status)	adjustFirstArrowPath(Path p);
COMMON(status)	adjustSecondArrowPath(Path p);
COMMON(status)	makeClassPath(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/postscript.c */
COMMON(StringObj) getPostscriptObject(Any obj, Bool ls, Area a);
COMMON(void)	ps_put_char(int c);
COMMON(void)	ps_output(char *fm, ...);
COMMON(status)	ps_font(FontObj font);
COMMON(status)	postscriptDrawable(int ox, int oy, int w, int h, int depth, int iscolor);
COMMON(Sheet)	makePSDefinitions(void);
COMMON(status)	postscriptGraphical(Any obj);
COMMON(status)	drawPostScriptDevice(Device dev);
COMMON(status)	drawPostScriptFigure(Figure f);
COMMON(status)	drawPostScriptTree(Tree tree);
COMMON(status)	drawPostScriptBox(Box b);
COMMON(status)	drawPostScriptCircle(Circle c);
COMMON(status)	drawPostScriptEllipse(Ellipse e);
COMMON(status)	drawPostScriptPath(Path p);
COMMON(status)	drawPostScriptBezier(Bezier b);
COMMON(status)	drawPostScriptLine(Line ln);
COMMON(status)	drawPostScriptArrow(Arrow a);
COMMON(status)	drawPostScriptArc(Arc a);
COMMON(status)	drawPostScriptBitmap(BitmapObj bm);
COMMON(status)	drawPostScriptImage(Image image);
COMMON(status)	drawPostScriptText(TextObj t);
COMMON(status)	postscriptFrame(FrameObj fr);
COMMON(status)	postscriptDisplay(DisplayObj d);

/* /staff/jan/src/pl/packages/xpce/src/gra/scrollbar.c */
COMMON(Int)	getMarginScrollBar(ScrollBar sb);
COMMON(status)	placeScrollBar(ScrollBar sb, Graphical gr);
COMMON(status)	bubbleScrollBar(ScrollBar sb, Int l, Int s, Int v);
COMMON(status)	makeClassScrollBar(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/text.c */
COMMON(void)	str_format(String out, const String in, const int width, const FontObj font);
COMMON(status)	repaintText(TextObj t, int x, int y, int w, int h);
COMMON(Int)	get_pointed_text(TextObj t, int x, int y);
COMMON(status)	transparentText(TextObj t, Bool val);
COMMON(status)	fontText(TextObj t, FontObj font);
COMMON(status)	borderText(TextObj t, Int border);
COMMON(status)	stringText(TextObj t, CharArray s);
COMMON(status)	showCaretText(TextObj t, Any val);
COMMON(status)	pasteText(TextObj t, Int buffer);
COMMON(status)	lengthText(TextObj t, Int l);
COMMON(status)	marginText(TextObj t, Int width, Name wrap);
COMMON(status)	makeClassText(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/tree.c */
COMMON(status)	requestComputeTree(Tree t);
COMMON(status)	displayTree(Tree t, Node n);
COMMON(status)	unzoomTree(Tree t);
COMMON(status)	zoomTree(Tree t, Node n);
COMMON(status)	makeClassTree(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/visual.c */
COMMON(status)	resetVisual(VisualObj v);
COMMON(status)	destroyVisual(VisualObj v);
COMMON(Any)	getReportToVisual(VisualObj v);
COMMON(status)	reportVisual(VisualObj v, Name kind, CharArray fmt, int argc, Any *argv);
COMMON(status)	alertReporteeVisual(Any v);
COMMON(status)	makeClassVisual(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/pixmap.c */
COMMON(Colour)	getReplacementColourPixmap(PixmapObj pm);
COMMON(status)	makeClassPixmap(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/elevation.c */
COMMON(Elevation) getModifyElevation(Elevation e, Name att, Any val);
COMMON(status)	makeClassElevation(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/pen.c */
COMMON(status)	makeClassPen(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/draw.c */
COMMON(void)	r_3d_rectangular_polygon(int n, IPoint pts, Elevation e, int flags);

/* /staff/jan/src/pl/packages/xpce/src/gra/colourmap.c */
COMMON(status)	makeClassColourMap(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/bezier.c */
COMMON(status)	adjustFirstArrowBezier(Bezier b);
COMMON(status)	adjustSecondArrowBezier(Bezier b);
COMMON(status)	makeClassBezier(Class class);

/* /staff/jan/src/pl/packages/xpce/src/gra/hsv.c */
COMMON(void)	RGBToHSV(float r, float g, float b, float *H, float *S, float *V);
COMMON(void)	HSVToRGB(float hue, float sat, float V, float *R, float *G, float *B);
