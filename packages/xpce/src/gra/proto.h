
/* ../src/gra/arc.c */
void		points_arc(Arc a, int *sx, int *sy, int *ex, int *ey);
status		makeClassArc(Class class);

/* ../src/gra/arrow.c */
status		makeClassArrow(Class class);

/* ../src/gra/bitmap.c */
status		updateSolidBitmap(BitmapObj bm);
status		makeClassBitmap(Class class);

/* ../src/gra/box.c */
status		makeClassBox(Class class);

/* ../src/gra/circle.c */
status		makeClassCircle(Class class);

/* ../src/gra/colour.c */
Int		getRedColour(Colour c);
Int		getGreenColour(Colour c);
Int		getBlueColour(Colour c);
Colour		getHiliteColour(Colour c);
Colour		getReduceColour(Colour c);
status		makeClassColour(Class class);

/* ../src/gra/connection.c */
status		updateHideExposeConnection(Connection c);
status		updateDeviceConnection(Connection c);
status		makeClassConnection(Class class);

/* ../src/gra/cursor.c */
status		makeClassCursor(Class class);

/* ../src/gra/device.c */
status		initialiseDevice(Device dev);
status		unlinkDevice(Device dev);
CursorObj	getDisplayedCursorDevice(Device dev);
Chain		getPointedObjectsDevice(Device dev, Any pos, Chain ch);
status		inspectDevice(Device dev, EventObj ev);
status		eventDevice(Any obj, EventObj ev);
status		advanceDevice(Device dev, Graphical gr, Bool propagate);
status		requestComputeDevice(Device dev, Any val);
status		computeGraphicalsDevice(Device dev);
status		computeLayoutDevice(Device dev);
status		computeDevice(Any obj);
status		updateBoundingBoxDevice(Device dev, Int *od);
status		computeBoundingBoxDevice(Device dev);
status		EnterRedrawAreaDevice(Device dev, Area a, DeviceDrawContext ctx);
void		ExitRedrawAreaDevice(Device dev, Area a, DeviceDrawContext ctx);
status		RedrawAreaDevice(Device dev, Area a);
status		flashDevice(Device dev, Area a, Int time);
status		clearDevice(Device dev);
status		displayDevice(Any Dev, Any Gr, Point pos);
status		appendDevice(Device dev, Graphical gr);
status		eraseDevice(Device dev, Graphical gr);
status		displayedGraphicalDevice(Device dev, Graphical gr, Bool val);
status		exposeDevice(Device dev, Graphical gr, Graphical gr2);
status		hideDevice(Device dev, Graphical gr, Graphical gr2);
status		swapGraphicalsDevice(Device dev, Graphical gr, Graphical gr2);
status		layoutDialogDevice(Device d, Size gap, Size bb, Size border);
status		appendDialogItemDevice(Device d, Graphical item, Name where);
Graphical	getMemberDevice(Device dev, Name name);
status		geometryDevice(Device dev, Int x, Int y, Int w, Int h);
status		makeClassDevice(Class class);

/* ../src/gra/ellipse.c */
status		makeClassEllipse(Class class);

/* ../src/gra/figure.c */
status		initialiseFigure(Figure f);
Any		RedrawBoxFigure(Figure f, Area area);
status		makeClassFigure(Class class);

/* ../src/gra/font.c */
status		replaceFont(FontObj f, DisplayObj d);
status		makeBuiltinFonts(void);
Int		getWidthFont(FontObj f, CharArray txt);
Int		getExFont(FontObj f);
Int		getHeightFont(FontObj f);
Int		getAscentFont(FontObj f);
Int		getDescentFont(FontObj f);
Bool		getFixedWidthFont(FontObj f);
Bool		getB16Font(FontObj f);
status		makeClassFont(Class class);

/* ../src/gra/format.c */
status		makeClassFormat(Class class);

/* ../src/gra/graphical.c */
status		initialiseGraphical(Any obj, Int x, Int y, Int w, Int h);
status		unlinkGraphical(Graphical gr);
status		copyGraphical(Any obj1, Any obj2);
status		DeviceGraphical(Any obj, Device dev);
status		deviceGraphical(Any obj, Device dev);
status		reparentGraphical(Graphical gr);
status		DisplayedGraphical(Any obj, Bool val);
status		displayedGraphical(Any obj, Bool val);
Bool		getIsDisplayedGraphical(Graphical gr, Device dev);
status		initialiseDeviceGraphical(Any obj, int *x, int *y, int *w, int *h);
status		initialiseRedrawAreaGraphical(Any obj, Area a, int *x, int *y, int *w, int *h, IArea redraw);
status		offsetDeviceGraphical(Any obj, int *x, int *y);
Area		getAbsoluteAreaGraphical(Graphical gr, Device device);
Graphical	getRootGraphical(Graphical gr);
PceWindow	getWindowGraphical(Graphical gr);
FrameObj	getFrameGraphical(Graphical gr);
DisplayObj	getDisplayGraphical(Graphical gr);
Application	getApplicationGraphical(Graphical gr);
Device		getCommonDeviceGraphical(Graphical gr1, Graphical gr2);
status		changedAreaGraphical(Any obj, Int x, Int y, Int w, Int h);
status		changedImageGraphical(Any obj, Int x, Int y, Int w, Int h);
status		changedEntireImageGraphical(Any obj);
status		redrawGraphical(Graphical gr, Area a);
status		requestComputeGraphical(Any obj, Any val);
status		ComputeGraphical(Any obj);
status		RedrawArea(Any obj, Area area);
status		paintSelectedGraphical(Graphical gr);
status		RedrawAreaGraphical(Any obj, Area area);
status		flushGraphical(Any gr);
status		synchroniseGraphical(Graphical gr, Bool always);
status		exposeGraphical(Any obj1, Any obj2);
status		setGraphical(Any obj, Int x, Int y, Int w, Int h);
status		doSetGraphical(Any obj, Int x, Int y, Int w, Int h);
status		requestGeometryGraphical(Any gr, Int x, Int y, Int w, Int h);
status		geometryGraphical(Any obj, Int x, Int y, Int w, Int h);
status		xGraphical(Graphical gr, Int x);
status		yGraphical(Graphical gr, Int y);
status		heightGraphical(Graphical gr, Int h);
status		positionGraphical(Graphical gr, Point pos);
status		centerGraphical(Graphical gr, Point pos);
status		relativeMoveGraphical(Graphical gr, Point pos);
status		init_resize_graphical(Any obj, Real xfactor, Real yfactor, Point origin, float *xf, float *yf, int *ox, int *oy);
Area		getAreaGraphical(Graphical gr);
Int		getXGraphical(Graphical gr);
Int		getYGraphical(Graphical gr);
Int		getWidthGraphical(Graphical gr);
Int		getHeightGraphical(Graphical gr);
Int		getLeftSideGraphical(Graphical gr);
Int		getRightSideGraphical(Graphical gr);
Int		getBottomSideGraphical(Graphical gr);
Point		getPositionGraphical(Graphical gr);
status		get_absolute_xy_graphical(Graphical gr, Device *dev, Int *X, Int *Y);
Int		getAbsoluteXGraphical(Any gr, Device dev);
Int		getAbsoluteYGraphical(Any gr, Device dev);
Point		getDisplayPositionGraphical(Graphical gr);
Size		getSizeGraphical(Graphical gr);
status		appendDialogItemNetworkDevice(Device dev, Graphical gr1);
status		aboveGraphical(Graphical gr1, Graphical gr2);
status		belowGraphical(Graphical gr1, Graphical gr2);
status		rightGraphical(Graphical gr1, Graphical gr2);
status		leftGraphical(Graphical gr1, Graphical gr2);
status		referenceGraphical(Graphical gr, Point ref);
status		penGraphical(Graphical gr, Int pen);
status		shadowGraphical(Graphical gr, Int s);
status		fillPatternGraphical(Graphical gr, Image pattern);
status		colourGraphical(Graphical gr, Any c);
Any		getDisplayColourGraphical(Graphical gr);
Handle		getHandleGraphical(Graphical gr, Name name);
Point		getHandlePositionGraphical(Graphical gr, Name name, Device dev);
Chain		getHandlesGraphical(Graphical gr, Point pos, Name kind, Int distance);
status		activeGraphical(Graphical gr, Bool val);
status		focusCursorGraphical(Graphical gr, CursorObj cursor);
status		focusGraphical(Graphical gr, Recogniser recogniser, CursorObj cursor, Name button);
status		updateConnectionsGraphical(Graphical gr, Int level);
status		connectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
status		attachConnectionGraphical(Graphical gr, Connection c);
status		detachConnectionGraphical(Graphical gr, Connection c);
status		connectedGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
status		disconnectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
status		eventGraphical(Any obj, EventObj ev);
Bool		getKeyboardFocusGraphical(Graphical gr);
status		generateEventGraphical(Graphical gr, Name name);
status		inEventAreaGraphical(Graphical gr, Int xc, Int yc);
Chain		getAllRecognisersGraphical(Any obj, Bool create);
status		assignGraphical(Any obj, Name slot, Any value);
status		flashGraphical(Graphical gr, Area a, Int time);
status		alertGraphical(Graphical gr);
Node		getNodeGraphical(Graphical gr);
status		pointerGraphical(Graphical gr, Point pos);
Any		getMasterGraphical(Graphical gr);
status		nameGraphical(Graphical gr, Name name);
status		initialiseNewSlotGraphical(Graphical gr, Variable new);
status		clipGraphical(Graphical gr, Area a);
status		unclipGraphical(Graphical gr);
status		makeClassGraphical(Class class);

/* ../src/gra/handle.c */
status		getXYHandle(Handle h, Graphical gr, Device dev, Int *X, Int *Y);
Int		getXHandle(Handle h, Graphical gr, Device dev);
Int		getYHandle(Handle h, Graphical gr, Device dev);
status		makeClassHandle(Class class);

/* ../src/gra/image.c */
status		initialiseImage(Image image, SourceSink data, Int w, Int h, Name kind);
Image		getConvertImage(Class class, Any obj);
status		XopenImage(Image image, DisplayObj d);
status		XcloseImage(Image image, DisplayObj d);
status		loadImage(Image image, SourceSink file, CharArray path);
status		fillImage(Image image, Any pattern, Area area);
Image		getMonochromeImage(Image image);
status		makeClassImage(Class class);

/* ../src/gra/joint.c */
status		initialiseJoint(Joint jt, Int x, Int y, Int w, Int h, Name arrows);
status		copyJoint(Joint jt1, Joint jt2);
status		setArrowsJoint(Joint jt, Graphical first, Graphical second);
status		makeClassJoint(Class class);

/* ../src/gra/line.c */
status		initialiseLine(Line ln, Int xa, Int ya, Int xb, Int yb, Name arrows);
status		adjustFirstArrowLine(Line ln);
status		adjustSecondArrowLine(Line ln);
status		computeLine(Line ln);
status		copyLine(Line l1, Line l2);
status		paintSelectedLine(Line ln);
status		pointsLine(Line ln, Int sx, Int sy, Int ex, Int ey);
Point		getIntersectionLine(Line l1, Line l2);
Real		getAngleLine(Line ln, Point p);
status		makeClassLine(Class class);

/* ../src/gra/link.c */
status		makeClassLink(Class class);

/* ../src/gra/listbrowser.c */
status		requestGeometryListBrowser(ListBrowser lb, Int x, Int y, Int w, Int h);
Size		getSizeListBrowser(ListBrowser lb);
status		executeSearchListBrowser(ListBrowser lb);
status		typedListBrowser(ListBrowser lb, EventId id);
status		selectedListBrowser(ListBrowser lb, DictItem di);
status		selectionListBrowser(ListBrowser lb, Any obj);
Any		getSelectionListBrowser(ListBrowser lb);
status		scrollToListBrowser(ListBrowser lb, Int index);
status		normaliseListBrowser(ListBrowser lb, DictItem di);
DictItem	getMemberListBrowser(ListBrowser lb, Any key);
Chain		getContainsListBrowser(ListBrowser lb);
status		makeClassListBrowser(Class class);

/* ../src/gra/node.c */
status		updateDisplayedTree(Tree t);
status		relateImageNode(Node n, Node n2);
status		forAllNode(Node n, Code msg);
status		forSomeNode(Node n, Code msg);
Node		getFindNodeNode(Node n, Graphical gr);
status		makeClassNode(Class class);

/* ../src/gra/path.c */
status		adjustFirstArrowPath(Path p);
status		adjustSecondArrowPath(Path p);
status		makeClassPath(Class class);

/* ../src/gra/postscript.c */
StringObj	getPostscriptObject(Any obj, Bool ls, Area a);
void		ps_put_char(int c);
void		ps_output(char *fm, ...);
status		ps_font(FontObj font);
status		postscriptDrawable(int ox, int oy, int w, int h);
Sheet		makePSDefinitions(void);
status		postscriptGraphical(Any obj);
status		drawPostScriptDevice(Device dev);
status		drawPostScriptFigure(Figure f);
status		drawPostScriptTree(Tree tree);
status		drawPostScriptBox(Box b);
status		drawPostScriptCircle(Circle c);
status		drawPostScriptEllipse(Ellipse e);
status		drawPostScriptPath(Path p);
status		drawPostScriptLine(Line ln);
status		drawPostScriptArrow(Arrow a);
status		drawPostScriptArc(Arc a);
status		drawPostScriptBitmap(BitmapObj bm);
status		drawPostScriptImage(Image image);
status		drawPostScriptText(TextObj t);
status		postscriptFrame(FrameObj fr);
status		postscriptDisplay(DisplayObj d);

/* ../src/gra/scrollbar.c */
Int		getMarginScrollBar(ScrollBar sb);
status		placeScrollBar(ScrollBar sb, Graphical gr);
status		bubbleScrollBar(ScrollBar sb, Int l, Int s, Int v);
status		makeClassScrollBar(Class class);

/* ../src/gra/text.c */
void		str_format(String out, const String in, const int width, const FontObj font);
status		repaintText(TextObj t, int x, int y, int w, int h);
Int		get_pointed_text(TextObj t, int x, int y);
status		transparentText(TextObj t, Bool val);
status		fontText(TextObj t, FontObj font);
status		borderText(TextObj t, Int border);
status		stringText(TextObj t, CharArray s);
status		showCaretText(TextObj t, Any val);
status		pasteText(TextObj t, Int buffer);
status		lengthText(TextObj t, Int l);
status		marginText(TextObj t, Int width, Name wrap);
status		makeClassText(Class class);

/* ../src/gra/tree.c */
status		requestComputeTree(Tree t);
status		displayTree(Tree t, Node n);
status		unzoomTree(Tree t);
status		zoomTree(Tree t, Node n);
status		makeClassTree(Class class);

/* ../src/gra/visual.c */
status		resetVisual(VisualObj v);
status		destroyVisual(VisualObj v);
Any		getReportToVisual(VisualObj v);
status		reportVisual(VisualObj v, Name kind, CharArray fmt, int argc, Any *argv);
status		alertReporteeVisual(Any v);
status		makeClassVisual(Class class);

/* ../src/gra/pixmap.c */
Colour		getReplacementColourPixmap(PixmapObj pm);
status		makeClassPixmap(Class class);

/* ../src/gra/elevation.c */
Elevation	getModifyElevation(Elevation e, Name att, Any val);
status		makeClassElevation(Class class);

/* ../src/gra/pen.c */
status		makeClassPen(Class class);

/* ../src/gra/draw.c */
void		r_3d_rectangular_polygon(int n, IPoint pts, Elevation e, int flags);

/* ../src/gra/colourmap.c */
status		makeClassColourMap(Class class);
