
/* gra/arc.c */
void		points_arc(Arc a, int *sx, int *sy, int *ex, int *ey);
status		makeClassArc(Class class);

/* gra/arrow.c */
status		computeArrow(Arrow a);
status		pointsArrow(Arrow a, Int tx, Int ty, Int rx, Int ry);
status		paintArrow(Arrow a, Int tx, Int ty, Int rx, Int ry);
status		makeClassArrow(Class class);

/* gra/bitmap.c */
status		makeClassBitmap(Class class);

/* gra/box.c */
status		makeClassBox(Class class);

/* gra/circle.c */
status		makeClassCircle(Class class);

/* gra/colour.c */
status		equalColour(Colour c1, Colour c2);
Int		getRedColour(Colour c);
Int		getGreenColour(Colour c);
Int		getBlueColour(Colour c);
Colour		getHiliteColour(Colour c);
Colour		getReduceColour(Colour c);
status		makeClassColour(Class class);

/* gra/connection.c */
status		updateHideExposeConnection(Connection c);
status		updateDeviceConnection(Connection c);
status		makeClassConnection(Class class);
int		distanceLineToPoint(int x1, int y1, int x2, int y2, int px, int py);

/* gra/cursor.c */
status		makeClassCursor(Class class);

/* gra/device.c */
status		initialiseDevice(Device dev);
status		unlinkDevice(Device dev);
CursorObj	getFindCursorDevice(Device dev);
Chain		getPointedObjectsDevice(Device dev, Any pos, Chain ch);
status		inspectDevice(Device dev, EventObj ev);
status		eventDevice(Any obj, EventObj ev);
status		advanceDevice(Device dev, Graphical gr);
status		requestComputeDevice(Device dev, Any val);
status		computeGraphicalsDevice(Device dev);
status		computeDevice(Any obj);
status		computeBoundingBoxDevice(Device dev);
status		changedUnionDevice(Device dev, Int ox, Int oy, Int ow, Int oh);
status		RedrawAreaDevice(Device dev, Area a);
status		displayDevice(Any Dev, Any Gr, Point pos);
status		appendDevice(Device dev, Graphical gr);
status		eraseDevice(Device dev, Graphical gr);
status		displayedGraphicalDevice(Device dev, Graphical gr, Bool val);
status		exposeDevice(Device dev, Graphical gr, Graphical gr2);
status		hideDevice(Device dev, Graphical gr, Graphical gr2);
status		swapGraphicalsDevice(Device dev, Graphical gr, Graphical gr2);
status		computeFormatDevice(Device dev);
status		layoutDialogDevice(Device d, Size gap);
status		appendDialogItemDevice(Device d, Graphical item, Name where);
Graphical	getMemberDevice(Device dev, Name name);
status		geometryDevice(Device dev, Int x, Int y, Int w, Int h);
status		makeClassDevice(Class class);

/* gra/ellipse.c */
status		makeClassEllipse(Class class);

/* gra/figure.c */
status		initialiseFigure(Figure f);
status		computeFigure(Figure f);
status		makeClassFigure(Class class);

/* gra/font.c */
status		replaceFont(FontObj f, DisplayObj d);
status		loadFontFamilyDisplay(DisplayObj d, Name fam);
status		loadFontsDisplay(DisplayObj d);
status		makeBuiltinFonts(void);
Int		getExFont(FontObj f);
Int		getHeightFont(FontObj f);
Int		getAscentFont(FontObj f);
Int		getDescentFont(FontObj f);
Bool		getFixedWidthFont(FontObj f);
Bool		getB16Font(FontObj f);
status		makeClassFont(Class class);

/* gra/format.c */
status		makeClassFormat(Class class);

/* gra/graphical.c */
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
Int		getLeftSideGraphical(Graphical gr);
Int		getRightSideGraphical(Graphical gr);
status		get_absolute_xy_graphical(Graphical gr, Device *dev, Int *X, Int *Y);
Int		getAbsoluteXGraphical(Any gr, Device dev);
Int		getAbsoluteYGraphical(Any gr, Device dev);
Point		getDisplayPositionGraphical(Graphical gr);
Size		getSizeGraphical(Graphical gr);
status		aboveGraphical(Graphical gr1, Graphical gr2);
status		belowGraphical(Graphical gr1, Graphical gr2);
status		rightGraphical(Graphical gr1, Graphical gr2);
status		leftGraphical(Graphical gr1, Graphical gr2);
status		penGraphical(Graphical gr, Int pen);
status		shadowGraphical(Graphical gr, Int s);
status		elevationGraphical(Graphical gr, Elevation e);
status		fillPatternGraphical(Graphical gr, Image pattern);
status		colourGraphical(Graphical gr, Any c);
Any		getDisplayColourGraphical(Graphical gr);
status		selectedGraphical(Graphical gr, Bool val);
Handle		getHandleGraphical(Graphical gr, Name name);
Point		getHandlePositionGraphical(Graphical gr, Name name, Device dev);
Chain		getHandlesGraphical(Graphical gr, Point pos, Name kind, Int distance);
status		focusCursorGraphical(Graphical gr, CursorObj cursor);
status		focusGraphical(Graphical gr, Recogniser recogniser, CursorObj cursor, Name button);
status		updateConnectionsGraphical(Graphical gr, Int level);
status		connectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
status		attachConnectionGraphical(Graphical gr, Connection c);
status		detachConnectionGraphical(Graphical gr, Connection c);
status		disconnectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
status		eventGraphical(Any obj, EventObj ev);
Bool		getKeyboardFocusGraphical(Graphical gr);
status		generateEventGraphical(Graphical gr, Name name);
status		inEventAreaGraphical(Graphical gr, Int xc, Int yc);
Chain		getAllRecognisersGraphical(Any obj, Bool create);
status		assignGraphical(Any obj, Name slot, Any value);
status		alertGraphical(Graphical gr);
Node		getNodeGraphical(Graphical gr);
status		pointerGraphical(Graphical gr, Point pos);
Any		getMasterGraphical(Graphical gr);
status		nameGraphical(Graphical gr, Name name);
status		initialiseNewSlotGraphical(Graphical gr, Variable new);
status		makeClassGraphical(Class class);

/* gra/handle.c */
status		getXYHandle(Handle h, Graphical gr, Device dev, Int *X, Int *Y);
Int		getXHandle(Handle h, Graphical gr, Device dev);
Int		getYHandle(Handle h, Graphical gr, Device dev);
status		makeClassHandle(Class class);

/* gra/image.c */
status		initialiseImage(Image image, Name name, Int w, Int h, Name kind);
Image		getConvertImage(Class class, Any obj);
status		XopenImage(Image image, DisplayObj d);
status		XcloseImage(Image image, DisplayObj d);
status		loadImage(Image image, FileObj file, CharArray path);
status		resizeImage(Image image, Int w, Int h);
status		makeClassImage(Class class);

/* gra/joint.c */
status		initialiseJoint(Joint jt, Int x, Int y, Int w, Int h, Name arrows);
status		copyJoint(Joint jt1, Joint jt2);
status		setArrowsJoint(Joint jt, Arrow first, Arrow second);
status		makeClassJoint(Class class);

/* gra/line.c */
status		initialiseLine(Line ln, Int xa, Int ya, Int xb, Int yb, Name arrows);
status		copyLine(Line l1, Line l2);
Int		getStartXLine(Line ln);
Int		getStartYLine(Line ln);
Int		getEndXLine(Line ln);
Int		getEndYLine(Line ln);
status		pointsLine(Line ln, Int sx, Int sy, Int ex, Int ey);
Point		getIntersectionLine(Line l1, Line l2);
Real		getAngleLine(Line ln, Point p);
status		makeClassLine(Class class);

/* gra/link.c */
status		makeClassLink(Class class);

/* gra/listbrowser.c */
Name		getLabelListBrowser(ListBrowser lb);
Size		getSizeListBrowser(ListBrowser lb);
status		selectedListBrowser(ListBrowser lb, DictItem di);
status		selectionListBrowser(ListBrowser lb, Any obj);
Any		getSelectionListBrowser(ListBrowser lb);
status		scrollToListBrowser(ListBrowser lb, Int index);
status		normaliseListBrowser(ListBrowser lb, DictItem di);
status		fontListBrowser(ListBrowser lb, FontObj font);
DictItem	getMemberListBrowser(ListBrowser lb, Any key);
Chain		getContainsListBrowser(ListBrowser lb);
status		makeClassListBrowser(Class class);

/* gra/node.c */
status		updateDisplayedTree(Tree t);
status		forAllNode(Node n, Code msg);
status		forSomeNode(Node n, Code msg);
Node		getFindNodeNode(Node n, Graphical gr);
status		makeClassNode(Class class);

/* gra/path.c */
status		makeClassPath(Class class);

/* gra/postscript.c */
StringObj	getPostscriptObject(Any obj, Bool ls, Area a);
void		ps_put_char(int c);
void		ps_output(char *fm, ...);
status		ps_font(FontObj font);
status		postscriptDrawable(int ox, int oy, int w, int h);
status		postscriptGraphical(Any obj);
status		drawPostScriptDevice(Device dev);
status		drawPostScriptFigure(Figure f);
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

/* gra/scrollbar.c */
Int		getMarginScrollBar(ScrollBar sb);
status		placeScrollBar(ScrollBar sb, Graphical gr);
status		RedrawAreaScrollBar(ScrollBar s, Area a);
status		bubbleScrollBar(ScrollBar sb, Int l, Int s, Int v);
status		makeClassScrollBar(Class class);

/* gra/text.c */
status		computeText(TextObj t);
void		str_format(String out, const String in, const int width, const FontObj font);
status		repaintText(TextObj t, int x, int y, int w, int h);
status		transparentText(TextObj t, Bool val);
Bool		getTransparentText(TextObj t);
status		fontText(TextObj t, FontObj font);
status		borderText(TextObj t, Int border);
status		stringText(TextObj t, CharArray s);
status		showCaretText(TextObj t, Any val);
status		caretText(TextObj t, Int where);
status		pasteText(TextObj t, Int buffer);
status		lengthText(TextObj t, Int l);
status		marginText(TextObj t, Int width, Name wrap);
status		makeClassText(Class class);

/* gra/tree.c */
status		requestComputeTree(Tree t);
status		displayTree(Tree t, Node n);
status		unzoomTree(Tree t);
status		zoomTree(Tree t, Node n);
status		makeClassTree(Class class);

/* gra/visual.c */
status		resetVisual(VisualObj v);
status		destroyVisual(VisualObj v);
Any		getReportToVisual(VisualObj v);
status		reportVisual(VisualObj v, Name kind, CharArray fmt, int argc, Any *argv);
status		alertReporteeVisual(Any v);
status		makeClassVisual(Class class);

/* gra/pixmap.c */
Colour		getReplacementColourPixmap(PixmapObj pm);
status		makeClassPixmap(Class class);

/* gra/elevation.c */
Elevation	getModifyElevation(Elevation e, Name att, Any val);
status		makeClassElevation(Class class);
