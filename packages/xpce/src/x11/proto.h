
/* ../src/x11/canvas.c */

/* ../src/x11/fshell.c */

/* ../src/x11/xcommon.c */
XtAppContext	pceXtAppContext(XtAppContext ctx);
Widget		widgetWindow(PceWindow sw);
Widget		widgetFrame(FrameObj fr);
void		setXImageImage(Image image, XImage *i);
Atom		DisplayAtom(DisplayObj d, Name name);
char *		DisplayAtomToString(DisplayObj d, Atom atom);
Atom		FrameAtom(FrameObj fr, Name name);
char *		FrameAtomToString(FrameObj fr, Atom a);
Atom		WmProtocols(FrameObj fr);
status		postscriptXImage(XImage *im, int fx, int fy, int w, int h, Display *disp, Colormap cmap, int depth, int colorimage);
unsigned long	getPixelColour(Colour c, DisplayObj d);
void		x11_set_gc_foreground(DisplayObj d, Any fg, int gcs, GC *gc);
status		allocNearestColour(Display *display, Colormap map, int depth, Name vt, XColor *c);
XColor **	makeSparceCInfo(Display *disp, Colormap cmap, XImage *img, int *ncolours);
void		greySparceCInfo(XColor **cinfo, int depth);
void		freeSparceCInfo(XColor **table, int depth);
EventObj	CtoEvent(Any window, XEvent *event);
Status		XSetWMProtocols(Display *dpy, Window w, Atom *protocols, int count);
void		XtPopupSpringLoaded(Widget widget);

/* ../src/x11/xconvert.c */
XImage *	CreateXImageFromData(unsigned char *data, int width, int height);
XImage *	readImageFile(Image image, IOSTREAM *fd);
XImage *	attachXpmImageImage(Image image, XpmImage *xpm);

/* ../src/x11/x11-compat.c */
void *		dlopen(char *path, int mode);
void *		dlsym(void *handle, char *symbol);
void *		dlclose(void *handle);
void *		__iconv_open(void);

/* ../src/x11/xppm.c */
XImage *	read_ppm_file(Display *disp, Colormap cmap, int depth, IOSTREAM *fd);
int		write_pnm_file(IOSTREAM *fd, XImage *img, Display *disp, Colormap cmap, int scale, int fmt, int encode);
