#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/x11/canvas.c */

/* /staff/jan/src/pl/packages/xpce/src/x11/fshell.c */

/* /staff/jan/src/pl/packages/xpce/src/x11/xcommon.c */
COMMON(void *)	pceXtAppContext(void *ctx);
COMMON(Widget)	widgetWindow(PceWindow sw);
COMMON(Widget)	widgetFrame(FrameObj fr);
COMMON(void)	setXImageImage(Image image, XImage *i);
COMMON(Atom)	DisplayAtom(DisplayObj d, Name name);
COMMON(char *)	DisplayAtomToString(DisplayObj d, Atom atom);
COMMON(Atom)	FrameAtom(FrameObj fr, Name name);
COMMON(char *)	FrameAtomToString(FrameObj fr, Atom a);
COMMON(Atom)	WmProtocols(FrameObj fr);
COMMON(int)	shift_for_mask(unsigned long mask);
COMMON(status)	postscriptXImage(XImage *im, int fx, int fy, int w, int h, Display *disp, Colormap cmap, int depth, int iscolor);
COMMON(int)	intensityXColor(XColor *c);
COMMON(unsigned long) getPixelColour(Colour c, DisplayObj d);
COMMON(void)	x11_set_gc_foreground(DisplayObj d, Any fg, int gcs, GC *gc);
COMMON(status)	allocNearestColour(Display *display, Colormap map, int depth, Name vt, XColor *c);
COMMON(EventObj) CtoEvent(Any window, XEvent *event);
COMMON(Status)	XSetWMProtocols(Display *dpy, Window w, Atom *protocols, int count);
COMMON(void)	XtPopupSpringLoaded(Widget widget);

/* /staff/jan/src/pl/packages/xpce/src/x11/xconvert.c */
COMMON(XImage *) CreateXImageFromData(unsigned char *data, int width, int height);
COMMON(XImage *) readImageFile(Image image, IOSTREAM *fd);
COMMON(XImage *) attachXpmImageImage(Image image, XpmImage *xpm);
COMMON(int)	write_jpeg_file(IOSTREAM *fd, XImage *img, Display *disp, Colormap cmap, Image image);
COMMON(int)	write_gif_file(IOSTREAM *fd, XImage *img, XImage *msk, Display *disp, Colormap cmap);

/* /staff/jan/src/pl/packages/xpce/src/x11/x11-compat.c */
COMMON(void *)	dlopen(char *path, int mode);
COMMON(void *)	dlsym(void *handle, char *symbol);
COMMON(void *)	dlclose(void *handle);
COMMON(void *)	__iconv_open(void);

/* /staff/jan/src/pl/packages/xpce/src/x11/xppm.c */
COMMON(XImage *) read_ppm_file(Display *disp, Colormap cmap, int depth, IOSTREAM *fd);
COMMON(int)	write_pnm_file(IOSTREAM *fd, XImage *img, Display *disp, Colormap cmap, int scale, int fmt, int encode);
