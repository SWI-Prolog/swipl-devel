
/* img/jdatasrc.c */
int 		METHODDEF(void);
int 		METHODDEF(int boolean);
int 		METHODDEF(void);
int 		METHODDEF(void);
int 		GLOBAL(void);

/* img/jpegtoxpm.c */
int		convert_colourmap(int ncolors, JSAMPARRAY colourmap, XpmImage *img);
int		readJPEGtoXpmImage(IOSTREAM *fd, XpmImage *img);

/* img/gifread.c */
const char *	GIFError(void);
int		GIFReadFD(IOSTREAM *fd, PIXEL **data, int *width, int *height, GIFAllocColorTable at, GIFAllocColor ac, void *closure);

/* img/giftoxpm.c */
int		alloc_colortable(int ncolors, void *closure);
int		alloc_color(int index, int r, int g, int b, void *closure);
int		XpmReadGIF(IOSTREAM *fd, XpmImage *img);
