
/* ../src/img/jdatasrc.c */
int 		METHODDEF(void);
int 		METHODDEF(int boolean);
int 		METHODDEF(void);
int 		METHODDEF(void);
int 		GLOBAL(void);

/* ../src/img/jpegtoxpm.c */
int		readJPEGtoXpmImage(IOSTREAM *fd, XpmImage *img);

/* ../src/img/gifread.c */
const char *	GIFError(void);
int		GIFReadFD(IOSTREAM *fd, PIXEL **data, int *width, int *height, GIFAllocColorTable at, GIFAllocColor ac, void *closure);

/* ../src/img/giftoxpm.c */
int		alloc_colortable(int ncolors, void *closure);
int		alloc_color(int index, int r, int g, int b, void *closure);
int		XpmReadGIF(IOSTREAM *fd, XpmImage *img);
