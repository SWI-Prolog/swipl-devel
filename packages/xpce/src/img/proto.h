
/* img/jdatasrc.c */
int 		METHODDEF(void);
int 		METHODDEF(int boolean);
int 		METHODDEF(void);
int 		METHODDEF(void);
int 		GLOBAL(void);

/* img/jpegtoxpm.c */
int		readJPEGtoXpmImage(IOSTREAM *fd, XpmImage *img);

/* img/gifread.c */
const char *	GIFError(void);
int		GIFReadFD(IOSTREAM *fd, PIXEL **data, int *width, int *height, GIFAllocColorTable at, GIFAllocColor ac, GIFDoExtension doext, void *closure);

/* img/giftoxpm.c */
int		XpmReadGIF(IOSTREAM *fd, XpmImage *img);
