
/* ../src/img/jdatasrc.c */
int 		METHODDEF(void);
int 		METHODDEF(int boolean);
int 		METHODDEF(void);
int 		METHODDEF(void);
int 		GLOBAL(void);

/* ../src/img/jdatadst.c */
int 		METHODDEF(void);
int 		METHODDEF(int boolean);
int 		METHODDEF(void);
int 		GLOBAL(void);

/* ../src/img/jpegtoxpm.c */
int		readJPEGtoXpmImage(IOSTREAM *fd, XpmImage *img, Image image);

/* ../src/img/gifread.c */
const char *	GIFError(void);
int		GIFReadFD(IOSTREAM *fd, PIXEL **data, int *width, int *height, GIFAllocColorTable at, GIFAllocColor ac, GIFDoExtension doext, void *closure);

/* ../src/img/giftoxpm.c */
int		XpmReadGIF(IOSTREAM *fd, XpmImage *img);

/* ../src/img/gifwrite.c */
int		gifwrite_grey(FILE *fp, unsigned char *image, long w, long h);
int		gifwrite_rgb(FILE *fp, unsigned char *rgbimage, long w, long h);
int		gifwrite_colmap(FILE *fp, unsigned char *image, long w, long h, unsigned char *red_map, unsigned char *green_map, unsigned char *blue_map);
