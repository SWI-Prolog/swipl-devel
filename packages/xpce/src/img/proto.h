#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/img/jdatasrc.c */
COMMON(int) )	METHODDEF(void);
COMMON(int) )	METHODDEF(int boolean);
COMMON(int) )	METHODDEF(void);
COMMON(int) )	METHODDEF(void);
COMMON(int) )	GLOBAL(void);

/* /staff/jan/src/pl/packages/xpce/src/img/jdatadst.c */
COMMON(int) )	METHODDEF(void);
COMMON(int) )	METHODDEF(int boolean);
COMMON(int) )	METHODDEF(void);
COMMON(int) )	GLOBAL(void);

/* /staff/jan/src/pl/packages/xpce/src/img/jpegtoxpm.c */
COMMON(int)	readJPEGtoXpmImage(IOSTREAM *fd, XpmImage *img, Image image);

/* /staff/jan/src/pl/packages/xpce/src/img/gifread.c */
COMMON(const char *) GIFError(void);
COMMON(int)	GIFReadFD(IOSTREAM *fd, PIXEL **data, int *width, int *height, GIFAllocColorTable at, GIFAllocColor ac, GIFDoExtension doext, void *closure);

/* /staff/jan/src/pl/packages/xpce/src/img/giftoxpm.c */
COMMON(int)	XpmReadGIF(IOSTREAM *fd, XpmImage *img);

/* /staff/jan/src/pl/packages/xpce/src/img/gifwrite.c */
COMMON(int)	gifwrite_grey(FILE *fp, unsigned char *image, long w, long h);
COMMON(int)	gifwrite_rgb(FILE *fp, unsigned char *rgbimage, unsigned char *mask, long w, long h);
COMMON(int)	gifwrite_colmap(FILE *fp, unsigned char *image, long w, long h, unsigned char *red_map, unsigned char *green_map, unsigned char *blue_map);

/* /staff/jan/src/pl/packages/xpce/src/img/imgutil.c */
COMMON(int)	image_type_from_data(char *data, int size);
