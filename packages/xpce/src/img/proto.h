
/* /staff/jan/src/pl/packages/xpce/src/img/jdatasrc.c */
int 		METHODDEF(void);
int 		METHODDEF(int boolean);
int 		METHODDEF(void);
int 		METHODDEF(void);
int 		GLOBAL(void);

/* /staff/jan/src/pl/packages/xpce/src/img/jdatadst.c */
int 		METHODDEF(void);
int 		METHODDEF(int boolean);
int 		METHODDEF(void);
int 		GLOBAL(void);

/* /staff/jan/src/pl/packages/xpce/src/img/jpegtoxpm.c */
int		readJPEGtoXpmImage(IOSTREAM *fd, XpmImage *img, Image image);

/* /staff/jan/src/pl/packages/xpce/src/img/gifread.c */
const char *	GIFError(void);
int		GIFReadFD(IOSTREAM *fd, PIXEL **data, int *width, int *height, GIFAllocColorTable at, GIFAllocColor ac, GIFDoExtension doext, void *closure);

/* /staff/jan/src/pl/packages/xpce/src/img/giftoxpm.c */
int		XpmReadGIF(IOSTREAM *fd, XpmImage *img);

/* /staff/jan/src/pl/packages/xpce/src/img/gifwrite.c */
int		gifwrite_grey(FILE *fp, unsigned char *image, long w, long h);
int		gifwrite_rgb(FILE *fp, unsigned char *rgbimage, unsigned char *mask, long w, long h);
int		gifwrite_colmap(FILE *fp, unsigned char *image, long w, long h, unsigned char *red_map, unsigned char *green_map, unsigned char *blue_map);

/* /staff/jan/src/pl/packages/xpce/src/img/imgutil.c */
int		image_type_from_data(char *data, int size);
