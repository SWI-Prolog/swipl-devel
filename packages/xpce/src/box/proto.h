#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/box/boxes.c */
COMMON(void)	initBoxes(void);

/* /staff/jan/src/pl/packages/xpce/src/box/hbox.c */
COMMON(status)	initialiseHBox(HBox hb, Int width, Int ascent, Int descent, Rubber rubber);
COMMON(status)	makeClassHBox(Class class);

/* /staff/jan/src/pl/packages/xpce/src/box/tbox.c */
COMMON(status)	initialiseTBox(TBox tb, CharArray text, Style style);
COMMON(HBox)	getSpaceHBoxFont(FontObj f);
COMMON(void)	drawTBox(TBox tb, int x, int y, int w);
COMMON(status)	makeClassTBox(Class class);

/* /staff/jan/src/pl/packages/xpce/src/box/parbox.c */
COMMON(Tuple)	getFindParBox(ParBox pb, Code test);
COMMON(status)	makeClassParBox(Class class);

/* /staff/jan/src/pl/packages/xpce/src/box/grbox.c */
COMMON(status)	computeGrBox(GrBox grb);
COMMON(status)	computeAscentDescentGrBox(GrBox grb);
COMMON(status)	makeClassGrBox(Class class);

/* /staff/jan/src/pl/packages/xpce/src/box/rubber.c */
COMMON(status)	makeClassRubber(Class class);

/* /staff/jan/src/pl/packages/xpce/src/box/lbox.c */
COMMON(status)	makeClassLBox(Class class);
