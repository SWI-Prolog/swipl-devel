
/* ../src/box/boxes.c */
void		initBoxes(void);

/* ../src/box/hbox.c */
status		initialiseHBox(HBox hb, Int width, Int ascent, Int descent, Rubber rubber);
status		makeClassHBox(Class class);

/* ../src/box/tbox.c */
status		initialiseTBox(TBox tb, CharArray text, Style style);
void		drawTBox(TBox tb, int x, int y, int w);
status		makeClassTBox(Class class);

/* ../src/box/parbox.c */
status		makeClassParBox(Class class);

/* ../src/box/grbox.c */
status		computeAscentDescentGrBox(GrBox grb);
status		makeClassGrBox(Class class);

/* ../src/box/rubber.c */
status		makeClassRubber(Class class);

/* ../src/box/lbox.c */
status		makeClassLBox(Class class);
