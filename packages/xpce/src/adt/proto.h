#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/adt/area.c */
COMMON(status)	intersectionArea(Area a, Area b);
COMMON(status)	unionNormalisedArea(Area a, Area b);
COMMON(status)	sizeArea(Area a, Size s);
COMMON(Size)	getSizeArea(Area a);
COMMON(status)	pointInArea(Area a, Point p);
COMMON(status)	overlapArea(Area a, Area b);
COMMON(status)	relativeMoveArea(Area a, Point p);
COMMON(status)	relativeMoveBackArea(Area a, Point p);
COMMON(Point)	getPositionArea(Area a);
COMMON(status)	copyArea(Area a, Area b);
COMMON(Area)	getCopyArea(Area a);
COMMON(status)	insideArea(Area a, Area b);
COMMON(Int)	getDistanceXArea(Area a, Area b);
COMMON(Int)	getDistanceYArea(Area a, Area b);
COMMON(Int)	getDistanceArea(Area a, Area b);
COMMON(status)	clearArea(Area a);
COMMON(status)	setArea(Area a, Int x, Int y, Int w, Int h);
COMMON(status)	increaseArea(Area a, Int i);
COMMON(status)	orientationArea(Area a, Name orientation);
COMMON(Name)	getOrientationArea(Area a);
COMMON(status)	makeClassArea(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/atable.c */
COMMON(status)	makeClassAtable(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/attribute.c */
COMMON(status)	makeClassAttribute(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/bool.c */
COMMON(status)	makeClassBool(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/chain.c */
COMMON(status)	initialiseChainv(Chain ch, int argc, Any *argv);
COMMON(status)	clearChain(Chain ch);
COMMON(Int)	getSizeChain(Chain ch);
COMMON(status)	prependChain(register Chain ch, Any obj);
COMMON(status)	appendChain(register Chain ch, Any obj);
COMMON(status)	addChain(register Chain ch, Any obj);
COMMON(status)	insertChain(Chain ch, Any obj);
COMMON(status)	insertAfterChain(Chain ch, Any obj, Any obj2);
COMMON(status)	insertBeforeChain(Chain ch, Any obj, Any obj2);
COMMON(status)	swapChain(Chain ch, Any obj1, Any obj2);
COMMON(status)	deleteHeadChain(Chain ch);
COMMON(status)	deleteChain(Chain ch, register Any obj);
COMMON(status)	deleteCellChain(Chain ch, Cell cell);
COMMON(status)	memberChain(Chain ch, Any obj);
COMMON(Any)	getNextChain(Chain ch, Any val);
COMMON(Any)	getPreviousChain(Chain ch, Any val);
COMMON(status)	forAllChain(Chain ch, Code code, Bool safe);
COMMON(status)	forSomeChain(Chain ch, Code code, Bool safe);
COMMON(Any)	getFindChain(Chain ch, Code code);
COMMON(Chain)	getFindAllChain(Chain ch, Code code);
COMMON(status)	mergeChain(Chain ch, Chain ch2);
COMMON(status)	replaceChain(Chain ch, Any obj1, Any obj2);
COMMON(status)	emptyChain(Chain ch);
COMMON(Chain)	getCopyChain(Chain ch);
COMMON(int)	qsortCompareObjects(const void *o1, const void *o2);
COMMON(status)	sortChain(Chain ch, Code msg, Bool unique);
COMMON(status)	sortNamesChain(Chain ch, Bool unique);
COMMON(Tuple)	getCompleteNameChain(Chain ch, CharArray prefix, Function map, Bool ignore_case);
COMMON(Chain)	getIntersectionChain(Chain ch, Chain ch2);
COMMON(Any)	getHeadChain(Chain ch);
COMMON(Any)	getDeleteHeadChain(Chain ch);
COMMON(Any)	getTailChain(Chain ch);
COMMON(status)	moveBeforeChain(Chain ch, Any obj1, Any obj2);
COMMON(status)	moveAfterChain(Chain ch, Any obj1, Any obj2);
COMMON(status)	beforeChain(Chain ch, Any obj1, Any obj2);
COMMON(Any)	getNth1Chain(Chain ch, Int index);
COMMON(Any)	getNth0Chain(Chain ch, Int index);
COMMON(status)	cellValueChain(Chain ch, Int c, Any obj);
COMMON(Cell)	getNth0CellChain(Chain ch, Int index);
COMMON(Int)	getIndexChain(Chain ch, Any obj);
COMMON(Int)	getArityChain(Chain ch);
COMMON(Any)	getArgChain(Chain ch, Int arg);
COMMON(status)	makeClassChain(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/chaintable.c */
COMMON(status)	appendChainTable(ChainTable ct, Any name, Any value);
COMMON(status)	addChainTable(ChainTable ct, Any name, Any value);
COMMON(status)	makeClassChainTable(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/constant.c */
COMMON(status)	makeClassConstant(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/date.c */
COMMON(Date)	CtoDate(long int time);
COMMON(status)	makeClassDate(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/dict.c */
COMMON(DictItem) getMemberDict(Dict dict, Any obj);
COMMON(status)	deleteDict(Dict dict, Any obj);
COMMON(status)	appendDict(Dict dict, DictItem di);
COMMON(DictItem) getFindIndexDict(Dict dict, Int ln);
COMMON(DictItem) getFindPrefixDict(Dict dict, StringObj str, Int from, Bool ign_case);
COMMON(status)	clearDict(Dict dict);
COMMON(status)	makeClassDict(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/dictitem.c */
COMMON(CharArray) getLabelDictItem(DictItem di);
COMMON(status)	makeClassDictItem(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/hashtable.c */
COMMON(HashTable) createHashTable(Int buckets, Name refer);
COMMON(status)	freeHashTable(HashTable ht);
COMMON(status)	initialiseHashTable(HashTable ht, Int buckets);
COMMON(status)	appendHashTable(HashTable ht, Any name, Any value);
COMMON(status)	deleteHashTable(HashTable ht, Any name);
COMMON(status)	clearHashTable(HashTable ht);
COMMON(status)	makeClassHashTable(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/number.c */
COMMON(Number)	CtoNumber(long i);
COMMON(status)	makeClassNumber(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/point.c */
COMMON(status)	equalPoint(Point p1, Point p2);
COMMON(status)	copyPoint(Point p1, Point p2);
COMMON(status)	setPoint(Point pt, Int x, Int y);
COMMON(status)	offsetPoint(Point pt, Int x, Int y);
COMMON(int)	get_distance_point(Point p, int x, int y);
COMMON(Int)	getDistancePoint(Point p, Point q);
COMMON(Point)	getMidPoint(Point p, Point q);
COMMON(status)	plusPoint(Point p, Point q);
COMMON(status)	minusPoint(Point p, Point q);
COMMON(status)	makeClassPoint(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/real.c */
COMMON(void)	setReal(Real r, double f);
COMMON(double)	valReal(Real r);
COMMON(Real)	CtoReal(double f);
COMMON(Real)	getConvertReal(Class class, Any obj);
COMMON(status)	valueReal(Real r, Real v);
COMMON(status)	makeClassReal(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/region.c */
COMMON(status)	insideRegion(RegionObj r, Area a, Point p);
COMMON(status)	makeClassRegion(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/sheet.c */
COMMON(Sheet)	getCopySheet(Sheet sh);
COMMON(status)	isAttributeSheet(Sheet sh, Any name);
COMMON(Attribute) getMemberSheet(Sheet sh, register Any name);
COMMON(status)	deleteSheet(Sheet sh, Any name);
COMMON(Any)	getValueSheet(Sheet sh, Any name);
COMMON(status)	valueSheet(Sheet sh, Any name, Any value);
COMMON(status)	makeClassSheet(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/size.c */
COMMON(status)	equalSize(Size s, Size s2);
COMMON(status)	copySize(Size s, Size s2);
COMMON(Size)	getCopySize(Size s);
COMMON(status)	setSize(Size s, Int w, Int h);
COMMON(status)	makeClassSize(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/tuple.c */
COMMON(status)	initialiseTuple(Tuple t, Any first, Any second);
COMMON(status)	makeClassTuple(Class class);

/* /staff/jan/src/pl/packages/xpce/src/adt/vector.c */
COMMON(status)	initialiseVectorv(Vector v, int argc, Any *argv);
COMMON(Vector)	createVectorv(int argc, Any *argv);
COMMON(status)	unlinkVector(Vector v);
COMMON(Int)	getLowIndexVector(Vector v);
COMMON(Int)	getHighIndexVector(Vector v);
COMMON(status)	rangeVector(Vector v, Int low, Int high);
COMMON(status)	clearVector(Vector v);
COMMON(Any)	getTailVector(Vector v);
COMMON(Vector)	getCopyVector(Vector v);
COMMON(status)	fillVector(Vector v, Any obj, Int from, Int to);
COMMON(status)	shiftVector(Vector v, Int places);
COMMON(Any)	getElementVector(Vector v, Int e);
COMMON(status)	elementVector(Vector v, Int e, Any obj);
COMMON(status)	appendVector(Vector v, int argc, Any obj []);
COMMON(Int)	getIndexVector(Vector v, Any obj);
COMMON(Any)	getArgVector(Vector v, Int arg);
COMMON(Int)	getArityVector(Vector v);
COMMON(status)	makeClassVector(Class class);
