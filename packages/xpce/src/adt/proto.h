
/* adt/area.c */
status		intersectionArea(Area a, Area b);
status		unionNormalisedArea(Area a, Area b);
status		sizeArea(Area a, Size s);
Size		getSizeArea(Area a);
status		overlapArea(Area a, Area b);
status		relativeMoveArea(Area a, Point p);
status		relativeMoveBackArea(Area a, Point p);
Point		getPositionArea(Area a);
status		inArea(Area a, Int x, Int y);
status		copyArea(Area a, Area b);
status		insideArea(Area a, Area b);
Int		getDistanceXArea(Area a, Area b);
Int		getDistanceYArea(Area a, Area b);
Int		getDistanceArea(Area a, Area b);
status		clearArea(Area a);
status		setArea(Area a, Int x, Int y, Int w, Int h);
status		increaseArea(Area a, Int i);
status		orientationArea(Area a, Name orientation);
Name		getOrientationArea(Area a);
status		makeClassArea(Class class);

/* adt/atable.c */
status		makeClassAtable(Class class);

/* adt/attribute.c */
status		makeClassAttribute(Class class);

/* adt/bool.c */
status		makeClassBool(Class class);

/* adt/chain.c */
status		initialiseChainv(Chain ch, int argc, Any *argv);
status		clearChain(Chain ch);
Int		getSizeChain(Chain ch);
status		prependChain(register Chain ch, Any obj);
status		appendChain(register Chain ch, Any obj);
status		addChain(register Chain ch, Any obj);
status		insertChain(Chain ch, Any obj);
status		insertAfterChain(Chain ch, Any obj, Any obj2);
status		swapChain(Chain ch, Any obj1, Any obj2);
status		deleteHeadChain(Chain ch);
status		deleteChain(Chain ch, register Any obj);
status		deleteCellChain(Chain ch, Cell cell);
status		memberChain(Chain ch, Any obj);
status		forAllChain(Chain ch, Code code, Bool safe);
status		forSomeChain(Chain ch, Code code, Bool safe);
Any		getFindChain(Chain ch, Code code);
Chain		getFindAllChain(Chain ch, Code code);
status		mergeChain(Chain ch, Chain ch2);
status		replaceChain(Chain ch, Any obj1, Any obj2);
status		emptyChain(Chain ch);
Chain		getCopyChain(Chain ch);
int		qsortCompareObjects(const void *o1, const void *o2);
status		sortChain(Chain ch, Code msg);
status		sortNamesChain(Chain ch);
Tuple		getCompleteNameChain(Chain ch, CharArray prefix, Function map, Bool ignore_case);
Chain		getIntersectionChain(Chain ch, Chain ch2);
Any		getHeadChain(Chain ch);
Any		getDeleteHeadChain(Chain ch);
Any		getTailChain(Chain ch);
status		moveBeforeChain(Chain ch, Any obj1, Any obj2);
status		moveAfterChain(Chain ch, Any obj1, Any obj2);
status		beforeChain(Chain ch, Any obj1, Any obj2);
Any		getNth1Chain(Chain ch, Int index);
status		cellValueChain(Chain ch, Int c, Any obj);
Cell		getNth0CellChain(Chain ch, Int index);
Int		getIndexChain(Chain ch, Any obj);
Int		getArityChain(Chain ch);
Any		getArgChain(Chain ch, Int arg);
status		makeClassChain(Class class);

/* adt/chaintable.c */
status		appendChainTable(ChainTable ct, Any name, Any value);
status		makeClassChainTable(Class class);

/* adt/constant.c */
status		makeClassConstant(Class class);

/* adt/date.c */
Date		CtoDate(long int time);
status		makeClassDate(Class class);

/* adt/dict.c */
DictItem	getMemberDict(Dict dict, Any obj);
status		deleteDict(Dict dict, Any obj);
status		appendDict(Dict dict, DictItem di);
status		membersDict(Dict dict, Chain members);
DictItem	getFindIndexDict(Dict dict, Int ln);
DictItem	getFindPrefixDict(Dict dict, StringObj str, Int from, Bool ign_case);
status		clearDict(Dict dict);
Any		getBrowserDict(Dict d);
status		makeClassDict(Class class);

/* adt/dictitem.c */
CharArray	getLabelDictItem(DictItem di);
status		makeClassDictItem(Class class);

/* adt/hashtable.c */
HashTable	createHashTable(Int buckets, Bool refer);
status		freeHashTable(HashTable ht);
status		initialiseHashTable(HashTable ht, Int buckets);
status		appendHashTable(HashTable ht, Any name, Any value);
status		deleteHashTable(HashTable ht, Any name);
status		clearHashTable(HashTable ht);
status		makeClassHashTable(Class class);

/* adt/number.c */
status		makeClassNumber(Class class);

/* adt/point.c */
status		equalPoint(Point p1, Point p2);
status		copyPoint(Point p1, Point p2);
status		setPoint(Point pt, Int x, Int y);
status		offsetPoint(Point pt, Int x, Int y);
int		get_distance_point(Point p, int x, int y);
Int		getDistancePoint(Point p, Point q);
status		plusPoint(Point p, Point q);
status		minusPoint(Point p, Point q);
Point		getPlusPoint(Point p1, Point p2);
Point		getMinusPoint(Point p1, Point p2);
Point		getMirrorPoint(Point p1, Point p2);
status		makeClassPoint(Class class);

/* adt/real.c */
Real		CtoReal(float f);
Real		getConvertReal(Class class, Any obj);
status		equalReal(Real r, Real r2);
status		valueReal(Real r, Real v);
status		makeClassReal(Class class);

/* adt/region.c */
status		insideRegion(RegionObj r, Area a, Point p);
status		makeClassRegion(Class class);

/* adt/sheet.c */
Sheet		getCopySheet(Sheet sh);
status		isAttributeSheet(Sheet sh, Any name);
Attribute	getMemberSheet(Sheet sh, register Any name);
status		deleteSheet(Sheet sh, Any name);
Any		getValueSheet(Sheet sh, Any name);
status		valueSheet(Sheet sh, Any name, Any value);
status		makeClassSheet(Class class);

/* adt/size.c */
status		equalSize(Size s, Size s2);
status		copySize(Size s, Size s2);
status		setSize(Size s, Int w, Int h);
status		makeClassSize(Class class);

/* adt/tuple.c */
status		initialiseTuple(Tuple t, Any first, Any second);
status		makeClassTuple(Class class);

/* adt/vector.c */
status		initialiseVectorv(Vector v, int argc, Any *argv);
Vector		createVectorv(int argc, Any *argv);
Any		getTailVector(Vector v);
Vector		getCopyVector(Vector v);
status		fillVector(Vector v, Any obj, Int from, Int to);
status		shiftVector(Vector v, Int places);
Any		getElementVector(Vector v, Int e);
status		elementVector(Vector v, Int e, Any obj);
status		appendVector(Vector v, int argc, Any obj []);
Any		getArgVector(Vector v, Int arg);
Int		getArityVector(Vector v);
status		makeClassVector(Class class);
