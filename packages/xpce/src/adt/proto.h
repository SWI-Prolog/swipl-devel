
/* ../src/adt/area.c */
status		intersectionArea(Area a, Area b);
status		unionNormalisedArea(Area a, Area b);
status		sizeArea(Area a, Size s);
Size		getSizeArea(Area a);
status		pointInArea(Area a, Point p);
status		overlapArea(Area a, Area b);
status		relativeMoveArea(Area a, Point p);
status		relativeMoveBackArea(Area a, Point p);
Point		getPositionArea(Area a);
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

/* ../src/adt/atable.c */
status		makeClassAtable(Class class);

/* ../src/adt/attribute.c */
status		makeClassAttribute(Class class);

/* ../src/adt/bool.c */
status		makeClassBool(Class class);

/* ../src/adt/chain.c */
status		initialiseChainv(Chain ch, int argc, Any *argv);
status		clearChain(Chain ch);
Int		getSizeChain(Chain ch);
status		prependChain(register Chain ch, Any obj);
status		appendChain(register Chain ch, Any obj);
status		addChain(register Chain ch, Any obj);
status		insertChain(Chain ch, Any obj);
status		insertAfterChain(Chain ch, Any obj, Any obj2);
status		insertBeforeChain(Chain ch, Any obj, Any obj2);
status		swapChain(Chain ch, Any obj1, Any obj2);
status		deleteHeadChain(Chain ch);
status		deleteChain(Chain ch, register Any obj);
status		deleteCellChain(Chain ch, Cell cell);
status		memberChain(Chain ch, Any obj);
Any		getNextChain(Chain ch, Any val);
Any		getPreviousChain(Chain ch, Any val);
status		forAllChain(Chain ch, Code code, Bool safe);
status		forSomeChain(Chain ch, Code code, Bool safe);
Any		getFindChain(Chain ch, Code code);
Chain		getFindAllChain(Chain ch, Code code);
status		mergeChain(Chain ch, Chain ch2);
status		replaceChain(Chain ch, Any obj1, Any obj2);
status		emptyChain(Chain ch);
Chain		getCopyChain(Chain ch);
int		qsortCompareObjects(const void *o1, const void *o2);
status		sortChain(Chain ch, Code msg, Bool unique);
status		sortNamesChain(Chain ch, Bool unique);
Tuple		getCompleteNameChain(Chain ch, CharArray prefix, Function map, Bool ignore_case);
Chain		getIntersectionChain(Chain ch, Chain ch2);
Any		getHeadChain(Chain ch);
Any		getDeleteHeadChain(Chain ch);
Any		getTailChain(Chain ch);
status		moveBeforeChain(Chain ch, Any obj1, Any obj2);
status		moveAfterChain(Chain ch, Any obj1, Any obj2);
status		beforeChain(Chain ch, Any obj1, Any obj2);
Any		getNth1Chain(Chain ch, Int index);
Any		getNth0Chain(Chain ch, Int index);
status		cellValueChain(Chain ch, Int c, Any obj);
Cell		getNth0CellChain(Chain ch, Int index);
Int		getIndexChain(Chain ch, Any obj);
Int		getArityChain(Chain ch);
Any		getArgChain(Chain ch, Int arg);
status		makeClassChain(Class class);

/* ../src/adt/chaintable.c */
status		appendChainTable(ChainTable ct, Any name, Any value);
status		makeClassChainTable(Class class);

/* ../src/adt/constant.c */
status		makeClassConstant(Class class);

/* ../src/adt/date.c */
Date		CtoDate(long int time);
status		makeClassDate(Class class);

/* ../src/adt/dict.c */
DictItem	getMemberDict(Dict dict, Any obj);
status		deleteDict(Dict dict, Any obj);
status		appendDict(Dict dict, DictItem di);
DictItem	getFindIndexDict(Dict dict, Int ln);
DictItem	getFindPrefixDict(Dict dict, StringObj str, Int from, Bool ign_case);
status		clearDict(Dict dict);
status		makeClassDict(Class class);

/* ../src/adt/dictitem.c */
CharArray	getLabelDictItem(DictItem di);
status		makeClassDictItem(Class class);

/* ../src/adt/hashtable.c */
HashTable	createHashTable(Int buckets, Name refer);
status		freeHashTable(HashTable ht);
status		initialiseHashTable(HashTable ht, Int buckets);
status		appendHashTable(HashTable ht, Any name, Any value);
status		deleteHashTable(HashTable ht, Any name);
status		clearHashTable(HashTable ht);
status		makeClassHashTable(Class class);

/* ../src/adt/number.c */
Number		CtoNumber(long i);
status		makeClassNumber(Class class);

/* ../src/adt/point.c */
status		equalPoint(Point p1, Point p2);
status		copyPoint(Point p1, Point p2);
status		setPoint(Point pt, Int x, Int y);
status		offsetPoint(Point pt, Int x, Int y);
int		get_distance_point(Point p, int x, int y);
Int		getDistancePoint(Point p, Point q);
status		plusPoint(Point p, Point q);
status		minusPoint(Point p, Point q);
status		makeClassPoint(Class class);

/* ../src/adt/real.c */
void		setReal(Real r, double f);
double		valReal(Real r);
Real		CtoReal(double f);
Real		getConvertReal(Class class, Any obj);
status		valueReal(Real r, Real v);
status		makeClassReal(Class class);

/* ../src/adt/region.c */
status		insideRegion(RegionObj r, Area a, Point p);
status		makeClassRegion(Class class);

/* ../src/adt/sheet.c */
Sheet		getCopySheet(Sheet sh);
status		isAttributeSheet(Sheet sh, Any name);
Attribute	getMemberSheet(Sheet sh, register Any name);
status		deleteSheet(Sheet sh, Any name);
Any		getValueSheet(Sheet sh, Any name);
status		valueSheet(Sheet sh, Any name, Any value);
status		makeClassSheet(Class class);

/* ../src/adt/size.c */
status		equalSize(Size s, Size s2);
status		copySize(Size s, Size s2);
status		setSize(Size s, Int w, Int h);
status		makeClassSize(Class class);

/* ../src/adt/tuple.c */
status		initialiseTuple(Tuple t, Any first, Any second);
status		makeClassTuple(Class class);

/* ../src/adt/vector.c */
status		initialiseVectorv(Vector v, int argc, Any *argv);
Vector		createVectorv(int argc, Any *argv);
status		unlinkVector(Vector v);
Int		getLowIndexVector(Vector v);
Int		getHighIndexVector(Vector v);
status		rangeVector(Vector v, Int low, Int high);
status		clearVector(Vector v);
Any		getTailVector(Vector v);
Vector		getCopyVector(Vector v);
status		fillVector(Vector v, Any obj, Int from, Int to);
status		shiftVector(Vector v, Int places);
Any		getElementVector(Vector v, Int e);
status		elementVector(Vector v, Int e, Any obj);
status		appendVector(Vector v, int argc, Any obj []);
Int		getIndexVector(Vector v, Any obj);
Any		getArgVector(Vector v, Int arg);
Int		getArityVector(Vector v);
status		makeClassVector(Class class);
