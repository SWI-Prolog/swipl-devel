#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/fmt/layoutmgr.c */
COMMON(status)	initialiseLayoutManager(Any obj);
COMMON(status)	requestComputeLayoutManager(LayoutManager mgr, Any val);
COMMON(status)	ComputeLayoutManager(Any obj);
COMMON(status)	makeClassLayoutManager(Class class);

/* /staff/jan/src/pl/packages/xpce/src/fmt/layoutitf.c */
COMMON(status)	initialiseLayoutInterface(Any obj, Graphical image);
COMMON(status)	unlinkLayoutInterface(Any obj);
COMMON(status)	changedAreaLayoutInterface(LayoutInterface itf);
COMMON(status)	makeClassLayoutInterface(Class class);

/* /staff/jan/src/pl/packages/xpce/src/fmt/table.c */
COMMON(TableRow) getRowTable(Table tab, Any y, Bool create);
COMMON(TableColumn) getColumnTable(Table tab, Any x, Bool create);
COMMON(void)	table_row_range(Table tab, int *ymin, int *ymax);
COMMON(void)	table_column_range(Table tab, int *xmin, int *xmax);
COMMON(Any)	getCellFromPositionTable(Table tab, Any pos, Bool onborder);
COMMON(status)	changedTable(Table tab);
COMMON(void)	cell_stretchability(TableCell cell, Name which, stretch *s);
COMMON(status)	makeClassTable(Class class);

/* /staff/jan/src/pl/packages/xpce/src/fmt/tabcell.c */
COMMON(Name)	getHalignTableCell(TableCell cell);
COMMON(Name)	getValignTableCell(TableCell cell);
COMMON(void)	table_cell_padding(TableCell cell, int *pxptr, int *pyptr);
COMMON(void)	dims_table_cell(TableCell cell, TableCellDimensions dims);
COMMON(status)	placeImageTableCell(TableCell cell);
COMMON(status)	makeClassTableCell(Class class);

/* /staff/jan/src/pl/packages/xpce/src/fmt/tabslice.c */
COMMON(Any)	findNamedSlice(Vector v, Name name);
COMMON(status)	makeClassTableSlice(Class class);
COMMON(status)	makeClassTableColumn(Class class);
COMMON(TableCell) getCellTableRow(TableRow row, Any x);
COMMON(status)	cellTableRow(TableRow row, Int col, TableCell cell);
COMMON(status)	indexTableRow(TableRow row, Int index);
COMMON(status)	makeClassTableRow(Class class);
