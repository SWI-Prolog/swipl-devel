
/* ../src/fmt/layoutmgr.c */
status		initialiseLayoutManager(Any obj);
status		requestComputeLayoutManager(LayoutManager mgr, Any val);
status		ComputeLayoutManager(Any obj);
status		makeClassLayoutManager(Class class);

/* ../src/fmt/layoutitf.c */
status		initialiseLayoutInterface(Any obj, Graphical image);
status		unlinkLayoutInterface(Any obj);
status		changedAreaLayoutInterface(LayoutInterface itf);
status		makeClassLayoutInterface(Class class);

/* ../src/fmt/table.c */
TableRow	getRowTable(Table tab, Any y, Bool create);
TableColumn	getColumnTable(Table tab, Any x, Bool create);
status		changedTable(Table tab);
void		cell_stretchability(TableCell cell, Name which, stretch *s);
status		makeClassTable(Class class);

/* ../src/fmt/tabcell.c */
Name		getHalignTableCell(TableCell cell);
Name		getValignTableCell(TableCell cell);
void		table_cell_padding(TableCell cell, int *pxptr, int *pyptr);
void		dims_table_cell(TableCell cell, TableCellDimensions dims);
status		placeImageTableCell(TableCell cell);
status		makeClassTableCell(Class class);

/* ../src/fmt/tabslice.c */
Any		findNamedSlice(Vector v, Name name);
status		makeClassTableSlice(Class class);
status		makeClassTableColumn(Class class);
TableCell	getCellTableRow(TableRow row, Any x);
status		cellTableRow(TableRow row, Int col, TableCell cell);
status		indexTableRow(TableRow row, Int index);
status		makeClassTableRow(Class class);
