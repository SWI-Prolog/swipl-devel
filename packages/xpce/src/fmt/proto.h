
/* fmt/layoutmgr.c */
status		initialiseLayoutManager(Any obj);
status		requestComputeLayoutManager(LayoutManager mgr, Any val);
status		ComputeLayoutManager(Any obj);
status		makeClassLayoutManager(Class class);

/* fmt/layoutitf.c */
status		initialiseLayoutInterface(Any obj, Graphical image);
status		unlinkLayoutInterface(Any obj);
status		changedAreaLayoutInterface(LayoutInterface itf);
status		makeClassLayoutInterface(Class class);

/* fmt/table.c */
TableRow	getRowTable(Table tab, Int y, Bool create);
TableColumn	getColumnTable(Table tab, Int x, Bool create);
status		changedTable(Table tab);
status		makeClassTable(Class class);

/* fmt/tabcell.c */
status		initialiseTableCell(TableCell c, Graphical image);
Name		getHalignTableCell(TableCell cell);
Name		getValignTableCell(TableCell cell);
void		table_cell_padding(TableCell cell, int *pxptr, int *pyptr);
void		dims_table_cell(TableCell cell, TableCellDimensions dims);
status		placeImageTableCell(TableCell cell);
status		makeClassTableCell(Class class);

/* fmt/tabslice.c */
status		initialiseTableSlice(TableSlice c);
status		makeClassTableSlice(Class class);
TableCell	getCellTableColumn(TableColumn col, Int y);
status		makeClassTableColumn(Class class);
TableCell	getCellTableRow(TableRow row, Int x);
status		cellTableRow(TableRow row, Int col, TableCell cell);
status		indexTableRow(TableRow row, Int index);
status		makeClassTableRow(Class class);
