
/* ../src/men/button.c */
int		accelerator_code(Name a);
status		RedrawAreaButton(Button b, Area a);
Point		getReferenceButton(Button b);
status		makeButtonGesture(void);
status		makeClassButton(Class class);

/* ../src/men/dialogitem.c */
status		createDialogItem(Any obj, Name name);
status		unlinkDialogItem(DialogItem di);
status		RedrawLabelDialogItem(Any obj, int acc, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
status		dia_label_size(Any obj, int *w, int *h, int *isimage);
status		labelDialogItem(DialogItem di, Any label);
status		labelFontDialogItem(DialogItem di, FontObj font);
status		eventDialogItem(Any obj, EventObj ev);
status		changedDialogItem(Any obj);
Point		getReferenceDialogItem(Any obj);
status		modifiedDialogItem(Any di, Bool modified);
status		assignAccelerators(Chain objects, Name prefix, Name label_method);
status		makeClassDialogItem(Class class);

/* ../src/men/label.c */
status		makeClassLabel(Class class);

/* ../src/men/menu.c */
status		initialiseMenu(Menu m, Name name, Name kind, Code msg);
void		area_menu_item(Menu m, MenuItem mi, int *x, int *y, int *w, int *h);
Int		getCenterYMenuItemMenu(Menu m, Any obj);
MenuItem	getItemFromEventMenu(Menu m, EventObj ev);
status		forwardMenu(Menu m, Code msg, EventObj ev);
MenuItem	findMenuItemMenu(Menu m, Any spec);
status		previewMenu(Menu m, MenuItem mi);
status		selectionMenu(Menu m, Any selection);
status		toggleMenu(Menu m, MenuItem mi);
status		appendMenu(Menu m, Any mi);
status		deleteMenu(Menu m, Any obj);
status		updateMenu(Menu m, Any context);
status		makeClassMenu(Class class);

/* ../src/men/menubar.c */
status		makeClassMenuBar(Class class);

/* ../src/men/menuitem.c */
status		selectedMenuItem(MenuItem mi, Bool val);
status		hasValueMenuItem(MenuItem mi, Any value);
status		makeClassMenuItem(Class class);

/* ../src/men/popup.c */
status		defaultPopupImages(PopupObj p);
status		makeClassPopup(Class class);

/* ../src/men/slider.c */
status		makeClassSlider(Class class);

/* ../src/men/textitem.c */
status		initialiseTextItem(TextItem ti, Name name, Any val, Code msg);
Browser		CompletionBrowser(void);
status		completerShownDialogItem(Any di);
status		quitCompleterDialogItem(Any di);
status		selectCompletionDialogItem(Any item, Chain matches, CharArray searchstring, Int autohide);
status		styleTextItem(TextItem ti, Name style);
int		text_item_combo_width(TextItem ti);
status		typedTextItem(TextItem ti, EventId id);
status		applyTextItem(TextItem ti, Bool always);
status		displayedValueTextItem(TextItem ti, CharArray txt);
status		valueWidthTextItem(TextItem ti, Int val);
status		makeClassTextItem(Class class);

/* ../src/men/tab.c */
status		makeClassTab(Class class);

/* ../src/men/diagroup.c */
status		initialiseDialogGroup(DialogGroup g, Name name, Name kind);
void		compute_label_size_dialog_group(DialogGroup g, int *w, int *h);
status		labelFormatDialogGroup(DialogGroup g, Name fmt);
status		RedrawLabelDialogGroup(DialogGroup g, int acc, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
status		eventDialogGroup(DialogGroup g, EventObj ev);
status		makeClassDialogGroup(Class class);

/* ../src/men/tabstack.c */
status		makeClassTabStack(Class class);

/* ../src/men/labelbox.c */
status		makeClassLabelBox(Class class);

/* ../src/men/intitem.c */
status		makeClassIntItem(Class class);
