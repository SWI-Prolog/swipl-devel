#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/men/button.c */
COMMON(int)	accelerator_code(Name a);
COMMON(status)	RedrawAreaButton(Button b, Area a);
COMMON(Point)	getReferenceButton(Button b);
COMMON(status)	makeButtonGesture(void);
COMMON(status)	isApplyButton(Button b);
COMMON(status)	makeClassButton(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/dialogitem.c */
COMMON(status)	createDialogItem(Any obj, Name name);
COMMON(status)	unlinkDialogItem(DialogItem di);
COMMON(status)	RedrawLabelDialogItem(Any obj, int acc, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
COMMON(status)	dia_label_size(Any obj, int *w, int *h, int *isimage);
COMMON(status)	labelDialogItem(DialogItem di, Any label);
COMMON(status)	labelFontDialogItem(DialogItem di, FontObj font);
COMMON(status)	eventDialogItem(Any obj, EventObj ev);
COMMON(status)	changedDialogItem(Any obj);
COMMON(Point)	getReferenceDialogItem(Any obj);
COMMON(status)	modifiedDialogItem(Any di, Bool modified);
COMMON(status)	assignAccelerators(Chain objects, Name prefix, Name label_method);
COMMON(status)	makeClassDialogItem(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/label.c */
COMMON(status)	makeClassLabel(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/menu.c */
COMMON(status)	initialiseMenu(Menu m, Name name, Name kind, Code msg);
COMMON(void)	area_menu_item(Menu m, MenuItem mi, int *x, int *y, int *w, int *h);
COMMON(Int)	getCenterYMenuItemMenu(Menu m, Any obj);
COMMON(MenuItem) getItemFromEventMenu(Menu m, EventObj ev);
COMMON(status)	forwardMenu(Menu m, Code msg, EventObj ev);
COMMON(MenuItem) findMenuItemMenu(Menu m, Any spec);
COMMON(status)	previewMenu(Menu m, MenuItem mi);
COMMON(status)	selectionMenu(Menu m, Any selection);
COMMON(status)	toggleMenu(Menu m, MenuItem mi);
COMMON(status)	appendMenu(Menu m, Any mi);
COMMON(status)	deleteMenu(Menu m, Any obj);
COMMON(status)	updateMenu(Menu m, Any context);
COMMON(status)	makeClassMenu(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/menubar.c */
COMMON(status)	makeClassMenuBar(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/menuitem.c */
COMMON(status)	selectedMenuItem(MenuItem mi, Bool val);
COMMON(status)	hasValueMenuItem(MenuItem mi, Any value);
COMMON(status)	makeClassMenuItem(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/popup.c */
COMMON(status)	defaultPopupImages(PopupObj p);
COMMON(status)	makeClassPopup(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/slider.c */
COMMON(status)	makeClassSlider(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/textitem.c */
COMMON(status)	initialiseTextItem(TextItem ti, Name name, Any val, Code msg);
COMMON(Browser)	CompletionBrowser(void);
COMMON(status)	completerShownDialogItem(Any di);
COMMON(status)	quitCompleterDialogItem(Any di);
COMMON(status)	selectCompletionDialogItem(Any item, Chain matches, CharArray searchstring, Int autohide);
COMMON(status)	forwardCompletionEvent(EventObj ev);
COMMON(status)	styleTextItem(TextItem ti, Name style);
COMMON(int)	text_item_combo_width(TextItem ti);
COMMON(status)	typedTextItem(TextItem ti, EventId id);
COMMON(status)	applyTextItem(TextItem ti, Bool always);
COMMON(status)	displayedValueTextItem(TextItem ti, CharArray txt);
COMMON(status)	valueWidthTextItem(TextItem ti, Int val);
COMMON(status)	makeClassTextItem(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/tab.c */
COMMON(status)	changedLabelImageTab(Tab t);
COMMON(status)	makeClassTab(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/diagroup.c */
COMMON(status)	initialiseDialogGroup(DialogGroup g, Name name, Name kind);
COMMON(void)	compute_label_size_dialog_group(DialogGroup g, int *w, int *h);
COMMON(status)	labelFormatDialogGroup(DialogGroup g, Name fmt);
COMMON(status)	RedrawLabelDialogGroup(DialogGroup g, int acc, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
COMMON(status)	eventDialogGroup(DialogGroup g, EventObj ev);
COMMON(status)	makeClassDialogGroup(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/tabstack.c */
COMMON(status)	makeClassTabStack(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/labelbox.c */
COMMON(status)	makeClassLabelBox(Class class);

/* /staff/jan/src/pl/packages/xpce/src/men/intitem.c */
COMMON(status)	makeClassIntItem(Class class);
