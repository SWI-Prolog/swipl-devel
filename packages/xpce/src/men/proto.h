
/* button.c */
status		RedrawAreaButton(Button b, Area a);
Point		getReferenceButton(Button b);
status		makeButtonGesture(void);
status		makeClassButton(Class class);

/* dialogitem.c */
status		createDialogItem(Any obj, Name name);
status		unlinkDialogItem(DialogItem di);
status		labelDialogItem(DialogItem di, Name label);
status		forwardDialogItem(DialogItem di, Code msg, EventObj ev);
status		eventDialogItem(Any obj, EventObj ev);
status		changedDialogItem(Any obj);
Point		getReferenceDialogItem(Any obj);
Bool		getModifiedDialogItem(Dialog di);
status		modifiedDialogItem(Any di, Bool modified);
status		makeClassDialogItem(Class class);

/* label.c */
status		makeClassLabel(Class class);

/* menu.c */
status		initialiseMenu(Menu m, Name name, Name kind, Code msg);
void		area_menu_item(Menu m, MenuItem mi, int *x, int *y, int *w, int *h);
Int		getCenterYMenuItemMenu(Menu m, Any obj);
MenuItem	getItemFromEventMenu(Menu m, EventObj ev);
status		forwardMenu(Menu m, Code msg, EventObj ev);
MenuItem	findMenuItemMenu(Menu m, Any spec);
status		previewMenu(Menu m, MenuItem mi);
status		selectionMenu(Menu m, Any selection);
status		toggleMenu(Menu m, MenuItem mi);
status		deleteMenu(Menu m, Any obj);
status		updateMenu(Menu m, Any context);
status		modifiedMenu(Menu m, Bool val);
status		makeClassMenu(Class class);

/* menubar.c */
status		makeClassMenuBar(Class class);

/* menuitem.c */
status		selectedMenuItem(MenuItem mi, Bool val);
status		hasValueMenuItem(MenuItem mi, Any value);
status		makeClassMenuItem(Class class);

/* popup.c */
status		keyPopup(PopupObj p, Name key);
status		makeClassPopup(Class class);

/* slider.c */
status		makeClassSlider(Class class);

/* textitem.c */
status		makeClassTextItem(Class class);
