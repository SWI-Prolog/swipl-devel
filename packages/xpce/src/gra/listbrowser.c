/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static Int    normalise_index(ListBrowser, Int);
static status clearSelectionListBrowser(ListBrowser);
static status ChangeItemListBrowser(ListBrowser, DictItem);
static status ChangedListBrowser(ListBrowser);
static status geometryListBrowser(ListBrowser, Int, Int, Int, Int);
static status executeSearchListBrowser(ListBrowser);
static status forwardListBrowser(ListBrowser, DictItem, Name);
static status deselectListBrowser(ListBrowser, DictItem);
static status showLabelListBrowser(ListBrowser, Bool);
static status setSelectionListBrowser(ListBrowser, DictItem);
static status toggleSelectionListBrowser(ListBrowser, DictItem);
static status openListBrowser(ListBrowser, DictItem);
static status selectListBrowser(ListBrowser, DictItem);
static status scrollUpListBrowser(ListBrowser, Int);
static status scrollDownListBrowser(ListBrowser, Int);
static status extendPrefixListBrowser(ListBrowser);

#define dictItemName(di) (isDefault(di->label) ? di->key : di->label)

		/********************************
		*            CREATE		*
		********************************/

static status
initialiseListBrowser(ListBrowser lb, Dict dict, Int w, Int h)
{ int fw, fh, iw, ih;

  if ( isDefault(dict) )
    dict = newObject(ClassDict, 0);

  if ( notNil(dict->browser) )
    return errorPce(lb, NAME_alreadyShown, dict, dict->browser);

  assign(lb, size, newObject(ClassSize, 0));
  copySize(lb->size, getResourceValueObject(lb, NAME_size));
  if ( notDefault(w) ) assign(lb->size, w, w);
  if ( notDefault(h) ) assign(lb->size, h, h);

  initialiseDevice((Device) lb);

  assign(lb,   dict,                  dict);
  assign(dict, browser,               lb);
  assign(lb,   status, 		      NAME_inactive);
  assign(lb,   key_binding,	      newObject(ClassKeyBinding, NIL,
						NAME_listBrowser, 0));
  assign(lb,   select_message,        NIL);
  assign(lb,   select_middle_message, NIL);
  assign(lb,   open_message,          NIL);
  assign(lb,   multiple_selection,    OFF);
  assign(lb,   selection,             NIL);
  assign(lb,   start,	              ZERO);
  assign(lb,   search_string,         NIL);
  assign(lb,   search_origin,         ZERO);
  assign(lb,   search_hit,	      toInt(-1));
  assign(lb,   label_text,	      NIL);
  assign(lb,   styles,		      newObject(ClassSheet, 0));
  assign(lb, selection_style, getResourceValueObject(lb, NAME_selectionStyle));

  lb->start_cell = NIL;

  assign(lb, font, getResourceValueObject(lb, NAME_font));
  fw = valInt(getExFont(lb->font));
  fh = valInt(getHeightFont(lb->font));
  iw = valInt(lb->size->w) * fw + 2 * TXT_X_MARGIN;
  ih = valInt(lb->size->h) * fh + 2 * TXT_Y_MARGIN;

  assign(lb, image, newObject(ClassTextImage, lb, toInt(iw), toInt(ih), 0));
  assign(lb->image, wrap, NAME_none);
  assign(lb, scroll_bar, newObject(ClassScrollBar, lb, NAME_vertical, 0));

  send(lb->image, NAME_cursor, getResourceValueObject(lb, NAME_cursor), 0);
  send(lb->image, NAME_set,
       lb->scroll_bar->area->w, ZERO, DEFAULT, toInt(ih), 0);
  displayDevice(lb, lb->scroll_bar, DEFAULT);
  displayDevice(lb, lb->image, DEFAULT);

  doSetGraphical(lb, DEFAULT, DEFAULT, toInt(iw), toInt(ih));

  succeed;
}


static status
unlinkListBrowser(ListBrowser lb)
{ if ( notNil(lb->dict) )
  { assign(lb->dict, browser, NIL);
    assign(lb, dict, NIL);
  }

  return unlinkDevice((Device) lb);
}


static Any
lbReceiver(ListBrowser lb)
{ if ( instanceOfObject(lb->device, ClassBrowser) )
    return lb->device;

  return lb;
}

		 /*******************************
		 *	     LOAD/SAVE		*
		 *******************************/

static status
storeListBrowser(ListBrowser lb, FileObj file)
{ return storeSlotsObject(lb, file);
}


static status
loadListBrowser(ListBrowser lb, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(lb, fd, def));

  if ( isNil(lb->status) )
    assign(lb, status, NAME_inactive);

  lb->start_cell = NIL;

  succeed;
}


		/********************************
		*            LABEL		*
		********************************/

static status
labelListBrowser(ListBrowser lb, Name lbl)
{ showLabelListBrowser(lb, ON);

  send(lb->label_text, NAME_string, lbl, 0);
  geometryListBrowser(lb, DEFAULT, DEFAULT, DEFAULT, DEFAULT);

  succeed;
}


Name
getLabelListBrowser(ListBrowser lb)
{ if ( notNil(lb->label_text) )
    answer(getValueCharArray((CharArray) lb->label_text->string));

  fail;
}


static status
showLabelListBrowser(ListBrowser lb, Bool val)
{ if ( isNil(lb->label_text) )
  { if ( val == ON )
    { assign(lb, label_text,
	     newObject(ClassText, getLabelNameName(lb->name), NAME_left,
		       getResourceValueObject(lb, NAME_labelFont), 0));
      marginText(lb->label_text, lb->area->w, NAME_clip);
      displayDevice(lb, lb->label_text, DEFAULT);
      return geometryListBrowser(lb, DEFAULT, DEFAULT,
				 add(lb->image->area->x, lb->image->area->w),
				 lb->image->area->h);
    } else
      succeed;
  }

  if ( lb->label_text->displayed != val )
  { DisplayedGraphical(lb->label_text, val);
    return geometryListBrowser(lb, DEFAULT, DEFAULT, DEFAULT, DEFAULT);
  }

  succeed;
}


static Bool
getShowLabelListBrowser(ListBrowser lb)
{ if ( notNil(lb->label_text) )
    answer(lb->label_text->displayed);

  answer(OFF);
}


		 /*******************************
		 *	     TYPING		*
		 *******************************/

static status
statusListBrowser(ListBrowser lb, Name stat)
{ if ( lb->status != stat )
  { assign(lb, status, stat);
    penGraphical((Graphical) lb->image,
		 stat == NAME_active ? add(lb->pen, ONE) : lb->pen);
  }

  succeed;
}


static status
nextListBrowser(ListBrowser lb)
{ return send(lb->device, NAME_advance, lb, 0);
}


static status
extendPrefixOrNextListBrowser(ListBrowser lb)
{ if ( notNil(lb->search_string) )
  { StringObj ext = lb->search_string;

    extendPrefixListBrowser(lb);
    if ( lb->search_string != ext )
      succeed;
  }

  return nextListBrowser(lb);
}


static status
WantsKeyboardFocusListBrowser(ListBrowser lb)
{ if ( notNil(lb->dict) &&
       getSizeChain(lb->dict->members) != ZERO )
    succeed;

  fail;
}


		/********************************
		*          SCROLLBAR		*
		********************************/

static Int
getViewListBrowser(ListBrowser lb)
{ answer(div(getViewTextImage(lb->image), toInt(BROWSER_LINE_WIDTH)));
}


static Int
getLengthListBrowser(ListBrowser lb)
{ answer(notNil(lb->dict) ? lb->dict->members->size : ZERO);
}


		/********************************
		*            GEOMETRY		*
		********************************/

static status
geometryListBrowser(ListBrowser lb, Int x, Int y, Int w, Int h)
{ int ix, iw, sw, iy, ih;
  int pen = valInt(lb->pen);

  if ( isDefault(x) ) x = lb->area->x;
  if ( isDefault(y) ) y = lb->area->y;
  if ( isDefault(w) ) w = lb->area->w;
  if ( isDefault(h) ) h = lb->area->h;

  if ( valInt(w) < 50 ) w = toInt(50);
  if ( valInt(h) < 20 ) h = toInt(20);

  sw = isNil(lb->scroll_bar) ? 0 : valInt(getMarginScrollBar(lb->scroll_bar));
  iw = valInt(w) - abs(sw);

  assign(lb->size, w, div(toInt(iw), getExFont(lb->font)));
  assign(lb->size, h, div(h, getHeightFont(lb->font)));
  
  ix = (sw < 0 ? -sw : 0);
  if ( getShowLabelListBrowser(lb) == ON )
  { send(lb->label_text, NAME_set, ZERO, ZERO, w, 0);
    iy = valInt(lb->label_text->area->h) - pen;
  } else
  { iy = 0;
  }
  ih = valInt(h) - iy;

  send(lb->image, NAME_set, toInt(ix), toInt(iy), toInt(iw), toInt(ih), 0);
  if ( notNil(lb->scroll_bar) )
    placeScrollBar(lb->scroll_bar, (Graphical) lb->image);

  return geometryDevice((Device) lb, x, y, DEFAULT, DEFAULT);
}


static status
SizeListBrowser(ListBrowser lb, Size size)
{ return doSetGraphical(lb, DEFAULT, DEFAULT, size->w, size->h);
}


static status
requestGeometryListBrowser(ListBrowser lb, Int x, Int y, Int w, Int h)
{ Any v;

  if ( notDefault(w) )
    w = mul(w, getExFont(lb->font));
  if ( notDefault(h) )
    h = mul(h, getHeightFont(lb->font));

  if ( instanceOfObject(v = lbReceiver(lb), ClassWindow) )
    requestGeometryWindow(v, x, y, w, h);
  else
    requestGeometryGraphical(lb, x, y, w, h);

  succeed;
}


Size
getSizeListBrowser(ListBrowser lb)
{ answer(lb->size);
}


static Int
getWidthListBrowser(ListBrowser lb)
{ answer(lb->size->w);
}


static Int
getHeightListBrowser(ListBrowser lb)
{ answer(lb->size->h);
}

		/********************************
		*            FETCH		*
		********************************/

static Dict	     current_dict;	/* Currently displayed dict */
static Cell	     current_cell;	/* Cell of this item */
static int	     current_item;	/* Index of current name */
static int	     current_index;	/* Current location */
static String 	     current_name;	/* Working on this name */
static int	     current_search;	/* search feedback */
static unsigned char current_atts;	/* Attributes for it */
static FontObj	     current_font;	/* Current font */
static Colour	     current_colour;	/* Current colour */
static Any	     current_background; /* Current background */

static void
compute_current(ListBrowser lb)
{ if ( notNil(current_cell) )
  { DictItem di = (DictItem) current_cell->value;
    Name label = dictItemName(di);
    Style style;

    assert(valInt(di->index) == current_item);
    current_name = &label->data;

    if ( notDefault(di->style) &&
	 (style = getValueSheet(lb->styles, di->style)) )
    { current_font	 = style->font;
      current_colour     = style->colour;
      current_background = style->background;
      current_atts       = style->attributes;

      if ( isDefault(current_font) )
	current_font = lb->font;
    } else
    { current_font       = lb->font;
      current_colour     = DEFAULT;
      current_background = DEFAULT;
      current_atts       = 0;
    }

    if ( selectedListBrowser(lb, di) )
    { if ( isDefault(lb->selection_style) )
	current_atts ^= TXT_HIGHLIGHTED;
      else
      { current_atts |= lb->selection_style->attributes;
	if ( notDefault(lb->selection_style->font) )
	  current_font = lb->selection_style->font;
	if ( notDefault(lb->selection_style->colour) )
	  current_colour = lb->selection_style->colour;
	if ( notDefault(lb->selection_style->background) )
	  current_background = lb->selection_style->background;
      }
    }

    if ( di->index == lb->search_hit )
    { current_search = lb->search_string->data.size;
    } else
      current_search = 0;
  } else
  { current_name       = NULL;		/* past the end */
    current_atts       = 0;
    current_font       = lb->font;
    current_colour     = DEFAULT;
    current_background = DEFAULT;
  }
}


static Cell
find_cell_dict(Dict dict, Int item)
{ if ( notNil(dict) )
  { Cell cell;

    for_cell(cell, dict->members)
      if ( ((DictItem) cell->value)->index == item )
	return cell;
  }

  return NIL;
}


static void
seek_list_browser(ListBrowser lb, long int index)
{ int item = index / BROWSER_LINE_WIDTH;
  Dict d = lb->dict;

  if ( isNil(d) )
    return;

  if ( item != current_item || d != current_dict )
  { if ( item < current_item || d != current_dict )
    { current_cell = find_cell_dict(lb->dict, toInt(item));
      assert(current_cell != NULL);
      current_dict = d;
    } else
    { for( ; item > current_item && notNil(current_cell); current_item++ )
    	current_cell = current_cell->next;
      assert(current_cell != NULL);
    }

    current_item = item;

    compute_current(lb);
  }  

  current_index = index;
}


static long
scan_list_browser(ListBrowser lb, long int from, int dir,
		  int how, int category, int *eof)
{ int item = from / BROWSER_LINE_WIDTH;

  assert(dir > 0 && how == TEXT_SCAN_FOR && category == EL);
  
  *eof = (isNil(lb->dict) ||
	  ((item + 1 >= valInt(lb->dict->members->size)) ? TRUE : FALSE));

  return (item + 1) * BROWSER_LINE_WIDTH - 1;
}


static long
fetch_list_browser(ListBrowser lb, TextChar tc)
{ int index = current_index;
  int pos   = current_index++ % BROWSER_LINE_WIDTH;

  if ( current_name )
  { int len = current_name->size;

    if ( pos < len )
      tc->c = str_fetch(current_name, pos);
    else if ( pos == len )
    { tc->c = '\n';
      current_index = ((index / BROWSER_LINE_WIDTH) + 1) * BROWSER_LINE_WIDTH;
    }
  } else
    tc->c = EOB;

  tc->font       = current_font;
  tc->attributes = current_atts;
  tc->colour	 = current_colour;
  tc->background = current_background;

  if ( pos < current_search )
  { Style s = getResourceValueObject(lb, NAME_isearchStyle);

    if ( s && notDefault(s) )
    { tc->attributes |= s->attributes;
      if ( notDefault(s->font) )       tc->font = s->font;
      if ( notDefault(s->colour) )     tc->colour = s->colour;
      if ( notDefault(s->background) ) tc->background = s->background;
    } else
      tc->attributes ^= TXT_HIGHLIGHTED;
  }

  return current_index;
}


static Int
getSeekFunctionListBrowser(ListBrowser lb)
{ DEBUG(NAME_SeekFunction, printf("seek_list_browser = 0x%lx\n",
				  (ulong) seek_list_browser));
  answer(TextPointerToInt(seek_list_browser));
}


static Int
getScanFunctionListBrowser(ListBrowser lb)
{ answer(TextPointerToInt(scan_list_browser));
}


static Int
getFetchFunctionListBrowser(ListBrowser lb)
{ answer(TextPointerToInt(fetch_list_browser));
}


		/********************************
		*            REDRAW		*
		********************************/

static status
computeListBrowser(ListBrowser lb)
{ if ( notNil(lb->request_compute) )
  { DictItem di;

    assign(lb, start, normalise_index(lb, lb->start));

    if ( isNil(lb->start_cell) ||
	 !isProperObject((di = lb->start_cell->value)) ||
	 di->index != lb->start )
      lb->start_cell = find_cell_dict(lb->dict, lb->start);

    current_cell = lb->start_cell;
    current_item = valInt(lb->start);
    current_dict = lb->dict;
    compute_current(lb);

    computeTextImage(lb->image);
    requestComputeGraphical(lb->scroll_bar, DEFAULT); /* TBD: where to put? */
    return computeDevice(lb);
  }
  
  succeed;
}


static Int
normalise_index(ListBrowser lb, Int index)
{ Int size = (notNil(lb->dict) ? lb->dict->members->size : ZERO);

  if ( valInt(index) >= valInt(size) )
    index = sub(size, ONE);

  if ( valInt(index) < 0 )
    return ZERO;

  return index;
}


		/********************************
		*           SEARCHING		*
		********************************/

static StringObj
getExtendPrefixDict(Dict dict, CharArray pref, Bool ign_case)
{ LocalString(common, &pref->data, LINESIZE);
  Cell cell;
  int hit = FALSE;

  common->size = 0;

  for_cell(cell, dict->members)
  { DictItem di = cell->value;
    String name = &dictItemName(di)->data;

    if ( name->size > LINESIZE || name->encoding != common->encoding )
      continue;
      
    if ( ign_case == OFF )
    { if ( str_prefix(name, &pref->data) )
      { if ( !hit++ )
	  str_cpy(common, name);
	else
	  common->size = str_common_length(common, name);
      }
    } else
    { if ( str_icase_prefix(name, &pref->data) )
      { if ( !hit++ )
        { str_cpy(common, name);
	  str_downcase(common, 0, common->size);
	} else
	  common->size = str_icase_common_length(common, name);
      }
    }
  }
  
  answer(StringToString(common));
}


static status
extendPrefixListBrowser(ListBrowser lb)
{ if ( notNil(lb->dict) )
  { StringObj ext;

    ext = getExtendPrefixDict(lb->dict,
			      isNil(lb->search_string)
			        ? (CharArray) CtoName("")
			        : (CharArray) lb->search_string,
			      getResourceValueObject(lb,
						     NAME_searchIgnoreCase));

    assign(lb, search_string, ext);
    executeSearchListBrowser(lb);
  }
  
  succeed;
}


static status
extendToCurrentListBrowser(ListBrowser lb)
{ if ( notNil(lb->search_string) )
  { DictItem di;

    if ( notNil(lb->dict) && (di=getFindIndexDict(lb->dict, lb->search_hit)) )
    { assign(lb, search_string,
	     newObject(ClassString, name_procent_s, dictItemName(di), 0));
      return executeSearchListBrowser(lb);
    }
  }

  fail;
}


static status
cancelSearchListBrowser(ListBrowser lb)
{ DictItem di;

  assign(lb, search_string, NIL);
  assign(lb, search_origin, ZERO);
  if ( valInt(lb->search_hit) >= 0 )
  { if ( notNil(lb->dict) && (di=getFindIndexDict(lb->dict, lb->search_hit)) )
      ChangeItemListBrowser(lb, di);
    assign(lb, search_hit, toInt(-1));
  }

  succeed;
}


static status
executeSearchListBrowser(ListBrowser lb)
{ DictItem di;

  if ( isNil(lb->dict) ||
       !(di=getFindPrefixDict(lb->dict, lb->search_string,
			      lb->search_origin,
			      getResourceValueObject(lb,
						     NAME_searchIgnoreCase))))
    fail;

  if ( valInt(lb->search_hit) >= 0 )
  { DictItem old = getFindIndexDict(lb->dict, lb->search_hit);

    if ( old != FAIL )
      ChangeItemListBrowser(lb, old);
  }
  assign(lb, search_hit, di->index);
  normaliseListBrowser(lb, di);
  return ChangeItemListBrowser(lb, di);
}


static status
repeatSearchListBrowser(ListBrowser lb, Int chr, EventObj ev)
{ if ( notNil(lb->search_string) )
  { Int oldorg = lb->search_origin;

    assign(lb, search_origin, add(lb->search_hit, ONE));
    if ( !executeSearchListBrowser(lb) )
    { assign(lb, search_origin, oldorg);
      fail;
    }
    succeed;
  }

  fail;
}


static status
backwardDeleteCharListBrowser(ListBrowser lb)
{ StringObj ss = lb->search_string;

  if ( notNil(ss) )
  { int size = valInt(getSizeCharArray(ss));
    
    if ( size > 1 )
    { deleteString(ss, toInt(size-1), DEFAULT);
      return executeSearchListBrowser(lb);
    }

    cancelSearchListBrowser(lb);
  }

  fail;
}


static status
insertSelfListBrowser(ListBrowser lb, Int times, Int chr)
{ wchar c;

  if ( isDefault(times) )
    times = ONE;

  if ( isDefault(chr) )
  { EventObj ev = EVENT->value;

    if ( instanceOfObject(ev, ClassEvent) && isAEvent(ev, NAME_printable) )
      c = valInt(getIdEvent(ev));
    else
      return errorPce(lb, NAME_noCharacter);
  } else
    c = valInt(chr);
    
  { LocalString(s, str_nl(), valInt(times)); /* TBD */
    int i;

    for(i=0; i<valInt(times); )
      str_store(s, i++, c);

    if ( isNil(lb->search_string) )
    { assign(lb, search_string, StringToString(s));
      if ( getResourceValueObject(lb, NAME_clearSelectionOnSearch) == ON )
	clearSelectionListBrowser(lb);
    } else
    { if ( !instanceOfObject(lb->search_string, ClassString) )
	assign(lb, search_string,
	       newObject(ClassString, name_procent_s, lb->search_string, 0));
      str_insert_string(lb->search_string, DEFAULT, s);
    }

    if ( !executeSearchListBrowser(lb) )
    { StringObj ss = lb->search_string;
      int size = valInt(getSizeCharArray(ss));

      if ( size > 1 )
	deleteString(ss, toInt(size-1), DEFAULT);
      else
	cancelSearchListBrowser(lb);

      fail;
    }
  }
  
  succeed;
}


static status
enterListBrowser(ListBrowser lb)
{ DictItem di;

  TRY( notNil(lb->dict) && (di=getFindIndexDict(lb->dict, lb->search_hit)) );

  cancelSearchListBrowser(lb);
  setSelectionListBrowser(lb, di);
  forwardListBrowser(lb, di, NAME_left);
  openListBrowser(lb, di);

  succeed;
}


		/********************************
		*        EVENT HANDLING		*
		********************************/


static status
typedListBrowser(ListBrowser lb, EventId id)
{ return typedKeyBinding(lb->key_binding, id, lbReceiver(lb));
}


static DictItem
getDictItemListBrowser(ListBrowser lb, EventObj ev)
{ Int where = getIndexTextImage(lb->image, ev);

  if ( notNil(lb->dict) )
    answer(getFindIndexDict(lb->dict,
			    toInt(valInt(where)/BROWSER_LINE_WIDTH)));

  fail;
}


static status
eventListBrowser(ListBrowser lb, EventObj ev)
{ if ( isAEvent(ev, NAME_focus) )
  { if ( isAEvent(ev, NAME_activateKeyboardFocus) )
      return send(lb, NAME_status, NAME_active, 0);
    if ( isAEvent(ev, NAME_deactivateKeyboardFocus) )
    { cancelSearchListBrowser(lb);
      return send(lb, NAME_status, NAME_inactive, 0);
    }
  }

  if ( eventDevice(lb, ev) )
    succeed;

  if ( isAEvent(ev, NAME_keyboard) )
    return send(lb, NAME_typed, getIdEvent(ev), 0);

/*
  if ( isAEvent(ev, NAME_areaEnter) )
    return send(lb, NAME_keyboardFocus, 0);
*/

  if ( isAEvent(ev, NAME_button) )
  { DictItem di = getDictItemListBrowser(lb, ev);

    if ( di != FAIL && notNil(lb->popup) && isAEvent(ev, NAME_msRightDown) )
    { send(popupGesture(), NAME_context, di, 0);
      if ( postEvent(ev, (Graphical) lb, popupGesture()) == FAIL )
	send(popupGesture(), NAME_context, NIL, 0);
      else
	succeed;
    } else if ( isAEvent(ev, NAME_msLeftDown) ||
	        isAEvent(ev, NAME_msMiddleDown) )
    { if ( di == FAIL )
	return clearSelectionListBrowser(lb);

					  /* Single selection selection */
      if ( lb->multiple_selection == OFF )
      { Name multi = getMulticlickEvent(ev);

	if ( equalName(multi, NAME_single) )
	{ setSelectionListBrowser(lb, di);
	  return forwardListBrowser(lb, di, getButtonEvent(ev));
	} if ( equalName(multi, NAME_double) )
	  return openListBrowser(lb, di);
      } else				/* Multiple selection */
      { if ( hasModifierEvent(ev, MODIFIER_shift) )
	  toggleSelectionListBrowser(lb, di);
        else
	  setSelectionListBrowser(lb, di);

	return forwardListBrowser(lb, di, getButtonEvent(ev));
      }
    }
  }
  
  fail;
}

		/********************************
		*         EDIT FUNCTIONS	*
		********************************/

static status
forwardListBrowser(ListBrowser lb, DictItem di, Name button)
{ Code msg = NIL;

  if ( button == NAME_middle )
  { if ( notNil(lb->select_middle_message) )
      msg = lb->select_middle_message;
    else if ( notNil(lb->select_message) )
      msg = lb->select_message;
  } else
  { if ( notNil(lb->select_message) )
      msg = lb->select_message;
  }

  if ( notNil(msg) )
  { /*DisplayObj d = getDisplayGraphical((Graphical)lb);

    busyCursorDisplay(d, DEFAULT, DEFAULT); */
    forwardReceiverCode(msg, lbReceiver(lb), di, 0);
 /* busyCursorDisplay(d, NIL, DEFAULT); */
  }

  succeed;
}


static status
setSelectionListBrowser(ListBrowser lb, DictItem di)
{ cancelSearchListBrowser(lb);
  clearSelectionListBrowser(lb);
  selectListBrowser(lb, di);

  succeed;
}


static status
toggleSelectionListBrowser(ListBrowser lb, DictItem di)
{ cancelSearchListBrowser(lb);
  if ( selectedListBrowser(lb, di) )
    deselectListBrowser(lb, di);
  else
    selectListBrowser(lb, di);
    
  succeed;
}


static status
openListBrowser(ListBrowser lb, DictItem di)
{ if ( notNil(lb->open_message) )
  { DisplayObj d = getDisplayGraphical((Graphical)lb);

    busyCursorDisplay(d, DEFAULT, DEFAULT);
    forwardReceiverCode(lb->open_message, lbReceiver(lb), di, 0);
    busyCursorDisplay(d, NIL, DEFAULT);
  }

  succeed;
}

		/********************************
		*       SELECTION HANDLING	*
		********************************/

status
selectedListBrowser(ListBrowser lb, DictItem di)
{ if ( instanceOfObject(lb->selection, ClassChain) )
    return memberChain(lb->selection, di);

  if ( notNil(lb->selection) && (DictItem) lb->selection == di )
    succeed;

  fail;
}


static status
deselectListBrowser(ListBrowser lb, DictItem di)
{ if ( instanceOfObject(lb->selection, ClassChain) )
  { if ( deleteChain(lb->selection, di) )
      ChangeItemListBrowser(lb, di);
  } else if ( notNil(lb->selection) && (DictItem) lb->selection == di )
  { assign(lb, selection, NIL);
    ChangeItemListBrowser(lb, di);
  }
  
  succeed;
}


static status
selectListBrowser(ListBrowser lb, DictItem di)
{ if ( selectedListBrowser(lb, di) )
    succeed;

  if ( lb->multiple_selection == ON )
  { appendChain(lb->selection, di);
    ChangeItemListBrowser(lb, di);
  } else
  { if ( notNil(lb->selection) )
      deselectListBrowser(lb, lb->selection);
    assign(lb, selection, di);
    ChangeItemListBrowser(lb, di);
  }
  
  succeed;
}


static status
clearSelectionListBrowser(ListBrowser lb)
{ if ( instanceOfObject(lb->selection, ClassChain) )
  { Chain ch = (Chain) lb->selection;

    while( notNil(ch->head) )
      deselectListBrowser(lb, ch->head->value);
  } else if ( notNil(lb->selection) )
    deselectListBrowser(lb, lb->selection);
  
  succeed;
}


status
selectionListBrowser(ListBrowser lb, Any obj)
{ clearSelectionListBrowser(lb);

  if ( instanceOfObject(obj, ClassChain) )
  { Chain ch = obj;
    Cell cell;

    for_cell(cell, ch)
      sendv(lb, NAME_select, 1, (Any *)&cell->value);
  } else if ( notNil(obj) )
    selectListBrowser(lb, obj);
    
  succeed;
}


Any
getSelectionListBrowser(ListBrowser lb)
{ if ( notNil(lb->selection) )
    answer(lb->selection);

  fail;
}

		/********************************
		*          SCROLLING		*
		********************************/

status
scrollToListBrowser(ListBrowser lb, Int index)
{ if ( isDefault(index) )
    index = (notNil(lb->dict) ? lb->dict->members->size : ZERO);
  index = normalise_index(lb, index);

  assign(lb, start, index);
  return startTextImage(lb->image, mul(index, toInt(BROWSER_LINE_WIDTH)), ZERO);
}


status
normaliseListBrowser(ListBrowser lb, DictItem di)
{ int here = valInt(di->index);
  int start, last;

  computeListBrowser(lb);
  start = valInt(lb->image->start) / BROWSER_LINE_WIDTH;
  last  = (valInt(lb->image->end) - 1) / BROWSER_LINE_WIDTH;

  if ( here >= start && here <= last )
    succeed;
  if ( here == start-1 )
    return scrollDownListBrowser(lb, ONE);
  if ( here == last+1 )
    return scrollUpListBrowser(lb, ONE);
  
  return scrollToListBrowser(lb,
			toInt(here - valInt(getLinesTextImage(lb->image))/2));
}


static status
scrollUpListBrowser(ListBrowser lb, Int arg)
{ Int lines = (isDefault(arg) ? sub(getLinesTextImage(lb->image), ONE) : arg);

  return scrollToListBrowser(lb, add(lb->start, lines));
}


static status
scrollDownListBrowser(ListBrowser lb, Int arg)
{ Int lines = (isDefault(arg) ? sub(getLinesTextImage(lb->image), ONE) : arg);

  return scrollToListBrowser(lb, sub(lb->start, lines));
}


static status
scrollVerticalListBrowser(ListBrowser lb, Name dir, Name unit, Int amount)
{ if ( equalName(unit, NAME_file) )
  { if ( equalName(dir, NAME_goto) )
    { int size = (isNil(lb->dict) ? 0 : valInt(lb->dict->members->size));
      int h = (size * valInt(amount)) / 1000;

      scrollToListBrowser(lb, toInt(h));
    }
  } else if ( equalName(unit, NAME_page) )
  { int d = (valInt(getLinesTextImage(lb->image)) * valInt(amount)) / 1000;

    if ( d < 1 )
      d = 1;

    if ( equalName(dir, NAME_forwards) )
      scrollUpListBrowser(lb, toInt(d));
    else
      scrollDownListBrowser(lb, toInt(d));
  } else if ( unit == NAME_line )
  { if ( dir == NAME_forwards )
      scrollUpListBrowser(lb, amount);
    else
      scrollDownListBrowser(lb, amount);
  }

  succeed;
}


		/********************************
		*          ATTRIBUTES		*
		********************************/


static status
dictListBrowser(ListBrowser lb, Dict dict)
{ if ( lb->dict == dict )
    succeed;

  if ( notNil(dict) && notNil(dict->browser) )
    return errorPce(lb, NAME_alreadyShown, dict, dict->browser);

  if ( notNil(lb->dict) )
    assign(lb->dict, browser, NIL);
  assign(lb, dict, dict);
  if ( notNil(dict) )
    assign(dict, browser, lb);
  scrollToListBrowser(lb, ZERO);
  lb->start_cell = NIL;

  return ChangedListBrowser(lb);
}


status
fontListBrowser(ListBrowser lb, FontObj font)
{ if ( lb->font != font )
  { assign(lb, font, font);
    setGraphical(lb, DEFAULT, DEFAULT, lb->size->w, lb->size->h);
    return ChangedListBrowser(lb);
  }
  
  succeed;
}


static status
styleListBrowser(ListBrowser lb, Name name, Style style)
{ valueSheet(lb->styles, name, style);
  ChangedListBrowser(lb);

  succeed;    
}


static status
multipleSelectionListBrowser(ListBrowser lb, Bool val)
{ if ( lb->multiple_selection != val )
  { if ( val == ON )
    { if ( isNil(lb->selection) )
        assign(lb, selection, newObject(ClassChain, 0));
      else
      	assign(lb, selection, newObject(ClassChain, lb->selection, 0));
    } else
    { if ( emptyChain(lb->selection) )
      { assign(lb, selection, NIL);
      } else
      { Cell cell;
	int start = TRUE;

	for_cell(cell, (Chain)lb->selection)
	{ if ( start )
	    start = FALSE;
	  else
	    deselectListBrowser(lb, cell->value);
	}
	assign(lb, selection, ((Chain) lb->selection)->head->value);
      }
    }
    assign(lb, multiple_selection, val);
  }
  
  succeed;
}

		/********************************
		*      CHANGE NOTIFICATIONS	*
		********************************/


static status
DeleteItemListBrowser(ListBrowser lb, DictItem di)
{ Int where = mul(di->index, toInt(BROWSER_LINE_WIDTH));

  deselectListBrowser(lb, di);
  if ( di->index == lb->start && notNil(lb->start_cell) )
    lb->start_cell = lb->start_cell->next;
  if ( valInt(di->index) <= valInt(lb->start) && lb->start != ZERO )
    assign(lb, start, sub(lb->start, ONE));

  current_dict = NULL;			/* clears cache */
  return InsertTextImage(lb->image, where, toInt(-BROWSER_LINE_WIDTH));
}


static status
InsertItemListBrowser(ListBrowser lb, DictItem di)
{ Int where = mul(di->index, toInt(BROWSER_LINE_WIDTH));

  current_dict = NULL;			/* clears cache */
  return InsertTextImage(lb->image, where, toInt(BROWSER_LINE_WIDTH));
}


static status
ClearListBrowser(ListBrowser lb)
{ if ( !isFreeingObj(lb) )
  { int size = (isNil(lb->dict) ? 0 : valInt(lb->dict->members->size));

    lb->start_cell = NIL;
    assign(lb, start, ZERO);

    if ( instanceOfObject(lb->selection, ClassChain) )
      clearChain(lb->selection);
    else
      assign(lb, selection, NIL);

    current_dict = NULL;			/* clears cache */
    InsertTextImage(lb->image, ZERO, toInt(size * -BROWSER_LINE_WIDTH));
  }

  succeed;
}


static status
ChangeItemListBrowser(ListBrowser lb, DictItem di)
{ Int from = mul(di->index, toInt(BROWSER_LINE_WIDTH));
  Int to   = add(from, toInt(BROWSER_LINE_WIDTH));

  return ChangedRegionTextImage(lb->image, from, to);
}


static status
ChangedListBrowser(ListBrowser lb)
{ current_dict = NULL;			/* clears cache */
  ChangedRegionTextImage(lb->image, ZERO, toInt(PCE_MAX_INT));

  succeed;
}

		/********************************
		*          DELEGATION		*
		********************************/

static status
tabStopsListBrowser(ListBrowser lb, Vector v)
{ return tabStopsTextImage(lb->image, v);
  
  succeed;
}
					/* avoid capture by device */

static status
clearListBrowser(ListBrowser lb)
{ if ( notNil(lb->dict) )
    send(lb->dict, NAME_clear, 0);

  succeed;
}


DictItem
getMemberListBrowser(ListBrowser lb, Any key)
{ if ( notNil(lb->dict) )
    answer(getMemberDict(lb->dict, key));

  fail;
}


		/********************************
		*             VISUAL		*
		********************************/

Chain
getContainsListBrowser(ListBrowser lb)
{ if ( notNil(lb->dict) )
    answer(answerObject(ClassChain, lb->dict, 0));

  fail;
}


static Any
getMasterListBrowser(ListBrowser lb)
{ if ( instanceOfObject(lb->device, ClassBrowser) )
    answer(lb->device);

  answer(lb);
}


status
makeClassListBrowser(Class class)
{ sourceClass(class, makeClassListBrowser, __FILE__, "$Revision$");

  setLoadStoreFunctionClass(class, loadListBrowser, storeListBrowser);

  localClass(class, NAME_dict, NAME_delegate, "dict*", NAME_get,
	     "Associated dict object (table of items)");
  localClass(class, NAME_image, NAME_components, "text_image", NAME_get,
	     "TextImage used to display textlines");
  localClass(class, NAME_scrollBar, NAME_components, "scroll_bar", NAME_get,
	     "Scrollbar used to scroll window");
  localClass(class, NAME_labelText, NAME_components, "text*", NAME_get,
	     "Text object that displays the label");
  localClass(class, NAME_status, NAME_event, "{active,inactive}", NAME_get,
	     "Handle typing?");
  localClass(class, NAME_keyBinding, NAME_accelerator, "key_binding",
	     NAME_both, "Key binding table");
  localClass(class, NAME_selection, NAME_selection,
	     "chain|member:dict_item*", NAME_none,
	     "Selected items");
  localClass(class, NAME_selectionStyle, NAME_appearance, "[style]",
	     NAME_get, "Style for selection feedback");
  localClass(class, NAME_multipleSelection, NAME_selection, "bool", NAME_get,
	     "If @on, multiple items may be selected");
  localClass(class, NAME_selectMessage, NAME_action, "code*", NAME_both,
	     "Send on left-click on item");
  localClass(class, NAME_selectMiddleMessage, NAME_action, "code*",
	     NAME_both,
	     "Send on middle-click on item");
  localClass(class, NAME_openMessage, NAME_action, "code*", NAME_both,
	     "Send on keyboard selection or double click");
  localClass(class, NAME_popup, NAME_menu, "popup*", NAME_both,
	     "Associated popup menu");
  localClass(class, NAME_font, NAME_appearance, "font", NAME_get,
	     "Font for displayed items");
  localClass(class, NAME_styles, NAME_appearance, "sheet", NAME_get,
	     "Name --> style mapping");
  localClass(class, NAME_size, NAME_area, "characters=size", NAME_get,
	     "Size in characters/lines");
  localClass(class, NAME_start, NAME_scroll, "int", NAME_get,
	     "Object on top-row of display");
  localClass(class, NAME_searchOrigin, NAME_search, "int", NAME_none,
	     "Start of incremental search");
  localClass(class, NAME_searchHit, NAME_search, "int", NAME_none,
	     "Current hit");
  localClass(class, NAME_searchString, NAME_search, "char_array*", NAME_none,
	     "Current search string");
  localClass(class, NAME_startCell, NAME_cache, "alien:Cell", NAME_none,
	     "Cell reference to top-row of display");

  termClass(class, "list_browser", 3, NAME_dict, NAME_width, NAME_height);
  delegateClass(class, NAME_dict);

  storeMethod(class, NAME_font, fontListBrowser);
  storeMethod(class, NAME_dict, dictListBrowser);
  storeMethod(class, NAME_multipleSelection, multipleSelectionListBrowser);
  storeMethod(class, NAME_selection, selectionListBrowser);
  storeMethod(class, NAME_status, statusListBrowser);

  sendMethod(class, NAME_initialise, DEFAULT,
	     3, "dict=[dict]", "width=[int]", "height=[int]",
	     "Create from dict, width and height",
	     initialiseListBrowser);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Unlink from dict and device",
	     unlinkListBrowser);
  sendMethod(class, NAME_requestGeometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Map size to character units",
	     requestGeometryListBrowser);
  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Resize the text_image",
	     geometryListBrowser);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Recompute the image",
	     computeListBrowser);
  sendMethod(class, NAME_next, NAME_caret, 0,
	     "Move caret to next item (`device ->advance')",
	     nextListBrowser);
  sendMethod(class, NAME_extendPrefixOrNext, NAME_caret, 0,
	     "->extend_prefix or ->next",
	     extendPrefixOrNextListBrowser);
  sendMethod(class, NAME_WantsKeyboardFocus, NAME_event, 0,
	     "Test if ready to accept input (non-empty)",
	     WantsKeyboardFocusListBrowser);
  sendMethod(class, NAME_typed, NAME_accelerator, 1, "event_id",
	     "Handle typed character",
	     typedListBrowser);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Handle arbitrary event",
	     eventListBrowser);
  sendMethod(class, NAME_Size, NAME_area, 1, "pixels=size",
	     "Set size in pixels (trap window resize)",
	     SizeListBrowser);
  sendMethod(class, NAME_normalise, NAME_scroll, 1, "member:dict_item",
	     "Make specified item visible",
	     normaliseListBrowser);
  sendMethod(class, NAME_scrollTo, NAME_scroll, 1, "[int]",
	     "Make nth-1 item start of window",
	     scrollToListBrowser);
  sendMethod(class, NAME_scrollUp, NAME_scroll, 1,"[int]",
	     "Scroll lines up (default one window)",
	     scrollUpListBrowser);
  sendMethod(class, NAME_scrollDown, NAME_scroll, 1,"[int]",
	     "Scroll lines down (default one window)",
	     scrollDownListBrowser);
  sendMethod(class, NAME_tabStops, NAME_appearance, 1, "vector*",
	     "Set tab-stops (pixels)",
	     tabStopsListBrowser);
  sendMethod(class, NAME_style, NAME_appearance, 2,
	     "dict_item=name", "style=style",
	     "Set style associated with name",
	     styleListBrowser);
  sendMethod(class, NAME_clear, NAME_edit, 0,
	     "Remove all items from the associated dict",
	     clearListBrowser);
  sendMethod(class, NAME_label, NAME_label, 1, "name",
	     "Set the name of the label",
	     labelListBrowser);
  sendMethod(class, NAME_showLabel, NAME_appearance, 1, "show=bool",
	     "Show/unshow the label",
	     showLabelListBrowser);

  sendMethod(class, NAME_scrollVertical, NAME_scroll,
	     3, "{forwards,backwards,goto}", "{file,page,line}", "int",
	     "Handle scroll_bar request",
	     scrollVerticalListBrowser);

  sendMethod(class, NAME_DeleteItem, NAME_repaint, 1, "dict_item",
	     "Handle deleted item from dict",
	     DeleteItemListBrowser);
  sendMethod(class, NAME_InsertItem, NAME_repaint, 1, "dict_item",
	     "Handle inserted item from dict",
	     InsertItemListBrowser);
  sendMethod(class, NAME_ChangeItem, NAME_repaint, 1, "dict_item",
	     "Handle changed item from dict",
	     ChangeItemListBrowser);
  sendMethod(class, NAME_Clear, NAME_repaint, 0,
	     "Handle clear from dict",
	     ClearListBrowser);

  sendMethod(class, NAME_selected, NAME_selection, 1, "member:dict_item",
	     "Test if item is selected",
	     selectedListBrowser);
  sendMethod(class, NAME_select, NAME_selection, 1, "member:dict_item",
	     "Select (add to selection) item",
	     selectListBrowser);
  sendMethod(class, NAME_deselect, NAME_selection, 1, "member:dict_item",
	     "Unselect (remove from selection) item",
	     deselectListBrowser);

  sendMethod(class, NAME_insertSelf, NAME_search, 2,
	     "times=[int]", "character=[char]",
	     "Start/Continue incremental search",
	     insertSelfListBrowser);
  sendMethod(class, NAME_enter, NAME_search, 0,
	     "Select current item as double-click",
	     enterListBrowser);
  sendMethod(class, NAME_cancelSearch, NAME_search, 0,
	     "Cancel the current search operation",
	     cancelSearchListBrowser);
  sendMethod(class, NAME_keyboardQuit, NAME_search, 0,
	     "Equivalent to ->cancel_search",
	     cancelSearchListBrowser);
  sendMethod(class, NAME_backwardDeleteChar, NAME_search, 0,
	     "Undo last search extension",
	     backwardDeleteCharListBrowser);
  sendMethod(class, NAME_extendPrefix, NAME_search, 0,
	     "Extend search with common part",
	     extendPrefixListBrowser);
  sendMethod(class, NAME_extendToCurrent, NAME_search, 0,
	     "Extend search to current item",
	     extendToCurrentListBrowser);
  sendMethod(class, NAME_repeatSearch, NAME_search, 0,
	     "Repeat with same string",
	     repeatSearchListBrowser);

  getMethod(class, NAME_selection, DEFAULT, "chain|dict_item", 0,
	    "Current value of selection",
	    getSelectionListBrowser);
  getMethod(class, NAME_width, NAME_area, "characters=int", 0,
	    "Width in character units",
	    getWidthListBrowser);
  getMethod(class, NAME_height, NAME_area, "characters=int", 0,
	    "Height in character units",
	    getHeightListBrowser);
  getMethod(class, NAME_SeekFunction, NAME_internal, "int", 0,
	    "Pointer to C-function to seek to position",
	    getSeekFunctionListBrowser);
  getMethod(class, NAME_ScanFunction, NAME_internal, "int", 0,
	    "Pointer to C-function to scan for char-type",
	    getScanFunctionListBrowser);
  getMethod(class, NAME_FetchFunction, NAME_internal, "int", 0,
	    "Pointer to C-function to fetch char",
	    getFetchFunctionListBrowser);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "Dict visualised",
	    getContainsListBrowser);
  getMethod(class, NAME_member, NAME_member, "dict_item", 1, "name",
	    "DictItem with given key",
	    getMemberListBrowser);
  getMethod(class, NAME_dictItem, NAME_event, "dict_item", 1, "event",
	    "DictItem on which event occurred",
	    getDictItemListBrowser);
  getMethod(class, NAME_master, DEFAULT, "device", 0,
	    "Principal visual I'm part of (self or browser)",
	    getMasterListBrowser);
  getMethod(class, NAME_label, NAME_label, "name", 0,
	    "Current value of the label",
	    getLabelListBrowser);
  getMethod(class, NAME_showLabel, NAME_appearance, "bool", 0,
	    "Bool indicating if label is visible",
	    getShowLabelListBrowser);
  getMethod(class, NAME_length, NAME_scroll, "int", 0,
	    "Length of contents (for scroll_bar)",
	    getLengthListBrowser);
  getMethod(class, NAME_view, NAME_scroll, "int", 0,
	    "Length of view (for scroll_bar)",
	    getViewListBrowser);
	    

  attach_resource(class, "size",  	 "size",      "size(15,10)",
		  "Default size in `characters x lines'");
  attach_resource(class, "font",   	 "font",      "@helvetica_roman_14",
		  "Default font");
  attach_resource(class, "cursor", 	 "cursor",    "right_ptr",
		  "Default cursor");
  attach_resource(class, "clear_selection_on_search", "bool", "@on",
		  "@on: clear selection when searching");
  attach_resource(class, "search_ignore_case",        "bool", "@on",
		  "@on: ignore case when searching");
  attach_resource(class, "label_font", "font", "@helvetica_bold_14",
		  "Font used to display the label");
  attach_resource(class, "selection_style", "[style]",
		  "@default", "Style object for <-selection");
  attach_resource(class, "isearch_style", "[style]",
		  "@default", "Style for incremental search");

  succeed;
}

