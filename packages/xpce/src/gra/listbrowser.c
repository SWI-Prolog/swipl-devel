/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static Int    normalise_index(ListBrowser, Int);
static status clearSelectionListBrowser(ListBrowser);
static status ChangeItemListBrowser(ListBrowser, DictItem);
static status ChangedListBrowser(ListBrowser);
static status geometryListBrowser(ListBrowser, Int, Int, Int, Int);
static status deselectListBrowser(ListBrowser, DictItem);
static status showLabelListBrowser(ListBrowser, Bool);
static status selectListBrowser(ListBrowser, DictItem);
static status scrollUpListBrowser(ListBrowser, Int);
static status scrollDownListBrowser(ListBrowser, Int);
static status extendPrefixListBrowser(ListBrowser);

#define swap(x, y)	{ int z; z=x; x=y; y=z; }

		/********************************
		*            CREATE		*
		********************************/

static status
initialiseListBrowser(ListBrowser lb, Dict dict, Int w, Int h)
{ int fw, fh, iw, ih;

  if ( isDefault(dict) )
    dict = newObject(ClassDict, EAV);

  if ( notNil(dict->browser) )
    return errorPce(lb, NAME_alreadyShown, dict, dict->browser);

  assign(lb, size, newObject(ClassSize, EAV));
  copySize(lb->size, getClassVariableValueObject(lb, NAME_size));
  if ( notDefault(w) ) assign(lb->size, w, w);
  if ( notDefault(h) ) assign(lb->size, h, h);

  initialiseDevice((Device) lb);

  assign(lb,   pen,		      getClassVariableValueObject(lb, NAME_pen));
  assign(lb,   dict,                  dict);
  assign(dict, browser,               lb);
  assign(lb,   status, 		      NAME_inactive);
  assign(lb,   key_binding,	      newObject(ClassKeyBinding, NIL,
						NAME_listBrowser, EAV));
  assign(lb,   select_message,        NIL);
  assign(lb,   open_message,          NIL);
  assign(lb,   cancel_message,	      NIL);
  assign(lb,   multiple_selection,    OFF);
  assign(lb,   selection,             NIL);
  assign(lb,   start,	              ZERO);
  assign(lb,   search_string,         NIL);
  assign(lb,   search_origin,         ZERO);
  assign(lb,   search_hit,	      toInt(-1));
  assign(lb,   label_text,	      NIL);
  assign(lb,   styles,		      newObject(ClassSheet, EAV));
  assign(lb,   selection_style,       getClassVariableValueObject(lb,
						      NAME_selectionStyle));

  lb->start_cell = NIL;

  assign(lb, font, getClassVariableValueObject(lb, NAME_font));
  fw = valInt(getExFont(lb->font));
  fh = valInt(getHeightFont(lb->font));
  iw = valInt(lb->size->w) * fw + 2 * TXT_X_MARGIN;
  ih = valInt(lb->size->h) * fh + 2 * TXT_Y_MARGIN;

  assign(lb, image, newObject(ClassTextImage, lb, toInt(iw), toInt(ih), EAV));
  assign(lb->image, wrap, NAME_none);
  assign(lb, scroll_bar, newObject(ClassScrollBar, lb, NAME_vertical, EAV));

  send(lb->image, NAME_cursor, getClassVariableValueObject(lb, NAME_cursor), EAV);
  send(lb->image, NAME_set,
       lb->scroll_bar->area->w, ZERO, DEFAULT, toInt(ih), EAV);
  displayDevice(lb, lb->scroll_bar, DEFAULT);
  displayDevice(lb, lb->image, DEFAULT);
  if ( notNil(lb->scroll_bar) )
    iw += valInt(getMarginScrollBar(lb->scroll_bar));

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
		 *		REDRAW		*
		 *******************************/

static status
RedrawAreaListBrowser(ListBrowser lb, Area a)
{ Any obg = r_background(getClassVariableValueObject(lb, NAME_background));

  RedrawAreaDevice((Device)lb, a);
  if ( lb->pen != ZERO )
  { int x, y, w, h;
    int th = valInt(lb->image->area->y);

    initialiseDeviceGraphical(lb, &x, &y, &w, &h);
    y += th;
    h -= th;

    if ( h > 0 )
    { r_thickness(valInt(lb->pen));
      r_dash(lb->texture);

      r_box(x, y, w, h, 0, NIL);
    }
  }

  r_background(obg);

  succeed;
}

		 /*******************************
		 *	     LOAD/SAVE		*
		 *******************************/

static status
storeListBrowser(ListBrowser lb, FileObj file)
{ return storeSlotsObject(lb, file);
}


static status
loadListBrowser(ListBrowser lb, IOSTREAM *fd, ClassDef def)
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

  send(lb->label_text, NAME_string, lbl, EAV);
  geometryListBrowser(lb, DEFAULT, DEFAULT, DEFAULT, DEFAULT);

  succeed;
}


static Name
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
	     newObject(ClassText, GetLabelNameName(lb->name), NAME_left,
		       getClassVariableValueObject(lb, NAME_labelFont), EAV));
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
  { Elevation z;

    assign(lb, status, stat);

				/* avoid unnecessary flickering (hack) */
    if ( !((z = getClassVariableValueObject(lb->image, NAME_elevation)) &&
	   notNil(z)) )
    { penGraphical((Graphical) lb->image,
		   stat == NAME_active ? add(lb->pen, ONE) : lb->pen);
    }
  }

  succeed;
}


static status
nextListBrowser(ListBrowser lb)
{ return send(lb->device, NAME_advance, lb, EAV);
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

  if ( isDefault(w) || isDefault(h) )
    computeBoundingBoxDevice((Device)lb);

  if ( isDefault(x) ) x = lb->area->x;
  if ( isDefault(y) ) y = lb->area->y;
  if ( isDefault(w) ) w = lb->area->w;
  if ( isDefault(h) ) h = lb->area->h;

  if ( valInt(w) < 50 ) w = toInt(50);
  if ( valInt(h) < 20 ) h = toInt(20);

  sw = isNil(lb->scroll_bar) ? 0 : valInt(getMarginScrollBar(lb->scroll_bar));
  iw = valInt(w) - abs(sw);

  { int fw = valInt(getExFont(lb->font));
    int fh = valInt(getHeightFont(lb->font));
    
    assign(lb->size, w, toInt((iw - 2 * TXT_X_MARGIN)/fw));
    assign(lb->size, h, toInt((valInt(h) - 2 * TXT_Y_MARGIN)/fh));
  }
  
  ix = (sw < 0 ? -sw : 0);
  if ( getShowLabelListBrowser(lb) == ON )
  { send(lb->label_text, NAME_set, ZERO, ZERO, w, EAV);
    iy = valInt(lb->label_text->area->h) - pen;
  } else
  { iy = 0;
  }
  ih = valInt(h) - iy;

  send(lb->image, NAME_set, toInt(ix), toInt(iy), toInt(iw), toInt(ih), EAV);
  if ( notNil(lb->scroll_bar) )
    placeScrollBar(lb->scroll_bar, (Graphical) lb->image);

  return geometryDevice((Device) lb, x, y, DEFAULT, DEFAULT);
}


static status
SizeListBrowser(ListBrowser lb, Size size)
{ return doSetGraphical(lb, DEFAULT, DEFAULT, size->w, size->h);
}


status
requestGeometryListBrowser(ListBrowser lb, Int x, Int y, Int w, Int h)
{ PceWindow v;

  if ( notDefault(w) )
  { w = mul(w, getExFont(lb->font));
    if ( notNil(lb->scroll_bar) )
      w = add(w, getMarginScrollBar(lb->scroll_bar));
    w = add(w, toInt(2 * TXT_X_MARGIN));
  }

  if ( notDefault(h) )
  { h = mul(h, getHeightFont(lb->font));
    h = add(h, toInt(2 * TXT_Y_MARGIN));
  }

  if ( instanceOfObject(v = lbReceiver(lb), ClassWindow) )
  { int b = (valInt(v->tile->border) + valInt(v->pen)) * 2;

    if ( notDefault(w) )
      w = add(w, toInt(b));
    if ( notDefault(h) )
      h = add(h, toInt(b));

    requestGeometryWindow(v, x, y, w, h);
  } else
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
static Image	     current_image;	/* Image to flag line */

static void
compute_current(ListBrowser lb)
{ if ( notNil(current_cell) )
  { DictItem di = (DictItem) current_cell->value;
    CharArray label = getLabelDictItem(di);
    Style style;

    assert(valInt(di->index) == current_item);
    current_name = (label ? &label->data : (String) NULL);

    if ( notDefault(di->style) &&
	 (style = getValueSheet(lb->styles, di->style)) )
    { current_font	 = style->font;
      current_colour     = style->colour;
      current_background = style->background;
      current_atts       = style->attributes;
      current_image      = style->icon;

      if ( isDefault(current_font) )
	current_font = lb->font;
    } else
    { current_font       = lb->font;
      current_colour     = DEFAULT;
      current_background = DEFAULT;
      current_atts       = 0;
      current_image      = NIL;
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
    current_image      = NIL;
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
seek_list_browser(Any obj, long int index)
{ ListBrowser lb = obj;
  int item = index / BROWSER_LINE_WIDTH;
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
scan_list_browser(Any obj, long int from, int dir,
		  int how, int category, int *eof)
{ ListBrowser lb = obj;
  int item = from / BROWSER_LINE_WIDTH;

  assert(dir > 0 && how == TEXT_SCAN_FOR && category == EL);
  
  *eof = (isNil(lb->dict) ||
	  ((item + 1 >= valInt(lb->dict->members->size)) ? TRUE : FALSE));

  return (item + 1) * BROWSER_LINE_WIDTH - 1;
}


static long
fetch_list_browser(Any obj, TextChar tc)
{ ListBrowser lb = obj;
  int index = current_index;
  int pos   = current_index++ % BROWSER_LINE_WIDTH;

  if ( current_name )
  { int len = current_name->size;

    if ( pos <= len )
    { if ( pos == 0 )
      { if ( notNil(current_image) )
	{ tc->value.image  = current_image;
	  tc->type = CHAR_IMAGE;
	} else
	{ tc->value.image = NULL_IMAGE;
	  tc->type = CHAR_IMAGE;
	}
      } else
      { tc->value.c = str_fetch(current_name, pos-1);
	tc->type = CHAR_ASCII;
      }
    } else /* if ( pos == len+1 ) */
    { tc->value.c = '\n';
      tc->type = CHAR_ASCII;
      current_index = ((index / BROWSER_LINE_WIDTH) + 1) * BROWSER_LINE_WIDTH;
    }
  } else
  { tc->value.c = EOB;
    tc->type = CHAR_ASCII;
  }

  tc->font         = current_font;
  tc->attributes   = current_atts;
  tc->colour	   = current_colour;
  tc->background   = current_background;
  tc->index        = index;

  if ( pos > 0 && pos <= current_search )
  { Style s = getClassVariableValueObject(lb, NAME_isearchStyle);

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

static void
rewind_list_browser(Any obj)
{ ListBrowser lb = (ListBrowser) obj;
  DictItem di;

  assign(lb, start, normalise_index(lb, lb->start));

  if ( isNil(lb->start_cell) ||
       !isProperObject((di = lb->start_cell->value)) ||
       di->index != lb->start )
    lb->start_cell = find_cell_dict(lb->dict, lb->start);

  current_cell = lb->start_cell;
  current_item = valInt(lb->start);
  current_dict = lb->dict;
  compute_current(lb);
}


static SeekFunction
getSeekFunctionListBrowser(ListBrowser lb)
{ DEBUG(NAME_SeekFunction,
	Cprintf("seek_list_browser = 0x%p\n", seek_list_browser));
  answer(seek_list_browser);
}


static ScanFunction
getScanFunctionListBrowser(ListBrowser lb)
{ answer(scan_list_browser);
}


static FetchFunction
getFetchFunctionListBrowser(ListBrowser lb)
{ answer(fetch_list_browser);
}


static MarginFunction
getMarginFunctionListBrowser(ListBrowser lb)
{ answer((MarginFunction) NULL);
}


static RewindFunction
getRewindFunctionListBrowser(ListBrowser lb)
{ answer(rewind_list_browser);
}



		/********************************
		*            REDRAW		*
		********************************/

static status
computeListBrowser(ListBrowser lb)
{ if ( notNil(lb->request_compute) )
  { computeTextImage(lb->image);
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
    CharArray c = getLabelDictItem(di);
    String name;

    if ( !c )
      continue;

    name = &c->data;
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
			      getClassVariableValueObject(lb,
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
	     newObject(ClassString, name_procent_s, getLabelDictItem(di), EAV));
      return executeSearchListBrowser(lb);
    }
  }

  fail;
}


static status
cancelSearchListBrowser(ListBrowser lb)
{ DictItem di;

  assign(lb, caret, NIL);
  assign(lb, search_string, NIL);
  assign(lb, search_origin, ZERO);
  if ( valInt(lb->search_hit) >= 0 )
  { if ( notNil(lb->dict) && (di=getFindIndexDict(lb->dict, lb->search_hit)) )
      ChangeItemListBrowser(lb, di);
    assign(lb, search_hit, toInt(-1));
  }

  succeed;
}


status
executeSearchListBrowser(ListBrowser lb)
{ DictItem di;

  if ( isNil(lb->dict) ||
       !(di=getFindPrefixDict(lb->dict, lb->search_string,
			      lb->search_origin,
			      getClassVariableValueObject(lb,
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
    
  { LocalString(s, str_nl(NULL), valInt(times)); /* TBD */
    int i;

    for(i=0; i<valInt(times); )
      str_store(s, i++, c);

    if ( isNil(lb->search_string) )
    { assign(lb, search_string, StringToString(s));
      if ( getClassVariableValueObject(lb, NAME_clearSelectionOnSearch) == ON )
	clearSelectionListBrowser(lb);
    } else
    { if ( !instanceOfObject(lb->search_string, ClassString) )
	assign(lb, search_string,
	       newObject(ClassString, name_procent_s, lb->search_string, EAV));
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

  if ( isNil(lb->dict) )
    fail;

  if ( (di=getFindIndexDict(lb->dict, lb->search_hit)) )
  { send(lb, NAME_changeSelection, NAME_set, di, EAV);
    return forwardListBrowser(lb, NAME_open);
  }

  return forwardListBrowser(lb, NAME_open);
}

		/********************************
		*        EVENT HANDLING		*
		********************************/


status
typedListBrowser(ListBrowser lb, EventId id)
{ return typedKeyBinding(lb->key_binding, id, lbReceiver(lb));
}


DictItem
getDictItemListBrowser(ListBrowser lb, EventObj ev)
{ if ( insideEvent(ev, (Graphical)lb->image) )
  { Int where = getIndexTextImage(lb->image, ev);

    if ( where && notNil(lb->dict) )
      answer(getFindIndexDict(lb->dict,
			      toInt(valInt(where)/BROWSER_LINE_WIDTH)));
  }

  fail;
}


Any
selectBrowserGesture()
{ static Any g = NULL;

  if ( !g )
    g = globalObject(NAME_browserSelectGesture, ClassBrowserSelectGesture, EAV);
  
  return g;
}



static status
eventListBrowser(ListBrowser lb, EventObj ev)
{ if ( isAEvent(ev, NAME_focus) )
  { if ( isAEvent(ev, NAME_activateKeyboardFocus) )
      return send(lb, NAME_status, NAME_active, EAV);
    if ( isAEvent(ev, NAME_deactivateKeyboardFocus) )
    { cancelSearchListBrowser(lb);
      return send(lb, NAME_status, NAME_inactive, EAV);
    }
  }

  if ( eventDevice(lb, ev) )
    succeed;

  if ( isAEvent(ev, NAME_keyboard) )
    return send(lb, NAME_typed, getIdEvent(ev), EAV);

  if ( mapWheelMouseEvent(ev, lb) )
    succeed;

  if ( isAEvent(ev, NAME_button) )
  { DictItem di = getDictItemListBrowser(lb, ev);

    if ( di && notNil(lb->popup) && isAEvent(ev, NAME_msRightDown) )
    { send(popupGesture(), NAME_context, di, EAV);

      if ( !postEvent(ev, (Graphical) lb, popupGesture()) )
	send(popupGesture(), NAME_context, NIL, EAV);
      else
	succeed;
    } else
      return postEvent(ev, (Graphical)lb, selectBrowserGesture());
  }
  
  fail;
}

		/********************************
		*         EDIT FUNCTIONS	*
		********************************/

static status
changeSelectionListBrowser(ListBrowser lb, Name action, DictItem di)
{ cancelSearchListBrowser(lb);

  if ( action == NAME_cancel )
  { assign(lb, selection_origin, NIL);

    clearSelectionListBrowser(lb);
    if ( instanceOfObject(di, ClassChain) )
    { Cell cell;

      for_cell(cell, (Chain)di)
      { selectListBrowser(lb, cell->value);
      }
    } else if ( instanceOfObject(di, ClassDictItem) )
      selectListBrowser(lb, di);

    if ( instanceOfObject(lb->cancel_message, ClassCode) )
      forwardReceiverCode(lb->cancel_message,
			  lbReceiver(lb),
			  EAV);

    succeed;
  }

  if ( action != NAME_clear && isDefault(di) )
    return errorPce(di, NAME_unexpectedType, nameToType(NAME_dictItem));

  if ( action == NAME_set )
  { clearSelectionListBrowser(lb);
    selectListBrowser(lb, di);
    assign(lb, selection_origin, di->index);
  } else if ( action == NAME_toggle )
  { if ( selectedListBrowser(lb, di) )
      deselectListBrowser(lb, di);
    else
    { selectListBrowser(lb, di);
      assign(lb, selection_origin, di->index);
    }
  } else if ( action == NAME_extend )
  { if ( isNil(lb->selection) || isNil(lb->selection_origin) )
    { selectListBrowser(lb, di);
      assign(lb, selection_origin, di->index);
    } else
    { Chain ch = lb->selection;
      Cell cell, c2;
      int low, high;

      low = valInt(di->index);
      high = valInt(lb->selection_origin);
      if ( low > high )
	swap(low, high);

      for_cell_save(cell, c2, ch)
      { DictItem di2 = cell->value;

	if ( valInt(di2->index) < low || valInt(di2->index) > high )
	  deselectListBrowser(lb, di2);
      }

      if ( (cell = find_cell_dict(lb->dict, toInt(low))) )
      { for( ; notNil(cell); cell = cell->next )
	{ DictItem di2 = cell->value;

	  selectListBrowser(lb, di2);
	  if ( valInt(di2->index) == high )
	    break;
	}
      } else
      { clearSelectionListBrowser(lb);
	selectListBrowser(lb, di);
	assign(lb, selection_origin, di->index);
      }
    }
  } else /* clear */
  { clearSelectionListBrowser(lb);
    assign(lb, selection_origin, NIL);
  }

  succeed;
}


status
forwardListBrowser(ListBrowser lb, Name action)
{ if ( notNil(lb->selection) )
  { if ( notNil(lb->select_message) )
      forwardReceiverCode(lb->select_message, lbReceiver(lb),
			  lb->selection, EAV);

    if ( action == NAME_open )
    { if ( notNil(lb->open_message) )
      { DisplayObj d = getDisplayGraphical((Graphical)lb);

	busyCursorDisplay(d, DEFAULT, DEFAULT);
	forwardReceiverCode(lb->open_message, lbReceiver(lb),
			    lb->selection, EAV);
	busyCursorDisplay(d, NIL, DEFAULT);
      }
    }
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

  if ( isDefault(arg) )
    cancelSearchListBrowser(lb);
  return scrollToListBrowser(lb, add(lb->start, lines));
}


static status
scrollDownListBrowser(ListBrowser lb, Int arg)
{ Int lines = (isDefault(arg) ? sub(getLinesTextImage(lb->image), ONE) : arg);

  if ( isDefault(arg) )
    cancelSearchListBrowser(lb);
  return scrollToListBrowser(lb, sub(lb->start, lines));
}


static status
recenterListBrowser(ListBrowser lb, Int arg)
{ cancelSearchListBrowser(lb);

  succeed;
}


static status
scrollVerticalListBrowser(ListBrowser lb, Name dir, Name unit, Int amount)
{ if ( unit == NAME_file )
  { if ( dir == NAME_goto )
    { int size = (isNil(lb->dict) ? 0 : valInt(lb->dict->members->size));
      int view = valInt(getLinesTextImage(lb->image));
      int h = ((size-view) * valInt(amount)) / 1000;

      if ( h < 0 )
	h = 0;

      scrollToListBrowser(lb, toInt(h));
    }
  } else if ( unit == NAME_page )
  { int d = (valInt(getLinesTextImage(lb->image)) * valInt(amount)) / 1000;

    if ( d < 1 )
      d = 1;

    if ( dir == NAME_forwards )
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


static status
showScrollBarListBrowser(ListBrowser lb, Bool show, ScrollBar sb)
{ if ( isDefault(sb) || sb == lb->scroll_bar )
  { computeBoundingBoxDevice((Device) lb);
    DisplayedGraphical(lb->scroll_bar, show);
    geometryListBrowser(lb, DEFAULT, DEFAULT, lb->area->w, lb->area->h);
  }

  succeed;
}


		 /*******************************
		 *	  LINE UP/DOWN		*
		 *******************************/

static int
onPage(DictItem di, int start, int end)
{ if ( valInt(di->index) >= start &&
       valInt(di->index) <= end )
    succeed;

  fail;
}


static status
nextLineListBrowser(ListBrowser lb, Int lines)
{ if ( notNil(lb->dict) )
  { int times = isDefault(lines) ? 1 : valInt(lines);
    DictItem di = NULL;

    if ( times == 0 )
      succeed;

    if ( valInt(lb->search_hit) >= 0 )			/* Searching */
    { Int newi = normalise_index(lb, toInt(valInt(lb->search_hit) + times));

      di = getNth0Chain(lb->dict->members, newi);
      if ( di )
      { CharArray lbl = getLabelDictItem(di);
	DictItem di2 = getNth0Chain(lb->dict->members, lb->search_hit);
	Bool ign_case = getClassVariableValueObject(lb,
						    NAME_searchIgnoreCase);

	ChangeItemListBrowser(lb, di2);

	if ( !prefixCharArray(lbl, (CharArray)lb->search_string, ign_case) ||
	     getSizeCharArray(lb->search_string) == ZERO )
	{ assign(lb, search_string,
		 newObject(ClassString, name_procent_s, lbl, EAV));
	  assign(lb, search_origin, newi);
	}
	assign(lb, search_hit, newi);
      }
    } else
    { int start = valInt(lb->image->start) / BROWSER_LINE_WIDTH;
      int last  = (valInt(lb->image->end) - 1) / BROWSER_LINE_WIDTH;
      int oldcaret = -1;
      int caret = -1;

      if ( notNil(lb->caret) )
      { caret = valInt(lb->caret);
      } else if ( instanceOfObject(lb->selection, ClassDictItem) )
      { if ( onPage(lb->selection, start, last) )
	{ DictItem di2 = lb->selection;

	  caret = valInt(di2->index);
	}
      } else if ( instanceOfObject(lb->selection, ClassChain) )
      { Cell cell;

	for_cell(cell, (Chain)lb->selection)
	{ DictItem di2 = cell->value;

	  if ( onPage(di2, start, last) )
	  { caret = valInt(di2->index);
	    break;
	  }
	}
      }
      if ( caret >= 0 )
      { caret = valInt(normalise_index(lb, toInt(caret)));
	oldcaret = caret;
      } else
	caret = start;
	
      caret += times;
      caret = valInt(normalise_index(lb, toInt(caret)));
      di   = getNth0Chain(lb->dict->members, toInt(caret));

      if ( di )
      { assign(lb, caret, toInt(caret));

	if ( lb->multiple_selection == ON &&
	     instanceOfObject(EVENT->value, ClassEvent) )
	{ EventObj ev = EVENT->value;

	  if ( valInt(ev->buttons) & BUTTON_shift )
	    send(lb, NAME_changeSelection, NAME_extend, di, EAV);
	  else
	    send(lb, NAME_changeSelection, NAME_set, di, EAV);
	} else
	  send(lb, NAME_changeSelection, NAME_set, di, EAV);
      }
    }

    if ( di )
    { normaliseListBrowser(lb, di);
      return ChangeItemListBrowser(lb, di);
    }

    fail;
  }

  fail;
}


static status
previousLineListBrowser(ListBrowser lb, Int lines)
{ if ( isDefault(lines) )
    lines = toInt(-1);
  else
    lines = neg(lines);
  
  return nextLineListBrowser(lb, lines);
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


static status
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
selectionStyleListBrowser(ListBrowser lb, Style style)
{ if ( lb->selection_style != style )
  { assign(lb, selection_style, style);
    ChangedListBrowser(lb);
  }

  succeed;
}


static status
multipleSelectionListBrowser(ListBrowser lb, Bool val)
{ if ( lb->multiple_selection != val )
  { if ( val == ON )
    { if ( isNil(lb->selection) )
        assign(lb, selection, newObject(ClassChain, EAV));
      else
      	assign(lb, selection, newObject(ClassChain, lb->selection, EAV));
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


status
backgroundListBrowser(ListBrowser lb, Any bg)
{ return backgroundTextImage(lb->image, bg);
  
  succeed;
}
					/* avoid capture by device */

static status
clearListBrowser(ListBrowser lb)
{ if ( notNil(lb->dict) )
    send(lb->dict, NAME_clear, EAV);

  succeed;
}


DictItem
getMemberListBrowser(ListBrowser lb, Any key)
{ if ( notNil(lb->dict) )
    answer(getMemberDict(lb->dict, key));

  fail;
}


static status
referenceListBrowser(ListBrowser lb, Point ref)
{ return referenceGraphical((Graphical) lb, ref);
}


		/********************************
		*             VISUAL		*
		********************************/

Chain
getContainsListBrowser(ListBrowser lb)
{ if ( notNil(lb->dict) )
    answer(answerObject(ClassChain, lb->dict, EAV));

  fail;
}


static Any
getMasterListBrowser(ListBrowser lb)
{ if ( instanceOfObject(lb->device, ClassBrowser) )
    answer(lb->device);

  answer(lb);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_scrollVertical[] =
        { "{forwards,backwards,goto}", "{file,page,line}", "int" };
static char *T_showScrollBar[] =
        { "show=[bool]", "which=[scroll_bar]" };
static char *T_changeSelection[] =
        { "action={set,toggle,extend,clear,cancel}",
	  "context=[dict_item|chain]" };
static char *T_initialise[] =
        { "dict=[dict]", "width=[int]", "height=[int]" };
static char *T_style[] =
        { "style_name=name", "style=style" };
static char *T_insertSelf[] =
        { "times=[int]", "character=[char]" };
static char *T_xADintD_yADintD_widthADintD_heightADintD[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_listBrowser[] =
{ SV(NAME_dict, "dict*", IV_GET|IV_STORE, dictListBrowser,
     NAME_delegate, "Associated dict object (table of items)"),
  IV(NAME_image, "text_image", IV_GET,
     NAME_components, "TextImage used to display textlines"),
  IV(NAME_scrollBar, "scroll_bar", IV_GET,
     NAME_components, "Scrollbar used to scroll window"),
  IV(NAME_labelText, "text*", IV_GET,
     NAME_components, "Text object that displays the label"),
  SV(NAME_status, "{active,inactive}", IV_GET|IV_STORE, statusListBrowser,
     NAME_event, "Handle typing?"),
  IV(NAME_keyBinding, "key_binding", IV_BOTH,
     NAME_accelerator, "Key binding table"),
  SV(NAME_selection, "chain|member:dict_item*", IV_NONE|IV_STORE,
     selectionListBrowser,
     NAME_selection, "Selected items"),
  SV(NAME_selectionStyle, "[style]", IV_GET|IV_STORE,
     selectionStyleListBrowser,
     NAME_appearance, "Style for selection feedback"),
  SV(NAME_multipleSelection, "bool", IV_GET|IV_STORE, multipleSelectionListBrowser,
     NAME_selection, "If @on, multiple items may be selected"),
  IV(NAME_selectMessage, "code*", IV_BOTH,
     NAME_action, "Send on left-click on item"),
  IV(NAME_openMessage, "code*", IV_BOTH,
     NAME_action, "Send on keyboard selection or double click"),
  IV(NAME_cancelMessage, "code*", IV_BOTH,
     NAME_action, "Send on drag-select with `up' outside browser"),
  IV(NAME_popup, "popup*", IV_BOTH,
     NAME_menu, "Associated popup menu"),
  SV(NAME_font, "font", IV_GET|IV_STORE, fontListBrowser,
     NAME_appearance, "Font for displayed items"),
  IV(NAME_styles, "sheet", IV_GET,
     NAME_appearance, "Name --> style mapping"),
  IV(NAME_size, "characters=size", IV_GET,
     NAME_area, "Size in characters/lines"),
  IV(NAME_start, "int", IV_GET,
     NAME_scroll, "Object on top-row of display"),
  IV(NAME_searchOrigin, "int", IV_NONE,
     NAME_search, "Start of incremental search"),
  IV(NAME_searchHit, "int", IV_NONE,
     NAME_search, "Current hit"),
  IV(NAME_searchString, "char_array*", IV_NONE,
     NAME_search, "Current search string"),
  IV(NAME_caret, "int*", IV_NONE,
     NAME_keyboard, "Location for ->next_line"),
  IV(NAME_selectionOrigin, "int*", IV_NONE,
     NAME_keyboard, "Origin for ->change_selection: extend"),
  IV(NAME_startCell, "alien:Cell", IV_NONE,
     NAME_cache, "Cell reference to top-row of display")
};

/* Send Methods */

static senddecl send_listBrowser[] =
{ SM(NAME_compute, 0, NULL, computeListBrowser,
     DEFAULT, "Recompute the image"),
  SM(NAME_geometry, 4, T_xADintD_yADintD_widthADintD_heightADintD, geometryListBrowser,
     DEFAULT, "Resize the text_image"),
  SM(NAME_initialise, 3, T_initialise, initialiseListBrowser,
     DEFAULT, "Create from dict, width and height"),
  SM(NAME_requestGeometry, 4, T_xADintD_yADintD_widthADintD_heightADintD, requestGeometryListBrowser,
     DEFAULT, "Map size to character units"),
  SM(NAME_unlink, 0, NULL, unlinkListBrowser,
     DEFAULT, "Unlink from dict and device"),
  SM(NAME_typed, 1, "event_id", typedListBrowser,
     NAME_accelerator, "Handle typed character"),
  SM(NAME_showLabel, 1, "show=bool", showLabelListBrowser,
     NAME_appearance, "Show/unshow the label"),
  SM(NAME_style, 2, T_style, styleListBrowser,
     NAME_appearance, "Set style associated with name"),
  SM(NAME_tabStops, 1, "vector*", tabStopsListBrowser,
     NAME_appearance, "Set tab-stops (pixels)"),
  SV(NAME_background, 1, "[colour|pixmap]", backgroundListBrowser,
     NAME_appearance, "Background colour"),
  SM(NAME_Size, 1, "pixels=size", SizeListBrowser,
     NAME_area, "Set size in pixels (trap window resize)"),
  SM(NAME_extendPrefixOrNext, 0, NULL, extendPrefixOrNextListBrowser,
     NAME_caret, "->extend_prefix or ->next"),
  SM(NAME_next, 0, NULL, nextListBrowser,
     NAME_caret, "Move caret to next item (`device ->advance')"),
  SM(NAME_reference, 1, "point", referenceListBrowser,
     NAME_dialogItem, "Set reference as dialog_item"),
  SM(NAME_clear, 0, NULL, clearListBrowser,
     NAME_edit, "Remove all items from the associated dict"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, WantsKeyboardFocusListBrowser,
     NAME_event, "Test if ready to accept input (non-empty)"),
  SM(NAME_event, 1, "event", eventListBrowser,
     NAME_event, "Handle arbitrary event"),
  SM(NAME_label, 1, "name", labelListBrowser,
     NAME_label, "Set the name of the label"),
  SM(NAME_ChangeItem, 1, "dict_item", ChangeItemListBrowser,
     NAME_repaint, "Handle changed item from dict"),
  SM(NAME_Clear, 0, NULL, ClearListBrowser,
     NAME_repaint, "Handle clear from dict"),
  SM(NAME_DeleteItem, 1, "dict_item", DeleteItemListBrowser,
     NAME_repaint, "Handle deleted item from dict"),
  SM(NAME_InsertItem, 1, "dict_item", InsertItemListBrowser,
     NAME_repaint, "Handle inserted item from dict"),
  SM(NAME_normalise, 1, "member:dict_item", normaliseListBrowser,
     NAME_scroll, "Make specified item visible"),
  SM(NAME_scrollDown, 1, "[int]", scrollDownListBrowser,
     NAME_scroll, "Scroll lines down (default one window)"),
  SM(NAME_scrollTo, 1, "[int]", scrollToListBrowser,
     NAME_scroll, "Make nth-1 item start of window"),
  SM(NAME_scrollUp, 1, "[int]", scrollUpListBrowser,
     NAME_scroll, "Scroll lines up (default one window)"),
  SM(NAME_recenter, 1, "[int]", recenterListBrowser,
     NAME_scroll, "Recenter current line (to be implemented)"),
  SM(NAME_scrollVertical, 3, T_scrollVertical, scrollVerticalListBrowser,
     NAME_scroll, "Handle scroll_bar request"),
  SM(NAME_showScrollBar, 2, T_showScrollBar, showScrollBarListBrowser,
     NAME_scroll, "Control visibility of the <-scroll_bar"),
  SM(NAME_nextLine, 1, "[int]", nextLineListBrowser,
     NAME_selection, "Set selection to next item"),
  SM(NAME_previousLine, 1, "[int]", previousLineListBrowser,
     NAME_selection, "Set selection to previous item"),
  SM(NAME_backwardDeleteChar, 0, NULL, backwardDeleteCharListBrowser,
     NAME_search, "Undo last search extension"),
  SM(NAME_cancelSearch, 0, NULL, cancelSearchListBrowser,
     NAME_search, "Cancel the current search operation"),
  SM(NAME_enter, 0, NULL, enterListBrowser,
     NAME_search, "Select current item as double-click"),
  SM(NAME_extendPrefix, 0, NULL, extendPrefixListBrowser,
     NAME_search, "Extend search with common part"),
  SM(NAME_extendToCurrent, 0, NULL, extendToCurrentListBrowser,
     NAME_search, "Extend search to current item"),
  SM(NAME_insertSelf, 2, T_insertSelf, insertSelfListBrowser,
     NAME_search, "Start/Continue incremental search"),
  SM(NAME_keyboardQuit, 0, NULL, cancelSearchListBrowser,
     NAME_search, "Equivalent to ->cancel_search"),
  SM(NAME_repeatSearch, 0, NULL, repeatSearchListBrowser,
     NAME_search, "Repeat with same string"),
  SM(NAME_changeSelection, 2, T_changeSelection, changeSelectionListBrowser,
     NAME_selection, "Hook in selection management"),
  SM(NAME_deselect, 1, "member:dict_item", deselectListBrowser,
     NAME_selection, "Unselect (remove from selection) item"),
  SM(NAME_select, 1, "member:dict_item", selectListBrowser,
     NAME_selection, "Select (add to selection) item"),
  SM(NAME_selected, 1, "member:dict_item", selectedListBrowser,
     NAME_selection, "Test if item is selected")
};

/* Get Methods */

static getdecl get_listBrowser[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsListBrowser,
     DEFAULT, "Dict visualised"),
  GM(NAME_master, 0, "device", NULL, getMasterListBrowser,
     DEFAULT, "Principal visual I'm part of (self or browser)"),
  GM(NAME_selection, 0, "chain|dict_item", NULL, getSelectionListBrowser,
     DEFAULT, "Current value of selection"),
  GM(NAME_showLabel, 0, "bool", NULL, getShowLabelListBrowser,
     NAME_appearance, "Bool indicating if label is visible"),
  GM(NAME_height, 0, "characters=int", NULL, getHeightListBrowser,
     NAME_area, "Height in character units"),
  GM(NAME_width, 0, "characters=int", NULL, getWidthListBrowser,
     NAME_area, "Width in character units"),
  GM(NAME_dictItem, 1, "dict_item", "event", getDictItemListBrowser,
     NAME_event, "DictItem on which event occurred"),
  GM(NAME_FetchFunction, 0, "alien:FetchFunction", NULL, getFetchFunctionListBrowser,
     NAME_internal, "Pointer to C-function to fetch char"),
  GM(NAME_MarginFunction, 0, "alien:MarginFunction", NULL, getMarginFunctionListBrowser,
     NAME_internal, "Pointer to C-function to fetch margins"),
  GM(NAME_RewindFunction, 0, "alien:RewindFunction", NULL, getRewindFunctionListBrowser,
     NAME_internal, "Pointer to C-function to rewind object"),
  GM(NAME_ScanFunction, 0, "alien:ScanFunction", NULL, getScanFunctionListBrowser,
     NAME_internal, "Pointer to C-function to scan for char-type"),
  GM(NAME_SeekFunction, 0, "alien:SeekFunction", NULL, getSeekFunctionListBrowser,
     NAME_internal, "Pointer to C-function to seek to position"),
  GM(NAME_label, 0, "name", NULL, getLabelListBrowser,
     NAME_label, "Current value of the label"),
  GM(NAME_member, 1, "dict_item", "any", getMemberListBrowser,
     NAME_member, "DictItem with given key"),
  GM(NAME_length, 0, "int", NULL, getLengthListBrowser,
     NAME_scroll, "Length of contents (for scroll_bar)"),
  GM(NAME_view, 0, "int", NULL, getViewListBrowser,
     NAME_scroll, "Length of view (for scroll_bar)")
};

/* Resources */

static classvardecl rc_listBrowser[] =
{ RC(NAME_background, "colour|pixmap", "white",
     "Colour/fill pattern of the background"),
  RC(NAME_clearSelectionOnSearch, "bool", "@on",
     "@on: clear selection when searching"),
  RC(NAME_cursor, "cursor", "right_ptr",
     "Default cursor"),
  RC(NAME_font, "font", "normal",
     "Default font"),
  RC(NAME_isearchStyle, "[style]",
     UXWIN("when(@colour_display,\n"
	   "     style(background := green),\n"
	   "     style(background:= @grey25_image))",
	   "@_isearch_style"),
     "Style for incremental search"),
  RC(NAME_labelFont, "font", "bold",
     "Font used to display the label"),
  RC(NAME_pen, "0..", "1",
     "Thickness of box around list_browser"),
  RC(NAME_searchIgnoreCase, "bool", "@on",
     "@on: ignore case when searching"),
  RC(NAME_selectionStyle, "[style]",
     UXWIN("when(@colour_display,\n"
	   "     style(background := black, colour := white),\n"
	   "     style(highlight  := @on))",
	   "@_select_style"),
     "Style object for <-selection"),
  RC(NAME_size, "size", "size(15,10)",
     "Default size in `characters x lines'")
};

/* Class Declaration */

static Name listBrowser_termnames[] = { NAME_dict, NAME_width, NAME_height };

ClassDecl(listBrowser_decls,
          var_listBrowser, send_listBrowser, get_listBrowser, rc_listBrowser,
          3, listBrowser_termnames,
          "$Rev$");


status
makeClassListBrowser(Class class)
{ declareClass(class, &listBrowser_decls);

  setLoadStoreFunctionClass(class, loadListBrowser, storeListBrowser);
  setRedrawFunctionClass(class, RedrawAreaListBrowser);
  delegateClass(class, NAME_dict);

  succeed;
}

