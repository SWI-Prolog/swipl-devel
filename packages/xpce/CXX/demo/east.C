/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
East-project demo.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <pce/Pce.h>
#include <pce/Bitmap.h>
#include <pce/Class.h>
#include <pce/ConnectGesture.h>
#include <pce/Create.h>
#include <pce/Dialog.h>
#include <pce/Divide.h>
#include <pce/Equation.h>
#include <pce/Handler.h>
#include <pce/KeyBinding.h>
#include <pce/Label.h>
#include <pce/Line.h>
#include <pce/MenuBar.h>
#include <pce/MenuItem.h>
#include <pce/Message.h>
#include <pce/MoveGesture.h>
#include <pce/Obtain.h>
#include <pce/Picture.h>
#include <pce/Plus.h>
#include <pce/Popup.h>
#include <pce/Text.h>


		 /*******************************
		 *	XPCE CLASS V_FORM	*
		 *******************************/

PceGlobal iconSpatial("icon_spatial", "spatial",
		      PceEquation("xref", PcePlus("x", PceDivide("w", 2))),
		      PceEquation("yref", PcePlus("y", "h")),
		      PceEquation("xref", PcePlus("x", PceDivide("w", 2))),
		      PceEquation("yref", "y"));

PceGlobal vformLink("vform_link", "link",
		    "from", "to", PceLine(0,0,0,0,"second"));

PceGlobal vformRecogniser("vform_recogniser", "handler_group",
			  PceMoveGesture(PceArg("middle")),	// G++
			  PceConnectGesture("left", "", vformLink),
			  PceHandler("area_enter",
				     PceMessage(TheReceiver, "start_edit")),
			  PceHandler("area_exit",
				     PceMessage(TheReceiver, "stop_edit")));

PceGlobal   inHandle("in_handle",    "handle", 0,   5, "to",   "to");

PceGlobal outGHandle("out_g_handle", "handle", 64, 10, "from", "good");
PceGlobal outWHandle("out_w_handle", "handle", 64, 22, "from", "wrong");

PceGlobal outYHandle("out_y_handle", "handle",  7, 20, "from", "y");
PceGlobal outNHandle("out_n_handle", "handle", 45, 20, "from", "n");

PceGlobal out3AHandle("out3a_handle", "handle", 64,  7, "from", "a");
PceGlobal out3BHandle("out3b_handle", "handle", 64, 16, "from", "b");
PceGlobal out3CHandle("out3c_handle", "handle", 64, 25, "from", "c");

PceGlobal out5AHandle("out5a_handle", "handle", 64,  5, "from", "a");
PceGlobal out5BHandle("out5b_handle", "handle", 64, 11, "from", "b");
PceGlobal out5CHandle("out5c_handle", "handle", 64, 16, "from", "c");
PceGlobal out5DHandle("out5d_handle", "handle", 64, 22, "from", "d");
PceGlobal out5EHandle("out5e_handle", "handle", 64, 27, "from", "e");

#define streq(s, q)	(strcmp((s), (q)) == 0)

PceStatus
initialiseVForm(PceReceiver f, PceArg type, PceArg name)
{ PceArg bm, t;

  f.sendSuper("initialise");

  f.send("display", bm=PceBitmap(type.get("append", ".bm")));
  f.send("display", t =PceText(name));
  iconSpatial.send("forwards", bm, t);

  f.send("handle", inHandle);

  if ( streq(type, "open") )
  { f.send("handle", outGHandle);
    f.send("handle", outWHandle);
  } else if ( streq(type, "yesno") )
  { f.send("handle", outYHandle);
    f.send("handle", outNHandle);
  } else if ( streq(type, "multi3") )
  { f.send("handle", out3AHandle);
    f.send("handle", out3BHandle);
    f.send("handle", out3CHandle);
  } else if ( streq(type, "multi5") )
  { f.send("handle", out5AHandle);
    f.send("handle", out5BHandle);
    f.send("handle", out5CHandle);
    f.send("handle", out5DHandle);
    f.send("handle", out5EHandle);
  }

  return SUCCEED;
}


PceStatus
eventVForm(PceReceiver f, PceArg ev)
{ return vformRecogniser.send("event", ev);
}


PceStatus
startEditVForm(PceReceiver f)
{ PceArg t = f.get("member", "text");
  
  if ( t.get("show_caret") == TheOff )
  { t.get("device").send("for_all", "v_form",
			 PceMessage(TheArg1, "stop_edit"));
    t.send("pen", 1);
    t.send("show_caret", TheOn);
  }

  return SUCCEED;
}


PceStatus
stopEditVForm(PceReceiver f)
{ PceArg t = f.get("member", "text");
  
  if ( t.get("show_caret") == TheOn )
  { t.send("pen", 0);
    t.send("show_caret", TheOff);
  }

  return SUCCEED;
}



PceStatus
makeClassVForm(PceClass cl)
{ cl.defsendmethod("initialise", "oms", "Create from type and name",
		   initialiseVForm,
		   "kind={open,yesno,multi3,multi5}", "name=name");
  cl.defsendmethod("event", "event", "Move and connect handling",
		   eventVForm, "event");
  cl.defsendmethod("start_edit", "event", "Prepare for editing name",
		   startEditVForm);
  cl.defsendmethod("stop_edit", "event", "Edited: forward changes",
		   stopEditVForm);

  return SUCCEED;
}


PceClass VFormClass("v_form", "device", "Intelligent form icon",
		    makeClassVForm);


		 /*******************************
		 *	 CREATE THE EDITOR	*
		 *******************************/

void
makeEditor()
{ PcePicture canvas;
  PceDialog  dialog;
  PceMenuBar mb;
  PcePopup file("file");
  PcePopup edit("edit");
  PceKeyBinding kb;

  canvas.send("below", dialog);
  dialog.send("append", mb);
  dialog.send("append",
	      PceLabel("reporter",
		       "Type one of `o', `y', `3' or `5'"),
	      "right");
  mb.send("append", file);
  mb.send("append", edit);

  file.send("append",
	    PceMenuItem("quit",
			PceMessage(ThePce, "die")));
  edit.send("append",
	    PceMenuItem("layout",
			PceMessage(PceObtain(canvas.get("graphicals"), "head"),
				   "layout")));

  canvas.send("recogniser", kb);
  kb.send("function", "o", PceMessage(canvas, "display",
				      PceCreate("v_form", "open", "open"),
				      PceObtain(TheEvent, "position")));
  kb.send("function", "y", PceMessage(canvas, "display",
				      PceCreate("v_form", "yesno", "yesno"),
				      PceObtain(TheEvent, "position")));
  kb.send("function", "3", PceMessage(canvas, "display",
				      PceCreate("v_form", "multi3", "multi3"),
				      PceObtain(TheEvent, "position")));
  kb.send("function", "5", PceMessage(canvas, "display",
				      PceCreate("v_form", "multi5", "multi5"),
				      PceObtain(TheEvent, "position")));

  dialog.get("frame").send("label", "Intelligent Form Editor 0.0");
  dialog.send("open");
}



PceStatus
pceInitApplication(int argc, char *argv[])
{ makeEditor();

  return SUCCEED;
}
