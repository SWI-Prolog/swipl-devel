#include<pce/Pce.h>
#include<stdlib.h>
#include<iostream.h>
#include<pce/Chain.h>
#include<pce/Class.h>

static PceVariable *KaObj_title;
static PceVariable *KaObj_subs;
static PceVariable *KaObj_supers;
static PceVariable *KaObj_graphics;

/*#define newChain PceObject("chain")*/
/*#define newChain PceObject(ClassChain)*/
#define newChain PceChain()

static PceStatus initialise_KaObj(PceReceiver n, PceArg title)
 {
   n.store(KaObj_title, title);
   n.store(KaObj_subs, newChain);
   n.store(KaObj_supers, newChain);
   n.store(KaObj_graphics, newChain);
  return SUCCEED;
 }


PceStatus add_subclass_KaObj(PceReceiver n, PceArg object)
{
  cout << "A method that does nothing";
  return SUCCEED;
}


PceStatus makeClassKaObj(PceClass cl)
{
  KaObj_title =
    cl.defvar("title", "Object", "Object's title",
	      "name", "both", TheNil);
  KaObj_subs =
    cl.defvar("subs", "Relations", "sub-chain",
	      "chain*", "get", TheNil);
  KaObj_supers = 
    cl.defvar("supers", "Relations", "super-chain",
	      "chain*", "get", TheNil);
  KaObj_graphics =
    cl.defvar("graphics", "Graphics", "Displayed graphics",
	      "chain*", "get", TheNil);

  cl.defsendmethod("initialise", "oms", "Create from name",
		   initialise_KaObj, "title=name");

  cl.defsendmethod("add_subclass", "Relations", "add subclass",
		   add_subclass_KaObj, "object=KaObj");

  return SUCCEED;
}

PceClass ClassKaObj("KaObj", "object", "Super for all KaObjects", makeClassKaObj);

class PceKaObj :public PceObject
{
public:
  PceKaObj(PceArg title) :
    PceObject(ClassKaObj, title)
      {
      }
};


inline PceKaObj AsKaObj(PceArg a)
{
  return *((PceKaObj*) &a);
}




PceStatus pceInitApplication(int argc, char *argv[])
{
  PceKaObj top("top_node");

  cout << "type of KaObj sub slot value :" << (char *)top.get("subs").get("class_name");

  return SUCCEED;
}





