/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Partial C++ implementation of  the  underlying   database  model  of the
CommonKads Workbench written Prolog. The   implementation  is incomplete
and untested.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <stdio.h>
#include <pce/Pce.h>
#include <pce/Create.h>
#include <pce/Message.h>

static PceArg PceNappend("append");
static PceArg PceNclone_style_variable("clone_style_variable");
static PceArg PceNdirectory("directory");
static PceArg PceNerror("error");
static PceArg PceNexists("exists");
static PceArg PceNfile("file");
static PceArg PceNfor_all("for_all");
static PceArg PceNinstance_of("instance_of");
static PceArg PceNlocal_id("local_id");
static PceArg PceNmember("member");
static PceArg PceNmodified("modified");
static PceArg PceNname("name");
static PceArg PceNnext_id("next_id");
static PceArg PceNobject("object");
static PceArg PceNsave_in_file("save_in_file");
static PceArg PceNsave_style_variable("save_style_variable");
static PceArg PceNslot("slot");

		 /*******************************
		 *	   CLASS DB_OBJECT	*
		 *******************************/

static PceVariable *object_partition;
static PceVariable *object_local_id;

PceStatus
initialiseDbObject(PceReceiver o, PceArg part)
{ o.store(object_partition, part);
  o.store(object_local_id, part.get(PceNnext_id));

  return part.send(PceNappend, o);
}


void
makeClassDbObject()
{ PceClass cl("db_object", PceNobject, "Simple persistent object");

  object_partition =
    cl.defvar("partition", "partition", "Partition I'm part of",
	      "db_partition", "get", TheNil);
  object_local_id =
    cl.defvar(PceNlocal_id, "partition", "Identifier in partition",
	      "int", "get", 0);

  cl.send(PceNclone_style_variable, "partition", "value");

  cl.defsendmethod("initialise", "oms", "Create in partition",
		   initialiseDbObject, "db_partition");
}


		 /*******************************
		 *	 CLASS DB_PARTITION	*
		 *******************************/

static PceVariable *part_name;
static PceVariable *part_current_id;
static PceVariable *part_modified;
static PceVariable *part_object_table;
static PceVariable *part_db;

static PceStatus
initialiseDbPartition(PceReceiver p, PceArg db, PceArg name)
{ p.store(part_name, name);
  p.store(part_db, db);

  return db.send(PceNappend, p);
}


static PceArg
getNextIdDbPartition(PceReceiver p)
{ int i = (int)p.fetch(part_current_id) + 1;
  
  p.store(part_current_id, i);

  return i;
}


static PceArg
getMemberDbPartition(PceReceiver p, PceArg id)
{ return p.fetch(part_object_table).get(PceNmember, id);
}


static PceStatus
appendDbPartition(PceReceiver p, PceArg obj)
{ p.store(part_modified, TheOn);
  p.fetch(part_object_table).send(PceNappend, obj.get(PceNlocal_id), obj);

  return TRUE;
}


static PceStatus
saveDbPartition(PceReceiver p, PceArg always)
{ if ( p.fetch(part_modified) == TheOn || always == TheOn )
  { PceArg file = p.fetch(part_db).get(PceNdirectory).get(PceNfile,
							  p.fetch(part_name));
    if ( p.send(PceNsave_in_file, file) )
      return p.store(part_modified, TheOff);

    return FALSE;
  }
  
  return TRUE;
}


void
makeClassDbPartition()
{ PceClass cl("db_partition", PceNobject, "Group of related db_objects");

  part_name =
    cl.defvar(PceNname, "naming", "Name (id) of the partition",
	      PceNname, "get", "unnamed");
  part_current_id =
    cl.defvar("current_id", "partition", "Next db_object <-local_id",
	      "int", "get", 0);
  part_modified =
    cl.defvar(PceNmodified, "modify", "Has any object been modified?",
	      "bool", "both", TheOn);
  part_object_table =
    cl.defvar("object_table", "partition", "Mapping local_id -->db_object",
	      "hash_table", "get", PceCreate("hash_table"));
  part_db =
    cl.defvar("db", "database", "The database I belong too",
	      "db", "get", TheNil);

  cl.send(PceNsave_style_variable, "db", "nil");

  cl.defsendmethod("initialise", "oms", "Create in db from name",
		   initialiseDbPartition, "db", PceNname);
  cl.defsendmethod(PceNappend, "change", "Invoked when db_object is added",
		   appendDbPartition, "db_object");
  cl.defsendmethod("save", "storage", "Save if partition is modified",
		   saveDbPartition, "always=[bool]");

  cl.defgetmethod(PceNnext_id, "oms", "<-current_id++",
		  "int", getNextIdDbPartition);
  cl.defgetmethod(PceNmember, "set", "local_id ==> db_object",
		  "db_object", getMemberDbPartition, "int");
}


		 /*******************************
		 *	     CLASS DB		*
		 *******************************/

static PceVariable *db_name;
static PceVariable *db_directory;
static PceVariable *db_members;


static PceStatus
initialiseDb(PceReceiver db, PceArg name, PceArg dir)
{ db.store(db_name, name);
  db.store(db_directory, dir);

  return TRUE;
}


static PceStatus
appendDb(PceReceiver db, PceArg part)
{ return db.fetch(db_members).send(PceNappend, part.get(PceNname), part);
}


static PceArg
getMemberDb(PceReceiver db, PceArg name, PceArg create)
{ PceArg table = db.fetch(db_members);
  PceArg part;

  if ( (part = table.get(PceNmember, name)) )
    return part;

  PceArg file = db.fetch(db_directory).get(PceNfile, name);
  if ( file.send(PceNexists) )
  { if ( !(part = file.get(PceNobject)) )
      return FALSE;

    if ( part.send(PceNinstance_of, "db_partition") )
    { db.send(PceNappend, part);
      part.send(PceNslot, "db", db);
      part.send(PceNmodified, TheOff);

      return part;
    }
    
    db.send(PceNerror, "No paritition file", file);

    return FALSE;
  }
  
  if ( create == TheOn )
    return PceObject("db_partition", db, name);

  return FALSE;
}
  

static PceStatus
saveDb(PceReceiver p, PceArg always)
{ return p.fetch(db_members).send(PceNfor_all,
				  PceMessage(TheArg2, "save", always));
}


void
makeClassDb()
{ PceClass cl("db", PceNobject, "Group of related db_partitions");

  db_name =
    cl.defvar(PceNname, "naming", "Name of the database",
	      PceNname, "get", "database");
  db_directory =
    cl.defvar(PceNdirectory, "storage", "Directory the partitions are stored",
	      PceNdirectory, "get", TheNil);
  db_members =
    cl.defvar("members", "partition", "Hash-table holding loaded parts",
	      "hash_table", "get", PceCreate("hash_table"));

  cl.defsendmethod("initialise", "oms", "Create from name and directory",
		   initialiseDb, PceNname, PceNdirectory);
  cl.defsendmethod(PceNappend, "partition", "Append a partition",
		   appendDb, "db_partition");
  cl.defsendmethod("save", "storage", "Save if all modified partitions",
		   saveDb, "always=[bool]");

  cl.defgetmethod(PceNmember, "set", "part-name ==> db_partition",
		  "db_partition", getMemberDb, "name=name", "create=[bool]");
}



		 /*******************************
		 *	  INITIALISATION	*
		 *******************************/

PceStatus
pceInitApplication(int argc, char *argv[])
{ makeClassDbObject();
  makeClassDbPartition();
  makeClassDb();

  return TRUE;
}
