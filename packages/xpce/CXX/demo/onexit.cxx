#include <pce/Pce.h>
#include <pce/Call.h>
#include <iostream.h>
#include <stdlib.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Shows handling of exit and exit-hooks.   On some platforms, the argument
will always be 0, as the system exit   hooks does not provide the status
handed to exit()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PceStatus
help(PceArg status)
{ cout << "Help, dying code " << (int) status << endl;

  return SUCCEED;
}


PceStatus
pceInitApplication(int argc, char *argv[])
{ ThePce.send("exit_message", PceCall(help, TheArg1));

  ThePce.send("write_ln", "registered");
  exit(21);

  return SUCCEED;
}
