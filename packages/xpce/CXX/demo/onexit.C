#include <pce/Pce.h>
#include <pce/Call.h>
#include <stdio.h>
#include <stdlib.h>

PceStatus
help(PceArg status)
{ printf("Help, dying code %d\n", (int) status);

  return SUCCEED;
}


PceStatus
pceInitApplication(int argc, char *argv[])
{ ThePce.send("exit_message", PceCall(help, TheArg1));

  ThePce.send("write_ln", "registered");
  exit(21);

  return SUCCEED;
}
