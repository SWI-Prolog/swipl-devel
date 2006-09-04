#include "tai.h"
#include "caldate.h"

void caldate_normalize(struct caldate *cd)
{
  caldate_frommjd(cd,caldate_mjd(cd),(int *) 0,(int *) 0);
}
