intptr_t
skip_list_v2(Word l, Word *tailp ARG_LD)
{ deRef(l);

  if ( !isList(*l) )
  { *tailp = l;
    return 0;
  }

  else
  {
    Word checkCell, currentCell;
    intptr_t length = 0;
    int power, lam;

    checkCell = currentCell = l;

    lam = 0;
    power = 1;

    while ( TRUE )
    {
      currentCell = TailList(currentCell);
      deRef(currentCell);
      length++;

      if ( !isList(*currentCell) || (*checkCell == *currentCell) )
      { break;
      }

      lam++;
      if ( power == lam )
      {
        checkCell = currentCell;
        power *= 2;
        lam = 0;
      }
    }

    *tailp = currentCell;
    return length;
  }
}
