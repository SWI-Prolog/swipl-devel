static int
wcis_name_start_char(int c)
{ if ( c <= 0x037d )
  { if ( c <= 0x007a )
    { if ( c <= 0x005a )
      { return (c >= 0x0041 && c <= 0x005a);
      } else
      { if ( c <= 0x005f )
        { return (c == 0x005f);} else
        { return (c >= 0x0061 && c <= 0x007a);
        }
      }
    } else
    { if ( c <= 0x00f6 )
      { if ( c <= 0x00d6 )
        { return (c >= 0x00c0 && c <= 0x00d6);
        } else
        { return (c >= 0x00d8 && c <= 0x00f6);
        }
      } else
      { if ( c <= 0x02ff )
        { return (c >= 0x00f8 && c <= 0x02ff);
        } else
        { return (c >= 0x0370 && c <= 0x037d);
        }
      }
    }
  } else
  { if ( c <= 0x2fef )
    { if ( c <= 0x200d )
      { if ( c <= 0x1fff )
        { return (c >= 0x037f && c <= 0x1fff);
        } else
        { return (c >= 0x200c && c <= 0x200d);
        }
      } else
      { if ( c <= 0x218f )
        { return (c >= 0x2070 && c <= 0x218f);
        } else
        { return (c >= 0x2c00 && c <= 0x2fef);
        }
      }
    } else
    { if ( c <= 0xfdcf )
      { if ( c <= 0xd7ff )
        { return (c >= 0x3001 && c <= 0xd7ff);
        } else
        { return (c >= 0xf900 && c <= 0xfdcf);
        }
      } else
      { if ( c <= 0xfffd )
        { return (c >= 0xfdf0 && c <= 0xfffd);
        } else
        { return (c >= 0x10000 && c <= 0xeffff);
        }
      }
    }
  }
}

static int
wcis_name_extender_char(int c)
{ if ( c <= 0x0039 )
  { if ( c <= 0x002d )
    { return (c == 0x002d);} else
    { return (c >= 0x0030 && c <= 0x0039);
    }
  } else
  { if ( c <= 0x00b7 )
    { return (c == 0x00b7);} else
    { if ( c <= 0x036f )
      { return (c >= 0x0300 && c <= 0x036f);
      } else
      { return (c >= 0x203f && c <= 0x2040);
      }
    }
  }
}

