/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Convert between HSV  (Hue-Saturnation-Value)   and  RGB (Red-Green-Blue)
colour models. XPCE uses the RGB  model internally, but provides methods
to class colour to  deal  with  the  HSV   model  as  it  is  much  more
comfortable in computing human aspects  of   colour  preception  such as
distance, shading, etc.

	RBG	Set of intensities in range 0.0-1.0
	HSV	Also ranged 0.0-1.0
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
RGBToHSV(float r, float g, float b, float *H, float *S, float *V)
{ float cmax, cmin;
  float h, s, v;

  cmax = r;
  cmin = r;
  if ( g > cmax )
  { cmax = g;
  } else if ( g < cmin )
  { cmin = g;
  }
  if ( b > cmax )
  { cmax = b;
  } else if ( b < cmin )
  { cmin = b;
  }
  v = cmax;

  if ( v > 0.0 )
  { s = (cmax - cmin) / cmax;
  } else
  { s = 0.0;
  }

  if ( s > 0 )
  { if ( r == cmax )
    { h = (float)0.17 * (g - b) / (cmax - cmin);
    } else if ( g == cmax )
    { h = (float)0.33 + (float)0.17 * (b - r) / (cmax - cmin);
    } else
    { h = (float)0.67 + (float)0.17 * (r - g) / (cmax - cmin);
    }
    if ( h < 0.0 )
    { h = h + (float)1.0;
    }
  } else
  { h = 0.0;
  }

  *H = h;
  *S = s;
  *V = v;
}


void
HSVToRGB(float hue, float sat, float V, float *R, float *G, float *B)
{ float r, g, b;


  if (hue > 0.17 && hue <= 0.33)	/* green/red */
  { g = 1.0;
    r = ((float)0.33 - hue) / (float)0.16;
    b = 0.0;
  } else if (hue > 0.33 && hue <= 0.5)	/* green/blue */
  { g = 1.0;
    b = (hue - (float)0.33) / (float)0.17;
    r = 0.0;
  } else if (hue > 0.5 && hue <= 0.67)	/* blue/green */
  { b = 1.0;
    g = ((float)0.67 - hue) / (float)0.17;
    r = 0.0;
  } else if (hue > 0.67 && hue <= 0.83)	/* blue/red */
  { b = 1.0;
    r = (hue - (float)0.67) / (float)0.16;
    g = 0.0;
  } else if (hue > 0.83 && hue <= 1.0)	/* red/blue */
  { r = 1.0;
    b = ((float)1.0 - hue) / (float)0.17;
    g = 0.0;
  } else				/* red/green */
  { r = 1.0;
    g = hue / (float)0.17;
    b = 0.0;
  }

  r = (sat * r + ((float)1.0 - sat));
  g = (sat * g + ((float)1.0 - sat));
  b = (sat * b + ((float)1.0 - sat));

  r = r * V;
  g = g * V;
  b = b * V;

  *R = r;
  *G = g;
  *B = b;
}
