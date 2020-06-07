////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020 The Koko Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution
//
// This file is part of Koko.
//
// Koko is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Koko is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Koko; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

// Provides functions for RGB color value lookup
//
// Ulf GRIESMANN, June 2020

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "rgb_hash.h" // hash table for color lookup

#define COLSTRLEN 32  // max. color string length


//--- Prototypes ---------------------------------------------------------

void c_rgbval( const char *colorname, int lcn, int *R, int *G, int *B );
void c_rgbhex( const char *colorname, int lcn, char *hexstring, int lhs );
void c_rgbint( const char *colorname, int lcn, int *cval );
static void lookup_rgb( char *colorname, int *R, int *G, int *B );
static void hex_to_int( char *hexstr, int *colint);


//------------------------------------------------------------------------

// F90 - callable
// For a specified color name, returns a triplet of
// RGB (red - green - blue) values in the range 0 .. 255.
// If the color lookup fails, the returned values are negative.
//
// INPUT
// colorname :  a color name or a #RRGGBB string
// lcn :        number of characters in Fortran colorname string
//
// OUTPUT
// R,G,B :      red green blue values corresponding to the color
void
c_rgbval( const char *colorname, int lcn, int *R, int *G, int *B )
{
   char loc_colorname[COLSTRLEN];
   int nc, r, g, b, rgb;

   // make sure we don't write outside the local buffer
   if (lcn > COLSTRLEN - 1) {
      nc = COLSTRLEN - 1;
   }
   else {
      nc = lcn;
   }
   
   // turn color name into a C string
   strncpy(loc_colorname, colorname, nc);
   loc_colorname[nc] = '\0';   // in case someone messes up the color name

   if (loc_colorname[0] == '#') {      // a hex string was passed
      hex_to_int(loc_colorname, (int *)&rgb);
      r =  rgb &  255;
      g = (rgb & (255 << 8))  >> 8;
      b = (rgb & (255 << 16)) >> 16;
   }
   else {    // look up keyword - color triplet
      lookup_rgb(loc_colorname, &r, &g, &b);
   }

   // return results
   *R = r;
   *G = g;
   *B = b;
}


// F90 - callable
// For a specified color name, returns a triplet of
// RGB (red - green - blue) values in the form of a
// hex format string: #RRGGBB
// If the color lookup fails, the first character in the
// return string will be 'F'
//
// INPUT
// colorname :  a color name (camelHump format, no spaces)
// lcn :        number of characters in Fortran colorname string
//
// OUTPUT
// hexstring :  RGB color coded as hexadecimal string
// lhs :        length of the Fortran string variable
void
c_rgbhex( const char *colorname, int lcn, char *hexstring, int lhs )
{
   char loc_colorname[COLSTRLEN];
   int nc, r, g, b;

   // make sure we don't write outside the local buffer
   if (lcn > COLSTRLEN - 1) {
      nc = COLSTRLEN - 1;
   }
   else {
      nc = lcn;
   }
   
   // turn color name into a C string
   strncpy(loc_colorname, colorname, nc);
   loc_colorname[nc] = '\0';   // in case someone messes up the color name

   // look up keyword - color triplet structure
   lookup_rgb(loc_colorname, &r, &g, &b);

   // return Fortran string with hex-coded rgb values
   if (lhs < 8) {              // output string is too short
      hexstring[0] = 'F';
      memset(hexstring+1, ' ', lhs-1);
   }
   else {
      sprintf(hexstring, "#%2X%2X%2X", r, g, b);
      memset(hexstring+7, ' ', lhs-7);
   }
}


// F90 - callable
// For a specified color name, returns a triplet of
// RGB (red - green - blue) values in the form of a
// 24-bit integer number.
// If the color lookup fails, a negative number is returnd.
//
// INPUT
// colorname :  a color name (camelHump format, no spaces)
// lcn :        number of characters in Fortran colorname string
//
// OUTPUT
// cval :       RGB color coded as an integer number. First byte
//              is red, 2nd byte is green 3rd byte is blue
void
c_rgbint( const char *colorname, int lcn, int *cval )
{
   char loc_colorname[COLSTRLEN];
   int nc, r, g, b;

   // make sure we don't write outside the local buffer
   if (lcn > COLSTRLEN - 1) {
      nc = COLSTRLEN - 1;
   }
   else {
      nc = lcn;
   }
   
   // turn color name into a C string
   strncpy(loc_colorname, colorname, nc);
   loc_colorname[nc] = '\0';   // in case someone messes up the color name

   if (loc_colorname[0] == '#') { // a hex string was passed
      hex_to_int(loc_colorname, cval);
   }
   else {
      // look up keyword - color triplet structure
      lookup_rgb(loc_colorname, &r, &g, &b);

      // encode as 24-bit integer
      if (r < 0) {
	*cval = -1;
      }
      else {
	*cval = (b << 16) & (g << 8) & r;
      }
   }
}


// Function that performs the rgb value lookup using a hash function
// generated by gperf
static void
lookup_rgb( char *colorname, int *R, int *G, int *B )
{
   struct keyword *pk;   // pointer to keyword structure
   int k;

   // turn string lowercase for lookup
   for (k=0; colorname[k]; k++) {
      colorname[k] = tolower(colorname[k]);
   }

   // perform color lookup
   pk = (struct keyword *)in_word_set(colorname, strlen(colorname));

   if (pk == NULL) {
     *R = -1;
     *G = -1;
     *B = -1;
   }
   else {
     *R = pk->rgb[0];
     *G = pk->rgb[1];
     *B = pk->rgb[2];
   }
}


// Function converts a color hex string in the format #RRGGBB into
// a 24-bit integer where the first byte contains R, the second byte
// contains G, and the third byte contains B
static void
hex_to_int( char *hexstr, int *colint)
{
   char buf[16];

   // re-arrange colors in string
   // to form the 24-bit hex number BBGGRR
   buf[0] = hexstr[5];  // BB
   buf[1] = hexstr[6];
   buf[2] = hexstr[3];  // GG
   buf[3] = hexstr[4];
   buf[4] = hexstr[1];  // RR
   buf[5] = hexstr[2];
   buf[6] = '\0';

   *colint = strtol(buf, NULL, 16);
}
