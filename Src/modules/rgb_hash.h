/* ANSI-C code produced by gperf version 3.1 */
/* Command-line: gperf rgb_hash.gperf  */
/* Computed positions: -k'1,3,5-8,12-13,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

#line 1 "rgb_hash.gperf"

/*
 * Copyright (C) 2020 The Koko Project Developers
 *  	     
 * See the file COPYRIGHT.md in the top-level directory of this
 * distribution
 *
 * This file is part of Koko.
 *
 * Koko is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Koko is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Koko; see the file COPYING.  If not, see
 * <https://www.gnu.org/licenses/>.
 *
 * -----------------------------------------------------------------
 * Creates a lookup table which returns RGB color values for colors
 * specified by their names as character strings.
 * -----------------------------------------------------------------
 *
 * process with:
 *    gperf rgb_hash.gperf > rgb_hash.h
 */
#line 36 "rgb_hash.gperf"
struct keyword {
   char *name;
   short int rgb[3];
};

#define TOTAL_KEYWORDS 658
#define MIN_WORD_LENGTH 3
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 27
#define MAX_HASH_VALUE 4598
/* maximum key range = 4572, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (register const char *str, register size_t len)
{
  static unsigned short asso_values[] =
    {
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,  555,   25,
        20,    5,    0,  640,  185,  180,  165,  160, 4599, 4599,
      4599, 4599, 4599, 4599, 4599,   10,   15,  130,   15, 4599,
       225,   20,    0,    0, 4599,    0,    0,  560,    0,  456,
       400, 4599,  290,    0,    0, 4599,   55,   15, 4599,    0,
      4599, 4599, 4599, 4599, 4599, 4599, 4599,   80,  620,  910,
       995,    0,   10,  255,   85,  791,   10,   30,    5,    5,
       645,  585,  200,   60,    0,  265,    0,  120,  445,  970,
      4599,    0, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599, 4599,
      4599, 4599, 4599, 4599, 4599, 4599
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[12]];
      /*FALLTHROUGH*/
      case 12:
        hval += asso_values[(unsigned char)str[11]];
      /*FALLTHROUGH*/
      case 11:
      case 10:
      case 9:
      case 8:
        hval += asso_values[(unsigned char)str[7]];
      /*FALLTHROUGH*/
      case 7:
        hval += asso_values[(unsigned char)str[6]];
      /*FALLTHROUGH*/
      case 6:
        hval += asso_values[(unsigned char)str[5]];
      /*FALLTHROUGH*/
      case 5:
        hval += asso_values[(unsigned char)str[4]];
      /*FALLTHROUGH*/
      case 4:
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      /*FALLTHROUGH*/
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

struct keyword *
in_word_set (register const char *str, register size_t len)
{
  static struct keyword wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 72 "rgb_hash.gperf"
      {"DimGrey",              {105, 105, 105}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 693 "rgb_hash.gperf"
      {"DarkGrey",             {169, 169, 169}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 93 "rgb_hash.gperf"
      {"DeepSkyBlue",          {  0, 191, 255}},
#line 253 "rgb_hash.gperf"
      {"DeepSkyBlue4",         {  0, 104, 139}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 252 "rgb_hash.gperf"
      {"DeepSkyBlue3",         {  0, 154, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 71 "rgb_hash.gperf"
      {"DimGray",              {105, 105, 105}},
      {""},
#line 74 "rgb_hash.gperf"
      {"SlateGrey",            {112, 128, 144}},
      {""}, {""},
#line 251 "rgb_hash.gperf"
      {"DeepSkyBlue2",         {  0, 178, 238}},
#line 70 "rgb_hash.gperf"
      {"DarkSlateGrey",        { 47,  79,  79}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 250 "rgb_hash.gperf"
      {"DeepSkyBlue1",         {  0, 191, 255}},
#line 694 "rgb_hash.gperf"
      {"DarkGray",             {169, 169, 169}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 94 "rgb_hash.gperf"
      {"SkyBlue",              {135, 206, 235}},
#line 257 "rgb_hash.gperf"
      {"SkyBlue4",             { 74, 112, 139}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 256 "rgb_hash.gperf"
      {"SkyBlue3",             {108, 166, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 96 "rgb_hash.gperf"
      {"SteelBlue",            { 70, 130, 180}},
#line 249 "rgb_hash.gperf"
      {"SteelBlue4",           { 54, 100, 139}},
      {""}, {""}, {""}, {""},
#line 248 "rgb_hash.gperf"
      {"SteelBlue3",           { 79, 148, 205}},
      {""}, {""},
#line 695 "rgb_hash.gperf"
      {"DarkBlue",             {  0,   0, 139}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 255 "rgb_hash.gperf"
      {"SkyBlue2",             {126, 192, 238}},
      {""},
#line 247 "rgb_hash.gperf"
      {"SteelBlue2",           { 92, 172, 238}},
      {""}, {""}, {""}, {""},
#line 246 "rgb_hash.gperf"
      {"SteelBlue1",           { 99, 184, 255}},
      {""}, {""},
#line 254 "rgb_hash.gperf"
      {"SkyBlue1",             {135, 206, 255}},
      {""}, {""}, {""}, {""}, {""},
#line 73 "rgb_hash.gperf"
      {"SlateGray",            {112, 128, 144}},
#line 265 "rgb_hash.gperf"
      {"SlateGray4",           {108, 123, 139}},
      {""}, {""},
#line 69 "rgb_hash.gperf"
      {"DarkSlateGray",        { 47,  79,  79}},
#line 297 "rgb_hash.gperf"
      {"DarkSlateGray4",       { 82, 139, 139}},
#line 264 "rgb_hash.gperf"
      {"SlateGray3",           {159, 182, 205}},
      {""}, {""}, {""},
#line 296 "rgb_hash.gperf"
      {"DarkSlateGray3",       {121, 205, 205}},
      {""}, {""}, {""}, {""}, {""},
#line 62 "rgb_hash.gperf"
      {"azure",                {240, 255, 255}},
#line 229 "rgb_hash.gperf"
      {"azure4",               {131, 139, 139}},
      {""}, {""}, {""},
#line 263 "rgb_hash.gperf"
      {"SlateGray2",           {185, 211, 238}},
#line 175 "rgb_hash.gperf"
      {"purple",               {160,  32, 240}},
#line 482 "rgb_hash.gperf"
      {"purple4",              { 85,  26, 139}},
      {""},
#line 295 "rgb_hash.gperf"
      {"DarkSlateGray2",       {141, 238, 238}},
#line 262 "rgb_hash.gperf"
      {"SlateGray1",           {198, 226, 255}},
#line 228 "rgb_hash.gperf"
      {"azure3",               {193, 205, 205}},
#line 49 "rgb_hash.gperf"
      {"AntiqueWhite",         {250, 235, 215}},
#line 189 "rgb_hash.gperf"
      {"AntiqueWhite4",        {139, 131, 120}},
#line 294 "rgb_hash.gperf"
      {"DarkSlateGray1",       {151, 255, 255}},
      {""}, {""},
#line 481 "rgb_hash.gperf"
      {"purple3",              {125,  38, 205}},
      {""}, {""}, {""}, {""}, {""},
#line 188 "rgb_hash.gperf"
      {"AntiqueWhite3",        {205, 192, 176}},
#line 86 "rgb_hash.gperf"
      {"SlateBlue",            {106,  90, 205}},
#line 233 "rgb_hash.gperf"
      {"SlateBlue4",           { 71,  60, 139}},
      {""}, {""},
#line 85 "rgb_hash.gperf"
      {"DarkSlateBlue",        { 72,  61, 139}},
      {""},
#line 232 "rgb_hash.gperf"
      {"SlateBlue3",           {105,  89, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 227 "rgb_hash.gperf"
      {"azure2",               {224, 238, 238}},
      {""}, {""}, {""},
#line 231 "rgb_hash.gperf"
      {"SlateBlue2",           {122, 103, 238}},
      {""},
#line 480 "rgb_hash.gperf"
      {"purple2",              {145,  44, 238}},
      {""}, {""},
#line 230 "rgb_hash.gperf"
      {"SlateBlue1",           {131, 111, 255}},
#line 226 "rgb_hash.gperf"
      {"azure1",               {240, 255, 255}},
      {""},
#line 187 "rgb_hash.gperf"
      {"AntiqueWhite2",        {238, 223, 204}},
#line 78 "rgb_hash.gperf"
      {"grey",                 {190, 190, 190}},
#line 500 "rgb_hash.gperf"
      {"grey4",                { 10,  10,  10}},
#line 580 "rgb_hash.gperf"
      {"grey44",               {112, 112, 112}},
#line 479 "rgb_hash.gperf"
      {"purple1",              {155,  48, 255}},
      {""}, {""},
#line 353 "rgb_hash.gperf"
      {"gold4",                {139, 117,   0}},
#line 560 "rgb_hash.gperf"
      {"grey34",               { 87,  87,  87}},
      {""},
#line 186 "rgb_hash.gperf"
      {"AntiqueWhite1",        {255, 239, 219}},
      {""},
#line 498 "rgb_hash.gperf"
      {"grey3",                {  8,   8,   8}},
#line 578 "rgb_hash.gperf"
      {"grey43",               {110, 110, 110}},
#line 345 "rgb_hash.gperf"
      {"LightYellow4",         {139, 139, 122}},
      {""}, {""},
#line 352 "rgb_hash.gperf"
      {"gold3",                {205, 173,   0}},
#line 558 "rgb_hash.gperf"
      {"grey33",               { 84,  84,  84}},
      {""}, {""}, {""}, {""},
#line 540 "rgb_hash.gperf"
      {"grey24",               { 61,  61,  61}},
#line 344 "rgb_hash.gperf"
      {"LightYellow3",         {205, 205, 180}},
      {""},
#line 79 "rgb_hash.gperf"
      {"LightGrey",            {211, 211, 211}},
      {""},
#line 520 "rgb_hash.gperf"
      {"grey14",               { 36,  36,  36}},
      {""}, {""}, {""}, {""},
#line 538 "rgb_hash.gperf"
      {"grey23",               { 59,  59,  59}},
      {""}, {""}, {""}, {""},
#line 518 "rgb_hash.gperf"
      {"grey13",               { 33,  33,  33}},
#line 95 "rgb_hash.gperf"
      {"LightSkyBlue",         {135, 206, 250}},
#line 261 "rgb_hash.gperf"
      {"LightSkyBlue4",        { 96, 123, 139}},
      {""},
#line 496 "rgb_hash.gperf"
      {"grey2",                {  5,   5,   5}},
#line 576 "rgb_hash.gperf"
      {"grey42",               {107, 107, 107}},
      {""}, {""}, {""},
#line 351 "rgb_hash.gperf"
      {"gold2",                {238, 201,   0}},
#line 556 "rgb_hash.gperf"
      {"grey32",               { 82,  82,  82}},
      {""},
#line 260 "rgb_hash.gperf"
      {"LightSkyBlue3",        {141, 182, 205}},
      {""},
#line 494 "rgb_hash.gperf"
      {"grey1",                {  3,   3,   3}},
#line 574 "rgb_hash.gperf"
      {"grey41",               {105, 105, 105}},
#line 343 "rgb_hash.gperf"
      {"LightYellow2",         {238, 238, 209}},
      {""}, {""},
#line 350 "rgb_hash.gperf"
      {"gold1",                {255, 215,   0}},
#line 554 "rgb_hash.gperf"
      {"grey31",               { 79,  79,  79}},
      {""}, {""}, {""}, {""},
#line 536 "rgb_hash.gperf"
      {"grey22",               { 56,  56,  56}},
#line 342 "rgb_hash.gperf"
      {"LightYellow1",         {255, 255, 224}},
      {""},
#line 140 "rgb_hash.gperf"
      {"peru",                 {205, 133,  63}},
#line 470 "rgb_hash.gperf"
      {"plum4",                {139, 102, 139}},
#line 516 "rgb_hash.gperf"
      {"grey12",               { 31,  31,  31}},
      {""}, {""},
#line 169 "rgb_hash.gperf"
      {"plum",                 {221, 160, 221}},
      {""},
#line 534 "rgb_hash.gperf"
      {"grey21",               { 54,  54,  54}},
      {""}, {""}, {""},
#line 469 "rgb_hash.gperf"
      {"plum3",                {205, 150, 205}},
#line 514 "rgb_hash.gperf"
      {"grey11",               { 28,  28,  28}},
      {""},
#line 259 "rgb_hash.gperf"
      {"LightSkyBlue2",        {164, 211, 238}},
#line 77 "rgb_hash.gperf"
      {"gray",                 {190, 190, 190}},
#line 499 "rgb_hash.gperf"
      {"gray4",                { 10,  10,  10}},
#line 579 "rgb_hash.gperf"
      {"gray44",               {112, 112, 112}},
      {""}, {""}, {""}, {""},
#line 559 "rgb_hash.gperf"
      {"gray34",               { 87,  87,  87}},
      {""},
#line 258 "rgb_hash.gperf"
      {"LightSkyBlue1",        {176, 226, 255}},
      {""},
#line 497 "rgb_hash.gperf"
      {"gray3",                {  8,   8,   8}},
#line 577 "rgb_hash.gperf"
      {"gray43",               {110, 110, 110}},
#line 405 "rgb_hash.gperf"
      {"LightSalmon4",         {139,  87,  66}},
      {""},
#line 76 "rgb_hash.gperf"
      {"LightSlateGrey",       {119, 136, 153}},
      {""},
#line 557 "rgb_hash.gperf"
      {"gray33",               { 84,  84,  84}},
      {""}, {""}, {""}, {""},
#line 539 "rgb_hash.gperf"
      {"gray24",               { 61,  61,  61}},
#line 404 "rgb_hash.gperf"
      {"LightSalmon3",         {205, 129,  98}},
      {""},
#line 80 "rgb_hash.gperf"
      {"LightGray",            {211, 211, 211}},
#line 468 "rgb_hash.gperf"
      {"plum2",                {238, 174, 238}},
#line 519 "rgb_hash.gperf"
      {"gray14",               { 36,  36,  36}},
      {""}, {""}, {""}, {""},
#line 537 "rgb_hash.gperf"
      {"gray23",               { 59,  59,  59}},
      {""}, {""}, {""},
#line 467 "rgb_hash.gperf"
      {"plum1",                {255, 187, 255}},
#line 517 "rgb_hash.gperf"
      {"gray13",               { 33,  33,  33}},
      {""}, {""}, {""},
#line 495 "rgb_hash.gperf"
      {"gray2",                {  5,   5,   5}},
#line 575 "rgb_hash.gperf"
      {"gray42",               {107, 107, 107}},
      {""}, {""}, {""}, {""},
#line 555 "rgb_hash.gperf"
      {"gray32",               { 82,  82,  82}},
      {""}, {""}, {""},
#line 493 "rgb_hash.gperf"
      {"gray1",                {  3,   3,   3}},
#line 573 "rgb_hash.gperf"
      {"gray41",               {105, 105, 105}},
#line 403 "rgb_hash.gperf"
      {"LightSalmon2",         {238, 149, 114}},
      {""},
#line 97 "rgb_hash.gperf"
      {"LightSteelBlue",       {176, 196, 222}},
#line 269 "rgb_hash.gperf"
      {"LightSteelBlue4",      {110, 123, 139}},
#line 553 "rgb_hash.gperf"
      {"gray31",               { 79,  79,  79}},
      {""}, {""}, {""},
#line 268 "rgb_hash.gperf"
      {"LightSteelBlue3",      {162, 181, 205}},
#line 535 "rgb_hash.gperf"
      {"gray22",               { 56,  56,  56}},
#line 402 "rgb_hash.gperf"
      {"LightSalmon1",         {255, 160, 122}},
      {""},
#line 98 "rgb_hash.gperf"
      {"LightBlue",            {173, 216, 230}},
#line 273 "rgb_hash.gperf"
      {"LightBlue4",           {104, 131, 139}},
#line 515 "rgb_hash.gperf"
      {"gray12",               { 31,  31,  31}},
      {""}, {""}, {""},
#line 272 "rgb_hash.gperf"
      {"LightBlue3",           {154, 192, 205}},
#line 533 "rgb_hash.gperf"
      {"gray21",               { 54,  54,  54}},
      {""}, {""}, {""},
#line 267 "rgb_hash.gperf"
      {"LightSteelBlue2",      {188, 210, 238}},
#line 513 "rgb_hash.gperf"
      {"gray11",               { 28,  28,  28}},
      {""}, {""}, {""},
#line 266 "rgb_hash.gperf"
      {"LightSteelBlue1",      {202, 225, 255}},
#line 680 "rgb_hash.gperf"
      {"grey94",               {240, 240, 240}},
      {""}, {""}, {""},
#line 271 "rgb_hash.gperf"
      {"LightBlue2",           {178, 223, 238}},
#line 660 "rgb_hash.gperf"
      {"grey84",               {214, 214, 214}},
      {""}, {""}, {""},
#line 270 "rgb_hash.gperf"
      {"LightBlue1",           {191, 239, 255}},
#line 678 "rgb_hash.gperf"
      {"grey93",               {237, 237, 237}},
      {""}, {""},
#line 75 "rgb_hash.gperf"
      {"LightSlateGray",       {119, 136, 153}},
#line 313 "rgb_hash.gperf"
      {"PaleGreen4",           { 84, 139,  84}},
#line 658 "rgb_hash.gperf"
      {"grey83",               {212, 212, 212}},
      {""}, {""}, {""},
#line 312 "rgb_hash.gperf"
      {"PaleGreen3",           {124, 205, 124}},
#line 640 "rgb_hash.gperf"
      {"grey74",               {189, 189, 189}},
      {""}, {""},
#line 90 "rgb_hash.gperf"
      {"RoyalBlue",            { 65, 105, 225}},
#line 237 "rgb_hash.gperf"
      {"RoyalBlue4",           { 39,  64, 139}},
#line 620 "rgb_hash.gperf"
      {"grey64",               {163, 163, 163}},
      {""}, {""},
#line 185 "rgb_hash.gperf"
      {"seashell4",            {139, 134, 130}},
#line 236 "rgb_hash.gperf"
      {"RoyalBlue3",           { 58,  95, 205}},
#line 638 "rgb_hash.gperf"
      {"grey73",               {186, 186, 186}},
      {""},
#line 59 "rgb_hash.gperf"
      {"seashell",             {255, 245, 238}},
#line 184 "rgb_hash.gperf"
      {"seashell3",            {205, 197, 191}},
#line 311 "rgb_hash.gperf"
      {"PaleGreen2",           {144, 238, 144}},
#line 618 "rgb_hash.gperf"
      {"grey63",               {161, 161, 161}},
      {""}, {""}, {""},
#line 310 "rgb_hash.gperf"
      {"PaleGreen1",           {154, 255, 154}},
#line 676 "rgb_hash.gperf"
      {"grey92",               {235, 235, 235}},
      {""}, {""}, {""},
#line 235 "rgb_hash.gperf"
      {"RoyalBlue2",           { 67, 110, 238}},
#line 656 "rgb_hash.gperf"
      {"grey82",               {209, 209, 209}},
      {""}, {""},
#line 183 "rgb_hash.gperf"
      {"seashell2",            {238, 229, 222}},
#line 234 "rgb_hash.gperf"
      {"RoyalBlue1",           { 72, 118, 255}},
#line 674 "rgb_hash.gperf"
      {"grey91",               {232, 232, 232}},
      {""},
#line 101 "rgb_hash.gperf"
      {"DarkTurquoise",        {  0, 206, 209}},
#line 182 "rgb_hash.gperf"
      {"seashell1",            {255, 245, 238}},
#line 277 "rgb_hash.gperf"
      {"LightCyan4",           {122, 139, 139}},
#line 654 "rgb_hash.gperf"
      {"grey81",               {207, 207, 207}},
      {""}, {""},
#line 88 "rgb_hash.gperf"
      {"LightSlateBlue",       {132, 112, 255}},
#line 276 "rgb_hash.gperf"
      {"LightCyan3",           {180, 205, 205}},
#line 636 "rgb_hash.gperf"
      {"grey72",               {184, 184, 184}},
      {""}, {""}, {""}, {""},
#line 616 "rgb_hash.gperf"
      {"grey62",               {158, 158, 158}},
      {""}, {""}, {""}, {""},
#line 634 "rgb_hash.gperf"
      {"grey71",               {181, 181, 181}},
      {""}, {""}, {""},
#line 275 "rgb_hash.gperf"
      {"LightCyan2",           {209, 238, 238}},
#line 614 "rgb_hash.gperf"
      {"grey61",               {156, 156, 156}},
      {""}, {""}, {""},
#line 274 "rgb_hash.gperf"
      {"LightCyan1",           {224, 255, 255}},
#line 679 "rgb_hash.gperf"
      {"gray94",               {240, 240, 240}},
      {""}, {""}, {""}, {""},
#line 659 "rgb_hash.gperf"
      {"gray84",               {214, 214, 214}},
      {""}, {""}, {""}, {""},
#line 677 "rgb_hash.gperf"
      {"gray93",               {237, 237, 237}},
      {""}, {""}, {""}, {""},
#line 657 "rgb_hash.gperf"
      {"gray83",               {212, 212, 212}},
      {""}, {""}, {""}, {""},
#line 639 "rgb_hash.gperf"
      {"gray74",               {189, 189, 189}},
      {""}, {""}, {""}, {""},
#line 619 "rgb_hash.gperf"
      {"gray64",               {163, 163, 163}},
      {""}, {""}, {""}, {""},
#line 637 "rgb_hash.gperf"
      {"gray73",               {186, 186, 186}},
      {""}, {""}, {""}, {""},
#line 617 "rgb_hash.gperf"
      {"gray63",               {161, 161, 161}},
      {""}, {""}, {""}, {""},
#line 675 "rgb_hash.gperf"
      {"gray92",               {235, 235, 235}},
      {""}, {""}, {""}, {""},
#line 655 "rgb_hash.gperf"
      {"gray82",               {209, 209, 209}},
      {""}, {""}, {""}, {""},
#line 673 "rgb_hash.gperf"
      {"gray91",               {232, 232, 232}},
      {""}, {""}, {""}, {""},
#line 653 "rgb_hash.gperf"
      {"gray81",               {207, 207, 207}},
      {""}, {""}, {""}, {""},
#line 635 "rgb_hash.gperf"
      {"gray72",               {184, 184, 184}},
      {""}, {""}, {""}, {""},
#line 615 "rgb_hash.gperf"
      {"gray62",               {158, 158, 158}},
      {""}, {""}, {""}, {""},
#line 633 "rgb_hash.gperf"
      {"gray71",               {181, 181, 181}},
      {""}, {""}, {""}, {""},
#line 613 "rgb_hash.gperf"
      {"gray61",               {156, 156, 156}},
      {""}, {""}, {""},
#line 510 "rgb_hash.gperf"
      {"grey9",                { 23,  23,  23}},
#line 590 "rgb_hash.gperf"
      {"grey49",               {125, 125, 125}},
      {""}, {""}, {""}, {""},
#line 570 "rgb_hash.gperf"
      {"grey39",               { 99,  99,  99}},
      {""}, {""}, {""},
#line 508 "rgb_hash.gperf"
      {"grey8",                { 20,  20,  20}},
#line 588 "rgb_hash.gperf"
      {"grey48",               {122, 122, 122}},
      {""},
#line 83 "rgb_hash.gperf"
      {"NavyBlue",             {  0,   0, 128}},
      {""}, {""},
#line 568 "rgb_hash.gperf"
      {"grey38",               { 97,  97,  97}},
#line 421 "rgb_hash.gperf"
      {"tomato4",              {139,  54,  38}},
      {""}, {""}, {""},
#line 550 "rgb_hash.gperf"
      {"grey29",               { 74,  74,  74}},
      {""}, {""}, {""}, {""},
#line 530 "rgb_hash.gperf"
      {"grey19",               { 48,  48,  48}},
#line 420 "rgb_hash.gperf"
      {"tomato3",              {205,  79,  57}},
      {""}, {""}, {""},
#line 548 "rgb_hash.gperf"
      {"grey28",               { 71,  71,  71}},
      {""}, {""}, {""}, {""},
#line 528 "rgb_hash.gperf"
      {"grey18",               { 46,  46,  46}},
      {""}, {""}, {""},
#line 506 "rgb_hash.gperf"
      {"grey7",                { 18,  18,  18}},
#line 586 "rgb_hash.gperf"
      {"grey47",               {120, 120, 120}},
      {""}, {""}, {""}, {""},
#line 566 "rgb_hash.gperf"
      {"grey37",               { 94,  94,  94}},
      {""}, {""}, {""},
#line 504 "rgb_hash.gperf"
      {"grey6",                { 15,  15,  15}},
#line 584 "rgb_hash.gperf"
      {"grey46",               {117, 117, 117}},
      {""}, {""}, {""}, {""},
#line 564 "rgb_hash.gperf"
      {"grey36",               { 92,  92,  92}},
#line 419 "rgb_hash.gperf"
      {"tomato2",              {238,  92,  66}},
      {""}, {""}, {""},
#line 546 "rgb_hash.gperf"
      {"grey27",               { 69,  69,  69}},
      {""}, {""}, {""}, {""},
#line 526 "rgb_hash.gperf"
      {"grey17",               { 43,  43,  43}},
#line 418 "rgb_hash.gperf"
      {"tomato1",              {255,  99,  71}},
      {""},
#line 385 "rgb_hash.gperf"
      {"tan4",                 {139,  90,  43}},
      {""},
#line 544 "rgb_hash.gperf"
      {"grey26",               { 66,  66,  66}},
      {""}, {""},
#line 384 "rgb_hash.gperf"
      {"tan3",                 {205, 133,  63}},
      {""},
#line 524 "rgb_hash.gperf"
      {"grey16",               { 41,  41,  41}},
      {""}, {""}, {""},
#line 509 "rgb_hash.gperf"
      {"gray9",                { 23,  23,  23}},
#line 589 "rgb_hash.gperf"
      {"gray49",               {125, 125, 125}},
      {""}, {""}, {""}, {""},
#line 569 "rgb_hash.gperf"
      {"gray39",               { 99,  99,  99}},
      {""}, {""},
#line 383 "rgb_hash.gperf"
      {"tan2",                 {238, 154,  73}},
#line 507 "rgb_hash.gperf"
      {"gray8",                { 20,  20,  20}},
#line 587 "rgb_hash.gperf"
      {"gray48",               {122, 122, 122}},
      {""}, {""},
#line 382 "rgb_hash.gperf"
      {"tan1",                 {255, 165,  79}},
      {""},
#line 567 "rgb_hash.gperf"
      {"gray38",               { 97,  97,  97}},
      {""}, {""},
#line 122 "rgb_hash.gperf"
      {"LimeGreen",            { 50, 205,  50}},
      {""},
#line 549 "rgb_hash.gperf"
      {"gray29",               { 74,  74,  74}},
      {""}, {""}, {""}, {""},
#line 529 "rgb_hash.gperf"
      {"gray19",               { 48,  48,  48}},
      {""}, {""},
#line 109 "rgb_hash.gperf"
      {"DarkGreen",            {  0, 100,   0}},
      {""},
#line 547 "rgb_hash.gperf"
      {"gray28",               { 71,  71,  71}},
      {""}, {""}, {""}, {""},
#line 527 "rgb_hash.gperf"
      {"gray18",               { 46,  46,  46}},
      {""}, {""}, {""},
#line 505 "rgb_hash.gperf"
      {"gray7",                { 18,  18,  18}},
#line 585 "rgb_hash.gperf"
      {"gray47",               {120, 120, 120}},
      {""}, {""}, {""}, {""},
#line 565 "rgb_hash.gperf"
      {"gray37",               { 94,  94,  94}},
      {""}, {""}, {""},
#line 503 "rgb_hash.gperf"
      {"gray6",                { 15,  15,  15}},
#line 583 "rgb_hash.gperf"
      {"gray46",               {117, 117, 117}},
      {""}, {""}, {""}, {""},
#line 563 "rgb_hash.gperf"
      {"gray36",               { 92,  92,  92}},
      {""}, {""}, {""}, {""},
#line 545 "rgb_hash.gperf"
      {"gray27",               { 69,  69,  69}},
      {""}, {""}, {""}, {""},
#line 525 "rgb_hash.gperf"
      {"gray17",               { 43,  43,  43}},
      {""}, {""}, {""}, {""},
#line 543 "rgb_hash.gperf"
      {"gray26",               { 66,  66,  66}},
      {""}, {""},
#line 309 "rgb_hash.gperf"
      {"SeaGreen4",            { 46, 139,  87}},
      {""},
#line 523 "rgb_hash.gperf"
      {"gray16",               { 41,  41,  41}},
      {""}, {""},
#line 308 "rgb_hash.gperf"
      {"SeaGreen3",            { 67, 205, 128}},
      {""},
#line 690 "rgb_hash.gperf"
      {"grey99",               {252, 252, 252}},
      {""}, {""},
#line 91 "rgb_hash.gperf"
      {"blue",                 {  0,   0, 255}},
#line 241 "rgb_hash.gperf"
      {"blue4",                {  0,   0, 139}},
#line 670 "rgb_hash.gperf"
      {"grey89",               {227, 227, 227}},
      {""}, {""}, {""}, {""},
#line 688 "rgb_hash.gperf"
      {"grey98",               {250, 250, 250}},
      {""}, {""},
#line 307 "rgb_hash.gperf"
      {"SeaGreen2",            { 78, 238, 148}},
#line 240 "rgb_hash.gperf"
      {"blue3",                {  0,   0, 205}},
#line 668 "rgb_hash.gperf"
      {"grey88",               {224, 224, 224}},
      {""}, {""},
#line 306 "rgb_hash.gperf"
      {"SeaGreen1",            { 84, 255, 159}},
#line 149 "rgb_hash.gperf"
      {"DarkSalmon",           {233, 150, 122}},
#line 650 "rgb_hash.gperf"
      {"grey79",               {201, 201, 201}},
      {""}, {""}, {""},
#line 68 "rgb_hash.gperf"
      {"black",                {  0,   0,   0}},
#line 630 "rgb_hash.gperf"
      {"grey69",               {176, 176, 176}},
      {""}, {""}, {""}, {""},
#line 648 "rgb_hash.gperf"
      {"grey78",               {199, 199, 199}},
      {""},
#line 305 "rgb_hash.gperf"
      {"DarkSeaGreen4",        {105, 139, 105}},
      {""}, {""},
#line 628 "rgb_hash.gperf"
      {"grey68",               {173, 173, 173}},
      {""}, {""}, {""}, {""},
#line 686 "rgb_hash.gperf"
      {"grey97",               {247, 247, 247}},
      {""},
#line 304 "rgb_hash.gperf"
      {"DarkSeaGreen3",        {155, 205, 155}},
      {""},
#line 239 "rgb_hash.gperf"
      {"blue2",                {  0,   0, 238}},
#line 666 "rgb_hash.gperf"
      {"grey87",               {222, 222, 222}},
      {""}, {""}, {""}, {""},
#line 684 "rgb_hash.gperf"
      {"grey96",               {245, 245, 245}},
      {""}, {""}, {""},
#line 238 "rgb_hash.gperf"
      {"blue1",                {  0,   0, 255}},
#line 664 "rgb_hash.gperf"
      {"grey86",               {219, 219, 219}},
      {""}, {""}, {""}, {""},
#line 646 "rgb_hash.gperf"
      {"grey77",               {196, 196, 196}},
      {""},
#line 177 "rgb_hash.gperf"
      {"thistle",              {216, 191, 216}},
#line 490 "rgb_hash.gperf"
      {"thistle4",             {139, 123, 139}},
      {""},
#line 626 "rgb_hash.gperf"
      {"grey67",               {171, 171, 171}},
      {""}, {""}, {""}, {""},
#line 644 "rgb_hash.gperf"
      {"grey76",               {194, 194, 194}},
      {""},
#line 303 "rgb_hash.gperf"
      {"DarkSeaGreen2",        {180, 238, 180}},
#line 489 "rgb_hash.gperf"
      {"thistle3",             {205, 181, 205}},
      {""},
#line 624 "rgb_hash.gperf"
      {"grey66",               {168, 168, 168}},
      {""}, {""}, {""}, {""},
#line 689 "rgb_hash.gperf"
      {"gray99",               {252, 252, 252}},
      {""},
#line 302 "rgb_hash.gperf"
      {"DarkSeaGreen1",        {193, 255, 193}},
      {""}, {""},
#line 669 "rgb_hash.gperf"
      {"gray89",               {227, 227, 227}},
      {""}, {""}, {""}, {""},
#line 687 "rgb_hash.gperf"
      {"gray98",               {250, 250, 250}},
      {""}, {""}, {""}, {""},
#line 667 "rgb_hash.gperf"
      {"gray88",               {224, 224, 224}},
      {""}, {""}, {""}, {""},
#line 649 "rgb_hash.gperf"
      {"gray79",               {201, 201, 201}},
      {""}, {""},
#line 488 "rgb_hash.gperf"
      {"thistle2",             {238, 210, 238}},
      {""},
#line 629 "rgb_hash.gperf"
      {"gray69",               {176, 176, 176}},
      {""}, {""}, {""},
#line 442 "rgb_hash.gperf"
      {"pink4",                {139,  99, 108}},
#line 647 "rgb_hash.gperf"
      {"gray78",               {199, 199, 199}},
      {""}, {""},
#line 487 "rgb_hash.gperf"
      {"thistle1",             {255, 225, 255}},
#line 181 "rgb_hash.gperf"
      {"snow4",                {139, 137, 137}},
#line 627 "rgb_hash.gperf"
      {"gray68",               {173, 173, 173}},
      {""}, {""}, {""},
#line 441 "rgb_hash.gperf"
      {"pink3",                {205, 145, 158}},
#line 685 "rgb_hash.gperf"
      {"gray97",               {247, 247, 247}},
      {""},
#line 100 "rgb_hash.gperf"
      {"PaleTurquoise",        {175, 238, 238}},
#line 281 "rgb_hash.gperf"
      {"PaleTurquoise4",       {102, 139, 139}},
#line 180 "rgb_hash.gperf"
      {"snow3",                {205, 201, 201}},
#line 665 "rgb_hash.gperf"
      {"gray87",               {222, 222, 222}},
      {""}, {""},
#line 280 "rgb_hash.gperf"
      {"PaleTurquoise3",       {150, 205, 205}},
      {""},
#line 683 "rgb_hash.gperf"
      {"gray96",               {245, 245, 245}},
      {""}, {""}, {""}, {""},
#line 663 "rgb_hash.gperf"
      {"gray86",               {219, 219, 219}},
      {""}, {""},
#line 161 "rgb_hash.gperf"
      {"pink",                 {255, 192, 203}},
      {""},
#line 645 "rgb_hash.gperf"
      {"gray77",               {196, 196, 196}},
      {""}, {""},
#line 279 "rgb_hash.gperf"
      {"PaleTurquoise2",       {174, 238, 238}},
      {""},
#line 625 "rgb_hash.gperf"
      {"gray67",               {171, 171, 171}},
      {""}, {""},
#line 278 "rgb_hash.gperf"
      {"PaleTurquoise1",       {187, 255, 255}},
#line 440 "rgb_hash.gperf"
      {"pink2",                {238, 169, 184}},
#line 643 "rgb_hash.gperf"
      {"gray76",               {194, 194, 194}},
      {""}, {""}, {""},
#line 179 "rgb_hash.gperf"
      {"snow2",                {238, 233, 233}},
#line 623 "rgb_hash.gperf"
      {"gray66",               {168, 168, 168}},
      {""}, {""}, {""},
#line 439 "rgb_hash.gperf"
      {"pink1",                {255, 181, 197}},
#line 600 "rgb_hash.gperf"
      {"grey54",               {138, 138, 138}},
      {""}, {""}, {""},
#line 178 "rgb_hash.gperf"
      {"snow1",                {255, 250, 250}},
#line 321 "rgb_hash.gperf"
      {"green4",               {  0, 139,   0}},
#line 337 "rgb_hash.gperf"
      {"khaki4",               {139, 134,  78}},
      {""}, {""}, {""},
#line 598 "rgb_hash.gperf"
      {"grey53",               {135, 135, 135}},
      {""}, {""}, {""}, {""},
#line 320 "rgb_hash.gperf"
      {"green3",               {  0, 205,   0}},
#line 336 "rgb_hash.gperf"
      {"khaki3",               {205, 198, 115}},
      {""}, {""}, {""},
#line 417 "rgb_hash.gperf"
      {"coral4",               {139,  62,  47}},
      {""}, {""}, {""},
#line 154 "rgb_hash.gperf"
      {"coral",                {255, 127,  80}},
#line 152 "rgb_hash.gperf"
      {"orange",               {255, 165,   0}},
#line 409 "rgb_hash.gperf"
      {"orange4",              {139,  90,   0}},
      {""}, {""},
#line 699 "rgb_hash.gperf"
      {"LightGreen",           {144, 238, 144}},
#line 416 "rgb_hash.gperf"
      {"coral3",               {205,  91,  69}},
#line 317 "rgb_hash.gperf"
      {"SpringGreen4",         {  0, 139,  69}},
      {""}, {""}, {""}, {""},
#line 408 "rgb_hash.gperf"
      {"orange3",              {205, 133,   0}},
      {""}, {""},
#line 458 "rgb_hash.gperf"
      {"VioletRed4",           {139,  34,  82}},
#line 596 "rgb_hash.gperf"
      {"grey52",               {133, 133, 133}},
#line 316 "rgb_hash.gperf"
      {"SpringGreen3",         {  0, 205, 102}},
      {""}, {""},
#line 457 "rgb_hash.gperf"
      {"VioletRed3",           {205,  50, 120}},
#line 319 "rgb_hash.gperf"
      {"green2",               {  0, 238,   0}},
#line 335 "rgb_hash.gperf"
      {"khaki2",               {238, 230, 133}},
      {""}, {""},
#line 63 "rgb_hash.gperf"
      {"AliceBlue",            {240, 248, 255}},
#line 594 "rgb_hash.gperf"
      {"grey51",               {130, 130, 130}},
      {""}, {""}, {""}, {""},
#line 318 "rgb_hash.gperf"
      {"green1",               {  0, 255,   0}},
#line 334 "rgb_hash.gperf"
      {"khaki1",               {255, 246, 143}},
      {""}, {""},
#line 456 "rgb_hash.gperf"
      {"VioletRed2",           {238,  58, 140}},
#line 415 "rgb_hash.gperf"
      {"coral2",               {238, 106,  80}},
      {""}, {""}, {""},
#line 455 "rgb_hash.gperf"
      {"VioletRed1",           {255,  62, 150}},
      {""},
#line 407 "rgb_hash.gperf"
      {"orange2",              {238, 154,   0}},
      {""}, {""}, {""},
#line 414 "rgb_hash.gperf"
      {"coral1",               {255, 114,  86}},
#line 315 "rgb_hash.gperf"
      {"SpringGreen2",         {  0, 238, 118}},
      {""}, {""},
#line 143 "rgb_hash.gperf"
      {"wheat",                {245, 222, 179}},
#line 381 "rgb_hash.gperf"
      {"wheat4",               {139, 126, 102}},
#line 406 "rgb_hash.gperf"
      {"orange1",              {255, 165,   0}},
      {""}, {""}, {""},
#line 599 "rgb_hash.gperf"
      {"gray54",               {138, 138, 138}},
#line 314 "rgb_hash.gperf"
      {"SpringGreen1",         {  0, 255, 127}},
      {""}, {""},
#line 155 "rgb_hash.gperf"
      {"LightCoral",           {240, 128, 128}},
#line 380 "rgb_hash.gperf"
      {"wheat3",               {205, 186, 150}},
      {""}, {""}, {""},
#line 50 "rgb_hash.gperf"
      {"PapayaWhip",           {255, 239, 213}},
#line 597 "rgb_hash.gperf"
      {"gray53",               {135, 135, 135}},
      {""},
#line 462 "rgb_hash.gperf"
      {"magenta4",             {139,   0, 139}},
      {""},
#line 293 "rgb_hash.gperf"
      {"cyan4",                {  0, 139, 139}},
#line 151 "rgb_hash.gperf"
      {"LightSalmon",          {255, 160, 122}},
#line 373 "rgb_hash.gperf"
      {"sienna4",              {139,  71,  38}},
      {""},
#line 429 "rgb_hash.gperf"
      {"red4",                 {139,   0,   0}},
      {""},
#line 697 "rgb_hash.gperf"
      {"DarkMagenta",          {139,   0, 139}},
      {""},
#line 461 "rgb_hash.gperf"
      {"magenta3",             {205,   0, 205}},
#line 428 "rgb_hash.gperf"
      {"red3",                 {205,   0,   0}},
#line 292 "rgb_hash.gperf"
      {"cyan3",                {  0, 205, 205}},
#line 46 "rgb_hash.gperf"
      {"FloralWhite",          {255, 250, 240}},
#line 372 "rgb_hash.gperf"
      {"sienna3",              {205, 104,  57}},
      {""}, {""},
#line 126 "rgb_hash.gperf"
      {"DarkKhaki",            {189, 183, 107}},
#line 52 "rgb_hash.gperf"
      {"bisque",               {255, 228, 196}},
#line 193 "rgb_hash.gperf"
      {"bisque4",              {139, 125, 107}},
      {""}, {""}, {""},
#line 379 "rgb_hash.gperf"
      {"wheat2",               {238, 216, 174}},
      {""}, {""},
#line 427 "rgb_hash.gperf"
      {"red2",                 {238,   0,   0}},
      {""},
#line 595 "rgb_hash.gperf"
      {"gray52",               {133, 133, 133}},
#line 192 "rgb_hash.gperf"
      {"bisque3",              {205, 183, 158}},
      {""},
#line 426 "rgb_hash.gperf"
      {"red1",                 {255,   0,   0}},
      {""},
#line 378 "rgb_hash.gperf"
      {"wheat1",               {255, 231, 186}},
      {""}, {""}, {""}, {""},
#line 593 "rgb_hash.gperf"
      {"gray51",               {130, 130, 130}},
      {""},
#line 460 "rgb_hash.gperf"
      {"magenta2",             {238,   0, 238}},
      {""},
#line 291 "rgb_hash.gperf"
      {"cyan2",                {  0, 238, 238}},
#line 168 "rgb_hash.gperf"
      {"violet",               {238, 130, 238}},
#line 371 "rgb_hash.gperf"
      {"sienna2",              {238, 121,  66}},
      {""}, {""},
#line 92 "rgb_hash.gperf"
      {"DodgerBlue",           { 30, 144, 255}},
#line 245 "rgb_hash.gperf"
      {"DodgerBlue4",          { 16,  78, 139}},
      {""},
#line 459 "rgb_hash.gperf"
      {"magenta1",             {255,   0, 255}},
      {""},
#line 290 "rgb_hash.gperf"
      {"cyan1",                {  0, 255, 255}},
#line 244 "rgb_hash.gperf"
      {"DodgerBlue3",          { 24, 116, 205}},
#line 370 "rgb_hash.gperf"
      {"sienna1",              {255, 130,  71}},
      {""}, {""}, {""}, {""},
#line 191 "rgb_hash.gperf"
      {"bisque2",              {238, 213, 183}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 243 "rgb_hash.gperf"
      {"DodgerBlue2",          { 28, 134, 238}},
#line 190 "rgb_hash.gperf"
      {"bisque1",              {255, 228, 196}},
      {""}, {""}, {""},
#line 242 "rgb_hash.gperf"
      {"DodgerBlue1",          { 30, 144, 255}},
      {""}, {""}, {""}, {""}, {""},
#line 167 "rgb_hash.gperf"
      {"magenta",              {255,   0, 255}},
      {""}, {""}, {""},
#line 139 "rgb_hash.gperf"
      {"sienna",               {160,  82,  45}},
      {""}, {""},
#line 115 "rgb_hash.gperf"
      {"PaleGreen",            {152, 251, 152}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 108 "rgb_hash.gperf"
      {"aquamarine",           {127, 255, 212}},
#line 301 "rgb_hash.gperf"
      {"aquamarine4",          { 69, 139, 116}},
      {""}, {""}, {""},
#line 425 "rgb_hash.gperf"
      {"OrangeRed4",           {139,  37,   0}},
#line 300 "rgb_hash.gperf"
      {"aquamarine3",          {102, 205, 170}},
      {""},
#line 82 "rgb_hash.gperf"
      {"navy",                 {  0,   0, 128}},
      {""},
#line 424 "rgb_hash.gperf"
      {"OrangeRed3",           {205,  55,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 197 "rgb_hash.gperf"
      {"PeachPuff4",           {139, 119, 101}},
      {""},
#line 299 "rgb_hash.gperf"
      {"aquamarine2",          {118, 238, 198}},
      {""}, {""},
#line 196 "rgb_hash.gperf"
      {"PeachPuff3",           {205, 175, 149}},
#line 423 "rgb_hash.gperf"
      {"OrangeRed2",           {238,  64,   0}},
#line 298 "rgb_hash.gperf"
      {"aquamarine1",          {127, 255, 212}},
      {""},
#line 53 "rgb_hash.gperf"
      {"PeachPuff",            {255, 218, 185}},
      {""},
#line 422 "rgb_hash.gperf"
      {"OrangeRed1",           {255,  69,   0}},
      {""}, {""},
#line 105 "rgb_hash.gperf"
      {"LightCyan",            {224, 255, 255}},
#line 119 "rgb_hash.gperf"
      {"chartreuse",           {127, 255,   0}},
#line 325 "rgb_hash.gperf"
      {"chartreuse4",          { 69, 139,   0}},
      {""}, {""}, {""},
#line 195 "rgb_hash.gperf"
      {"PeachPuff2",           {238, 203, 173}},
#line 324 "rgb_hash.gperf"
      {"chartreuse3",          {102, 205,   0}},
      {""}, {""}, {""},
#line 194 "rgb_hash.gperf"
      {"PeachPuff1",           {255, 218, 185}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 323 "rgb_hash.gperf"
      {"chartreuse2",          {118, 238,   0}},
      {""}, {""}, {""}, {""},
#line 322 "rgb_hash.gperf"
      {"chartreuse1",          {127, 255,   0}},
      {""}, {""}, {""}, {""},
#line 54 "rgb_hash.gperf"
      {"NavajoWhite",          {255, 222, 173}},
#line 201 "rgb_hash.gperf"
      {"NavajoWhite4",         {139, 121,  94}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 200 "rgb_hash.gperf"
      {"NavajoWhite3",         {205, 179, 139}},
      {""}, {""}, {""},
#line 124 "rgb_hash.gperf"
      {"ForestGreen",          { 34, 139,  34}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 156 "rgb_hash.gperf"
      {"tomato",               {255,  99,  71}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 199 "rgb_hash.gperf"
      {"NavajoWhite2",         {238, 207, 161}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 198 "rgb_hash.gperf"
      {"NavajoWhite1",         {255, 222, 173}},
      {""}, {""}, {""},
#line 153 "rgb_hash.gperf"
      {"DarkOrange",           {255, 140,   0}},
#line 413 "rgb_hash.gperf"
      {"DarkOrange4",          {139,  69,   0}},
      {""}, {""}, {""}, {""},
#line 412 "rgb_hash.gperf"
      {"DarkOrange3",          {205, 102,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 610 "rgb_hash.gperf"
      {"grey59",               {150, 150, 150}},
      {""}, {""}, {""}, {""}, {""},
#line 411 "rgb_hash.gperf"
      {"DarkOrange2",          {238, 118,   0}},
      {""}, {""}, {""},
#line 608 "rgb_hash.gperf"
      {"grey58",               {148, 148, 148}},
#line 410 "rgb_hash.gperf"
      {"DarkOrange1",          {255, 127,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 130 "rgb_hash.gperf"
      {"LightYellow",          {255, 255, 224}},
#line 454 "rgb_hash.gperf"
      {"maroon4",              {139,  28,  98}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 453 "rgb_hash.gperf"
      {"maroon3",              {205,  41, 144}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 132 "rgb_hash.gperf"
      {"gold",                 {255, 215,   0}},
      {""},
#line 606 "rgb_hash.gperf"
      {"grey57",               {145, 145, 145}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 604 "rgb_hash.gperf"
      {"grey56",               {143, 143, 143}},
      {""}, {""},
#line 106 "rgb_hash.gperf"
      {"CadetBlue",            { 95, 158, 160}},
#line 285 "rgb_hash.gperf"
      {"CadetBlue4",           { 83, 134, 139}},
      {""}, {""}, {""}, {""},
#line 284 "rgb_hash.gperf"
      {"CadetBlue3",           {122, 197, 205}},
      {""},
#line 452 "rgb_hash.gperf"
      {"maroon2",              {238,  48, 167}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 451 "rgb_hash.gperf"
      {"maroon1",              {255,  52, 179}},
#line 145 "rgb_hash.gperf"
      {"tan",                  {210, 180, 140}},
      {""},
#line 283 "rgb_hash.gperf"
      {"CadetBlue2",           {142, 229, 238}},
      {""}, {""}, {""}, {""},
#line 282 "rgb_hash.gperf"
      {"CadetBlue1",           {152, 245, 255}},
#line 609 "rgb_hash.gperf"
      {"gray59",               {150, 150, 150}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 607 "rgb_hash.gperf"
      {"gray58",               {148, 148, 148}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 605 "rgb_hash.gperf"
      {"gray57",               {145, 145, 145}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 603 "rgb_hash.gperf"
      {"gray56",               {143, 143, 143}},
#line 329 "rgb_hash.gperf"
      {"OliveDrab4",           {105, 139,  34}},
      {""}, {""}, {""}, {""},
#line 328 "rgb_hash.gperf"
      {"OliveDrab3",           {154, 205,  50}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 492 "rgb_hash.gperf"
      {"grey0",                {  0,   0,   0}},
#line 572 "rgb_hash.gperf"
      {"grey40",               {102, 102, 102}},
#line 327 "rgb_hash.gperf"
      {"OliveDrab2",           {179, 238,  58}},
      {""}, {""}, {""},
#line 552 "rgb_hash.gperf"
      {"grey30",               { 77,  77,  77}},
#line 326 "rgb_hash.gperf"
      {"OliveDrab1",           {192, 255,  62}},
#line 112 "rgb_hash.gperf"
      {"SeaGreen",             { 46, 139,  87}},
      {""}, {""},
#line 57 "rgb_hash.gperf"
      {"ivory",                {255, 255, 240}},
#line 213 "rgb_hash.gperf"
      {"ivory4",               {139, 139, 131}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 532 "rgb_hash.gperf"
      {"grey20",               { 51,  51,  51}},
#line 212 "rgb_hash.gperf"
      {"ivory3",               {205, 205, 193}},
      {""}, {""}, {""},
#line 512 "rgb_hash.gperf"
      {"grey10",               { 26,  26,  26}},
      {""}, {""}, {""},
#line 99 "rgb_hash.gperf"
      {"PowderBlue",           {176, 224, 230}},
      {""}, {""}, {""}, {""}, {""},
#line 44 "rgb_hash.gperf"
      {"WhiteSmoke",           {245, 245, 245}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 142 "rgb_hash.gperf"
      {"beige",                {245, 245, 220}},
#line 111 "rgb_hash.gperf"
      {"DarkSeaGreen",         {143, 188, 143}},
      {""}, {""}, {""}, {""},
#line 211 "rgb_hash.gperf"
      {"ivory2",               {238, 238, 224}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 61 "rgb_hash.gperf"
      {"MintCream",            {245, 255, 250}},
      {""}, {""},
#line 210 "rgb_hash.gperf"
      {"ivory1",               {255, 255, 240}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 491 "rgb_hash.gperf"
      {"gray0",                {  0,   0,   0}},
#line 571 "rgb_hash.gperf"
      {"gray40",               {102, 102, 102}},
      {""}, {""}, {""}, {""},
#line 551 "rgb_hash.gperf"
      {"gray30",               { 77,  77,  77}},
      {""}, {""}, {""}, {""},
#line 173 "rgb_hash.gperf"
      {"DarkViolet",           {148,   0, 211}},
      {""}, {""}, {""},
#line 341 "rgb_hash.gperf"
      {"LightGoldenrod4",      {139, 129,  76}},
      {""}, {""}, {""}, {""},
#line 340 "rgb_hash.gperf"
      {"LightGoldenrod3",      {205, 190, 112}},
#line 531 "rgb_hash.gperf"
      {"gray20",               { 51,  51,  51}},
      {""}, {""},
#line 438 "rgb_hash.gperf"
      {"HotPink4",             {139,  58,  98}},
      {""},
#line 511 "rgb_hash.gperf"
      {"gray10",               { 26,  26,  26}},
#line 478 "rgb_hash.gperf"
      {"DarkOrchid4",          {104,  34, 139}},
      {""}, {""}, {""}, {""},
#line 477 "rgb_hash.gperf"
      {"DarkOrchid3",          {154,  50, 205}},
      {""},
#line 437 "rgb_hash.gperf"
      {"HotPink3",             {205,  96, 144}},
#line 339 "rgb_hash.gperf"
      {"LightGoldenrod2",      {238, 220, 130}},
      {""}, {""}, {""}, {""},
#line 338 "rgb_hash.gperf"
      {"LightGoldenrod1",      {255, 236, 139}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 476 "rgb_hash.gperf"
      {"DarkOrchid2",          {178,  58, 238}},
      {""}, {""},
#line 357 "rgb_hash.gperf"
      {"goldenrod4",           {139, 105,  20}},
      {""},
#line 475 "rgb_hash.gperf"
      {"DarkOrchid1",          {191,  62, 255}},
#line 159 "rgb_hash.gperf"
      {"HotPink",              {255, 105, 180}},
      {""},
#line 356 "rgb_hash.gperf"
      {"goldenrod3",           {205, 155,  29}},
#line 43 "rgb_hash.gperf"
      {"GhostWhite",           {248, 248, 255}},
#line 401 "rgb_hash.gperf"
      {"salmon4",              {139,  76,  57}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 436 "rgb_hash.gperf"
      {"HotPink2",             {238, 106, 167}},
      {""}, {""},
#line 400 "rgb_hash.gperf"
      {"salmon3",              {205, 112,  84}},
      {""}, {""},
#line 355 "rgb_hash.gperf"
      {"goldenrod2",           {238, 180,  34}},
      {""}, {""},
#line 696 "rgb_hash.gperf"
      {"DarkCyan",             {  0, 139, 139}},
#line 435 "rgb_hash.gperf"
      {"HotPink1",             {255, 110, 180}},
#line 354 "rgb_hash.gperf"
      {"goldenrod1",           {255, 193,  37}},
      {""}, {""}, {""}, {""}, {""},
#line 672 "rgb_hash.gperf"
      {"grey90",               {229, 229, 229}},
      {""}, {""}, {""}, {""},
#line 652 "rgb_hash.gperf"
      {"grey80",               {204, 204, 204}},
      {""}, {""}, {""},
#line 502 "rgb_hash.gperf"
      {"grey5",                { 13,  13,  13}},
#line 582 "rgb_hash.gperf"
      {"grey45",               {115, 115, 115}},
      {""}, {""}, {""}, {""},
#line 562 "rgb_hash.gperf"
      {"grey35",               { 89,  89,  89}},
#line 399 "rgb_hash.gperf"
      {"salmon2",              {238, 130,  98}},
      {""}, {""},
#line 118 "rgb_hash.gperf"
      {"green",                {  0, 255,   0}},
#line 632 "rgb_hash.gperf"
      {"grey70",               {179, 179, 179}},
      {""}, {""}, {""}, {""},
#line 612 "rgb_hash.gperf"
      {"grey60",               {153, 153, 153}},
#line 398 "rgb_hash.gperf"
      {"salmon1",              {255, 140, 105}},
      {""}, {""}, {""},
#line 542 "rgb_hash.gperf"
      {"grey25",               { 64,  64,  64}},
      {""}, {""}, {""}, {""},
#line 522 "rgb_hash.gperf"
      {"grey15",               { 38,  38,  38}},
#line 349 "rgb_hash.gperf"
      {"yellow4",              {139, 139,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 116 "rgb_hash.gperf"
      {"SpringGreen",          {  0, 255, 127}},
#line 348 "rgb_hash.gperf"
      {"yellow3",              {205, 205,   0}},
      {""}, {""}, {""},
#line 174 "rgb_hash.gperf"
      {"BlueViolet",           {138,  43, 226}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 347 "rgb_hash.gperf"
      {"yellow2",              {238, 238,   0}},
      {""}, {""}, {""},
#line 671 "rgb_hash.gperf"
      {"gray90",               {229, 229, 229}},
      {""}, {""}, {""}, {""},
#line 651 "rgb_hash.gperf"
      {"gray80",               {204, 204, 204}},
#line 346 "rgb_hash.gperf"
      {"yellow1",              {255, 255,   0}},
      {""}, {""},
#line 501 "rgb_hash.gperf"
      {"gray5",                { 13,  13,  13}},
#line 581 "rgb_hash.gperf"
      {"gray45",               {115, 115, 115}},
      {""}, {""}, {""}, {""},
#line 561 "rgb_hash.gperf"
      {"gray35",               { 89,  89,  89}},
      {""}, {""}, {""}, {""},
#line 631 "rgb_hash.gperf"
      {"gray70",               {179, 179, 179}},
      {""}, {""}, {""}, {""},
#line 611 "rgb_hash.gperf"
      {"gray60",               {153, 153, 153}},
      {""},
#line 114 "rgb_hash.gperf"
      {"LightSeaGreen",        { 32, 178, 170}},
#line 104 "rgb_hash.gperf"
      {"cyan",                 {  0, 255, 255}},
      {""},
#line 541 "rgb_hash.gperf"
      {"gray25",               { 64,  64,  64}},
      {""}, {""},
#line 117 "rgb_hash.gperf"
      {"LawnGreen",            {124, 252,   0}},
      {""},
#line 521 "rgb_hash.gperf"
      {"gray15",               { 38,  38,  38}},
      {""}, {""}, {""}, {""},
#line 121 "rgb_hash.gperf"
      {"GreenYellow",          {173, 255,  47}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 138 "rgb_hash.gperf"
      {"SaddleBrown",          {139,  69,  19}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 127 "rgb_hash.gperf"
      {"khaki",                {240, 230, 140}},
      {""}, {""}, {""},
#line 682 "rgb_hash.gperf"
      {"grey95",               {242, 242, 242}},
      {""}, {""}, {""}, {""},
#line 662 "rgb_hash.gperf"
      {"grey85",               {217, 217, 217}},
      {""}, {""}, {""},
#line 89 "rgb_hash.gperf"
      {"MediumBlue",           {  0,   0, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 87 "rgb_hash.gperf"
      {"MediumSlateBlue",      {123, 104, 238}},
#line 642 "rgb_hash.gperf"
      {"grey75",               {191, 191, 191}},
      {""}, {""}, {""}, {""},
#line 622 "rgb_hash.gperf"
      {"grey65",               {166, 166, 166}},
#line 333 "rgb_hash.gperf"
      {"DarkOliveGreen4",      {110, 139,  61}},
      {""}, {""}, {""}, {""},
#line 332 "rgb_hash.gperf"
      {"DarkOliveGreen3",      {162, 205,  90}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 331 "rgb_hash.gperf"
      {"DarkOliveGreen2",      {188, 238, 104}},
      {""}, {""}, {""}, {""},
#line 330 "rgb_hash.gperf"
      {"DarkOliveGreen1",      {202, 255, 112}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 67 "rgb_hash.gperf"
      {"white",                {255, 255, 255}},
      {""}, {""}, {""},
#line 103 "rgb_hash.gperf"
      {"turquoise",            { 64, 224, 208}},
#line 289 "rgb_hash.gperf"
      {"turquoise4",           {  0, 134, 139}},
      {""}, {""}, {""}, {""},
#line 288 "rgb_hash.gperf"
      {"turquoise3",           {  0, 197, 205}},
      {""}, {""}, {""}, {""},
#line 681 "rgb_hash.gperf"
      {"gray95",               {242, 242, 242}},
      {""}, {""}, {""}, {""},
#line 661 "rgb_hash.gperf"
      {"gray85",               {217, 217, 217}},
      {""}, {""}, {""}, {""},
#line 287 "rgb_hash.gperf"
      {"turquoise2",           {  0, 229, 238}},
      {""}, {""}, {""}, {""},
#line 286 "rgb_hash.gperf"
      {"turquoise1",           {  0, 245, 255}},
      {""}, {""}, {""}, {""},
#line 641 "rgb_hash.gperf"
      {"gray75",               {191, 191, 191}},
      {""}, {""}, {""}, {""},
#line 621 "rgb_hash.gperf"
      {"gray65",               {166, 166, 166}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 42 "rgb_hash.gperf"
      {"snow",                 {255, 250, 250}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 84 "rgb_hash.gperf"
      {"CornflowerBlue",       {100, 149, 237}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 107 "rgb_hash.gperf"
      {"MediumAquamarine",     {102, 205, 170}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 397 "rgb_hash.gperf"
      {"brown4",               {139,  35,  35}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 396 "rgb_hash.gperf"
      {"brown3",               {205,  51,  51}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 164 "rgb_hash.gperf"
      {"maroon",               {176,  48,  96}},
      {""}, {""}, {""},
#line 434 "rgb_hash.gperf"
      {"DeepPink4",            {139,  10,  80}},
      {""}, {""}, {""}, {""},
#line 433 "rgb_hash.gperf"
      {"DeepPink3",            {205,  16, 118}},
#line 395 "rgb_hash.gperf"
      {"brown2",               {238,  59,  59}},
      {""}, {""}, {""},
#line 144 "rgb_hash.gperf"
      {"SandyBrown",           {244, 164,  96}},
      {""}, {""}, {""}, {""}, {""},
#line 394 "rgb_hash.gperf"
      {"brown1",               {255,  64,  64}},
      {""}, {""}, {""},
#line 432 "rgb_hash.gperf"
      {"DeepPink2",            {238,  18, 137}},
      {""}, {""}, {""}, {""},
#line 431 "rgb_hash.gperf"
      {"DeepPink1",            {255,  20, 147}},
      {""}, {""}, {""},
#line 160 "rgb_hash.gperf"
      {"DeepPink",             {255,  20, 147}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 166 "rgb_hash.gperf"
      {"VioletRed",            {208,  32, 144}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 48 "rgb_hash.gperf"
      {"linen",                {250, 240, 230}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 692 "rgb_hash.gperf"
      {"grey100",              {255, 255, 255}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 125 "rgb_hash.gperf"
      {"OliveDrab",            {107, 142,  35}},
      {""}, {""},
#line 66 "rgb_hash.gperf"
      {"MistyRose",            {255, 228, 225}},
#line 225 "rgb_hash.gperf"
      {"MistyRose4",           {139, 125, 123}},
      {""}, {""}, {""}, {""},
#line 224 "rgb_hash.gperf"
      {"MistyRose3",           {205, 183, 181}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 158 "rgb_hash.gperf"
      {"red",                  {255,   0,   0}},
      {""},
#line 223 "rgb_hash.gperf"
      {"MistyRose2",           {238, 213, 210}},
      {""}, {""}, {""}, {""},
#line 222 "rgb_hash.gperf"
      {"MistyRose1",           {255, 228, 225}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 209 "rgb_hash.gperf"
      {"cornsilk4",            {139, 136, 120}},
#line 592 "rgb_hash.gperf"
      {"grey50",               {127, 127, 127}},
      {""}, {""}, {""},
#line 208 "rgb_hash.gperf"
      {"cornsilk3",            {205, 200, 177}},
      {""}, {""}, {""}, {""},
#line 369 "rgb_hash.gperf"
      {"IndianRed4",           {139,  58,  58}},
      {""}, {""}, {""}, {""},
#line 368 "rgb_hash.gperf"
      {"IndianRed3",           {205,  85,  85}},
      {""}, {""}, {""}, {""},
#line 207 "rgb_hash.gperf"
      {"cornsilk2",            {238, 232, 205}},
      {""},
#line 691 "rgb_hash.gperf"
      {"gray100",              {255, 255, 255}},
      {""}, {""},
#line 206 "rgb_hash.gperf"
      {"cornsilk1",            {255, 248, 220}},
      {""}, {""}, {""},
#line 56 "rgb_hash.gperf"
      {"cornsilk",             {255, 248, 220}},
#line 367 "rgb_hash.gperf"
      {"IndianRed2",           {238,  99,  99}},
      {""}, {""}, {""}, {""},
#line 366 "rgb_hash.gperf"
      {"IndianRed1",           {255, 106, 106}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 157 "rgb_hash.gperf"
      {"OrangeRed",            {255,  69,   0}},
      {""}, {""}, {""}, {""}, {""},
#line 591 "rgb_hash.gperf"
      {"gray50",               {127, 127, 127}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 64 "rgb_hash.gperf"
      {"lavender",             {230, 230, 250}},
      {""}, {""},
#line 446 "rgb_hash.gperf"
      {"LightPink4",           {139,  95, 101}},
      {""}, {""}, {""}, {""},
#line 445 "rgb_hash.gperf"
      {"LightPink3",           {205, 140, 149}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 444 "rgb_hash.gperf"
      {"LightPink2",           {238, 162, 173}},
      {""}, {""}, {""}, {""},
#line 443 "rgb_hash.gperf"
      {"LightPink1",           {255, 174, 185}},
      {""}, {""}, {""},
#line 162 "rgb_hash.gperf"
      {"LightPink",            {255, 182, 193}},
      {""}, {""}, {""}, {""},
#line 365 "rgb_hash.gperf"
      {"RosyBrown4",           {139, 105, 105}},
      {""}, {""}, {""}, {""},
#line 364 "rgb_hash.gperf"
      {"RosyBrown3",           {205, 155, 155}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 150 "rgb_hash.gperf"
      {"salmon",               {250, 128, 114}},
      {""}, {""}, {""},
#line 363 "rgb_hash.gperf"
      {"RosyBrown2",           {238, 180, 180}},
      {""}, {""}, {""}, {""},
#line 362 "rgb_hash.gperf"
      {"RosyBrown1",           {255, 193, 193}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 146 "rgb_hash.gperf"
      {"chocolate",            {210, 105,  30}},
#line 389 "rgb_hash.gperf"
      {"chocolate4",           {139,  69,  19}},
      {""}, {""}, {""}, {""},
#line 388 "rgb_hash.gperf"
      {"chocolate3",           {205, 102,  29}},
#line 602 "rgb_hash.gperf"
      {"grey55",               {140, 140, 140}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 387 "rgb_hash.gperf"
      {"chocolate2",           {238, 118,  33}},
      {""}, {""}, {""}, {""},
#line 386 "rgb_hash.gperf"
      {"chocolate1",           {255, 127,  36}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 176 "rgb_hash.gperf"
      {"MediumPurple",         {147, 112, 219}},
#line 486 "rgb_hash.gperf"
      {"MediumPurple4",        { 93,  71, 139}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 485 "rgb_hash.gperf"
      {"MediumPurple3",        {137, 104, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 123 "rgb_hash.gperf"
      {"YellowGreen",          {154, 205,  50}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 484 "rgb_hash.gperf"
      {"MediumPurple2",        {159, 121, 238}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 601 "rgb_hash.gperf"
      {"gray55",               {140, 140, 140}},
      {""},
#line 483 "rgb_hash.gperf"
      {"MediumPurple1",        {171, 130, 255}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 698 "rgb_hash.gperf"
      {"DarkRed",              {139,   0,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 205 "rgb_hash.gperf"
      {"LemonChiffon4",        {139, 137, 112}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 204 "rgb_hash.gperf"
      {"LemonChiffon3",        {205, 201, 165}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 113 "rgb_hash.gperf"
      {"MediumSeaGreen",       { 60, 179, 113}},
      {""},
#line 393 "rgb_hash.gperf"
      {"firebrick4",           {139,  26,  26}},
      {""}, {""}, {""}, {""},
#line 392 "rgb_hash.gperf"
      {"firebrick3",           {205,  38,  38}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 203 "rgb_hash.gperf"
      {"LemonChiffon2",        {238, 233, 191}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 391 "rgb_hash.gperf"
      {"firebrick2",           {238,  44,  44}},
      {""}, {""},
#line 202 "rgb_hash.gperf"
      {"LemonChiffon1",        {255, 250, 205}},
      {""},
#line 390 "rgb_hash.gperf"
      {"firebrick1",           {255,  48,  48}},
      {""}, {""}, {""},
#line 147 "rgb_hash.gperf"
      {"firebrick",            {178,  34,  34}},
#line 110 "rgb_hash.gperf"
      {"DarkOliveGreen",       { 85, 107,  47}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 129 "rgb_hash.gperf"
      {"LightGoldenrodYellow", {250, 250, 210}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 47 "rgb_hash.gperf"
      {"OldLace",              {253, 245, 230}},
#line 221 "rgb_hash.gperf"
      {"LavenderBlush4",       {139, 131, 134}},
      {""}, {""}, {""}, {""},
#line 220 "rgb_hash.gperf"
      {"LavenderBlush3",       {205, 193, 197}},
      {""}, {""}, {""}, {""},
#line 133 "rgb_hash.gperf"
      {"LightGoldenrod",       {238, 221, 130}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 219 "rgb_hash.gperf"
      {"LavenderBlush2",       {238, 224, 229}},
      {""},
#line 172 "rgb_hash.gperf"
      {"DarkOrchid",           {153,  50, 204}},
      {""}, {""},
#line 218 "rgb_hash.gperf"
      {"LavenderBlush1",       {255, 240, 245}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 134 "rgb_hash.gperf"
      {"goldenrod",            {218, 165,  32}},
      {""}, {""}, {""}, {""}, {""},
#line 148 "rgb_hash.gperf"
      {"brown",                {165,  42,  42}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 65 "rgb_hash.gperf"
      {"LavenderBlush",        {255, 240, 245}},
      {""}, {""},
#line 131 "rgb_hash.gperf"
      {"yellow",               {255, 255,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 430 "rgb_hash.gperf"
      {"DebianRed",            {215,   7,  81}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 81 "rgb_hash.gperf"
      {"MidnightBlue",         { 25,  25, 112}},
      {""}, {""}, {""}, {""}, {""},
#line 217 "rgb_hash.gperf"
      {"honeydew4",            {131, 139, 131}},
      {""}, {""}, {""}, {""},
#line 216 "rgb_hash.gperf"
      {"honeydew3",            {193, 205, 193}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 215 "rgb_hash.gperf"
      {"honeydew2",            {224, 238, 224}},
      {""}, {""}, {""}, {""},
#line 214 "rgb_hash.gperf"
      {"honeydew1",            {240, 255, 240}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 377 "rgb_hash.gperf"
      {"burlywood4",           {139, 115,  85}},
      {""}, {""}, {""}, {""},
#line 376 "rgb_hash.gperf"
      {"burlywood3",           {205, 170, 125}},
      {""}, {""}, {""},
#line 136 "rgb_hash.gperf"
      {"RosyBrown",            {188, 143, 143}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 375 "rgb_hash.gperf"
      {"burlywood2",           {238, 197, 145}},
      {""}, {""}, {""}, {""},
#line 374 "rgb_hash.gperf"
      {"burlywood1",           {255, 211, 155}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 120 "rgb_hash.gperf"
      {"MediumSpringGreen",    {  0, 250, 154}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 450 "rgb_hash.gperf"
      {"PaleVioletRed4",       {139,  71,  93}},
      {""}, {""}, {""}, {""},
#line 449 "rgb_hash.gperf"
      {"PaleVioletRed3",       {205, 104, 137}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 448 "rgb_hash.gperf"
      {"PaleVioletRed2",       {238, 121, 159}},
      {""}, {""}, {""}, {""},
#line 447 "rgb_hash.gperf"
      {"PaleVioletRed1",       {255, 130, 171}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 58 "rgb_hash.gperf"
      {"LemonChiffon",         {255, 250, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 137 "rgb_hash.gperf"
      {"IndianRed",            {205,  92,  92}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 45 "rgb_hash.gperf"
      {"gainsboro",            {220, 220, 220}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 474 "rgb_hash.gperf"
      {"MediumOrchid4",        {122,  55, 139}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 473 "rgb_hash.gperf"
      {"MediumOrchid3",        {180,  82, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 472 "rgb_hash.gperf"
      {"MediumOrchid2",        {209,  95, 238}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 102 "rgb_hash.gperf"
      {"MediumTurquoise",      { 72, 209, 204}},
      {""}, {""},
#line 471 "rgb_hash.gperf"
      {"MediumOrchid1",        {224, 102, 255}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 361 "rgb_hash.gperf"
      {"DarkGoldenrod4",       {139, 101,   8}},
      {""}, {""}, {""}, {""},
#line 360 "rgb_hash.gperf"
      {"DarkGoldenrod3",       {205, 149,  12}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 359 "rgb_hash.gperf"
      {"DarkGoldenrod2",       {238, 173,  14}},
      {""}, {""}, {""}, {""},
#line 358 "rgb_hash.gperf"
      {"DarkGoldenrod1",       {255, 185,  15}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 466 "rgb_hash.gperf"
      {"orchid4",              {139,  71, 137}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 465 "rgb_hash.gperf"
      {"orchid3",              {205, 105, 201}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 464 "rgb_hash.gperf"
      {"orchid2",              {238, 122, 233}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 463 "rgb_hash.gperf"
      {"orchid1",              {255, 131, 250}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 55 "rgb_hash.gperf"
      {"moccasin",             {255, 228, 181}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 60 "rgb_hash.gperf"
      {"honeydew",             {240, 255, 240}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 141 "rgb_hash.gperf"
      {"burlywood",            {222, 184, 135}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 165 "rgb_hash.gperf"
      {"MediumVioletRed",      {199,  21, 133}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 163 "rgb_hash.gperf"
      {"PaleVioletRed",        {219, 112, 147}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 171 "rgb_hash.gperf"
      {"MediumOrchid",         {186,  85, 211}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 135 "rgb_hash.gperf"
      {"DarkGoldenrod",        {184, 134,  11}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 170 "rgb_hash.gperf"
      {"orchid",               {218, 112, 214}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 51 "rgb_hash.gperf"
      {"BlanchedAlmond",       {255, 235, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 128 "rgb_hash.gperf"
      {"PaleGoldenrod",        {238, 232, 170}}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register unsigned int key = hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
