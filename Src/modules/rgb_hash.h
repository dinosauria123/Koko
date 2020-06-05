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
#define MIN_HASH_VALUE 3
#define MAX_HASH_VALUE 3950
/* maximum key range = 3948, duplicates = 0 */

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
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,  645,   25,
        20,    5,    0,  556,  825,  630,  610,  485, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951,   80,  175,  825,
         0,    0,  170,    0,   15,  335,    0,  995,   35,  435,
       160,  195,  882,   45,    0,    0,  330,  250,  524,  974,
      3951,  225, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951, 3951,
      3951, 3951, 3951, 3951, 3951, 3951
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
      {""}, {""}, {""},
#line 158 "rgb_hash.gperf"
      {"red",                  {255,   0,   0}},
#line 429 "rgb_hash.gperf"
      {"red4",                 {139,   0,   0}},
#line 500 "rgb_hash.gperf"
      {"grey4",                { 10,  10,  10}},
#line 580 "rgb_hash.gperf"
      {"grey44",               {112, 112, 112}},
#line 698 "rgb_hash.gperf"
      {"darkred",              {139,   0,   0}},
      {""},
#line 428 "rgb_hash.gperf"
      {"red3",                 {205,   0,   0}},
      {""},
#line 560 "rgb_hash.gperf"
      {"grey34",               { 87,  87,  87}},
      {""}, {""}, {""},
#line 498 "rgb_hash.gperf"
      {"grey3",                {  8,   8,   8}},
#line 578 "rgb_hash.gperf"
      {"grey43",               {110, 110, 110}},
      {""}, {""}, {""}, {""},
#line 558 "rgb_hash.gperf"
      {"grey33",               { 84,  84,  84}},
      {""}, {""},
#line 427 "rgb_hash.gperf"
      {"red2",                 {238,   0,   0}},
      {""},
#line 540 "rgb_hash.gperf"
      {"grey24",               { 61,  61,  61}},
      {""}, {""},
#line 426 "rgb_hash.gperf"
      {"red1",                 {255,   0,   0}},
      {""},
#line 520 "rgb_hash.gperf"
      {"grey14",               { 36,  36,  36}},
      {""}, {""}, {""}, {""},
#line 538 "rgb_hash.gperf"
      {"grey23",               { 59,  59,  59}},
      {""}, {""},
#line 132 "rgb_hash.gperf"
      {"gold",                 {255, 215,   0}},
#line 353 "rgb_hash.gperf"
      {"gold4",                {139, 117,   0}},
#line 518 "rgb_hash.gperf"
      {"grey13",               { 33,  33,  33}},
      {""}, {""}, {""},
#line 496 "rgb_hash.gperf"
      {"grey2",                {  5,   5,   5}},
#line 576 "rgb_hash.gperf"
      {"grey42",               {107, 107, 107}},
      {""}, {""}, {""},
#line 352 "rgb_hash.gperf"
      {"gold3",                {205, 173,   0}},
#line 556 "rgb_hash.gperf"
      {"grey32",               { 82,  82,  82}},
      {""}, {""}, {""},
#line 494 "rgb_hash.gperf"
      {"grey1",                {  3,   3,   3}},
#line 574 "rgb_hash.gperf"
      {"grey41",               {105, 105, 105}},
      {""}, {""}, {""}, {""},
#line 554 "rgb_hash.gperf"
      {"grey31",               { 79,  79,  79}},
      {""}, {""}, {""}, {""},
#line 536 "rgb_hash.gperf"
      {"grey22",               { 56,  56,  56}},
      {""}, {""}, {""}, {""},
#line 516 "rgb_hash.gperf"
      {"grey12",               { 31,  31,  31}},
      {""}, {""}, {""}, {""},
#line 534 "rgb_hash.gperf"
      {"grey21",               { 54,  54,  54}},
      {""}, {""}, {""},
#line 351 "rgb_hash.gperf"
      {"gold2",                {238, 201,   0}},
#line 514 "rgb_hash.gperf"
      {"grey11",               { 28,  28,  28}},
      {""}, {""}, {""},
#line 499 "rgb_hash.gperf"
      {"gray4",                { 10,  10,  10}},
#line 579 "rgb_hash.gperf"
      {"gray44",               {112, 112, 112}},
      {""}, {""}, {""},
#line 350 "rgb_hash.gperf"
      {"gold1",                {255, 215,   0}},
#line 559 "rgb_hash.gperf"
      {"gray34",               { 87,  87,  87}},
      {""}, {""}, {""},
#line 497 "rgb_hash.gperf"
      {"gray3",                {  8,   8,   8}},
#line 577 "rgb_hash.gperf"
      {"gray43",               {110, 110, 110}},
      {""}, {""}, {""}, {""},
#line 557 "rgb_hash.gperf"
      {"gray33",               { 84,  84,  84}},
      {""}, {""}, {""}, {""},
#line 539 "rgb_hash.gperf"
      {"gray24",               { 61,  61,  61}},
      {""}, {""}, {""}, {""},
#line 519 "rgb_hash.gperf"
      {"gray14",               { 36,  36,  36}},
      {""}, {""}, {""}, {""},
#line 537 "rgb_hash.gperf"
      {"gray23",               { 59,  59,  59}},
      {""}, {""}, {""}, {""},
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
      {""}, {""}, {""}, {""},
#line 553 "rgb_hash.gperf"
      {"gray31",               { 79,  79,  79}},
      {""}, {""}, {""}, {""},
#line 535 "rgb_hash.gperf"
      {"gray22",               { 56,  56,  56}},
      {""}, {""}, {""}, {""},
#line 515 "rgb_hash.gperf"
      {"gray12",               { 31,  31,  31}},
      {""}, {""}, {""}, {""},
#line 533 "rgb_hash.gperf"
      {"gray21",               { 54,  54,  54}},
      {""}, {""}, {""}, {""},
#line 513 "rgb_hash.gperf"
      {"gray11",               { 28,  28,  28}},
      {""}, {""}, {""}, {""},
#line 321 "rgb_hash.gperf"
      {"green4",               {  0, 139,   0}},
      {""}, {""},
#line 109 "rgb_hash.gperf"
      {"darkgreen",            {  0, 100,   0}},
#line 265 "rgb_hash.gperf"
      {"slategray4",           {108, 123, 139}},
      {""},
#line 317 "rgb_hash.gperf"
      {"springgreen4",         {  0, 139,  69}},
      {""},
#line 185 "rgb_hash.gperf"
      {"seashell4",            {139, 134, 130}},
#line 264 "rgb_hash.gperf"
      {"slategray3",           {159, 182, 205}},
#line 320 "rgb_hash.gperf"
      {"green3",               {  0, 205,   0}},
      {""}, {""},
#line 184 "rgb_hash.gperf"
      {"seashell3",            {205, 197, 191}},
      {""}, {""},
#line 316 "rgb_hash.gperf"
      {"springgreen3",         {  0, 205, 102}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 263 "rgb_hash.gperf"
      {"slategray2",           {185, 211, 238}},
      {""}, {""}, {""},
#line 183 "rgb_hash.gperf"
      {"seashell2",            {238, 229, 222}},
#line 262 "rgb_hash.gperf"
      {"slategray1",           {198, 226, 255}},
      {""}, {""}, {""},
#line 182 "rgb_hash.gperf"
      {"seashell1",            {255, 245, 238}},
#line 181 "rgb_hash.gperf"
      {"snow4",                {139, 137, 137}},
      {""}, {""}, {""}, {""}, {""},
#line 319 "rgb_hash.gperf"
      {"green2",               {  0, 238,   0}},
      {""},
#line 59 "rgb_hash.gperf"
      {"seashell",             {255, 245, 238}},
      {""},
#line 180 "rgb_hash.gperf"
      {"snow3",                {205, 201, 201}},
      {""},
#line 315 "rgb_hash.gperf"
      {"springgreen2",         {  0, 238, 118}},
      {""}, {""}, {""},
#line 318 "rgb_hash.gperf"
      {"green1",               {  0, 255,   0}},
      {""}, {""}, {""},
#line 92 "rgb_hash.gperf"
      {"dodgerblue",           { 30, 144, 255}},
#line 245 "rgb_hash.gperf"
      {"dodgerblue4",          { 16,  78, 139}},
#line 314 "rgb_hash.gperf"
      {"springgreen1",         {  0, 255, 127}},
      {""}, {""}, {""},
#line 244 "rgb_hash.gperf"
      {"dodgerblue3",          { 24, 116, 205}},
      {""}, {""},
#line 78 "rgb_hash.gperf"
      {"grey",                 {190, 190, 190}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 179 "rgb_hash.gperf"
      {"snow2",                {238, 233, 233}},
#line 243 "rgb_hash.gperf"
      {"dodgerblue2",          { 28, 134, 238}},
      {""}, {""}, {""}, {""},
#line 242 "rgb_hash.gperf"
      {"dodgerblue1",          { 30, 144, 255}},
#line 373 "rgb_hash.gperf"
      {"sienna4",              {139,  71,  38}},
      {""},
#line 309 "rgb_hash.gperf"
      {"seagreen4",            { 46, 139,  87}},
#line 178 "rgb_hash.gperf"
      {"snow1",                {255, 250, 250}},
      {""}, {""},
#line 305 "rgb_hash.gperf"
      {"darkseagreen4",        {105, 139, 105}},
#line 308 "rgb_hash.gperf"
      {"seagreen3",            { 67, 205, 128}},
      {""}, {""},
#line 372 "rgb_hash.gperf"
      {"sienna3",              {205, 104,  57}},
      {""}, {""}, {""}, {""}, {""},
#line 304 "rgb_hash.gperf"
      {"darkseagreen3",        {155, 205, 155}},
      {""}, {""}, {""}, {""}, {""},
#line 307 "rgb_hash.gperf"
      {"seagreen2",            { 78, 238, 148}},
      {""}, {""}, {""}, {""},
#line 306 "rgb_hash.gperf"
      {"seagreen1",            { 84, 255, 159}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 152 "rgb_hash.gperf"
      {"orange",               {255, 165,   0}},
#line 409 "rgb_hash.gperf"
      {"orange4",              {139,  90,   0}},
      {""},
#line 157 "rgb_hash.gperf"
      {"orangered",            {255,  69,   0}},
#line 425 "rgb_hash.gperf"
      {"orangered4",           {139,  37,   0}},
      {""},
#line 371 "rgb_hash.gperf"
      {"sienna2",              {238, 121,  66}},
      {""}, {""},
#line 424 "rgb_hash.gperf"
      {"orangered3",           {205,  55,   0}},
      {""},
#line 408 "rgb_hash.gperf"
      {"orange3",              {205, 133,   0}},
#line 303 "rgb_hash.gperf"
      {"darkseagreen2",        {180, 238, 180}},
      {""}, {""}, {""},
#line 370 "rgb_hash.gperf"
      {"sienna1",              {255, 130,  71}},
      {""}, {""}, {""}, {""}, {""},
#line 302 "rgb_hash.gperf"
      {"darkseagreen1",        {193, 255, 193}},
      {""},
#line 423 "rgb_hash.gperf"
      {"orangered2",           {238,  64,   0}},
      {""}, {""}, {""},
#line 77 "rgb_hash.gperf"
      {"gray",                 {190, 190, 190}},
#line 422 "rgb_hash.gperf"
      {"orangered1",           {255,  69,   0}},
      {""}, {""}, {""},
#line 74 "rgb_hash.gperf"
      {"slategrey",            {112, 128, 144}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 407 "rgb_hash.gperf"
      {"orange2",              {238, 154,   0}},
      {""}, {""},
#line 118 "rgb_hash.gperf"
      {"green",                {  0, 255,   0}},
#line 139 "rgb_hash.gperf"
      {"sienna",               {160,  82,  45}},
      {""}, {""}, {""}, {""},
#line 116 "rgb_hash.gperf"
      {"springgreen",          {  0, 255, 127}},
#line 406 "rgb_hash.gperf"
      {"orange1",              {255, 165,   0}},
      {""}, {""},
#line 62 "rgb_hash.gperf"
      {"azure",                {240, 255, 255}},
#line 229 "rgb_hash.gperf"
      {"azure4",               {131, 139, 139}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 228 "rgb_hash.gperf"
      {"azure3",               {193, 205, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 227 "rgb_hash.gperf"
      {"azure2",               {224, 238, 238}},
      {""}, {""}, {""}, {""},
#line 138 "rgb_hash.gperf"
      {"saddlebrown",          {139,  69,  19}},
      {""}, {""}, {""}, {""},
#line 226 "rgb_hash.gperf"
      {"azure1",               {240, 255, 255}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 73 "rgb_hash.gperf"
      {"slategray",            {112, 128, 144}},
      {""}, {""},
#line 401 "rgb_hash.gperf"
      {"salmon4",              {139,  76,  57}},
      {""},
#line 134 "rgb_hash.gperf"
      {"goldenrod",            {218, 165,  32}},
#line 357 "rgb_hash.gperf"
      {"goldenrod4",           {139, 105,  20}},
      {""}, {""}, {""}, {""},
#line 356 "rgb_hash.gperf"
      {"goldenrod3",           {205, 155,  29}},
      {""},
#line 400 "rgb_hash.gperf"
      {"salmon3",              {205, 112,  84}},
#line 112 "rgb_hash.gperf"
      {"seagreen",             { 46, 139,  87}},
      {""}, {""}, {""},
#line 111 "rgb_hash.gperf"
      {"darkseagreen",         {143, 188, 143}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 355 "rgb_hash.gperf"
      {"goldenrod2",           {238, 180,  34}},
      {""}, {""}, {""},
#line 430 "rgb_hash.gperf"
      {"debianred",            {215,   7,  81}},
#line 354 "rgb_hash.gperf"
      {"goldenrod1",           {255, 193,  37}},
      {""}, {""}, {""},
#line 91 "rgb_hash.gperf"
      {"blue",                 {  0,   0, 255}},
#line 241 "rgb_hash.gperf"
      {"blue4",                {  0,   0, 139}},
#line 52 "rgb_hash.gperf"
      {"bisque",               {255, 228, 196}},
#line 193 "rgb_hash.gperf"
      {"bisque4",              {139, 125, 107}},
      {""}, {""}, {""}, {""},
#line 399 "rgb_hash.gperf"
      {"salmon2",              {238, 130,  98}},
#line 135 "rgb_hash.gperf"
      {"darkgoldenrod",        {184, 134,  11}},
#line 361 "rgb_hash.gperf"
      {"darkgoldenrod4",       {139, 101,   8}},
#line 240 "rgb_hash.gperf"
      {"blue3",                {  0,   0, 205}},
      {""},
#line 192 "rgb_hash.gperf"
      {"bisque3",              {205, 183, 158}},
      {""},
#line 360 "rgb_hash.gperf"
      {"darkgoldenrod3",       {205, 149,  12}},
#line 153 "rgb_hash.gperf"
      {"darkorange",           {255, 140,   0}},
#line 413 "rgb_hash.gperf"
      {"darkorange4",          {139,  69,   0}},
#line 398 "rgb_hash.gperf"
      {"salmon1",              {255, 140, 105}},
      {""}, {""}, {""},
#line 412 "rgb_hash.gperf"
      {"darkorange3",          {205, 102,   0}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 693 "rgb_hash.gperf"
      {"darkgrey",             {169, 169, 169}},
#line 359 "rgb_hash.gperf"
      {"darkgoldenrod2",       {238, 173,  14}},
      {""}, {""}, {""}, {""},
#line 358 "rgb_hash.gperf"
      {"darkgoldenrod1",       {255, 185,  15}},
      {""},
#line 411 "rgb_hash.gperf"
      {"darkorange2",          {238, 118,   0}},
      {""},
#line 695 "rgb_hash.gperf"
      {"darkblue",             {  0,   0, 139}},
      {""},
#line 239 "rgb_hash.gperf"
      {"blue2",                {  0,   0, 238}},
#line 410 "rgb_hash.gperf"
      {"darkorange1",          {255, 127,   0}},
#line 191 "rgb_hash.gperf"
      {"bisque2",              {238, 213, 183}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 238 "rgb_hash.gperf"
      {"blue1",                {  0,   0, 255}},
      {""},
#line 190 "rgb_hash.gperf"
      {"bisque1",              {255, 228, 196}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 680 "rgb_hash.gperf"
      {"grey94",               {240, 240, 240}},
#line 405 "rgb_hash.gperf"
      {"lightsalmon4",         {139,  87,  66}},
      {""},
#line 385 "rgb_hash.gperf"
      {"tan4",                 {139,  90,  43}},
      {""}, {""}, {""}, {""},
#line 384 "rgb_hash.gperf"
      {"tan3",                 {205, 133,  63}},
      {""},
#line 678 "rgb_hash.gperf"
      {"grey93",               {237, 237, 237}},
#line 404 "rgb_hash.gperf"
      {"lightsalmon3",         {205, 129,  98}},
      {""},
#line 96 "rgb_hash.gperf"
      {"steelblue",            { 70, 130, 180}},
#line 249 "rgb_hash.gperf"
      {"steelblue4",           { 54, 100, 139}},
      {""}, {""}, {""}, {""},
#line 248 "rgb_hash.gperf"
      {"steelblue3",           { 79, 148, 205}},
      {""}, {""}, {""},
#line 383 "rgb_hash.gperf"
      {"tan2",                 {238, 154,  73}},
#line 142 "rgb_hash.gperf"
      {"beige",                {245, 245, 220}},
      {""},
#line 94 "rgb_hash.gperf"
      {"skyblue",              {135, 206, 235}},
#line 257 "rgb_hash.gperf"
      {"skyblue4",             { 74, 112, 139}},
#line 382 "rgb_hash.gperf"
      {"tan1",                 {255, 165,  79}},
#line 48 "rgb_hash.gperf"
      {"linen",                {250, 240, 230}},
      {""}, {""}, {""}, {""},
#line 247 "rgb_hash.gperf"
      {"steelblue2",           { 92, 172, 238}},
      {""}, {""},
#line 256 "rgb_hash.gperf"
      {"skyblue3",             {108, 166, 205}},
      {""},
#line 246 "rgb_hash.gperf"
      {"steelblue1",           { 99, 184, 255}},
#line 676 "rgb_hash.gperf"
      {"grey92",               {235, 235, 235}},
#line 403 "rgb_hash.gperf"
      {"lightsalmon2",         {238, 149, 114}},
      {""}, {""},
#line 699 "rgb_hash.gperf"
      {"lightgreen",           {144, 238, 144}},
#line 397 "rgb_hash.gperf"
      {"brown4",               {139,  35,  35}},
      {""},
#line 694 "rgb_hash.gperf"
      {"darkgray",             {169, 169, 169}},
      {""}, {""},
#line 674 "rgb_hash.gperf"
      {"grey91",               {232, 232, 232}},
#line 402 "rgb_hash.gperf"
      {"lightsalmon1",         {255, 160, 122}},
      {""}, {""}, {""},
#line 396 "rgb_hash.gperf"
      {"brown3",               {205,  51,  51}},
      {""}, {""},
#line 86 "rgb_hash.gperf"
      {"slateblue",            {106,  90, 205}},
#line 233 "rgb_hash.gperf"
      {"slateblue4",           { 71,  60, 139}},
      {""}, {""}, {""}, {""},
#line 232 "rgb_hash.gperf"
      {"slateblue3",           {105,  89, 205}},
#line 150 "rgb_hash.gperf"
      {"salmon",               {250, 128, 114}},
      {""},
#line 255 "rgb_hash.gperf"
      {"skyblue2",             {126, 192, 238}},
      {""}, {""}, {""},
#line 600 "rgb_hash.gperf"
      {"grey54",               {138, 138, 138}},
      {""}, {""}, {""}, {""}, {""},
#line 254 "rgb_hash.gperf"
      {"skyblue1",             {135, 206, 255}},
      {""},
#line 231 "rgb_hash.gperf"
      {"slateblue2",           {122, 103, 238}},
#line 679 "rgb_hash.gperf"
      {"gray94",               {240, 240, 240}},
#line 598 "rgb_hash.gperf"
      {"grey53",               {135, 135, 135}},
      {""}, {""},
#line 230 "rgb_hash.gperf"
      {"slateblue1",           {131, 111, 255}},
#line 395 "rgb_hash.gperf"
      {"brown2",               {238,  59,  59}},
      {""}, {""}, {""}, {""},
#line 677 "rgb_hash.gperf"
      {"gray93",               {237, 237, 237}},
      {""}, {""},
#line 137 "rgb_hash.gperf"
      {"indianred",            {205,  92,  92}},
#line 369 "rgb_hash.gperf"
      {"indianred4",           {139,  58,  58}},
#line 394 "rgb_hash.gperf"
      {"brown1",               {255,  64,  64}},
      {""}, {""}, {""},
#line 368 "rgb_hash.gperf"
      {"indianred3",           {205,  85,  85}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 79 "rgb_hash.gperf"
      {"lightgrey",            {211, 211, 211}},
      {""}, {""},
#line 596 "rgb_hash.gperf"
      {"grey52",               {133, 133, 133}},
      {""}, {""},
#line 367 "rgb_hash.gperf"
      {"indianred2",           {238,  99,  99}},
#line 697 "rgb_hash.gperf"
      {"darkmagenta",          {139,   0, 139}},
      {""}, {""}, {""},
#line 366 "rgb_hash.gperf"
      {"indianred1",           {255, 106, 106}},
#line 675 "rgb_hash.gperf"
      {"gray92",               {235, 235, 235}},
#line 594 "rgb_hash.gperf"
      {"grey51",               {130, 130, 130}},
      {""}, {""}, {""},
#line 660 "rgb_hash.gperf"
      {"grey84",               {214, 214, 214}},
      {""}, {""}, {""},
#line 329 "rgb_hash.gperf"
      {"olivedrab4",           {105, 139,  34}},
#line 673 "rgb_hash.gperf"
      {"gray91",               {232, 232, 232}},
      {""}, {""}, {""},
#line 328 "rgb_hash.gperf"
      {"olivedrab3",           {154, 205,  50}},
#line 658 "rgb_hash.gperf"
      {"grey83",               {212, 212, 212}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 640 "rgb_hash.gperf"
      {"grey74",               {189, 189, 189}},
#line 345 "rgb_hash.gperf"
      {"lightyellow4",         {139, 139, 122}},
#line 101 "rgb_hash.gperf"
      {"darkturquoise",        {  0, 206, 209}},
#line 122 "rgb_hash.gperf"
      {"limegreen",            { 50, 205,  50}},
#line 327 "rgb_hash.gperf"
      {"olivedrab2",           {179, 238,  58}},
      {""},
#line 599 "rgb_hash.gperf"
      {"gray54",               {138, 138, 138}},
      {""}, {""},
#line 326 "rgb_hash.gperf"
      {"olivedrab1",           {192, 255,  62}},
#line 638 "rgb_hash.gperf"
      {"grey73",               {186, 186, 186}},
#line 344 "rgb_hash.gperf"
      {"lightyellow3",         {205, 205, 180}},
      {""}, {""}, {""},
#line 151 "rgb_hash.gperf"
      {"lightsalmon",          {255, 160, 122}},
#line 597 "rgb_hash.gperf"
      {"gray53",               {135, 135, 135}},
#line 145 "rgb_hash.gperf"
      {"tan",                  {210, 180, 140}},
      {""}, {""},
#line 656 "rgb_hash.gperf"
      {"grey82",               {209, 209, 209}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 654 "rgb_hash.gperf"
      {"grey81",               {207, 207, 207}},
      {""}, {""}, {""}, {""},
#line 124 "rgb_hash.gperf"
      {"forestgreen",          { 34, 139,  34}},
      {""}, {""}, {""}, {""},
#line 636 "rgb_hash.gperf"
      {"grey72",               {184, 184, 184}},
#line 343 "rgb_hash.gperf"
      {"lightyellow2",         {238, 238, 209}},
      {""},
#line 80 "rgb_hash.gperf"
      {"lightgray",            {211, 211, 211}},
      {""}, {""},
#line 595 "rgb_hash.gperf"
      {"gray52",               {133, 133, 133}},
      {""}, {""}, {""},
#line 634 "rgb_hash.gperf"
      {"grey71",               {181, 181, 181}},
#line 342 "rgb_hash.gperf"
      {"lightyellow1",         {255, 255, 224}},
      {""}, {""}, {""}, {""},
#line 593 "rgb_hash.gperf"
      {"gray51",               {130, 130, 130}},
      {""}, {""},
#line 148 "rgb_hash.gperf"
      {"brown",                {165,  42,  42}},
#line 659 "rgb_hash.gperf"
      {"gray84",               {214, 214, 214}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 657 "rgb_hash.gperf"
      {"gray83",               {212, 212, 212}},
      {""},
#line 85 "rgb_hash.gperf"
      {"darkslateblue",        { 72,  61, 139}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 639 "rgb_hash.gperf"
      {"gray74",               {189, 189, 189}},
      {""}, {""},
#line 76 "rgb_hash.gperf"
      {"lightslategrey",       {119, 136, 153}},
#line 149 "rgb_hash.gperf"
      {"darksalmon",           {233, 150, 122}},
      {""}, {""}, {""}, {""}, {""},
#line 637 "rgb_hash.gperf"
      {"gray73",               {186, 186, 186}},
#line 64 "rgb_hash.gperf"
      {"lavender",             {230, 230, 250}},
      {""},
#line 90 "rgb_hash.gperf"
      {"royalblue",            { 65, 105, 225}},
#line 237 "rgb_hash.gperf"
      {"royalblue4",           { 39,  64, 139}},
      {""}, {""}, {""}, {""},
#line 236 "rgb_hash.gperf"
      {"royalblue3",           { 58,  95, 205}},
#line 655 "rgb_hash.gperf"
      {"gray82",               {209, 209, 209}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 653 "rgb_hash.gperf"
      {"gray81",               {207, 207, 207}},
      {""},
#line 221 "rgb_hash.gperf"
      {"lavenderblush4",       {139, 131, 134}},
      {""},
#line 235 "rgb_hash.gperf"
      {"royalblue2",           { 67, 110, 238}},
      {""}, {""},
#line 220 "rgb_hash.gperf"
      {"lavenderblush3",       {205, 193, 197}},
      {""},
#line 234 "rgb_hash.gperf"
      {"royalblue1",           { 72, 118, 255}},
#line 635 "rgb_hash.gperf"
      {"gray72",               {184, 184, 184}},
      {""}, {""}, {""}, {""},
#line 213 "rgb_hash.gperf"
      {"ivory4",               {139, 139, 131}},
#line 65 "rgb_hash.gperf"
      {"lavenderblush",        {255, 240, 245}},
      {""},
#line 297 "rgb_hash.gperf"
      {"darkslategray4",       { 82, 139, 139}},
      {""},
#line 633 "rgb_hash.gperf"
      {"gray71",               {181, 181, 181}},
      {""},
#line 219 "rgb_hash.gperf"
      {"lavenderblush2",       {238, 224, 229}},
#line 296 "rgb_hash.gperf"
      {"darkslategray3",       {121, 205, 205}},
      {""},
#line 212 "rgb_hash.gperf"
      {"ivory3",               {205, 205, 193}},
      {""},
#line 218 "rgb_hash.gperf"
      {"lavenderblush1",       {255, 240, 245}},
      {""}, {""}, {""}, {""},
#line 114 "rgb_hash.gperf"
      {"lightseagreen",        { 32, 178, 170}},
#line 88 "rgb_hash.gperf"
      {"lightslateblue",       {132, 112, 255}},
      {""}, {""}, {""}, {""},
#line 295 "rgb_hash.gperf"
      {"darkslategray2",       {141, 238, 238}},
      {""}, {""}, {""}, {""},
#line 294 "rgb_hash.gperf"
      {"darkslategray1",       {151, 255, 255}},
      {""}, {""}, {""}, {""},
#line 125 "rgb_hash.gperf"
      {"olivedrab",            {107, 142,  35}},
      {""}, {""},
#line 454 "rgb_hash.gperf"
      {"maroon4",              {139,  28,  98}},
      {""},
#line 75 "rgb_hash.gperf"
      {"lightslategray",       {119, 136, 153}},
      {""},
#line 211 "rgb_hash.gperf"
      {"ivory2",               {238, 238, 224}},
      {""}, {""},
#line 133 "rgb_hash.gperf"
      {"lightgoldenrod",       {238, 221, 130}},
#line 341 "rgb_hash.gperf"
      {"lightgoldenrod4",      {139, 129,  76}},
      {""},
#line 453 "rgb_hash.gperf"
      {"maroon3",              {205,  41, 144}},
      {""}, {""},
#line 340 "rgb_hash.gperf"
      {"lightgoldenrod3",      {205, 190, 112}},
#line 210 "rgb_hash.gperf"
      {"ivory1",               {255, 255, 240}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 339 "rgb_hash.gperf"
      {"lightgoldenrod2",      {238, 220, 130}},
      {""}, {""}, {""}, {""},
#line 338 "rgb_hash.gperf"
      {"lightgoldenrod1",      {255, 236, 139}},
#line 620 "rgb_hash.gperf"
      {"grey64",               {163, 163, 163}},
      {""}, {""},
#line 98 "rgb_hash.gperf"
      {"lightblue",            {173, 216, 230}},
#line 273 "rgb_hash.gperf"
      {"lightblue4",           {104, 131, 139}},
      {""},
#line 452 "rgb_hash.gperf"
      {"maroon2",              {238,  48, 167}},
      {""}, {""},
#line 272 "rgb_hash.gperf"
      {"lightblue3",           {154, 192, 205}},
#line 618 "rgb_hash.gperf"
      {"grey63",               {161, 161, 161}},
      {""}, {""}, {""}, {""}, {""},
#line 451 "rgb_hash.gperf"
      {"maroon1",              {255,  52, 179}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 271 "rgb_hash.gperf"
      {"lightblue2",           {178, 223, 238}},
      {""}, {""}, {""}, {""},
#line 270 "rgb_hash.gperf"
      {"lightblue1",           {191, 239, 255}},
      {""}, {""}, {""},
#line 66 "rgb_hash.gperf"
      {"mistyrose",            {255, 228, 225}},
#line 225 "rgb_hash.gperf"
      {"mistyrose4",           {139, 125, 123}},
#line 417 "rgb_hash.gperf"
      {"coral4",               {139,  62,  47}},
      {""}, {""}, {""},
#line 224 "rgb_hash.gperf"
      {"mistyrose3",           {205, 183, 181}},
#line 616 "rgb_hash.gperf"
      {"grey62",               {158, 158, 158}},
      {""}, {""}, {""}, {""},
#line 416 "rgb_hash.gperf"
      {"coral3",               {205,  91,  69}},
      {""}, {""}, {""}, {""},
#line 614 "rgb_hash.gperf"
      {"grey61",               {156, 156, 156}},
      {""}, {""},
#line 63 "rgb_hash.gperf"
      {"aliceblue",            {240, 248, 255}},
#line 223 "rgb_hash.gperf"
      {"mistyrose2",           {238, 213, 210}},
      {""}, {""}, {""}, {""},
#line 222 "rgb_hash.gperf"
      {"mistyrose1",           {255, 228, 225}},
      {""},
#line 72 "rgb_hash.gperf"
      {"dimgrey",              {105, 105, 105}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 154 "rgb_hash.gperf"
      {"coral",                {255, 127,  80}},
      {""}, {""}, {""}, {""}, {""},
#line 415 "rgb_hash.gperf"
      {"coral2",               {238, 106,  80}},
      {""},
#line 70 "rgb_hash.gperf"
      {"darkslategrey",        { 47,  79,  79}},
#line 45 "rgb_hash.gperf"
      {"gainsboro",            {220, 220, 220}},
#line 293 "rgb_hash.gperf"
      {"cyan4",                {  0, 139, 139}},
#line 619 "rgb_hash.gperf"
      {"gray64",               {163, 163, 163}},
      {""},
#line 82 "rgb_hash.gperf"
      {"navy",                 {  0,   0, 128}},
      {""}, {""},
#line 414 "rgb_hash.gperf"
      {"coral1",               {255, 114,  86}},
      {""}, {""}, {""},
#line 292 "rgb_hash.gperf"
      {"cyan3",                {  0, 205, 205}},
#line 617 "rgb_hash.gperf"
      {"gray63",               {161, 161, 161}},
      {""},
#line 175 "rgb_hash.gperf"
      {"purple",               {160,  32, 240}},
#line 482 "rgb_hash.gperf"
      {"purple4",              { 85,  26, 139}},
#line 144 "rgb_hash.gperf"
      {"sandybrown",           {244, 164,  96}},
      {""},
#line 313 "rgb_hash.gperf"
      {"palegreen4",           { 84, 139,  84}},
      {""}, {""}, {""}, {""},
#line 312 "rgb_hash.gperf"
      {"palegreen3",           {124, 205, 124}},
      {""},
#line 481 "rgb_hash.gperf"
      {"purple3",              {125,  38, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 311 "rgb_hash.gperf"
      {"palegreen2",           {144, 238, 144}},
      {""}, {""},
#line 291 "rgb_hash.gperf"
      {"cyan2",                {  0, 238, 238}},
#line 615 "rgb_hash.gperf"
      {"gray62",               {158, 158, 158}},
#line 310 "rgb_hash.gperf"
      {"palegreen1",           {154, 255, 154}},
      {""}, {""}, {""},
#line 164 "rgb_hash.gperf"
      {"maroon",               {176,  48,  96}},
      {""}, {""}, {""},
#line 290 "rgb_hash.gperf"
      {"cyan1",                {  0, 255, 255}},
#line 613 "rgb_hash.gperf"
      {"gray61",               {156, 156, 156}},
      {""}, {""},
#line 480 "rgb_hash.gperf"
      {"purple2",              {145,  44, 238}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 71 "rgb_hash.gperf"
      {"dimgray",              {105, 105, 105}},
      {""},
#line 479 "rgb_hash.gperf"
      {"purple1",              {155,  48, 255}},
#line 510 "rgb_hash.gperf"
      {"grey9",                { 23,  23,  23}},
#line 590 "rgb_hash.gperf"
      {"grey49",               {125, 125, 125}},
      {""}, {""}, {""}, {""},
#line 570 "rgb_hash.gperf"
      {"grey39",               { 99,  99,  99}},
      {""}, {""}, {""},
#line 57 "rgb_hash.gperf"
      {"ivory",                {255, 255, 240}},
      {""}, {""},
#line 69 "rgb_hash.gperf"
      {"darkslategray",        { 47,  79,  79}},
      {""}, {""}, {""}, {""}, {""},
#line 97 "rgb_hash.gperf"
      {"lightsteelblue",       {176, 196, 222}},
#line 269 "rgb_hash.gperf"
      {"lightsteelblue4",      {110, 123, 139}},
#line 550 "rgb_hash.gperf"
      {"grey29",               { 74,  74,  74}},
      {""}, {""}, {""},
#line 268 "rgb_hash.gperf"
      {"lightsteelblue3",      {162, 181, 205}},
#line 530 "rgb_hash.gperf"
      {"grey19",               { 48,  48,  48}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 462 "rgb_hash.gperf"
      {"magenta4",             {139,   0, 139}},
      {""},
#line 267 "rgb_hash.gperf"
      {"lightsteelblue2",      {188, 210, 238}},
      {""}, {""}, {""}, {""},
#line 266 "rgb_hash.gperf"
      {"lightsteelblue1",      {202, 225, 255}},
      {""}, {""},
#line 461 "rgb_hash.gperf"
      {"magenta3",             {205,   0, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 177 "rgb_hash.gperf"
      {"thistle",              {216, 191, 216}},
#line 490 "rgb_hash.gperf"
      {"thistle4",             {139, 123, 139}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 172 "rgb_hash.gperf"
      {"darkorchid",           {153,  50, 204}},
#line 478 "rgb_hash.gperf"
      {"darkorchid4",          {104,  34, 139}},
#line 442 "rgb_hash.gperf"
      {"pink4",                {139,  99, 108}},
#line 489 "rgb_hash.gperf"
      {"thistle3",             {205, 181, 205}},
      {""}, {""},
#line 477 "rgb_hash.gperf"
      {"darkorchid3",          {154,  50, 205}},
      {""},
#line 460 "rgb_hash.gperf"
      {"magenta2",             {238,   0, 238}},
      {""},
#line 509 "rgb_hash.gperf"
      {"gray9",                { 23,  23,  23}},
#line 589 "rgb_hash.gperf"
      {"gray49",               {125, 125, 125}},
#line 441 "rgb_hash.gperf"
      {"pink3",                {205, 145, 158}},
#line 166 "rgb_hash.gperf"
      {"violetred",            {208,  32, 144}},
#line 458 "rgb_hash.gperf"
      {"violetred4",           {139,  34,  82}},
      {""},
#line 569 "rgb_hash.gperf"
      {"gray39",               { 99,  99,  99}},
      {""},
#line 459 "rgb_hash.gperf"
      {"magenta1",             {255,   0, 255}},
#line 457 "rgb_hash.gperf"
      {"violetred3",           {205,  50, 120}},
      {""},
#line 476 "rgb_hash.gperf"
      {"darkorchid2",          {178,  58, 238}},
      {""}, {""},
#line 104 "rgb_hash.gperf"
      {"cyan",                 {  0, 255, 255}},
      {""},
#line 475 "rgb_hash.gperf"
      {"darkorchid1",          {191,  62, 255}},
      {""}, {""}, {""}, {""},
#line 549 "rgb_hash.gperf"
      {"gray29",               { 74,  74,  74}},
      {""},
#line 488 "rgb_hash.gperf"
      {"thistle2",             {238, 210, 238}},
#line 456 "rgb_hash.gperf"
      {"violetred2",           {238,  58, 140}},
      {""},
#line 529 "rgb_hash.gperf"
      {"gray19",               { 48,  48,  48}},
      {""}, {""},
#line 455 "rgb_hash.gperf"
      {"violetred1",           {255,  62, 150}},
      {""},
#line 115 "rgb_hash.gperf"
      {"palegreen",            {152, 251, 152}},
#line 440 "rgb_hash.gperf"
      {"pink2",                {238, 169, 184}},
#line 487 "rgb_hash.gperf"
      {"thistle1",             {255, 225, 255}},
      {""}, {""}, {""},
#line 167 "rgb_hash.gperf"
      {"magenta",              {255,   0, 255}},
      {""}, {""}, {""}, {""},
#line 439 "rgb_hash.gperf"
      {"pink1",                {255, 181, 197}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 333 "rgb_hash.gperf"
      {"darkolivegreen4",      {110, 139,  61}},
      {""}, {""},
#line 47 "rgb_hash.gperf"
      {"oldlace",              {253, 245, 230}},
      {""},
#line 332 "rgb_hash.gperf"
      {"darkolivegreen3",      {162, 205,  90}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 502 "rgb_hash.gperf"
      {"grey5",                { 13,  13,  13}},
#line 582 "rgb_hash.gperf"
      {"grey45",               {115, 115, 115}},
#line 103 "rgb_hash.gperf"
      {"turquoise",            { 64, 224, 208}},
#line 289 "rgb_hash.gperf"
      {"turquoise4",           {  0, 134, 139}},
      {""}, {""},
#line 562 "rgb_hash.gperf"
      {"grey35",               { 89,  89,  89}},
#line 331 "rgb_hash.gperf"
      {"darkolivegreen2",      {188, 238, 104}},
#line 288 "rgb_hash.gperf"
      {"turquoise3",           {  0, 197, 205}},
      {""},
#line 81 "rgb_hash.gperf"
      {"midnightblue",         { 25,  25, 112}},
      {""},
#line 330 "rgb_hash.gperf"
      {"darkolivegreen1",      {202, 255, 112}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 140 "rgb_hash.gperf"
      {"peru",                 {205, 133,  63}},
#line 470 "rgb_hash.gperf"
      {"plum4",                {139, 102, 139}},
#line 542 "rgb_hash.gperf"
      {"grey25",               { 64,  64,  64}},
      {""},
#line 287 "rgb_hash.gperf"
      {"turquoise2",           {  0, 229, 238}},
      {""}, {""},
#line 522 "rgb_hash.gperf"
      {"grey15",               { 38,  38,  38}},
      {""},
#line 286 "rgb_hash.gperf"
      {"turquoise1",           {  0, 245, 255}},
      {""},
#line 469 "rgb_hash.gperf"
      {"plum3",                {205, 150, 205}},
      {""}, {""}, {""}, {""},
#line 83 "rgb_hash.gperf"
      {"navyblue",             {  0,   0, 128}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 42 "rgb_hash.gperf"
      {"snow",                 {255, 250, 250}},
      {""}, {""}, {""},
#line 468 "rgb_hash.gperf"
      {"plum2",                {238, 174, 238}},
#line 117 "rgb_hash.gperf"
      {"lawngreen",            {124, 252,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 467 "rgb_hash.gperf"
      {"plum1",                {255, 187, 255}},
      {""}, {""},
#line 108 "rgb_hash.gperf"
      {"aquamarine",           {127, 255, 212}},
#line 301 "rgb_hash.gperf"
      {"aquamarine4",          { 69, 139, 116}},
      {""}, {""}, {""}, {""},
#line 300 "rgb_hash.gperf"
      {"aquamarine3",          {102, 205, 170}},
#line 501 "rgb_hash.gperf"
      {"gray5",                { 13,  13,  13}},
#line 581 "rgb_hash.gperf"
      {"gray45",               {115, 115, 115}},
      {""}, {""}, {""}, {""},
#line 561 "rgb_hash.gperf"
      {"gray35",               { 89,  89,  89}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 299 "rgb_hash.gperf"
      {"aquamarine2",          {118, 238, 198}},
      {""}, {""}, {""}, {""},
#line 298 "rgb_hash.gperf"
      {"aquamarine1",          {127, 255, 212}},
      {""},
#line 541 "rgb_hash.gperf"
      {"gray25",               { 64,  64,  64}},
      {""}, {""}, {""}, {""},
#line 521 "rgb_hash.gperf"
      {"gray15",               { 38,  38,  38}},
      {""},
#line 508 "rgb_hash.gperf"
      {"grey8",                { 20,  20,  20}},
#line 588 "rgb_hash.gperf"
      {"grey48",               {122, 122, 122}},
      {""}, {""}, {""}, {""},
#line 568 "rgb_hash.gperf"
      {"grey38",               { 97,  97,  97}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 548 "rgb_hash.gperf"
      {"grey28",               { 71,  71,  71}},
      {""}, {""}, {""}, {""},
#line 528 "rgb_hash.gperf"
      {"grey18",               { 46,  46,  46}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 110 "rgb_hash.gperf"
      {"darkolivegreen",       { 85, 107,  47}},
      {""},
#line 506 "rgb_hash.gperf"
      {"grey7",                { 18,  18,  18}},
#line 586 "rgb_hash.gperf"
      {"grey47",               {120, 120, 120}},
      {""}, {""}, {""}, {""},
#line 566 "rgb_hash.gperf"
      {"grey37",               { 94,  94,  94}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 546 "rgb_hash.gperf"
      {"grey27",               { 69,  69,  69}},
      {""}, {""}, {""}, {""},
#line 526 "rgb_hash.gperf"
      {"grey17",               { 43,  43,  43}},
      {""}, {""},
#line 113 "rgb_hash.gperf"
      {"mediumseagreen",       { 60, 179, 113}},
#line 492 "rgb_hash.gperf"
      {"grey0",                {  0,   0,   0}},
#line 572 "rgb_hash.gperf"
      {"grey40",               {102, 102, 102}},
#line 421 "rgb_hash.gperf"
      {"tomato4",              {139,  54,  38}},
      {""}, {""}, {""},
#line 552 "rgb_hash.gperf"
      {"grey30",               { 77,  77,  77}},
      {""}, {""}, {""},
#line 507 "rgb_hash.gperf"
      {"gray8",                { 20,  20,  20}},
#line 587 "rgb_hash.gperf"
      {"gray48",               {122, 122, 122}},
#line 420 "rgb_hash.gperf"
      {"tomato3",              {205,  79,  57}},
      {""}, {""},
#line 381 "rgb_hash.gperf"
      {"wheat4",               {139, 126, 102}},
#line 567 "rgb_hash.gperf"
      {"gray38",               { 97,  97,  97}},
      {""}, {""},
#line 67 "rgb_hash.gperf"
      {"white",                {255, 255, 255}},
      {""},
#line 532 "rgb_hash.gperf"
      {"grey20",               { 51,  51,  51}},
      {""}, {""}, {""},
#line 380 "rgb_hash.gperf"
      {"wheat3",               {205, 186, 150}},
#line 512 "rgb_hash.gperf"
      {"grey10",               { 26,  26,  26}},
      {""}, {""}, {""}, {""},
#line 547 "rgb_hash.gperf"
      {"gray28",               { 71,  71,  71}},
#line 171 "rgb_hash.gperf"
      {"mediumorchid",         {186,  85, 211}},
#line 474 "rgb_hash.gperf"
      {"mediumorchid4",        {122,  55, 139}},
      {""}, {""},
#line 527 "rgb_hash.gperf"
      {"gray18",               { 46,  46,  46}},
      {""}, {""}, {""}, {""}, {""},
#line 419 "rgb_hash.gperf"
      {"tomato2",              {238,  92,  66}},
#line 473 "rgb_hash.gperf"
      {"mediumorchid3",        {180,  82, 205}},
      {""},
#line 89 "rgb_hash.gperf"
      {"mediumblue",           {  0,   0, 205}},
#line 107 "rgb_hash.gperf"
      {"mediumaquamarine",     {102, 205, 170}},
      {""}, {""}, {""},
#line 505 "rgb_hash.gperf"
      {"gray7",                { 18,  18,  18}},
#line 585 "rgb_hash.gperf"
      {"gray47",               {120, 120, 120}},
#line 418 "rgb_hash.gperf"
      {"tomato1",              {255,  99,  71}},
      {""}, {""},
#line 379 "rgb_hash.gperf"
      {"wheat2",               {238, 216, 174}},
#line 565 "rgb_hash.gperf"
      {"gray37",               { 94,  94,  94}},
      {""}, {""},
#line 365 "rgb_hash.gperf"
      {"rosybrown4",           {139, 105, 105}},
#line 128 "rgb_hash.gperf"
      {"palegoldenrod",        {238, 232, 170}},
      {""}, {""}, {""},
#line 364 "rgb_hash.gperf"
      {"rosybrown3",           {205, 155, 155}},
#line 378 "rgb_hash.gperf"
      {"wheat1",               {255, 231, 186}},
#line 170 "rgb_hash.gperf"
      {"orchid",               {218, 112, 214}},
#line 466 "rgb_hash.gperf"
      {"orchid4",              {139,  71, 137}},
      {""}, {""}, {""},
#line 545 "rgb_hash.gperf"
      {"gray27",               { 69,  69,  69}},
      {""},
#line 472 "rgb_hash.gperf"
      {"mediumorchid2",        {209,  95, 238}},
      {""}, {""},
#line 525 "rgb_hash.gperf"
      {"gray17",               { 43,  43,  43}},
#line 465 "rgb_hash.gperf"
      {"orchid3",              {205, 105, 201}},
      {""},
#line 363 "rgb_hash.gperf"
      {"rosybrown2",           {238, 180, 180}},
#line 491 "rgb_hash.gperf"
      {"gray0",                {  0,   0,   0}},
#line 571 "rgb_hash.gperf"
      {"gray40",               {102, 102, 102}},
      {""},
#line 471 "rgb_hash.gperf"
      {"mediumorchid1",        {224, 102, 255}},
#line 362 "rgb_hash.gperf"
      {"rosybrown1",           {255, 193, 193}},
#line 87 "rgb_hash.gperf"
      {"mediumslateblue",      {123, 104, 238}},
#line 551 "rgb_hash.gperf"
      {"gray30",               { 77,  77,  77}},
      {""},
#line 217 "rgb_hash.gperf"
      {"honeydew4",            {131, 139, 131}},
      {""},
#line 168 "rgb_hash.gperf"
      {"violet",               {238, 130, 238}},
      {""}, {""},
#line 216 "rgb_hash.gperf"
      {"honeydew3",            {193, 205, 193}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 531 "rgb_hash.gperf"
      {"gray20",               { 51,  51,  51}},
      {""}, {""}, {""}, {""},
#line 511 "rgb_hash.gperf"
      {"gray10",               { 26,  26,  26}},
#line 464 "rgb_hash.gperf"
      {"orchid2",              {238, 122, 233}},
#line 215 "rgb_hash.gperf"
      {"honeydew2",            {224, 238, 224}},
      {""},
#line 121 "rgb_hash.gperf"
      {"greenyellow",          {173, 255,  47}},
#line 93 "rgb_hash.gperf"
      {"deepskyblue",          {  0, 191, 255}},
#line 253 "rgb_hash.gperf"
      {"deepskyblue4",         {  0, 104, 139}},
#line 214 "rgb_hash.gperf"
      {"honeydew1",            {240, 255, 240}},
      {""}, {""}, {""},
#line 463 "rgb_hash.gperf"
      {"orchid1",              {255, 131, 250}},
      {""}, {""}, {""},
#line 337 "rgb_hash.gperf"
      {"khaki4",               {139, 134,  78}},
#line 252 "rgb_hash.gperf"
      {"deepskyblue3",         {  0, 154, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 336 "rgb_hash.gperf"
      {"khaki3",               {205, 198, 115}},
      {""}, {""},
#line 173 "rgb_hash.gperf"
      {"darkviolet",           {148,   0, 211}},
#line 155 "rgb_hash.gperf"
      {"lightcoral",           {240, 128, 128}},
      {""}, {""}, {""}, {""}, {""},
#line 349 "rgb_hash.gperf"
      {"yellow4",              {139, 139,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 348 "rgb_hash.gperf"
      {"yellow3",              {205, 205,   0}},
#line 251 "rgb_hash.gperf"
      {"deepskyblue2",         {  0, 178, 238}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 335 "rgb_hash.gperf"
      {"khaki2",               {238, 230, 133}},
#line 250 "rgb_hash.gperf"
      {"deepskyblue1",         {  0, 191, 255}},
#line 696 "rgb_hash.gperf"
      {"darkcyan",             {  0, 139, 139}},
      {""}, {""},
#line 690 "rgb_hash.gperf"
      {"grey99",               {252, 252, 252}},
      {""}, {""},
#line 51 "rgb_hash.gperf"
      {"blanchedalmond",       {255, 235, 205}},
      {""},
#line 334 "rgb_hash.gperf"
      {"khaki1",               {255, 246, 143}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 347 "rgb_hash.gperf"
      {"yellow2",              {238, 238,   0}},
      {""}, {""}, {""},
#line 46 "rgb_hash.gperf"
      {"floralwhite",          {255, 250, 240}},
      {""}, {""}, {""}, {""}, {""},
#line 346 "rgb_hash.gperf"
      {"yellow1",              {255, 255,   0}},
      {""}, {""}, {""}, {""},
#line 156 "rgb_hash.gperf"
      {"tomato",               {255,  99,  71}},
      {""}, {""}, {""},
#line 119 "rgb_hash.gperf"
      {"chartreuse",           {127, 255,   0}},
#line 325 "rgb_hash.gperf"
      {"chartreuse4",          { 69, 139,   0}},
      {""}, {""}, {""}, {""},
#line 324 "rgb_hash.gperf"
      {"chartreuse3",          {102, 205,   0}},
      {""}, {""}, {""},
#line 277 "rgb_hash.gperf"
      {"lightcyan4",           {122, 139, 139}},
      {""}, {""}, {""}, {""},
#line 276 "rgb_hash.gperf"
      {"lightcyan3",           {180, 205, 205}},
      {""}, {""},
#line 136 "rgb_hash.gperf"
      {"rosybrown",            {188, 143, 143}},
      {""},
#line 393 "rgb_hash.gperf"
      {"firebrick4",           {139,  26,  26}},
#line 323 "rgb_hash.gperf"
      {"chartreuse2",          {118, 238,   0}},
      {""}, {""}, {""},
#line 392 "rgb_hash.gperf"
      {"firebrick3",           {205,  38,  38}},
#line 322 "rgb_hash.gperf"
      {"chartreuse1",          {127, 255,   0}},
      {""}, {""}, {""},
#line 275 "rgb_hash.gperf"
      {"lightcyan2",           {209, 238, 238}},
      {""}, {""}, {""}, {""},
#line 274 "rgb_hash.gperf"
      {"lightcyan1",           {224, 255, 255}},
      {""},
#line 610 "rgb_hash.gperf"
      {"grey59",               {150, 150, 150}},
      {""}, {""},
#line 391 "rgb_hash.gperf"
      {"firebrick2",           {238,  44,  44}},
      {""}, {""}, {""}, {""},
#line 390 "rgb_hash.gperf"
      {"firebrick1",           {255,  48,  48}},
#line 689 "rgb_hash.gperf"
      {"gray99",               {252, 252, 252}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 100 "rgb_hash.gperf"
      {"paleturquoise",        {175, 238, 238}},
#line 281 "rgb_hash.gperf"
      {"paleturquoise4",       {102, 139, 139}},
      {""}, {""}, {""}, {""},
#line 280 "rgb_hash.gperf"
      {"paleturquoise3",       {150, 205, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 169 "rgb_hash.gperf"
      {"plum",                 {221, 160, 221}},
      {""}, {""}, {""}, {""},
#line 279 "rgb_hash.gperf"
      {"paleturquoise2",       {174, 238, 238}},
      {""}, {""}, {""}, {""},
#line 278 "rgb_hash.gperf"
      {"paleturquoise1",       {187, 255, 255}},
      {""}, {""}, {""}, {""},
#line 670 "rgb_hash.gperf"
      {"grey89",               {227, 227, 227}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 95 "rgb_hash.gperf"
      {"lightskyblue",         {135, 206, 250}},
#line 261 "rgb_hash.gperf"
      {"lightskyblue4",        { 96, 123, 139}},
      {""},
#line 123 "rgb_hash.gperf"
      {"yellowgreen",          {154, 205,  50}},
      {""}, {""},
#line 682 "rgb_hash.gperf"
      {"grey95",               {242, 242, 242}},
      {""}, {""},
#line 650 "rgb_hash.gperf"
      {"grey79",               {201, 201, 201}},
      {""},
#line 260 "rgb_hash.gperf"
      {"lightskyblue3",        {141, 182, 205}},
      {""},
#line 130 "rgb_hash.gperf"
      {"lightyellow",          {255, 255, 224}},
      {""},
#line 609 "rgb_hash.gperf"
      {"gray59",               {150, 150, 150}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 106 "rgb_hash.gperf"
      {"cadetblue",            { 95, 158, 160}},
#line 285 "rgb_hash.gperf"
      {"cadetblue4",           { 83, 134, 139}},
      {""}, {""}, {""}, {""},
#line 284 "rgb_hash.gperf"
      {"cadetblue3",           {122, 197, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 259 "rgb_hash.gperf"
      {"lightskyblue2",        {164, 211, 238}},
#line 143 "rgb_hash.gperf"
      {"wheat",                {245, 222, 179}},
      {""}, {""}, {""}, {""}, {""},
#line 283 "rgb_hash.gperf"
      {"cadetblue2",           {142, 229, 238}},
      {""}, {""},
#line 258 "rgb_hash.gperf"
      {"lightskyblue1",        {176, 226, 255}},
      {""},
#line 282 "rgb_hash.gperf"
      {"cadetblue1",           {152, 245, 255}},
      {""}, {""}, {""}, {""},
#line 504 "rgb_hash.gperf"
      {"grey6",                { 15,  15,  15}},
#line 584 "rgb_hash.gperf"
      {"grey46",               {117, 117, 117}},
      {""}, {""}, {""}, {""},
#line 564 "rgb_hash.gperf"
      {"grey36",               { 92,  92,  92}},
      {""}, {""},
#line 105 "rgb_hash.gperf"
      {"lightcyan",            {224, 255, 255}},
      {""},
#line 669 "rgb_hash.gperf"
      {"gray89",               {227, 227, 227}},
      {""}, {""},
#line 146 "rgb_hash.gperf"
      {"chocolate",            {210, 105,  30}},
#line 389 "rgb_hash.gperf"
      {"chocolate4",           {139,  69,  19}},
      {""}, {""}, {""},
#line 602 "rgb_hash.gperf"
      {"grey55",               {140, 140, 140}},
#line 388 "rgb_hash.gperf"
      {"chocolate3",           {205, 102,  29}},
#line 544 "rgb_hash.gperf"
      {"grey26",               { 66,  66,  66}},
      {""}, {""}, {""}, {""},
#line 524 "rgb_hash.gperf"
      {"grey16",               { 41,  41,  41}},
      {""},
#line 681 "rgb_hash.gperf"
      {"gray95",               {242, 242, 242}},
      {""}, {""},
#line 649 "rgb_hash.gperf"
      {"gray79",               {201, 201, 201}},
      {""}, {""}, {""},
#line 387 "rgb_hash.gperf"
      {"chocolate2",           {238, 118,  33}},
#line 49 "rgb_hash.gperf"
      {"antiquewhite",         {250, 235, 215}},
#line 189 "rgb_hash.gperf"
      {"antiquewhite4",        {139, 131, 120}},
      {""}, {""},
#line 386 "rgb_hash.gperf"
      {"chocolate1",           {255, 127,  36}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 188 "rgb_hash.gperf"
      {"antiquewhite3",        {205, 192, 176}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 688 "rgb_hash.gperf"
      {"grey98",               {250, 250, 250}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 662 "rgb_hash.gperf"
      {"grey85",               {217, 217, 217}},
      {""}, {""}, {""},
#line 187 "rgb_hash.gperf"
      {"antiquewhite2",        {238, 223, 204}},
      {""}, {""},
#line 503 "rgb_hash.gperf"
      {"gray6",                { 15,  15,  15}},
#line 583 "rgb_hash.gperf"
      {"gray46",               {117, 117, 117}},
      {""}, {""}, {""}, {""},
#line 563 "rgb_hash.gperf"
      {"gray36",               { 92,  92,  92}},
#line 186 "rgb_hash.gperf"
      {"antiquewhite1",        {255, 239, 219}},
      {""}, {""}, {""}, {""}, {""},
#line 642 "rgb_hash.gperf"
      {"grey75",               {191, 191, 191}},
      {""},
#line 127 "rgb_hash.gperf"
      {"khaki",                {240, 230, 140}},
#line 686 "rgb_hash.gperf"
      {"grey97",               {247, 247, 247}},
#line 446 "rgb_hash.gperf"
      {"lightpink4",           {139,  95, 101}},
      {""},
#line 601 "rgb_hash.gperf"
      {"gray55",               {140, 140, 140}},
      {""},
#line 543 "rgb_hash.gperf"
      {"gray26",               { 66,  66,  66}},
#line 445 "rgb_hash.gperf"
      {"lightpink3",           {205, 140, 149}},
      {""}, {""}, {""},
#line 523 "rgb_hash.gperf"
      {"gray16",               { 41,  41,  41}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 444 "rgb_hash.gperf"
      {"lightpink2",           {238, 162, 173}},
#line 141 "rgb_hash.gperf"
      {"burlywood",            {222, 184, 135}},
#line 377 "rgb_hash.gperf"
      {"burlywood4",           {139, 115,  85}},
      {""}, {""},
#line 443 "rgb_hash.gperf"
      {"lightpink1",           {255, 174, 185}},
      {""},
#line 376 "rgb_hash.gperf"
      {"burlywood3",           {205, 170, 125}},
      {""},
#line 672 "rgb_hash.gperf"
      {"grey90",               {229, 229, 229}},
#line 608 "rgb_hash.gperf"
      {"grey58",               {148, 148, 148}},
      {""},
#line 129 "rgb_hash.gperf"
      {"lightgoldenrodyellow", {250, 250, 210}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 687 "rgb_hash.gperf"
      {"gray98",               {250, 250, 250}},
      {""}, {""},
#line 375 "rgb_hash.gperf"
      {"burlywood2",           {238, 197, 145}},
      {""}, {""}, {""}, {""},
#line 374 "rgb_hash.gperf"
      {"burlywood1",           {255, 211, 155}},
      {""},
#line 630 "rgb_hash.gperf"
      {"grey69",               {176, 176, 176}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 661 "rgb_hash.gperf"
      {"gray85",               {217, 217, 217}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 606 "rgb_hash.gperf"
      {"grey57",               {145, 145, 145}},
      {""}, {""}, {""}, {""}, {""},
#line 641 "rgb_hash.gperf"
      {"gray75",               {191, 191, 191}},
      {""}, {""},
#line 685 "rgb_hash.gperf"
      {"gray97",               {247, 247, 247}},
      {""}, {""}, {""}, {""},
#line 668 "rgb_hash.gperf"
      {"grey88",               {224, 224, 224}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 438 "rgb_hash.gperf"
      {"hotpink4",             {139,  58,  98}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 592 "rgb_hash.gperf"
      {"grey50",               {127, 127, 127}},
#line 437 "rgb_hash.gperf"
      {"hotpink3",             {205,  96, 144}},
#line 174 "rgb_hash.gperf"
      {"blueviolet",           {138,  43, 226}},
      {""},
#line 648 "rgb_hash.gperf"
      {"grey78",               {199, 199, 199}},
      {""}, {""},
#line 43 "rgb_hash.gperf"
      {"ghostwhite",           {248, 248, 255}},
      {""},
#line 671 "rgb_hash.gperf"
      {"gray90",               {229, 229, 229}},
#line 607 "rgb_hash.gperf"
      {"gray58",               {148, 148, 148}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 666 "rgb_hash.gperf"
      {"grey87",               {222, 222, 222}},
      {""}, {""},
#line 54 "rgb_hash.gperf"
      {"navajowhite",          {255, 222, 173}},
#line 201 "rgb_hash.gperf"
      {"navajowhite4",         {139, 121,  94}},
#line 629 "rgb_hash.gperf"
      {"gray69",               {176, 176, 176}},
      {""},
#line 436 "rgb_hash.gperf"
      {"hotpink2",             {238, 106, 167}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 200 "rgb_hash.gperf"
      {"navajowhite3",         {205, 179, 139}},
      {""}, {""},
#line 435 "rgb_hash.gperf"
      {"hotpink1",             {255, 110, 180}},
      {""}, {""},
#line 646 "rgb_hash.gperf"
      {"grey77",               {196, 196, 196}},
      {""}, {""}, {""}, {""}, {""},
#line 605 "rgb_hash.gperf"
      {"gray57",               {145, 145, 145}},
      {""}, {""}, {""},
#line 652 "rgb_hash.gperf"
      {"grey80",               {204, 204, 204}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 667 "rgb_hash.gperf"
      {"gray88",               {224, 224, 224}},
      {""}, {""}, {""},
#line 199 "rgb_hash.gperf"
      {"navajowhite2",         {238, 207, 161}},
      {""}, {""}, {""}, {""}, {""},
#line 632 "rgb_hash.gperf"
      {"grey70",               {179, 179, 179}},
      {""}, {""}, {""},
#line 198 "rgb_hash.gperf"
      {"navajowhite1",         {255, 222, 173}},
      {""},
#line 591 "rgb_hash.gperf"
      {"gray50",               {127, 127, 127}},
      {""}, {""}, {""},
#line 647 "rgb_hash.gperf"
      {"gray78",               {199, 199, 199}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 622 "rgb_hash.gperf"
      {"grey65",               {166, 166, 166}},
#line 61 "rgb_hash.gperf"
      {"mintcream",            {245, 255, 250}},
      {""}, {""}, {""}, {""},
#line 44 "rgb_hash.gperf"
      {"whitesmoke",           {245, 245, 245}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 665 "rgb_hash.gperf"
      {"gray87",               {222, 222, 222}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 692 "rgb_hash.gperf"
      {"grey100",              {255, 255, 255}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 645 "rgb_hash.gperf"
      {"gray77",               {196, 196, 196}},
      {""},
#line 205 "rgb_hash.gperf"
      {"lemonchiffon4",        {139, 137, 112}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 651 "rgb_hash.gperf"
      {"gray80",               {204, 204, 204}},
      {""},
#line 204 "rgb_hash.gperf"
      {"lemonchiffon3",        {205, 201, 165}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 55 "rgb_hash.gperf"
      {"moccasin",             {255, 228, 181}},
      {""}, {""},
#line 631 "rgb_hash.gperf"
      {"gray70",               {179, 179, 179}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 203 "rgb_hash.gperf"
      {"lemonchiffon2",        {238, 233, 191}},
#line 163 "rgb_hash.gperf"
      {"palevioletred",        {219, 112, 147}},
#line 450 "rgb_hash.gperf"
      {"palevioletred4",       {139,  71,  93}},
      {""}, {""},
#line 621 "rgb_hash.gperf"
      {"gray65",               {166, 166, 166}},
      {""},
#line 449 "rgb_hash.gperf"
      {"palevioletred3",       {205, 104, 137}},
      {""}, {""},
#line 202 "rgb_hash.gperf"
      {"lemonchiffon1",        {255, 250, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 448 "rgb_hash.gperf"
      {"palevioletred2",       {238, 121, 159}},
#line 161 "rgb_hash.gperf"
      {"pink",                 {255, 192, 203}},
      {""}, {""}, {""},
#line 447 "rgb_hash.gperf"
      {"palevioletred1",       {255, 130, 171}},
      {""},
#line 691 "rgb_hash.gperf"
      {"gray100",              {255, 255, 255}},
      {""}, {""}, {""},
#line 628 "rgb_hash.gperf"
      {"grey68",               {173, 173, 173}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 99 "rgb_hash.gperf"
      {"powderblue",           {176, 224, 230}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 626 "rgb_hash.gperf"
      {"grey67",               {171, 171, 171}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 612 "rgb_hash.gperf"
      {"grey60",               {153, 153, 153}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 627 "rgb_hash.gperf"
      {"gray68",               {173, 173, 173}},
      {""}, {""}, {""}, {""}, {""},
#line 58 "rgb_hash.gperf"
      {"lemonchiffon",         {255, 250, 205}},
      {""}, {""}, {""},
#line 684 "rgb_hash.gperf"
      {"grey96",               {245, 245, 245}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 625 "rgb_hash.gperf"
      {"gray67",               {171, 171, 171}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 120 "rgb_hash.gperf"
      {"mediumspringgreen",    {  0, 250, 154}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 209 "rgb_hash.gperf"
      {"cornsilk4",            {139, 136, 120}},
      {""},
#line 611 "rgb_hash.gperf"
      {"gray60",               {153, 153, 153}},
      {""}, {""},
#line 208 "rgb_hash.gperf"
      {"cornsilk3",            {205, 200, 177}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 604 "rgb_hash.gperf"
      {"grey56",               {143, 143, 143}},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 207 "rgb_hash.gperf"
      {"cornsilk2",            {238, 232, 205}},
      {""},
#line 683 "rgb_hash.gperf"
      {"gray96",               {245, 245, 245}},
      {""}, {""},
#line 206 "rgb_hash.gperf"
      {"cornsilk1",            {255, 248, 220}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 102 "rgb_hash.gperf"
      {"mediumturquoise",      { 72, 209, 204}},
      {""}, {""}, {""}, {""},
#line 68 "rgb_hash.gperf"
      {"black",                {  0,   0,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 176 "rgb_hash.gperf"
      {"mediumpurple",         {147, 112, 219}},
#line 486 "rgb_hash.gperf"
      {"mediumpurple4",        { 93,  71, 139}},
#line 664 "rgb_hash.gperf"
      {"grey86",               {219, 219, 219}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 485 "rgb_hash.gperf"
      {"mediumpurple3",        {137, 104, 205}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 644 "rgb_hash.gperf"
      {"grey76",               {194, 194, 194}},
      {""}, {""},
#line 197 "rgb_hash.gperf"
      {"peachpuff4",           {139, 119, 101}},
      {""}, {""},
#line 603 "rgb_hash.gperf"
      {"gray56",               {143, 143, 143}},
      {""},
#line 196 "rgb_hash.gperf"
      {"peachpuff3",           {205, 175, 149}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 484 "rgb_hash.gperf"
      {"mediumpurple2",        {159, 121, 238}},
      {""}, {""}, {""},
#line 195 "rgb_hash.gperf"
      {"peachpuff2",           {238, 203, 173}},
      {""}, {""}, {""}, {""},
#line 194 "rgb_hash.gperf"
      {"peachpuff1",           {255, 218, 185}},
#line 483 "rgb_hash.gperf"
      {"mediumpurple1",        {171, 130, 255}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 165 "rgb_hash.gperf"
      {"mediumvioletred",      {199,  21, 133}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 663 "rgb_hash.gperf"
      {"gray86",               {219, 219, 219}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 60 "rgb_hash.gperf"
      {"honeydew",             {240, 255, 240}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 643 "rgb_hash.gperf"
      {"gray76",               {194, 194, 194}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 434 "rgb_hash.gperf"
      {"deeppink4",            {139,  10,  80}},
      {""}, {""}, {""}, {""},
#line 433 "rgb_hash.gperf"
      {"deeppink3",            {205,  16, 118}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 432 "rgb_hash.gperf"
      {"deeppink2",            {238,  18, 137}},
      {""}, {""}, {""}, {""},
#line 431 "rgb_hash.gperf"
      {"deeppink1",            {255,  20, 147}},
      {""}, {""},
#line 131 "rgb_hash.gperf"
      {"yellow",               {255, 255,   0}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 126 "rgb_hash.gperf"
      {"darkkhaki",            {189, 183, 107}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 53 "rgb_hash.gperf"
      {"peachpuff",            {255, 218, 185}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 624 "rgb_hash.gperf"
      {"grey66",               {168, 168, 168}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 84 "rgb_hash.gperf"
      {"cornflowerblue",       {100, 149, 237}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 147 "rgb_hash.gperf"
      {"firebrick",            {178,  34,  34}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 623 "rgb_hash.gperf"
      {"gray66",               {168, 168, 168}},
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
      {""}, {""}, {""}, {""},
#line 162 "rgb_hash.gperf"
      {"lightpink",            {255, 182, 193}},
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
#line 159 "rgb_hash.gperf"
      {"hotpink",              {255, 105, 180}},
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
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 56 "rgb_hash.gperf"
      {"cornsilk",             {255, 248, 220}},
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
      {""},
#line 160 "rgb_hash.gperf"
      {"deeppink",             {255,  20, 147}},
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
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 50 "rgb_hash.gperf"
      {"papayawhip",           {255, 239, 213}}
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
