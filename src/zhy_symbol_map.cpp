/* C++ code produced by gperf version 3.0.3 */
/* Command-line: gperf -L C++ -t zhy_symbol_map  */
/* Computed positions: -k'1-5,$' */

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
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 1 "zhy_symbol_map"

#include "stdafx.h"
#include "phonetic_symbol.h"
#include "zhy_symbol_map.h"
#line 6 "zhy_symbol_map"
struct ekho::SymbolCode;

#define TOTAL_KEYWORDS 5012
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 7
#define MIN_HASH_VALUE 62
#define MAX_HASH_VALUE 62481
/* maximum key range = 62420, duplicates = 0 */

inline unsigned int
ZHY_PHash::hash (register const char *str, register unsigned int len){
  static unsigned short asso_values[] =
    {
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,    25,
         15,    10,  6050,    20,     5,     0,  6175,  2228,     3,   175,
      62482, 62482, 62482,  1643,  1408, 13877,  4340,  7870,  1680,  1520,
          0,     0, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482,  2395, 14392,  6085,
       6393,  7837, 22884,  2135, 10627,    20, 15522,    65,  3715,   695,
       1325,    60, 12637,     5,   560,   380,  2965,  8992, 62482,  1010,
      15862,     0,  4465, 14809,   120,  8030, 15837,   140,    65, 62482,
       7551,  5965,    60,  4585, 62482, 20604, 62482, 13027, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482, 62482,
      62482, 62482
    };
  register int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[4]];
      /*FALLTHROUGH*/
      case 4:
        hval += asso_values[(unsigned char)str[3]+4];
      /*FALLTHROUGH*/
      case 3:
        hval += asso_values[(unsigned char)str[2]+14];
      /*FALLTHROUGH*/
      case 2:
        hval += asso_values[(unsigned char)str[1]+16];
      /*FALLTHROUGH*/
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

struct ekho::SymbolCode *
ZHY_PHash::in_word_set (register const char *str, register unsigned int len){
  static struct ekho::SymbolCode wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3654 "zhy_symbol_map"
      {"o7", 3646},
      {""}, {""}, {""}, {""},
#line 3653 "zhy_symbol_map"
      {"o6", 3645},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2365 "zhy_symbol_map"
      {"kik6", 2357},
      {""},
#line 2362 "zhy_symbol_map"
      {"kik3", 2354},
      {""}, {""},
#line 2274 "zhy_symbol_map"
      {"kak6", 2266},
      {""},
#line 2271 "zhy_symbol_map"
      {"kak3", 2263},
      {""}, {""}, {""}, {""},
#line 2361 "zhy_symbol_map"
      {"kik2", 2353},
      {""}, {""}, {""}, {""},
#line 2270 "zhy_symbol_map"
      {"kak2", 2262},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2360 "zhy_symbol_map"
      {"kik1", 2352},
      {""}, {""}, {""}, {""},
#line 2269 "zhy_symbol_map"
      {"kak1", 2261},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2261 "zhy_symbol_map"
      {"kaau7", 2253},
      {""}, {""}, {""}, {""},
#line 2233 "zhy_symbol_map"
      {"kaam7", 2225},
      {""},
#line 2211 "zhy_symbol_map"
      {"kaa6", 2203},
      {""},
#line 2208 "zhy_symbol_map"
      {"kaa3", 2200},
#line 2260 "zhy_symbol_map"
      {"kaau6", 2252},
      {""}, {""}, {""}, {""},
#line 2232 "zhy_symbol_map"
      {"kaam6", 2224},
      {""}, {""}, {""},
#line 2207 "zhy_symbol_map"
      {"kaa2", 2199},
#line 2257 "zhy_symbol_map"
      {"kaau3", 2249},
      {""}, {""}, {""}, {""},
#line 2229 "zhy_symbol_map"
      {"kaam3", 2221},
      {""}, {""}, {""}, {""},
#line 2256 "zhy_symbol_map"
      {"kaau2", 2248},
      {""}, {""}, {""}, {""},
#line 2228 "zhy_symbol_map"
      {"kaam2", 2220},
      {""}, {""}, {""}, {""},
#line 2259 "zhy_symbol_map"
      {"kaau5", 2251},
      {""}, {""}, {""},
#line 2206 "zhy_symbol_map"
      {"kaa1", 2198},
#line 2231 "zhy_symbol_map"
      {"kaam5", 2223},
      {""}, {""}, {""}, {""},
#line 2255 "zhy_symbol_map"
      {"kaau1", 2247},
      {""}, {""}, {""}, {""},
#line 2227 "zhy_symbol_map"
      {"kaam1", 2219},
      {""}, {""}, {""}, {""},
#line 2226 "zhy_symbol_map"
      {"kaak7", 2218},
      {""},
#line 2379 "zhy_symbol_map"
      {"kin6", 2371},
      {""},
#line 2376 "zhy_symbol_map"
      {"kin3", 2368},
      {""}, {""},
#line 2288 "zhy_symbol_map"
      {"kan6", 2280},
      {""},
#line 2285 "zhy_symbol_map"
      {"kan3", 2277},
#line 2225 "zhy_symbol_map"
      {"kaak6", 2217},
      {""}, {""}, {""},
#line 2375 "zhy_symbol_map"
      {"kin2", 2367},
      {""}, {""}, {""}, {""},
#line 2284 "zhy_symbol_map"
      {"kan2", 2276},
#line 2222 "zhy_symbol_map"
      {"kaak3", 2214},
      {""},
#line 2449 "zhy_symbol_map"
      {"kok6", 2441},
      {""},
#line 2446 "zhy_symbol_map"
      {"kok3", 2438},
      {""}, {""}, {""}, {""}, {""},
#line 2221 "zhy_symbol_map"
      {"kaak2", 2213},
      {""}, {""}, {""},
#line 2445 "zhy_symbol_map"
      {"kok2", 2437},
      {""}, {""}, {""}, {""},
#line 2374 "zhy_symbol_map"
      {"kin1", 2366},
#line 2224 "zhy_symbol_map"
      {"kaak5", 2216},
      {""}, {""}, {""},
#line 2283 "zhy_symbol_map"
      {"kan1", 2275},
      {""}, {""}, {""}, {""},
#line 2366 "zhy_symbol_map"
      {"kik7", 2358},
#line 2220 "zhy_symbol_map"
      {"kaak1", 2212},
      {""}, {""}, {""},
#line 2275 "zhy_symbol_map"
      {"kak7", 2267},
      {""}, {""}, {""}, {""},
#line 2444 "zhy_symbol_map"
      {"kok1", 2436},
#line 2387 "zhy_symbol_map"
      {"king7", 2379},
      {""}, {""}, {""}, {""},
#line 2296 "zhy_symbol_map"
      {"kang7", 2288},
      {""}, {""}, {""}, {""},
#line 2386 "zhy_symbol_map"
      {"king6", 2378},
      {""}, {""}, {""}, {""},
#line 2295 "zhy_symbol_map"
      {"kang6", 2287},
      {""}, {""}, {""}, {""},
#line 2383 "zhy_symbol_map"
      {"king3", 2375},
      {""}, {""}, {""}, {""},
#line 2292 "zhy_symbol_map"
      {"kang3", 2284},
      {""}, {""}, {""}, {""},
#line 2382 "zhy_symbol_map"
      {"king2", 2374},
      {""}, {""}, {""}, {""},
#line 2291 "zhy_symbol_map"
      {"kang2", 2283},
      {""}, {""}, {""}, {""},
#line 2385 "zhy_symbol_map"
      {"king5", 2377},
      {""}, {""}, {""}, {""},
#line 2294 "zhy_symbol_map"
      {"kang5", 2286},
      {""}, {""}, {""}, {""},
#line 2381 "zhy_symbol_map"
      {"king1", 2373},
      {""}, {""}, {""},
#line 2212 "zhy_symbol_map"
      {"kaa7", 2204},
#line 2290 "zhy_symbol_map"
      {"kang1", 2282},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2380 "zhy_symbol_map"
      {"kin7", 2372},
      {""}, {""}, {""}, {""},
#line 2289 "zhy_symbol_map"
      {"kan7", 2281},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2450 "zhy_symbol_map"
      {"kok7", 2442},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4108 "zhy_symbol_map"
      {"sik6", 4100},
      {""},
#line 4105 "zhy_symbol_map"
      {"sik3", 4097},
#line 2457 "zhy_symbol_map"
      {"kong7", 2449},
      {""},
#line 4003 "zhy_symbol_map"
      {"sak6", 3995},
      {""},
#line 4000 "zhy_symbol_map"
      {"sak3", 3992},
      {""}, {""}, {""}, {""},
#line 4104 "zhy_symbol_map"
      {"sik2", 4096},
#line 2456 "zhy_symbol_map"
      {"kong6", 2448},
      {""}, {""}, {""},
#line 3999 "zhy_symbol_map"
      {"sak2", 3991},
      {""}, {""}, {""}, {""}, {""},
#line 2453 "zhy_symbol_map"
      {"kong3", 2445},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2452 "zhy_symbol_map"
      {"kong2", 2444},
      {""}, {""}, {""},
#line 4103 "zhy_symbol_map"
      {"sik1", 4095},
      {""}, {""}, {""}, {""},
#line 3998 "zhy_symbol_map"
      {"sak1", 3990},
#line 2455 "zhy_symbol_map"
      {"kong5", 2447},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2451 "zhy_symbol_map"
      {"kong1", 2443},
      {""}, {""}, {""}, {""},
#line 3990 "zhy_symbol_map"
      {"saau7", 3982},
      {""}, {""}, {""}, {""},
#line 3955 "zhy_symbol_map"
      {"saam7", 3947},
      {""},
#line 3933 "zhy_symbol_map"
      {"saa6", 3925},
      {""},
#line 3930 "zhy_symbol_map"
      {"saa3", 3922},
#line 3989 "zhy_symbol_map"
      {"saau6", 3981},
      {""}, {""}, {""}, {""},
#line 3954 "zhy_symbol_map"
      {"saam6", 3946},
      {""}, {""}, {""},
#line 3929 "zhy_symbol_map"
      {"saa2", 3921},
#line 3986 "zhy_symbol_map"
      {"saau3", 3978},
      {""}, {""}, {""}, {""},
#line 3951 "zhy_symbol_map"
      {"saam3", 3943},
      {""}, {""}, {""}, {""},
#line 3985 "zhy_symbol_map"
      {"saau2", 3977},
      {""}, {""}, {""}, {""},
#line 3950 "zhy_symbol_map"
      {"saam2", 3942},
      {""}, {""}, {""}, {""},
#line 3988 "zhy_symbol_map"
      {"saau5", 3980},
      {""}, {""}, {""},
#line 3928 "zhy_symbol_map"
      {"saa1", 3920},
#line 3953 "zhy_symbol_map"
      {"saam5", 3945},
      {""}, {""}, {""}, {""},
#line 3984 "zhy_symbol_map"
      {"saau1", 3976},
      {""}, {""}, {""}, {""},
#line 3949 "zhy_symbol_map"
      {"saam1", 3941},
      {""}, {""}, {""}, {""},
#line 3948 "zhy_symbol_map"
      {"saak7", 3940},
      {""},
#line 4122 "zhy_symbol_map"
      {"sin6", 4114},
      {""},
#line 4119 "zhy_symbol_map"
      {"sin3", 4111},
      {""}, {""},
#line 4017 "zhy_symbol_map"
      {"san6", 4009},
      {""},
#line 4014 "zhy_symbol_map"
      {"san3", 4006},
#line 3947 "zhy_symbol_map"
      {"saak6", 3939},
      {""}, {""}, {""},
#line 4118 "zhy_symbol_map"
      {"sin2", 4110},
      {""}, {""}, {""}, {""},
#line 4013 "zhy_symbol_map"
      {"san2", 4005},
#line 3944 "zhy_symbol_map"
      {"saak3", 3936},
      {""},
#line 4199 "zhy_symbol_map"
      {"sok6", 4191},
      {""},
#line 4196 "zhy_symbol_map"
      {"sok3", 4188},
      {""}, {""}, {""}, {""}, {""},
#line 3943 "zhy_symbol_map"
      {"saak2", 3935},
      {""}, {""}, {""},
#line 4195 "zhy_symbol_map"
      {"sok2", 4187},
      {""}, {""}, {""}, {""},
#line 4117 "zhy_symbol_map"
      {"sin1", 4109},
#line 3946 "zhy_symbol_map"
      {"saak5", 3938},
      {""}, {""}, {""},
#line 4012 "zhy_symbol_map"
      {"san1", 4004},
      {""}, {""}, {""}, {""},
#line 4109 "zhy_symbol_map"
      {"sik7", 4101},
#line 3942 "zhy_symbol_map"
      {"saak1", 3934},
      {""}, {""}, {""},
#line 4004 "zhy_symbol_map"
      {"sak7", 3996},
      {""}, {""}, {""}, {""},
#line 4194 "zhy_symbol_map"
      {"sok1", 4186},
#line 4130 "zhy_symbol_map"
      {"sing7", 4122},
      {""}, {""}, {""}, {""},
#line 4025 "zhy_symbol_map"
      {"sang7", 4017},
      {""}, {""}, {""}, {""},
#line 4129 "zhy_symbol_map"
      {"sing6", 4121},
      {""}, {""}, {""}, {""},
#line 4024 "zhy_symbol_map"
      {"sang6", 4016},
      {""}, {""}, {""}, {""},
#line 4126 "zhy_symbol_map"
      {"sing3", 4118},
      {""}, {""}, {""}, {""},
#line 4021 "zhy_symbol_map"
      {"sang3", 4013},
      {""},
#line 2421 "zhy_symbol_map"
      {"koe6", 2413},
      {""},
#line 2418 "zhy_symbol_map"
      {"koe3", 2410},
#line 4125 "zhy_symbol_map"
      {"sing2", 4117},
      {""}, {""}, {""}, {""},
#line 4020 "zhy_symbol_map"
      {"sang2", 4012},
      {""}, {""}, {""},
#line 2417 "zhy_symbol_map"
      {"koe2", 2409},
#line 4128 "zhy_symbol_map"
      {"sing5", 4120},
      {""}, {""}, {""}, {""},
#line 4023 "zhy_symbol_map"
      {"sang5", 4015},
      {""}, {""}, {""}, {""},
#line 4124 "zhy_symbol_map"
      {"sing1", 4116},
      {""}, {""}, {""},
#line 3934 "zhy_symbol_map"
      {"saa7", 3926},
#line 4019 "zhy_symbol_map"
      {"sang1", 4011},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2416 "zhy_symbol_map"
      {"koe1", 2408},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2429 "zhy_symbol_map"
      {"koek7", 2421},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2428 "zhy_symbol_map"
      {"koek6", 2420},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2425 "zhy_symbol_map"
      {"koek3", 2417},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4123 "zhy_symbol_map"
      {"sin7", 4115},
#line 2424 "zhy_symbol_map"
      {"koek2", 2416},
      {""}, {""}, {""},
#line 4018 "zhy_symbol_map"
      {"san7", 4010},
      {""}, {""}, {""}, {""}, {""},
#line 2427 "zhy_symbol_map"
      {"koek5", 2419},
      {""}, {""}, {""}, {""},
#line 2240 "zhy_symbol_map"
      {"kaan7", 2232},
      {""},
#line 2940 "zhy_symbol_map"
      {"m7", 2932},
      {""},
#line 4200 "zhy_symbol_map"
      {"sok7", 4192},
#line 2423 "zhy_symbol_map"
      {"koek1", 2415},
      {""},
#line 2939 "zhy_symbol_map"
      {"m6", 2931},
      {""}, {""},
#line 2239 "zhy_symbol_map"
      {"kaan6", 2231},
      {""},
#line 3072 "zhy_symbol_map"
      {"mik6", 3064},
      {""},
#line 3069 "zhy_symbol_map"
      {"mik3", 3061},
#line 4207 "zhy_symbol_map"
      {"song7", 4199},
      {""},
#line 3002 "zhy_symbol_map"
      {"mak6", 2994},
      {""},
#line 2999 "zhy_symbol_map"
      {"mak3", 2991},
#line 2236 "zhy_symbol_map"
      {"kaan3", 2228},
      {""}, {""}, {""},
#line 3068 "zhy_symbol_map"
      {"mik2", 3060},
#line 4206 "zhy_symbol_map"
      {"song6", 4198},
      {""}, {""}, {""},
#line 2998 "zhy_symbol_map"
      {"mak2", 2990},
#line 2235 "zhy_symbol_map"
      {"kaan2", 2227},
      {""}, {""}, {""}, {""},
#line 4203 "zhy_symbol_map"
      {"song3", 4195},
      {""}, {""}, {""}, {""},
#line 2238 "zhy_symbol_map"
      {"kaan5", 2230},
      {""}, {""}, {""}, {""},
#line 4202 "zhy_symbol_map"
      {"song2", 4194},
      {""}, {""}, {""},
#line 3067 "zhy_symbol_map"
      {"mik1", 3059},
#line 2234 "zhy_symbol_map"
      {"kaan1", 2226},
      {""}, {""}, {""},
#line 2997 "zhy_symbol_map"
      {"mak1", 2989},
#line 4205 "zhy_symbol_map"
      {"song5", 4197},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4201 "zhy_symbol_map"
      {"song1", 4193},
      {""}, {""}, {""},
#line 2422 "zhy_symbol_map"
      {"koe7", 2414},
#line 2989 "zhy_symbol_map"
      {"maau7", 2981},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2946 "zhy_symbol_map"
      {"maa6", 2938},
      {""},
#line 2943 "zhy_symbol_map"
      {"maa3", 2935},
#line 2988 "zhy_symbol_map"
      {"maau6", 2980},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2942 "zhy_symbol_map"
      {"maa2", 2934},
#line 2985 "zhy_symbol_map"
      {"maau3", 2977},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2984 "zhy_symbol_map"
      {"maau2", 2976},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2987 "zhy_symbol_map"
      {"maau5", 2979},
      {""}, {""}, {""},
#line 2941 "zhy_symbol_map"
      {"maa1", 2933},
      {""}, {""}, {""}, {""}, {""},
#line 2983 "zhy_symbol_map"
      {"maau1", 2975},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2961 "zhy_symbol_map"
      {"maak7", 2953},
      {""},
#line 3079 "zhy_symbol_map"
      {"min6", 3071},
      {""},
#line 3076 "zhy_symbol_map"
      {"min3", 3068},
#line 2219 "zhy_symbol_map"
      {"kaai7", 2211},
      {""},
#line 3016 "zhy_symbol_map"
      {"man6", 3008},
      {""},
#line 3013 "zhy_symbol_map"
      {"man3", 3005},
#line 2960 "zhy_symbol_map"
      {"maak6", 2952},
      {""}, {""}, {""},
#line 3075 "zhy_symbol_map"
      {"min2", 3067},
#line 2218 "zhy_symbol_map"
      {"kaai6", 2210},
      {""}, {""}, {""},
#line 3012 "zhy_symbol_map"
      {"man2", 3004},
#line 2957 "zhy_symbol_map"
      {"maak3", 2949},
      {""},
#line 3121 "zhy_symbol_map"
      {"mok6", 3113},
      {""},
#line 3118 "zhy_symbol_map"
      {"mok3", 3110},
#line 2215 "zhy_symbol_map"
      {"kaai3", 2207},
      {""}, {""}, {""}, {""},
#line 2956 "zhy_symbol_map"
      {"maak2", 2948},
      {""}, {""}, {""},
#line 3117 "zhy_symbol_map"
      {"mok2", 3109},
#line 2214 "zhy_symbol_map"
      {"kaai2", 2206},
      {""}, {""}, {""},
#line 3074 "zhy_symbol_map"
      {"min1", 3066},
#line 2959 "zhy_symbol_map"
      {"maak5", 2951},
      {""}, {""}, {""},
#line 3011 "zhy_symbol_map"
      {"man1", 3003},
#line 2217 "zhy_symbol_map"
      {"kaai5", 2209},
      {""}, {""}, {""},
#line 3073 "zhy_symbol_map"
      {"mik7", 3065},
#line 2955 "zhy_symbol_map"
      {"maak1", 2947},
      {""}, {""}, {""},
#line 3003 "zhy_symbol_map"
      {"mak7", 2995},
#line 2213 "zhy_symbol_map"
      {"kaai1", 2205},
      {""}, {""}, {""},
#line 3116 "zhy_symbol_map"
      {"mok1", 3108},
#line 3087 "zhy_symbol_map"
      {"ming7", 3079},
      {""}, {""}, {""}, {""},
#line 3024 "zhy_symbol_map"
      {"mang7", 3016},
      {""}, {""}, {""}, {""},
#line 3086 "zhy_symbol_map"
      {"ming6", 3078},
      {""}, {""}, {""}, {""},
#line 3023 "zhy_symbol_map"
      {"mang6", 3015},
      {""}, {""}, {""}, {""},
#line 3083 "zhy_symbol_map"
      {"ming3", 3075},
      {""}, {""}, {""}, {""},
#line 3020 "zhy_symbol_map"
      {"mang3", 3012},
      {""},
#line 4164 "zhy_symbol_map"
      {"soe6", 4156},
      {""},
#line 4161 "zhy_symbol_map"
      {"soe3", 4153},
#line 3082 "zhy_symbol_map"
      {"ming2", 3074},
      {""}, {""}, {""}, {""},
#line 3019 "zhy_symbol_map"
      {"mang2", 3011},
      {""}, {""}, {""},
#line 4160 "zhy_symbol_map"
      {"soe2", 4152},
#line 3085 "zhy_symbol_map"
      {"ming5", 3077},
      {""}, {""}, {""}, {""},
#line 3022 "zhy_symbol_map"
      {"mang5", 3014},
      {""}, {""}, {""}, {""},
#line 3081 "zhy_symbol_map"
      {"ming1", 3073},
      {""}, {""}, {""},
#line 2947 "zhy_symbol_map"
      {"maa7", 2939},
#line 3018 "zhy_symbol_map"
      {"mang1", 3010},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4159 "zhy_symbol_map"
      {"soe1", 4151},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4172 "zhy_symbol_map"
      {"soek7", 4164},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4171 "zhy_symbol_map"
      {"soek6", 4163},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4168 "zhy_symbol_map"
      {"soek3", 4160},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3080 "zhy_symbol_map"
      {"min7", 3072},
#line 4167 "zhy_symbol_map"
      {"soek2", 4159},
      {""}, {""}, {""},
#line 3017 "zhy_symbol_map"
      {"man7", 3009},
      {""}, {""}, {""}, {""}, {""},
#line 4170 "zhy_symbol_map"
      {"soek5", 4162},
      {""}, {""}, {""}, {""},
#line 3962 "zhy_symbol_map"
      {"saan7", 3954},
      {""}, {""}, {""},
#line 3122 "zhy_symbol_map"
      {"mok7", 3114},
#line 4166 "zhy_symbol_map"
      {"soek1", 4158},
      {""}, {""}, {""}, {""},
#line 3961 "zhy_symbol_map"
      {"saan6", 3953},
      {""},
#line 4640 "zhy_symbol_map"
      {"wik6", 4632},
      {""},
#line 4637 "zhy_symbol_map"
      {"wik3", 4629},
#line 3129 "zhy_symbol_map"
      {"mong7", 3121},
      {""},
#line 4577 "zhy_symbol_map"
      {"wak6", 4569},
      {""},
#line 4574 "zhy_symbol_map"
      {"wak3", 4566},
#line 3958 "zhy_symbol_map"
      {"saan3", 3950},
      {""}, {""}, {""},
#line 4636 "zhy_symbol_map"
      {"wik2", 4628},
#line 3128 "zhy_symbol_map"
      {"mong6", 3120},
      {""}, {""}, {""},
#line 4573 "zhy_symbol_map"
      {"wak2", 4565},
#line 3957 "zhy_symbol_map"
      {"saan2", 3949},
      {""}, {""}, {""}, {""},
#line 3125 "zhy_symbol_map"
      {"mong3", 3117},
      {""}, {""}, {""}, {""},
#line 3960 "zhy_symbol_map"
      {"saan5", 3952},
      {""}, {""}, {""}, {""},
#line 3124 "zhy_symbol_map"
      {"mong2", 3116},
      {""}, {""}, {""},
#line 4635 "zhy_symbol_map"
      {"wik1", 4627},
#line 3956 "zhy_symbol_map"
      {"saan1", 3948},
      {""}, {""}, {""},
#line 4572 "zhy_symbol_map"
      {"wak1", 4564},
#line 3127 "zhy_symbol_map"
      {"mong5", 3119},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3123 "zhy_symbol_map"
      {"mong1", 3115},
      {""}, {""}, {""},
#line 4165 "zhy_symbol_map"
      {"soe7", 4157},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4528 "zhy_symbol_map"
      {"waa6", 4520},
      {""},
#line 4525 "zhy_symbol_map"
      {"waa3", 4517},
      {""}, {""},
#line 2267 "zhy_symbol_map"
      {"kai6", 2259},
      {""},
#line 2264 "zhy_symbol_map"
      {"kai3", 2256},
      {""}, {""}, {""}, {""},
#line 4524 "zhy_symbol_map"
      {"waa2", 4516},
      {""}, {""}, {""}, {""},
#line 2263 "zhy_symbol_map"
      {"kai2", 2255},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4523 "zhy_symbol_map"
      {"waa1", 4515},
      {""}, {""}, {""}, {""},
#line 2262 "zhy_symbol_map"
      {"kai1", 2254},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4543 "zhy_symbol_map"
      {"waak7", 4535},
      {""}, {""}, {""}, {""},
#line 3941 "zhy_symbol_map"
      {"saai7", 3933},
      {""},
#line 4584 "zhy_symbol_map"
      {"wan6", 4576},
      {""},
#line 4581 "zhy_symbol_map"
      {"wan3", 4573},
#line 4542 "zhy_symbol_map"
      {"waak6", 4534},
      {""}, {""}, {""}, {""},
#line 3940 "zhy_symbol_map"
      {"saai6", 3932},
      {""}, {""}, {""},
#line 4580 "zhy_symbol_map"
      {"wan2", 4572},
#line 4539 "zhy_symbol_map"
      {"waak3", 4531},
      {""},
#line 4661 "zhy_symbol_map"
      {"wok6", 4653},
      {""},
#line 4658 "zhy_symbol_map"
      {"wok3", 4650},
#line 3937 "zhy_symbol_map"
      {"saai3", 3929},
      {""}, {""}, {""}, {""},
#line 4538 "zhy_symbol_map"
      {"waak2", 4530},
      {""}, {""}, {""},
#line 4657 "zhy_symbol_map"
      {"wok2", 4649},
#line 3936 "zhy_symbol_map"
      {"saai2", 3928},
      {""}, {""}, {""}, {""},
#line 4541 "zhy_symbol_map"
      {"waak5", 4533},
      {""}, {""}, {""},
#line 4579 "zhy_symbol_map"
      {"wan1", 4571},
#line 3939 "zhy_symbol_map"
      {"saai5", 3931},
      {""}, {""}, {""},
#line 4641 "zhy_symbol_map"
      {"wik7", 4633},
#line 4537 "zhy_symbol_map"
      {"waak1", 4529},
      {""}, {""}, {""},
#line 4578 "zhy_symbol_map"
      {"wak7", 4570},
#line 3935 "zhy_symbol_map"
      {"saai1", 3927},
      {""}, {""}, {""},
#line 4656 "zhy_symbol_map"
      {"wok1", 4648},
#line 4648 "zhy_symbol_map"
      {"wing7", 4640},
      {""}, {""}, {""}, {""},
#line 4592 "zhy_symbol_map"
      {"wang7", 4584},
      {""}, {""}, {""}, {""},
#line 4647 "zhy_symbol_map"
      {"wing6", 4639},
      {""}, {""}, {""}, {""},
#line 4591 "zhy_symbol_map"
      {"wang6", 4583},
      {""}, {""}, {""}, {""},
#line 4644 "zhy_symbol_map"
      {"wing3", 4636},
      {""}, {""}, {""}, {""},
#line 4588 "zhy_symbol_map"
      {"wang3", 4580},
      {""},
#line 2442 "zhy_symbol_map"
      {"koi6", 2434},
      {""},
#line 2439 "zhy_symbol_map"
      {"koi3", 2431},
#line 4643 "zhy_symbol_map"
      {"wing2", 4635},
      {""}, {""}, {""}, {""},
#line 4587 "zhy_symbol_map"
      {"wang2", 4579},
      {""}, {""}, {""},
#line 2438 "zhy_symbol_map"
      {"koi2", 2430},
#line 4646 "zhy_symbol_map"
      {"wing5", 4638},
      {""}, {""}, {""}, {""},
#line 4590 "zhy_symbol_map"
      {"wang5", 4582},
      {""}, {""}, {""}, {""},
#line 4642 "zhy_symbol_map"
      {"wing1", 4634},
      {""}, {""}, {""},
#line 4529 "zhy_symbol_map"
      {"waa7", 4521},
#line 4586 "zhy_symbol_map"
      {"wang1", 4578},
      {""}, {""}, {""},
#line 2268 "zhy_symbol_map"
      {"kai7", 2260},
      {""}, {""}, {""}, {""},
#line 2437 "zhy_symbol_map"
      {"koi1", 2429},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4585 "zhy_symbol_map"
      {"wan7", 4577},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2968 "zhy_symbol_map"
      {"maan7", 2960},
      {""}, {""}, {""},
#line 4662 "zhy_symbol_map"
      {"wok7", 4654},
      {""}, {""}, {""}, {""}, {""},
#line 2967 "zhy_symbol_map"
      {"maan6", 2959},
      {""},
#line 3534 "zhy_symbol_map"
      {"nik6", 3526},
      {""},
#line 3531 "zhy_symbol_map"
      {"nik3", 3523},
#line 4669 "zhy_symbol_map"
      {"wong7", 4661},
      {""},
#line 3240 "zhy_symbol_map"
      {"nak6", 3232},
      {""},
#line 3237 "zhy_symbol_map"
      {"nak3", 3229},
#line 2964 "zhy_symbol_map"
      {"maan3", 2956},
      {""}, {""}, {""},
#line 3530 "zhy_symbol_map"
      {"nik2", 3522},
#line 4668 "zhy_symbol_map"
      {"wong6", 4660},
      {""}, {""}, {""},
#line 3236 "zhy_symbol_map"
      {"nak2", 3228},
#line 2963 "zhy_symbol_map"
      {"maan2", 2955},
      {""}, {""}, {""}, {""},
#line 4665 "zhy_symbol_map"
      {"wong3", 4657},
      {""}, {""}, {""}, {""},
#line 2966 "zhy_symbol_map"
      {"maan5", 2958},
      {""}, {""}, {""}, {""},
#line 4664 "zhy_symbol_map"
      {"wong2", 4656},
      {""}, {""}, {""},
#line 3529 "zhy_symbol_map"
      {"nik1", 3521},
#line 2962 "zhy_symbol_map"
      {"maan1", 2954},
      {""}, {""}, {""},
#line 3235 "zhy_symbol_map"
      {"nak1", 3227},
#line 4667 "zhy_symbol_map"
      {"wong5", 4659},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4663 "zhy_symbol_map"
      {"wong1", 4655},
      {""}, {""}, {""},
#line 2443 "zhy_symbol_map"
      {"koi7", 2435},
#line 3227 "zhy_symbol_map"
      {"naau7", 3219},
      {""}, {""}, {""}, {""},
#line 3192 "zhy_symbol_map"
      {"naam7", 3184},
      {""},
#line 3177 "zhy_symbol_map"
      {"naa6", 3169},
      {""},
#line 3174 "zhy_symbol_map"
      {"naa3", 3166},
#line 3226 "zhy_symbol_map"
      {"naau6", 3218},
      {""},
#line 3996 "zhy_symbol_map"
      {"sai6", 3988},
      {""},
#line 3993 "zhy_symbol_map"
      {"sai3", 3985},
#line 3191 "zhy_symbol_map"
      {"naam6", 3183},
      {""}, {""}, {""},
#line 3173 "zhy_symbol_map"
      {"naa2", 3165},
#line 3223 "zhy_symbol_map"
      {"naau3", 3215},
      {""}, {""}, {""},
#line 3992 "zhy_symbol_map"
      {"sai2", 3984},
#line 3188 "zhy_symbol_map"
      {"naam3", 3180},
      {""}, {""}, {""}, {""},
#line 3222 "zhy_symbol_map"
      {"naau2", 3214},
      {""}, {""}, {""}, {""},
#line 3187 "zhy_symbol_map"
      {"naam2", 3179},
      {""}, {""}, {""}, {""},
#line 3225 "zhy_symbol_map"
      {"naau5", 3217},
      {""}, {""}, {""},
#line 3172 "zhy_symbol_map"
      {"naa1", 3164},
#line 3190 "zhy_symbol_map"
      {"naam5", 3182},
      {""}, {""}, {""},
#line 3991 "zhy_symbol_map"
      {"sai1", 3983},
#line 3221 "zhy_symbol_map"
      {"naau1", 3213},
      {""}, {""}, {""}, {""},
#line 3186 "zhy_symbol_map"
      {"naam1", 3178},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3548 "zhy_symbol_map"
      {"nin6", 3540},
      {""},
#line 3545 "zhy_symbol_map"
      {"nin3", 3537},
#line 2954 "zhy_symbol_map"
      {"maai7", 2946},
      {""},
#line 3254 "zhy_symbol_map"
      {"nan6", 3246},
      {""},
#line 3251 "zhy_symbol_map"
      {"nan3", 3243},
      {""}, {""}, {""}, {""},
#line 3544 "zhy_symbol_map"
      {"nin2", 3536},
#line 2953 "zhy_symbol_map"
      {"maai6", 2945},
      {""}, {""}, {""},
#line 3250 "zhy_symbol_map"
      {"nan2", 3242},
      {""}, {""},
#line 3611 "zhy_symbol_map"
      {"nok6", 3603},
      {""},
#line 3608 "zhy_symbol_map"
      {"nok3", 3600},
#line 2950 "zhy_symbol_map"
      {"maai3", 2942},
      {""}, {""}, {""}, {""}, {""},
#line 3663 "zhy_symbol_map"
      {"oi2", 3655},
      {""}, {""},
#line 3607 "zhy_symbol_map"
      {"nok2", 3599},
#line 2949 "zhy_symbol_map"
      {"maai2", 2941},
#line 2354 "zhy_symbol_map"
      {"ki2", 2346},
      {""}, {""},
#line 3543 "zhy_symbol_map"
      {"nin1", 3535},
      {""}, {""}, {""}, {""},
#line 3249 "zhy_symbol_map"
      {"nan1", 3241},
#line 2952 "zhy_symbol_map"
      {"maai5", 2944},
      {""}, {""}, {""},
#line 3535 "zhy_symbol_map"
      {"nik7", 3527},
      {""}, {""}, {""}, {""},
#line 3241 "zhy_symbol_map"
      {"nak7", 3233},
#line 2948 "zhy_symbol_map"
      {"maai1", 2940},
      {""}, {""}, {""},
#line 3606 "zhy_symbol_map"
      {"nok1", 3598},
#line 3556 "zhy_symbol_map"
      {"ning7", 3548},
      {""}, {""}, {""}, {""},
#line 3262 "zhy_symbol_map"
      {"nang7", 3254},
      {""}, {""}, {""}, {""},
#line 3555 "zhy_symbol_map"
      {"ning6", 3547},
      {""}, {""}, {""}, {""},
#line 3261 "zhy_symbol_map"
      {"nang6", 3253},
      {""}, {""}, {""}, {""},
#line 3552 "zhy_symbol_map"
      {"ning3", 3544},
      {""}, {""}, {""}, {""},
#line 3258 "zhy_symbol_map"
      {"nang3", 3250},
      {""},
#line 4192 "zhy_symbol_map"
      {"soi6", 4184},
      {""},
#line 4189 "zhy_symbol_map"
      {"soi3", 4181},
#line 3551 "zhy_symbol_map"
      {"ning2", 3543},
      {""}, {""}, {""}, {""},
#line 3257 "zhy_symbol_map"
      {"nang2", 3249},
      {""}, {""}, {""},
#line 4188 "zhy_symbol_map"
      {"soi2", 4180},
#line 3554 "zhy_symbol_map"
      {"ning5", 3546},
      {""}, {""}, {""}, {""},
#line 3260 "zhy_symbol_map"
      {"nang5", 3252},
      {""}, {""}, {""}, {""},
#line 3550 "zhy_symbol_map"
      {"ning1", 3542},
      {""}, {""}, {""},
#line 3178 "zhy_symbol_map"
      {"naa7", 3170},
#line 3256 "zhy_symbol_map"
      {"nang1", 3248},
      {""}, {""}, {""},
#line 3997 "zhy_symbol_map"
      {"sai7", 3989},
      {""}, {""}, {""}, {""},
#line 4187 "zhy_symbol_map"
      {"soi1", 4179},
      {""}, {""}, {""},
#line 3668 "zhy_symbol_map"
      {"oi7", 3660},
      {""}, {""}, {""}, {""},
#line 2359 "zhy_symbol_map"
      {"ki7", 2351},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3652 "zhy_symbol_map"
      {"o5", 3644},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3549 "zhy_symbol_map"
      {"nin7", 3541},
      {""}, {""}, {""}, {""},
#line 3255 "zhy_symbol_map"
      {"nan7", 3247},
      {""},
#line 2410 "zhy_symbol_map"
      {"ko2", 2402},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4550 "zhy_symbol_map"
      {"waan7", 4542},
      {""}, {""}, {""},
#line 3612 "zhy_symbol_map"
      {"nok7", 3604},
      {""}, {""}, {""}, {""}, {""},
#line 4549 "zhy_symbol_map"
      {"waan6", 4541},
      {""}, {""}, {""}, {""},
#line 3619 "zhy_symbol_map"
      {"nong7", 3611},
      {""}, {""}, {""}, {""},
#line 4546 "zhy_symbol_map"
      {"waan3", 4538},
      {""}, {""}, {""}, {""},
#line 3618 "zhy_symbol_map"
      {"nong6", 3610},
      {""}, {""}, {""}, {""},
#line 4545 "zhy_symbol_map"
      {"waan2", 4537},
      {""}, {""}, {""}, {""},
#line 3615 "zhy_symbol_map"
      {"nong3", 3607},
      {""}, {""}, {""}, {""},
#line 4548 "zhy_symbol_map"
      {"waan5", 4540},
      {""}, {""}, {""}, {""},
#line 3614 "zhy_symbol_map"
      {"nong2", 3606},
      {""}, {""}, {""}, {""},
#line 4544 "zhy_symbol_map"
      {"waan1", 4536},
      {""}, {""}, {""}, {""},
#line 3617 "zhy_symbol_map"
      {"nong5", 3609},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3613 "zhy_symbol_map"
      {"nong1", 3605},
      {""}, {""}, {""},
#line 4193 "zhy_symbol_map"
      {"soi7", 4185},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2995 "zhy_symbol_map"
      {"mai6", 2987},
      {""},
#line 2992 "zhy_symbol_map"
      {"mai3", 2984},
      {""}, {""}, {""},
#line 2415 "zhy_symbol_map"
      {"ko7", 2407},
      {""}, {""},
#line 3662 "zhy_symbol_map"
      {"oi1", 3654},
      {""}, {""},
#line 2991 "zhy_symbol_map"
      {"mai2", 2983},
      {""},
#line 2353 "zhy_symbol_map"
      {"ki1", 2345},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3667 "zhy_symbol_map"
      {"oi6", 3659},
      {""}, {""}, {""}, {""},
#line 2358 "zhy_symbol_map"
      {"ki6", 2350},
      {""}, {""}, {""}, {""}, {""},
#line 2990 "zhy_symbol_map"
      {"mai1", 2982},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4536 "zhy_symbol_map"
      {"waai7", 4528},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4535 "zhy_symbol_map"
      {"waai6", 4527},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4532 "zhy_symbol_map"
      {"waai3", 4524},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4531 "zhy_symbol_map"
      {"waai2", 4523},
#line 4097 "zhy_symbol_map"
      {"si2", 4089},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4534 "zhy_symbol_map"
      {"waai5", 4526},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4530 "zhy_symbol_map"
      {"waai1", 4522},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3114 "zhy_symbol_map"
      {"moi6", 3106},
      {""},
#line 3111 "zhy_symbol_map"
      {"moi3", 3103},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3110 "zhy_symbol_map"
      {"moi2", 3102},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2409 "zhy_symbol_map"
      {"ko1", 2401},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2996 "zhy_symbol_map"
      {"mai7", 2988},
      {""}, {""}, {""},
#line 2414 "zhy_symbol_map"
      {"ko6", 2406},
#line 3109 "zhy_symbol_map"
      {"moi1", 3101},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4102 "zhy_symbol_map"
      {"si7", 4094},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4153 "zhy_symbol_map"
      {"so2", 4145},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3199 "zhy_symbol_map"
      {"naan7", 3191},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3198 "zhy_symbol_map"
      {"naan6", 3190},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3195 "zhy_symbol_map"
      {"naan3", 3187},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3194 "zhy_symbol_map"
      {"naan2", 3186},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3197 "zhy_symbol_map"
      {"naan5", 3189},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3193 "zhy_symbol_map"
      {"naan1", 3185},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3115 "zhy_symbol_map"
      {"moi7", 3107},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4570 "zhy_symbol_map"
      {"wai6", 4562},
      {""},
#line 4567 "zhy_symbol_map"
      {"wai3", 4559},
      {""}, {""}, {""},
#line 4158 "zhy_symbol_map"
      {"so7", 4150},
      {""}, {""}, {""}, {""}, {""},
#line 4566 "zhy_symbol_map"
      {"wai2", 4558},
      {""},
#line 4096 "zhy_symbol_map"
      {"si1", 4088},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4101 "zhy_symbol_map"
      {"si6", 4093},
      {""}, {""}, {""}, {""}, {""},
#line 4565 "zhy_symbol_map"
      {"wai1", 4557},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3185 "zhy_symbol_map"
      {"naai7", 3177},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3184 "zhy_symbol_map"
      {"naai6", 3176},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3181 "zhy_symbol_map"
      {"naai3", 3173},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3180 "zhy_symbol_map"
      {"naai2", 3172},
#line 3061 "zhy_symbol_map"
      {"mi2", 3053},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3183 "zhy_symbol_map"
      {"naai5", 3175},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3179 "zhy_symbol_map"
      {"naai1", 3171},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1357 "zhy_symbol_map"
      {"gik6", 1349},
      {""},
#line 1354 "zhy_symbol_map"
      {"gik3", 1346},
      {""}, {""},
#line 1266 "zhy_symbol_map"
      {"gak6", 1258},
      {""},
#line 1263 "zhy_symbol_map"
      {"gak3", 1255},
      {""}, {""}, {""}, {""},
#line 1353 "zhy_symbol_map"
      {"gik2", 1345},
      {""}, {""}, {""}, {""},
#line 1262 "zhy_symbol_map"
      {"gak2", 1254},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1352 "zhy_symbol_map"
      {"gik1", 1344},
      {""}, {""}, {""}, {""},
#line 1261 "zhy_symbol_map"
      {"gak1", 1253},
      {""},
#line 4152 "zhy_symbol_map"
      {"so1", 4144},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4571 "zhy_symbol_map"
      {"wai7", 4563},
#line 1253 "zhy_symbol_map"
      {"gaau7", 1245},
      {""}, {""},
#line 4157 "zhy_symbol_map"
      {"so6", 4149},
      {""},
#line 1218 "zhy_symbol_map"
      {"gaam7", 1210},
      {""},
#line 1196 "zhy_symbol_map"
      {"gaa6", 1188},
      {""},
#line 1193 "zhy_symbol_map"
      {"gaa3", 1185},
#line 1252 "zhy_symbol_map"
      {"gaau6", 1244},
      {""}, {""},
#line 3066 "zhy_symbol_map"
      {"mi7", 3058},
      {""},
#line 1217 "zhy_symbol_map"
      {"gaam6", 1209},
      {""}, {""}, {""},
#line 1192 "zhy_symbol_map"
      {"gaa2", 1184},
#line 1249 "zhy_symbol_map"
      {"gaau3", 1241},
      {""}, {""}, {""}, {""},
#line 1214 "zhy_symbol_map"
      {"gaam3", 1206},
      {""}, {""}, {""}, {""},
#line 1248 "zhy_symbol_map"
      {"gaau2", 1240},
      {""},
#line 2938 "zhy_symbol_map"
      {"m5", 2930},
      {""}, {""},
#line 1213 "zhy_symbol_map"
      {"gaam2", 1205},
      {""}, {""}, {""}, {""},
#line 1251 "zhy_symbol_map"
      {"gaau5", 1243},
      {""}, {""}, {""},
#line 1191 "zhy_symbol_map"
      {"gaa1", 1183},
#line 1216 "zhy_symbol_map"
      {"gaam5", 1208},
      {""}, {""}, {""}, {""},
#line 1247 "zhy_symbol_map"
      {"gaau1", 1239},
      {""}, {""}, {""}, {""},
#line 1212 "zhy_symbol_map"
      {"gaam1", 1204},
#line 3103 "zhy_symbol_map"
      {"mo2", 3095},
      {""}, {""}, {""},
#line 1211 "zhy_symbol_map"
      {"gaak7", 1203},
      {""},
#line 1371 "zhy_symbol_map"
      {"gin6", 1363},
      {""},
#line 1368 "zhy_symbol_map"
      {"gin3", 1360},
      {""}, {""},
#line 1280 "zhy_symbol_map"
      {"gan6", 1272},
      {""},
#line 1277 "zhy_symbol_map"
      {"gan3", 1269},
#line 1210 "zhy_symbol_map"
      {"gaak6", 1202},
      {""}, {""}, {""},
#line 1367 "zhy_symbol_map"
      {"gin2", 1359},
      {""}, {""}, {""}, {""},
#line 1276 "zhy_symbol_map"
      {"gan2", 1268},
#line 1207 "zhy_symbol_map"
      {"gaak3", 1199},
      {""},
#line 1448 "zhy_symbol_map"
      {"gok6", 1440},
      {""},
#line 1445 "zhy_symbol_map"
      {"gok3", 1437},
      {""}, {""}, {""}, {""}, {""},
#line 1206 "zhy_symbol_map"
      {"gaak2", 1198},
      {""}, {""}, {""},
#line 1444 "zhy_symbol_map"
      {"gok2", 1436},
      {""}, {""}, {""}, {""},
#line 1366 "zhy_symbol_map"
      {"gin1", 1358},
#line 1209 "zhy_symbol_map"
      {"gaak5", 1201},
      {""}, {""}, {""},
#line 1275 "zhy_symbol_map"
      {"gan1", 1267},
      {""}, {""}, {""}, {""},
#line 1358 "zhy_symbol_map"
      {"gik7", 1350},
#line 1205 "zhy_symbol_map"
      {"gaak1", 1197},
      {""},
#line 2364 "zhy_symbol_map"
      {"kik5", 2356},
      {""},
#line 1267 "zhy_symbol_map"
      {"gak7", 1259},
      {""}, {""},
#line 2273 "zhy_symbol_map"
      {"kak5", 2265},
      {""},
#line 1443 "zhy_symbol_map"
      {"gok1", 1435},
#line 1379 "zhy_symbol_map"
      {"ging7", 1371},
      {""}, {""}, {""}, {""},
#line 1288 "zhy_symbol_map"
      {"gang7", 1280},
      {""}, {""}, {""}, {""},
#line 1378 "zhy_symbol_map"
      {"ging6", 1370},
      {""}, {""}, {""}, {""},
#line 1287 "zhy_symbol_map"
      {"gang6", 1279},
      {""}, {""}, {""}, {""},
#line 1375 "zhy_symbol_map"
      {"ging3", 1367},
      {""}, {""}, {""}, {""},
#line 1284 "zhy_symbol_map"
      {"gang3", 1276},
      {""},
#line 3233 "zhy_symbol_map"
      {"nai6", 3225},
      {""},
#line 3230 "zhy_symbol_map"
      {"nai3", 3222},
#line 1374 "zhy_symbol_map"
      {"ging2", 1366},
      {""}, {""},
#line 3108 "zhy_symbol_map"
      {"mo7", 3100},
      {""},
#line 1283 "zhy_symbol_map"
      {"gang2", 1275},
      {""}, {""}, {""},
#line 3229 "zhy_symbol_map"
      {"nai2", 3221},
#line 1377 "zhy_symbol_map"
      {"ging5", 1369},
#line 3060 "zhy_symbol_map"
      {"mi1", 3052},
      {""}, {""}, {""},
#line 1286 "zhy_symbol_map"
      {"gang5", 1278},
      {""}, {""}, {""}, {""},
#line 1373 "zhy_symbol_map"
      {"ging1", 1365},
      {""}, {""}, {""},
#line 1197 "zhy_symbol_map"
      {"gaa7", 1189},
#line 1282 "zhy_symbol_map"
      {"gang1", 1274},
      {""},
#line 2210 "zhy_symbol_map"
      {"kaa5", 2202},
#line 3065 "zhy_symbol_map"
      {"mi6", 3057},
      {""}, {""}, {""}, {""}, {""},
#line 3228 "zhy_symbol_map"
      {"nai1", 3220},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3444 "zhy_symbol_map"
      {"ngau7", 3436},
      {""}, {""}, {""}, {""},
#line 3409 "zhy_symbol_map"
      {"ngam7", 3401},
      {""},
#line 1455 "zhy_symbol_map"
      {"gon6", 1447},
      {""},
#line 1452 "zhy_symbol_map"
      {"gon3", 1444},
#line 3443 "zhy_symbol_map"
      {"ngau6", 3435},
      {""},
#line 27 "zhy_symbol_map"
      {"aak6", 19},
      {""},
#line 24 "zhy_symbol_map"
      {"aak3", 16},
#line 3408 "zhy_symbol_map"
      {"ngam6", 3400},
      {""}, {""}, {""},
#line 1451 "zhy_symbol_map"
      {"gon2", 1443},
#line 3440 "zhy_symbol_map"
      {"ngau3", 3432},
      {""}, {""}, {""},
#line 23 "zhy_symbol_map"
      {"aak2", 15},
#line 3405 "zhy_symbol_map"
      {"ngam3", 3397},
      {""}, {""}, {""}, {""},
#line 3439 "zhy_symbol_map"
      {"ngau2", 3431},
      {""}, {""}, {""},
#line 1372 "zhy_symbol_map"
      {"gin7", 1364},
#line 3404 "zhy_symbol_map"
      {"ngam2", 3396},
#line 4629 "zhy_symbol_map"
      {"wi2", 4621},
#line 2378 "zhy_symbol_map"
      {"kin5", 2370},
      {""},
#line 1281 "zhy_symbol_map"
      {"gan7", 1273},
#line 3442 "zhy_symbol_map"
      {"ngau5", 3434},
      {""},
#line 2287 "zhy_symbol_map"
      {"kan5", 2279},
      {""},
#line 1450 "zhy_symbol_map"
      {"gon1", 1442},
#line 3407 "zhy_symbol_map"
      {"ngam5", 3399},
      {""}, {""}, {""},
#line 22 "zhy_symbol_map"
      {"aak1", 14},
#line 3438 "zhy_symbol_map"
      {"ngau1", 3430},
      {""}, {""}, {""},
#line 1449 "zhy_symbol_map"
      {"gok7", 1441},
#line 3403 "zhy_symbol_map"
      {"ngam1", 3395},
      {""},
#line 2448 "zhy_symbol_map"
      {"kok5", 2440},
      {""}, {""},
#line 3402 "zhy_symbol_map"
      {"ngak7", 3394},
      {""}, {""}, {""}, {""},
#line 1463 "zhy_symbol_map"
      {"gong7", 1455},
      {""}, {""}, {""}, {""},
#line 3401 "zhy_symbol_map"
      {"ngak6", 3393},
      {""}, {""}, {""}, {""},
#line 1462 "zhy_symbol_map"
      {"gong6", 1454},
      {""}, {""}, {""}, {""},
#line 3398 "zhy_symbol_map"
      {"ngak3", 3390},
      {""}, {""}, {""}, {""},
#line 1459 "zhy_symbol_map"
      {"gong3", 1451},
      {""},
#line 3604 "zhy_symbol_map"
      {"noi6", 3596},
      {""},
#line 3601 "zhy_symbol_map"
      {"noi3", 3593},
#line 3397 "zhy_symbol_map"
      {"ngak2", 3389},
      {""}, {""}, {""}, {""},
#line 1458 "zhy_symbol_map"
      {"gong2", 1450},
      {""}, {""}, {""},
#line 3600 "zhy_symbol_map"
      {"noi2", 3592},
#line 3400 "zhy_symbol_map"
      {"ngak5", 3392},
      {""}, {""}, {""}, {""},
#line 1461 "zhy_symbol_map"
      {"gong5", 1453},
#line 3102 "zhy_symbol_map"
      {"mo1", 3094},
      {""}, {""}, {""},
#line 3396 "zhy_symbol_map"
      {"ngak1", 3388},
      {""}, {""}, {""}, {""},
#line 1457 "zhy_symbol_map"
      {"gong1", 1449},
      {""}, {""}, {""},
#line 3234 "zhy_symbol_map"
      {"nai7", 3226},
      {""}, {""}, {""},
#line 3107 "zhy_symbol_map"
      {"mo6", 3099},
#line 3599 "zhy_symbol_map"
      {"noi1", 3591},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 41 "zhy_symbol_map"
      {"aan6", 33},
#line 4634 "zhy_symbol_map"
      {"wi7", 4626},
#line 38 "zhy_symbol_map"
      {"aan3", 30},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 37 "zhy_symbol_map"
      {"aan2", 29},
#line 3591 "zhy_symbol_map"
      {"noei7", 3583},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3590 "zhy_symbol_map"
      {"noei6", 3582},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3587 "zhy_symbol_map"
      {"noei3", 3579},
      {""}, {""}, {""},
#line 36 "zhy_symbol_map"
      {"aan1", 28},
      {""}, {""}, {""}, {""},
#line 1456 "zhy_symbol_map"
      {"gon7", 1448},
#line 3586 "zhy_symbol_map"
      {"noei2", 3578},
#line 4650 "zhy_symbol_map"
      {"wo2", 4642},
      {""}, {""},
#line 28 "zhy_symbol_map"
      {"aak7", 20},
      {""}, {""}, {""}, {""}, {""},
#line 3589 "zhy_symbol_map"
      {"noei5", 3581},
      {""}, {""}, {""}, {""},
#line 49 "zhy_symbol_map"
      {"aang7", 41},
      {""}, {""}, {""}, {""},
#line 3585 "zhy_symbol_map"
      {"noei1", 3577},
      {""}, {""}, {""}, {""},
#line 48 "zhy_symbol_map"
      {"aang6", 40},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 45 "zhy_symbol_map"
      {"aang3", 37},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 44 "zhy_symbol_map"
      {"aang2", 36},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 47 "zhy_symbol_map"
      {"aang5", 39},
      {""},
#line 4107 "zhy_symbol_map"
      {"sik5", 4099},
      {""}, {""}, {""}, {""},
#line 4002 "zhy_symbol_map"
      {"sak5", 3994},
      {""}, {""},
#line 43 "zhy_symbol_map"
      {"aang1", 35},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3605 "zhy_symbol_map"
      {"noi7", 3597},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1413 "zhy_symbol_map"
      {"goe6", 1405},
      {""},
#line 1410 "zhy_symbol_map"
      {"goe3", 1402},
      {""}, {""}, {""},
#line 4655 "zhy_symbol_map"
      {"wo7", 4647},
      {""}, {""}, {""}, {""}, {""},
#line 1409 "zhy_symbol_map"
      {"goe2", 1401},
      {""},
#line 4628 "zhy_symbol_map"
      {"wi1", 4620},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3932 "zhy_symbol_map"
      {"saa5", 3924},
#line 4633 "zhy_symbol_map"
      {"wi6", 4625},
#line 42 "zhy_symbol_map"
      {"aan7", 34},
      {""}, {""}, {""}, {""},
#line 1408 "zhy_symbol_map"
      {"goe1", 1400},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1428 "zhy_symbol_map"
      {"goek7", 1420},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3450 "zhy_symbol_map"
      {"nge6", 3442},
      {""},
#line 3447 "zhy_symbol_map"
      {"nge3", 3439},
#line 1427 "zhy_symbol_map"
      {"goek6", 1419},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3446 "zhy_symbol_map"
      {"nge2", 3438},
#line 1424 "zhy_symbol_map"
      {"goek3", 1416},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1423 "zhy_symbol_map"
      {"goek2", 1415},
#line 3523 "zhy_symbol_map"
      {"ni2", 3515},
#line 4121 "zhy_symbol_map"
      {"sin5", 4113},
      {""}, {""}, {""}, {""},
#line 4016 "zhy_symbol_map"
      {"san5", 4008},
      {""}, {""},
#line 1426 "zhy_symbol_map"
      {"goek5", 1418},
      {""}, {""}, {""},
#line 3445 "zhy_symbol_map"
      {"nge1", 3437},
#line 1225 "zhy_symbol_map"
      {"gaan7", 1217},
      {""}, {""}, {""}, {""},
#line 1422 "zhy_symbol_map"
      {"goek1", 1414},
      {""},
#line 4198 "zhy_symbol_map"
      {"sok5", 4190},
      {""}, {""},
#line 1224 "zhy_symbol_map"
      {"gaan6", 1216},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1221 "zhy_symbol_map"
      {"gaan3", 1213},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1220 "zhy_symbol_map"
      {"gaan2", 1212},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1223 "zhy_symbol_map"
      {"gaan5", 1215},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1219 "zhy_symbol_map"
      {"gaan1", 1211},
      {""}, {""}, {""}, {""}, {""},
#line 4649 "zhy_symbol_map"
      {"wo1", 4641},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1414 "zhy_symbol_map"
      {"goe7", 1406},
      {""}, {""},
#line 2420 "zhy_symbol_map"
      {"koe5", 2412},
#line 4654 "zhy_symbol_map"
      {"wo6", 4646},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3528 "zhy_symbol_map"
      {"ni7", 3520},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3579 "zhy_symbol_map"
      {"no2", 3571},
      {""}, {""},
#line 3451 "zhy_symbol_map"
      {"nge7", 3443},
      {""}, {""}, {""}, {""}, {""},
#line 1204 "zhy_symbol_map"
      {"gaai7", 1196},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1203 "zhy_symbol_map"
      {"gaai6", 1195},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1200 "zhy_symbol_map"
      {"gaai3", 1192},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1199 "zhy_symbol_map"
      {"gaai2", 1191},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1202 "zhy_symbol_map"
      {"gaai5", 1194},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3071 "zhy_symbol_map"
      {"mik5", 3063},
      {""}, {""},
#line 1198 "zhy_symbol_map"
      {"gaai1", 1190},
      {""},
#line 3001 "zhy_symbol_map"
      {"mak5", 2993},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3416 "zhy_symbol_map"
      {"ngan7", 3408},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3415 "zhy_symbol_map"
      {"ngan6", 3407},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4395 "zhy_symbol_map"
      {"tik6", 4387},
      {""},
#line 4392 "zhy_symbol_map"
      {"tik3", 4384},
#line 3412 "zhy_symbol_map"
      {"ngan3", 3404},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3584 "zhy_symbol_map"
      {"no7", 3576},
#line 4391 "zhy_symbol_map"
      {"tik2", 4383},
#line 3411 "zhy_symbol_map"
      {"ngan2", 3403},
      {""}, {""}, {""}, {""}, {""},
#line 3522 "zhy_symbol_map"
      {"ni1", 3514},
      {""}, {""}, {""},
#line 3414 "zhy_symbol_map"
      {"ngan5", 3406},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3410 "zhy_symbol_map"
      {"ngan1", 3402},
      {""},
#line 2945 "zhy_symbol_map"
      {"maa5", 2937},
#line 3527 "zhy_symbol_map"
      {"ni6", 3519},
#line 4390 "zhy_symbol_map"
      {"tik1", 4382},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4305 "zhy_symbol_map"
      {"taau7", 4297},
      {""}, {""}, {""}, {""},
#line 4270 "zhy_symbol_map"
      {"taam7", 4262},
      {""},
#line 4255 "zhy_symbol_map"
      {"taa6", 4247},
      {""},
#line 4252 "zhy_symbol_map"
      {"taa3", 4244},
#line 4304 "zhy_symbol_map"
      {"taau6", 4296},
      {""}, {""}, {""}, {""},
#line 4269 "zhy_symbol_map"
      {"taam6", 4261},
      {""}, {""}, {""},
#line 4251 "zhy_symbol_map"
      {"taa2", 4243},
#line 4301 "zhy_symbol_map"
      {"taau3", 4293},
      {""}, {""}, {""}, {""},
#line 4266 "zhy_symbol_map"
      {"taam3", 4258},
      {""}, {""}, {""}, {""},
#line 4300 "zhy_symbol_map"
      {"taau2", 4292},
      {""},
#line 3078 "zhy_symbol_map"
      {"min5", 3070},
      {""}, {""},
#line 4265 "zhy_symbol_map"
      {"taam2", 4257},
      {""},
#line 3015 "zhy_symbol_map"
      {"man5", 3007},
      {""}, {""},
#line 4303 "zhy_symbol_map"
      {"taau5", 4295},
      {""}, {""}, {""},
#line 4250 "zhy_symbol_map"
      {"taa1", 4242},
#line 4268 "zhy_symbol_map"
      {"taam5", 4260},
      {""}, {""}, {""}, {""},
#line 4299 "zhy_symbol_map"
      {"taau1", 4291},
      {""},
#line 3120 "zhy_symbol_map"
      {"mok5", 3112},
      {""}, {""},
#line 4264 "zhy_symbol_map"
      {"taam1", 4256},
      {""}, {""}, {""}, {""},
#line 3395 "zhy_symbol_map"
      {"ngai7", 3387},
      {""},
#line 4409 "zhy_symbol_map"
      {"tin6", 4401},
      {""},
#line 4406 "zhy_symbol_map"
      {"tin3", 4398},
#line 2247 "zhy_symbol_map"
      {"kaap7", 2239},
      {""},
#line 4325 "zhy_symbol_map"
      {"tan6", 4317},
      {""},
#line 4322 "zhy_symbol_map"
      {"tan3", 4314},
#line 3394 "zhy_symbol_map"
      {"ngai6", 3386},
      {""}, {""}, {""},
#line 4405 "zhy_symbol_map"
      {"tin2", 4397},
#line 2246 "zhy_symbol_map"
      {"kaap6", 2238},
      {""}, {""}, {""},
#line 4321 "zhy_symbol_map"
      {"tan2", 4313},
#line 3391 "zhy_symbol_map"
      {"ngai3", 3383},
      {""},
#line 4465 "zhy_symbol_map"
      {"tok6", 4457},
      {""},
#line 4462 "zhy_symbol_map"
      {"tok3", 4454},
#line 2243 "zhy_symbol_map"
      {"kaap3", 2235},
      {""}, {""}, {""}, {""},
#line 3390 "zhy_symbol_map"
      {"ngai2", 3382},
      {""}, {""}, {""},
#line 4461 "zhy_symbol_map"
      {"tok2", 4453},
#line 2242 "zhy_symbol_map"
      {"kaap2", 2234},
      {""}, {""}, {""},
#line 4404 "zhy_symbol_map"
      {"tin1", 4396},
#line 3393 "zhy_symbol_map"
      {"ngai5", 3385},
#line 3578 "zhy_symbol_map"
      {"no1", 3570},
      {""}, {""},
#line 4320 "zhy_symbol_map"
      {"tan1", 4312},
#line 2245 "zhy_symbol_map"
      {"kaap5", 2237},
      {""}, {""}, {""},
#line 4396 "zhy_symbol_map"
      {"tik7", 4388},
#line 3389 "zhy_symbol_map"
      {"ngai1", 3381},
#line 3969 "zhy_symbol_map"
      {"saang7", 3961},
      {""}, {""}, {""},
#line 2241 "zhy_symbol_map"
      {"kaap1", 2233},
#line 3968 "zhy_symbol_map"
      {"saang6", 3960},
#line 4163 "zhy_symbol_map"
      {"soe5", 4155},
#line 3583 "zhy_symbol_map"
      {"no6", 3575},
#line 4460 "zhy_symbol_map"
      {"tok1", 4452},
#line 4417 "zhy_symbol_map"
      {"ting7", 4409},
#line 3965 "zhy_symbol_map"
      {"saang3", 3957},
      {""}, {""}, {""},
#line 4333 "zhy_symbol_map"
      {"tang7", 4325},
#line 3964 "zhy_symbol_map"
      {"saang2", 3956},
#line 1259 "zhy_symbol_map"
      {"gai6", 1251},
      {""},
#line 1256 "zhy_symbol_map"
      {"gai3", 1248},
#line 4416 "zhy_symbol_map"
      {"ting6", 4408},
#line 3967 "zhy_symbol_map"
      {"saang5", 3959},
      {""}, {""}, {""},
#line 4332 "zhy_symbol_map"
      {"tang6", 4324},
#line 3963 "zhy_symbol_map"
      {"saang1", 3955},
      {""}, {""},
#line 1255 "zhy_symbol_map"
      {"gai2", 1247},
#line 4413 "zhy_symbol_map"
      {"ting3", 4405},
      {""}, {""}, {""}, {""},
#line 4329 "zhy_symbol_map"
      {"tang3", 4321},
      {""}, {""}, {""}, {""},
#line 4412 "zhy_symbol_map"
      {"ting2", 4404},
      {""}, {""}, {""}, {""},
#line 4328 "zhy_symbol_map"
      {"tang2", 4320},
      {""}, {""}, {""}, {""},
#line 4415 "zhy_symbol_map"
      {"ting5", 4407},
      {""}, {""}, {""},
#line 1254 "zhy_symbol_map"
      {"gai1", 1246},
#line 4331 "zhy_symbol_map"
      {"tang5", 4323},
      {""}, {""}, {""}, {""},
#line 4411 "zhy_symbol_map"
      {"ting1", 4403},
      {""}, {""}, {""},
#line 4256 "zhy_symbol_map"
      {"taa7", 4248},
#line 4327 "zhy_symbol_map"
      {"tang1", 4319},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4639 "zhy_symbol_map"
      {"wik5", 4631},
      {""},
#line 4410 "zhy_symbol_map"
      {"tin7", 4402},
      {""}, {""},
#line 4576 "zhy_symbol_map"
      {"wak5", 4568},
      {""},
#line 4326 "zhy_symbol_map"
      {"tan7", 4318},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4466 "zhy_symbol_map"
      {"tok7", 4458},
      {""},
#line 2436 "zhy_symbol_map"
      {"koeng7", 2428},
      {""}, {""}, {""}, {""},
#line 2435 "zhy_symbol_map"
      {"koeng6", 2427},
      {""}, {""}, {""},
#line 4473 "zhy_symbol_map"
      {"tong7", 4465},
#line 2432 "zhy_symbol_map"
      {"koeng3", 2424},
#line 1441 "zhy_symbol_map"
      {"goi6", 1433},
      {""},
#line 1438 "zhy_symbol_map"
      {"goi3", 1430},
      {""},
#line 2431 "zhy_symbol_map"
      {"koeng2", 2423},
      {""}, {""}, {""},
#line 4472 "zhy_symbol_map"
      {"tong6", 4464},
#line 2434 "zhy_symbol_map"
      {"koeng5", 2426},
      {""}, {""},
#line 1437 "zhy_symbol_map"
      {"goi2", 1429},
      {""},
#line 2430 "zhy_symbol_map"
      {"koeng1", 2422},
      {""}, {""}, {""},
#line 4469 "zhy_symbol_map"
      {"tong3", 4461},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4468 "zhy_symbol_map"
      {"tong2", 4460},
      {""},
#line 4527 "zhy_symbol_map"
      {"waa5", 4519},
      {""},
#line 1260 "zhy_symbol_map"
      {"gai7", 1252},
      {""}, {""},
#line 2266 "zhy_symbol_map"
      {"kai5", 2258},
      {""},
#line 1436 "zhy_symbol_map"
      {"goi1", 1428},
#line 4471 "zhy_symbol_map"
      {"tong5", 4463},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4467 "zhy_symbol_map"
      {"tong1", 4459},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1421 "zhy_symbol_map"
      {"goei7", 1413},
      {""},
#line 3457 "zhy_symbol_map"
      {"ngi6", 3449},
      {""},
#line 3454 "zhy_symbol_map"
      {"ngi3", 3446},
      {""}, {""}, {""}, {""}, {""},
#line 1420 "zhy_symbol_map"
      {"goei6", 1412},
      {""}, {""}, {""},
#line 3453 "zhy_symbol_map"
      {"ngi2", 3445},
      {""}, {""}, {""}, {""}, {""},
#line 1417 "zhy_symbol_map"
      {"goei3", 1409},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1416 "zhy_symbol_map"
      {"goei2", 1408},
      {""},
#line 4583 "zhy_symbol_map"
      {"wan5", 4575},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3452 "zhy_symbol_map"
      {"ngi1", 3444},
#line 1419 "zhy_symbol_map"
      {"goei5", 1411},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4660 "zhy_symbol_map"
      {"wok5", 4652},
      {""}, {""},
#line 1415 "zhy_symbol_map"
      {"goei1", 1407},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3976 "zhy_symbol_map"
      {"saap7", 3968},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 20 "zhy_symbol_map"
      {"aai6", 12},
      {""},
#line 17 "zhy_symbol_map"
      {"aai3", 9},
#line 3975 "zhy_symbol_map"
      {"saap6", 3967},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 16 "zhy_symbol_map"
      {"aai2", 8},
#line 3972 "zhy_symbol_map"
      {"saap3", 3964},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3971 "zhy_symbol_map"
      {"saap2", 3963},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3974 "zhy_symbol_map"
      {"saap5", 3966},
      {""}, {""}, {""},
#line 15 "zhy_symbol_map"
      {"aai1", 7},
      {""},
#line 2975 "zhy_symbol_map"
      {"maang7", 2967},
      {""}, {""},
#line 1442 "zhy_symbol_map"
      {"goi7", 1434},
#line 3970 "zhy_symbol_map"
      {"saap1", 3962},
#line 2974 "zhy_symbol_map"
      {"maang6", 2966},
#line 2441 "zhy_symbol_map"
      {"koi5", 2433},
      {""}, {""}, {""},
#line 2971 "zhy_symbol_map"
      {"maang3", 2963},
      {""}, {""}, {""}, {""},
#line 2970 "zhy_symbol_map"
      {"maang2", 2962},
      {""}, {""}, {""}, {""},
#line 2973 "zhy_symbol_map"
      {"maang5", 2965},
      {""}, {""}, {""}, {""},
#line 2969 "zhy_symbol_map"
      {"maang1", 2961},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4451 "zhy_symbol_map"
      {"toe6", 4443},
      {""},
#line 4448 "zhy_symbol_map"
      {"toe3", 4440},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4447 "zhy_symbol_map"
      {"toe2", 4439},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3458 "zhy_symbol_map"
      {"ngi7", 3450},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4446 "zhy_symbol_map"
      {"toe1", 4438},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1346 "zhy_symbol_map"
      {"gi2", 1338},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3533 "zhy_symbol_map"
      {"nik5", 3525},
      {""}, {""}, {""}, {""},
#line 3239 "zhy_symbol_map"
      {"nak5", 3231},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 21 "zhy_symbol_map"
      {"aai7", 13},
      {""}, {""}, {""}, {""}, {""},
#line 4277 "zhy_symbol_map"
      {"taan7", 4269},
      {""}, {""}, {""}, {""}, {""},
#line 4179 "zhy_symbol_map"
      {"soeng7", 4171},
      {""}, {""}, {""},
#line 4276 "zhy_symbol_map"
      {"taan6", 4268},
#line 4178 "zhy_symbol_map"
      {"soeng6", 4170},
      {""}, {""}, {""}, {""},
#line 4175 "zhy_symbol_map"
      {"soeng3", 4167},
      {""}, {""}, {""},
#line 4273 "zhy_symbol_map"
      {"taan3", 4265},
#line 4174 "zhy_symbol_map"
      {"soeng2", 4166},
      {""}, {""}, {""}, {""},
#line 4177 "zhy_symbol_map"
      {"soeng5", 4169},
      {""}, {""}, {""},
#line 4272 "zhy_symbol_map"
      {"taan2", 4264},
#line 4173 "zhy_symbol_map"
      {"soeng1", 4165},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4275 "zhy_symbol_map"
      {"taan5", 4267},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3176 "zhy_symbol_map"
      {"naa5", 3168},
      {""}, {""},
#line 4271 "zhy_symbol_map"
      {"taan1", 4263},
      {""},
#line 3995 "zhy_symbol_map"
      {"sai5", 3987},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1351 "zhy_symbol_map"
      {"gi7", 1343},
      {""}, {""}, {""}, {""}, {""},
#line 4452 "zhy_symbol_map"
      {"toe7", 4444},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3547 "zhy_symbol_map"
      {"nin5", 3539},
      {""}, {""}, {""},
#line 1402 "zhy_symbol_map"
      {"go2", 1394},
#line 3253 "zhy_symbol_map"
      {"nan5", 3245},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3610 "zhy_symbol_map"
      {"nok5", 3602},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2785 "zhy_symbol_map"
      {"lik6", 2777},
      {""},
#line 2782 "zhy_symbol_map"
      {"lik3", 2774},
#line 4263 "zhy_symbol_map"
      {"taai7", 4255},
      {""},
#line 2673 "zhy_symbol_map"
      {"lak6", 2665},
      {""},
#line 2670 "zhy_symbol_map"
      {"lak3", 2662},
      {""}, {""}, {""}, {""},
#line 2781 "zhy_symbol_map"
      {"lik2", 2773},
#line 4262 "zhy_symbol_map"
      {"taai6", 4254},
      {""}, {""}, {""},
#line 2669 "zhy_symbol_map"
      {"lak2", 2661},
      {""}, {""}, {""}, {""}, {""},
#line 4259 "zhy_symbol_map"
      {"taai3", 4251},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4258 "zhy_symbol_map"
      {"taai2", 4250},
#line 3320 "zhy_symbol_map"
      {"ng2", 3312},
      {""}, {""},
#line 2780 "zhy_symbol_map"
      {"lik1", 2772},
      {""}, {""}, {""}, {""},
#line 2668 "zhy_symbol_map"
      {"lak1", 2660},
#line 4261 "zhy_symbol_map"
      {"taai5", 4253},
      {""}, {""}, {""}, {""}, {""},
#line 4557 "zhy_symbol_map"
      {"waang7", 4549},
      {""}, {""}, {""},
#line 4257 "zhy_symbol_map"
      {"taai1", 4249},
#line 4556 "zhy_symbol_map"
      {"waang6", 4548},
#line 4191 "zhy_symbol_map"
      {"soi5", 4183},
      {""}, {""},
#line 2660 "zhy_symbol_map"
      {"laau7", 2652},
#line 4553 "zhy_symbol_map"
      {"waang3", 4545},
      {""}, {""}, {""},
#line 2625 "zhy_symbol_map"
      {"laam7", 2617},
#line 4552 "zhy_symbol_map"
      {"waang2", 4544},
#line 2603 "zhy_symbol_map"
      {"laa6", 2595},
      {""},
#line 2600 "zhy_symbol_map"
      {"laa3", 2592},
#line 2659 "zhy_symbol_map"
      {"laau6", 2651},
#line 4555 "zhy_symbol_map"
      {"waang5", 4547},
      {""},
#line 1407 "zhy_symbol_map"
      {"go7", 1399},
      {""},
#line 2624 "zhy_symbol_map"
      {"laam6", 2616},
#line 4551 "zhy_symbol_map"
      {"waang1", 4543},
      {""}, {""},
#line 2599 "zhy_symbol_map"
      {"laa2", 2591},
#line 2656 "zhy_symbol_map"
      {"laau3", 2648},
#line 1345 "zhy_symbol_map"
      {"gi1", 1337},
      {""}, {""}, {""},
#line 2621 "zhy_symbol_map"
      {"laam3", 2613},
      {""}, {""}, {""}, {""},
#line 2655 "zhy_symbol_map"
      {"laau2", 2647},
      {""}, {""}, {""}, {""},
#line 2620 "zhy_symbol_map"
      {"laam2", 2612},
#line 72 "zhy_symbol_map"
      {"ai2", 64},
      {""},
#line 1350 "zhy_symbol_map"
      {"gi6", 1342},
      {""},
#line 2658 "zhy_symbol_map"
      {"laau5", 2650},
#line 9 "zhy_symbol_map"
      {"aa2", 1},
      {""}, {""},
#line 2598 "zhy_symbol_map"
      {"laa1", 2590},
#line 2623 "zhy_symbol_map"
      {"laam5", 2615},
      {""}, {""}, {""}, {""},
#line 2654 "zhy_symbol_map"
      {"laau1", 2646},
      {""}, {""}, {""}, {""},
#line 2619 "zhy_symbol_map"
      {"laam1", 2611},
      {""}, {""}, {""}, {""},
#line 2618 "zhy_symbol_map"
      {"laak7", 2610},
      {""},
#line 2799 "zhy_symbol_map"
      {"lin6", 2791},
      {""},
#line 2796 "zhy_symbol_map"
      {"lin3", 2788},
      {""}, {""},
#line 2687 "zhy_symbol_map"
      {"lan6", 2679},
      {""},
#line 2684 "zhy_symbol_map"
      {"lan3", 2676},
#line 2617 "zhy_symbol_map"
      {"laak6", 2609},
      {""}, {""},
#line 3325 "zhy_symbol_map"
      {"ng7", 3317},
#line 2795 "zhy_symbol_map"
      {"lin2", 2787},
      {""}, {""}, {""}, {""},
#line 2683 "zhy_symbol_map"
      {"lan2", 2675},
#line 2614 "zhy_symbol_map"
      {"laak3", 2606},
      {""},
#line 2883 "zhy_symbol_map"
      {"lok6", 2875},
      {""},
#line 2880 "zhy_symbol_map"
      {"lok3", 2872},
      {""}, {""}, {""}, {""}, {""},
#line 2613 "zhy_symbol_map"
      {"laak2", 2605},
      {""}, {""}, {""},
#line 2879 "zhy_symbol_map"
      {"lok2", 2871},
      {""},
#line 114 "zhy_symbol_map"
      {"at2", 106},
      {""}, {""},
#line 2794 "zhy_symbol_map"
      {"lin1", 2786},
#line 2616 "zhy_symbol_map"
      {"laak5", 2608},
#line 107 "zhy_symbol_map"
      {"ap2", 99},
      {""}, {""},
#line 2682 "zhy_symbol_map"
      {"lan1", 2674},
      {""}, {""}, {""}, {""},
#line 2786 "zhy_symbol_map"
      {"lik7", 2778},
#line 2612 "zhy_symbol_map"
      {"laak1", 2604},
      {""}, {""}, {""},
#line 2674 "zhy_symbol_map"
      {"lak7", 2666},
      {""}, {""}, {""}, {""},
#line 2878 "zhy_symbol_map"
      {"lok1", 2870},
#line 2807 "zhy_symbol_map"
      {"ling7", 2799},
      {""}, {""}, {""}, {""},
#line 2695 "zhy_symbol_map"
      {"lang7", 2687},
      {""}, {""}, {""}, {""},
#line 2806 "zhy_symbol_map"
      {"ling6", 2798},
      {""}, {""},
#line 77 "zhy_symbol_map"
      {"ai7", 69},
      {""},
#line 2694 "zhy_symbol_map"
      {"lang6", 2686},
      {""}, {""},
#line 14 "zhy_symbol_map"
      {"aa7", 6},
      {""},
#line 2803 "zhy_symbol_map"
      {"ling3", 2795},
      {""}, {""}, {""}, {""},
#line 2691 "zhy_symbol_map"
      {"lang3", 2683},
      {""}, {""}, {""}, {""},
#line 2802 "zhy_symbol_map"
      {"ling2", 2794},
      {""}, {""}, {""}, {""},
#line 2690 "zhy_symbol_map"
      {"lang2", 2682},
      {""}, {""}, {""}, {""},
#line 2805 "zhy_symbol_map"
      {"ling5", 2797},
#line 1401 "zhy_symbol_map"
      {"go1", 1393},
      {""}, {""}, {""},
#line 2693 "zhy_symbol_map"
      {"lang5", 2685},
      {""}, {""}, {""}, {""},
#line 2801 "zhy_symbol_map"
      {"ling1", 2793},
      {""}, {""}, {""},
#line 2604 "zhy_symbol_map"
      {"laa7", 2596},
#line 2689 "zhy_symbol_map"
      {"lang1", 2681},
      {""},
#line 2994 "zhy_symbol_map"
      {"mai5", 2986},
#line 1406 "zhy_symbol_map"
      {"go6", 1398},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 119 "zhy_symbol_map"
      {"at7", 111},
      {""}, {""}, {""}, {""},
#line 112 "zhy_symbol_map"
      {"ap7", 104},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4311 "zhy_symbol_map"
      {"tai6", 4303},
      {""},
#line 4308 "zhy_symbol_map"
      {"tai3", 4300},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4307 "zhy_symbol_map"
      {"tai2", 4299},
      {""},
#line 3319 "zhy_symbol_map"
      {"ng1", 3311},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2800 "zhy_symbol_map"
      {"lin7", 2792},
      {""}, {""}, {""}, {""},
#line 2688 "zhy_symbol_map"
      {"lan7", 2680},
      {""}, {""}, {""},
#line 3324 "zhy_symbol_map"
      {"ng6", 3316},
      {""}, {""}, {""}, {""}, {""},
#line 4306 "zhy_symbol_map"
      {"tai1", 4298},
      {""}, {""}, {""}, {""},
#line 2884 "zhy_symbol_map"
      {"lok7", 2876},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2891 "zhy_symbol_map"
      {"long7", 2883},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2890 "zhy_symbol_map"
      {"long6", 2882},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2887 "zhy_symbol_map"
      {"long3", 2879},
#line 71 "zhy_symbol_map"
      {"ai1", 63},
      {""}, {""}, {""}, {""},
#line 8 "zhy_symbol_map"
      {"aa1", 0},
      {""}, {""}, {""},
#line 2886 "zhy_symbol_map"
      {"long2", 2878},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 76 "zhy_symbol_map"
      {"ai6", 68},
      {""},
#line 2889 "zhy_symbol_map"
      {"long5", 2881},
      {""}, {""},
#line 13 "zhy_symbol_map"
      {"aa6", 5},
      {""}, {""},
#line 3206 "zhy_symbol_map"
      {"naang7", 3198},
      {""}, {""}, {""},
#line 2885 "zhy_symbol_map"
      {"long1", 2877},
#line 3205 "zhy_symbol_map"
      {"naang6", 3197},
#line 3113 "zhy_symbol_map"
      {"moi5", 3105},
      {""}, {""}, {""},
#line 3202 "zhy_symbol_map"
      {"naang3", 3194},
      {""}, {""}, {""}, {""},
#line 3201 "zhy_symbol_map"
      {"naang2", 3193},
      {""}, {""}, {""}, {""},
#line 3204 "zhy_symbol_map"
      {"naang5", 3196},
      {""}, {""}, {""}, {""},
#line 3200 "zhy_symbol_map"
      {"naang1", 3192},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 113 "zhy_symbol_map"
      {"at1", 105},
#line 4458 "zhy_symbol_map"
      {"toi6", 4450},
      {""},
#line 4455 "zhy_symbol_map"
      {"toi3", 4447},
      {""},
#line 106 "zhy_symbol_map"
      {"ap1", 98},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4454 "zhy_symbol_map"
      {"toi2", 4446},
      {""}, {""}, {""},
#line 118 "zhy_symbol_map"
      {"at6", 110},
      {""}, {""}, {""}, {""},
#line 111 "zhy_symbol_map"
      {"ap6", 103},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4312 "zhy_symbol_map"
      {"tai7", 4304},
      {""}, {""}, {""}, {""},
#line 4453 "zhy_symbol_map"
      {"toi1", 4445},
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
#line 2841 "zhy_symbol_map"
      {"loe6", 2833},
      {""},
#line 2838 "zhy_symbol_map"
      {"loe3", 2830},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2837 "zhy_symbol_map"
      {"loe2", 2829},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4569 "zhy_symbol_map"
      {"wai5", 4561},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2836 "zhy_symbol_map"
      {"loe1", 2828},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4459 "zhy_symbol_map"
      {"toi7", 4451},
      {""}, {""}, {""}, {""}, {""},
#line 2856 "zhy_symbol_map"
      {"loek7", 2848},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2855 "zhy_symbol_map"
      {"loek6", 2847},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2852 "zhy_symbol_map"
      {"loek3", 2844},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2851 "zhy_symbol_map"
      {"loek2", 2843},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2854 "zhy_symbol_map"
      {"loek5", 2846},
      {""}, {""}, {""}, {""},
#line 2632 "zhy_symbol_map"
      {"laan7", 2624},
      {""}, {""}, {""}, {""},
#line 2850 "zhy_symbol_map"
      {"loek1", 2842},
      {""}, {""}, {""}, {""},
#line 2631 "zhy_symbol_map"
      {"laan6", 2623},
      {""}, {""}, {""}, {""},
#line 3213 "zhy_symbol_map"
      {"naap7", 3205},
      {""}, {""}, {""}, {""},
#line 2628 "zhy_symbol_map"
      {"laan3", 2620},
      {""}, {""}, {""}, {""},
#line 3212 "zhy_symbol_map"
      {"naap6", 3204},
      {""}, {""}, {""}, {""},
#line 2627 "zhy_symbol_map"
      {"laan2", 2619},
      {""}, {""}, {""}, {""},
#line 3209 "zhy_symbol_map"
      {"naap3", 3201},
      {""}, {""}, {""}, {""},
#line 2630 "zhy_symbol_map"
      {"laan5", 2622},
      {""},
#line 1356 "zhy_symbol_map"
      {"gik5", 1348},
      {""}, {""},
#line 3208 "zhy_symbol_map"
      {"naap2", 3200},
#line 4384 "zhy_symbol_map"
      {"ti2", 4376},
#line 1265 "zhy_symbol_map"
      {"gak5", 1257},
      {""}, {""},
#line 2626 "zhy_symbol_map"
      {"laan1", 2618},
      {""}, {""}, {""}, {""},
#line 3211 "zhy_symbol_map"
      {"naap5", 3203},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3207 "zhy_symbol_map"
      {"naap1", 3199},
      {""}, {""}, {""},
#line 2842 "zhy_symbol_map"
      {"loe7", 2834},
      {""}, {""},
#line 3649 "zhy_symbol_map"
      {"o2", 3641},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1195 "zhy_symbol_map"
      {"gaa5", 1187},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4871 "zhy_symbol_map"
      {"zik6", 4863},
      {""},
#line 4868 "zhy_symbol_map"
      {"zik3", 4860},
#line 2611 "zhy_symbol_map"
      {"laai7", 2603},
      {""},
#line 4773 "zhy_symbol_map"
      {"zak6", 4765},
      {""},
#line 4770 "zhy_symbol_map"
      {"zak3", 4762},
      {""}, {""}, {""},
#line 4389 "zhy_symbol_map"
      {"ti7", 4381},
#line 4867 "zhy_symbol_map"
      {"zik2", 4859},
#line 2610 "zhy_symbol_map"
      {"laai6", 2602},
      {""}, {""}, {""},
#line 4769 "zhy_symbol_map"
      {"zak2", 4761},
      {""}, {""}, {""}, {""}, {""},
#line 2607 "zhy_symbol_map"
      {"laai3", 2599},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1370 "zhy_symbol_map"
      {"gin5", 1362},
      {""}, {""},
#line 2606 "zhy_symbol_map"
      {"laai2", 2598},
      {""},
#line 1279 "zhy_symbol_map"
      {"gan5", 1271},
      {""},
#line 4866 "zhy_symbol_map"
      {"zik1", 4858},
      {""}, {""}, {""}, {""},
#line 4768 "zhy_symbol_map"
      {"zak1", 4760},
#line 2609 "zhy_symbol_map"
      {"laai5", 2601},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1447 "zhy_symbol_map"
      {"gok5", 1439},
      {""}, {""},
#line 2605 "zhy_symbol_map"
      {"laai1", 2597},
#line 4440 "zhy_symbol_map"
      {"to2", 4432},
      {""}, {""}, {""},
#line 4760 "zhy_symbol_map"
      {"zaau7", 4752},
      {""}, {""}, {""}, {""},
#line 4725 "zhy_symbol_map"
      {"zaam7", 4717},
      {""},
#line 4703 "zhy_symbol_map"
      {"zaa6", 4695},
      {""},
#line 4700 "zhy_symbol_map"
      {"zaa3", 4692},
#line 4759 "zhy_symbol_map"
      {"zaau6", 4751},
#line 3598 "zhy_symbol_map"
      {"noeng7", 3590},
      {""}, {""}, {""},
#line 4724 "zhy_symbol_map"
      {"zaam6", 4716},
#line 3597 "zhy_symbol_map"
      {"noeng6", 3589},
      {""}, {""},
#line 4699 "zhy_symbol_map"
      {"zaa2", 4691},
#line 4756 "zhy_symbol_map"
      {"zaau3", 4748},
#line 3594 "zhy_symbol_map"
      {"noeng3", 3586},
      {""}, {""}, {""},
#line 4721 "zhy_symbol_map"
      {"zaam3", 4713},
#line 3593 "zhy_symbol_map"
      {"noeng2", 3585},
      {""}, {""}, {""},
#line 4755 "zhy_symbol_map"
      {"zaau2", 4747},
#line 3596 "zhy_symbol_map"
      {"noeng5", 3588},
      {""}, {""}, {""},
#line 4720 "zhy_symbol_map"
      {"zaam2", 4712},
#line 3592 "zhy_symbol_map"
      {"noeng1", 3584},
      {""}, {""}, {""},
#line 4758 "zhy_symbol_map"
      {"zaau5", 4750},
      {""}, {""}, {""},
#line 4698 "zhy_symbol_map"
      {"zaa1", 4690},
#line 4723 "zhy_symbol_map"
      {"zaam5", 4715},
      {""}, {""}, {""}, {""},
#line 4754 "zhy_symbol_map"
      {"zaau1", 4746},
      {""}, {""}, {""}, {""},
#line 4719 "zhy_symbol_map"
      {"zaam1", 4711},
      {""},
#line 3232 "zhy_symbol_map"
      {"nai5", 3224},
      {""}, {""},
#line 4718 "zhy_symbol_map"
      {"zaak7", 4710},
      {""},
#line 4885 "zhy_symbol_map"
      {"zin6", 4877},
      {""},
#line 4882 "zhy_symbol_map"
      {"zin3", 4874},
      {""}, {""},
#line 4787 "zhy_symbol_map"
      {"zan6", 4779},
      {""},
#line 4784 "zhy_symbol_map"
      {"zan3", 4776},
#line 4717 "zhy_symbol_map"
      {"zaak6", 4709},
      {""}, {""}, {""},
#line 4881 "zhy_symbol_map"
      {"zin2", 4873},
      {""}, {""}, {""}, {""},
#line 4783 "zhy_symbol_map"
      {"zan2", 4775},
#line 4714 "zhy_symbol_map"
      {"zaak3", 4706},
      {""},
#line 4962 "zhy_symbol_map"
      {"zok6", 4954},
      {""},
#line 4959 "zhy_symbol_map"
      {"zok3", 4951},
      {""}, {""}, {""}, {""}, {""},
#line 4713 "zhy_symbol_map"
      {"zaak2", 4705},
      {""}, {""},
#line 4445 "zhy_symbol_map"
      {"to7", 4437},
#line 4958 "zhy_symbol_map"
      {"zok2", 4950},
      {""}, {""}, {""}, {""},
#line 4880 "zhy_symbol_map"
      {"zin1", 4872},
#line 4716 "zhy_symbol_map"
      {"zaak5", 4708},
#line 4383 "zhy_symbol_map"
      {"ti1", 4375},
      {""}, {""},
#line 4782 "zhy_symbol_map"
      {"zan1", 4774},
      {""}, {""}, {""}, {""},
#line 4872 "zhy_symbol_map"
      {"zik7", 4864},
#line 4712 "zhy_symbol_map"
      {"zaak1", 4704},
      {""},
#line 1454 "zhy_symbol_map"
      {"gon5", 1446},
      {""},
#line 4774 "zhy_symbol_map"
      {"zak7", 4766},
      {""}, {""},
#line 26 "zhy_symbol_map"
      {"aak5", 18},
#line 4388 "zhy_symbol_map"
      {"ti6", 4380},
#line 4957 "zhy_symbol_map"
      {"zok1", 4949},
#line 4893 "zhy_symbol_map"
      {"zing7", 4885},
      {""}, {""}, {""}, {""},
#line 4795 "zhy_symbol_map"
      {"zang7", 4787},
      {""},
#line 2477 "zhy_symbol_map"
      {"kuk6", 2469},
      {""},
#line 2474 "zhy_symbol_map"
      {"kuk3", 2466},
#line 4892 "zhy_symbol_map"
      {"zing6", 4884},
      {""}, {""}, {""}, {""},
#line 4794 "zhy_symbol_map"
      {"zang6", 4786},
      {""}, {""}, {""},
#line 2473 "zhy_symbol_map"
      {"kuk2", 2465},
#line 4889 "zhy_symbol_map"
      {"zing3", 4881},
      {""}, {""}, {""}, {""},
#line 4791 "zhy_symbol_map"
      {"zang3", 4783},
      {""}, {""}, {""}, {""},
#line 4888 "zhy_symbol_map"
      {"zing2", 4880},
      {""}, {""}, {""}, {""},
#line 4790 "zhy_symbol_map"
      {"zang2", 4782},
      {""}, {""}, {""}, {""},
#line 4891 "zhy_symbol_map"
      {"zing5", 4883},
      {""}, {""}, {""},
#line 2472 "zhy_symbol_map"
      {"kuk1", 2464},
#line 4793 "zhy_symbol_map"
      {"zang5", 4785},
      {""}, {""}, {""}, {""},
#line 4887 "zhy_symbol_map"
      {"zing1", 4879},
      {""}, {""}, {""},
#line 4704 "zhy_symbol_map"
      {"zaa7", 4696},
#line 4789 "zhy_symbol_map"
      {"zang1", 4781},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3603 "zhy_symbol_map"
      {"noi5", 3595},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2666 "zhy_symbol_map"
      {"lai6", 2658},
      {""},
#line 2663 "zhy_symbol_map"
      {"lai3", 2655},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2662 "zhy_symbol_map"
      {"lai2", 2654},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4886 "zhy_symbol_map"
      {"zin7", 4878},
      {""}, {""}, {""}, {""},
#line 4788 "zhy_symbol_map"
      {"zan7", 4780},
      {""}, {""},
#line 40 "zhy_symbol_map"
      {"aan5", 32},
      {""}, {""}, {""},
#line 4439 "zhy_symbol_map"
      {"to1", 4431},
      {""}, {""},
#line 2661 "zhy_symbol_map"
      {"lai1", 2653},
      {""}, {""}, {""}, {""},
#line 4963 "zhy_symbol_map"
      {"zok7", 4955},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4444 "zhy_symbol_map"
      {"to6", 4436},
      {""},
#line 4970 "zhy_symbol_map"
      {"zong7", 4962},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4969 "zhy_symbol_map"
      {"zong6", 4961},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4966 "zhy_symbol_map"
      {"zong3", 4958},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4965 "zhy_symbol_map"
      {"zong2", 4957},
      {""}, {""}, {""},
#line 2478 "zhy_symbol_map"
      {"kuk7", 2470},
      {""}, {""}, {""}, {""}, {""},
#line 4968 "zhy_symbol_map"
      {"zong5", 4960},
      {""}, {""}, {""}, {""},
#line 2485 "zhy_symbol_map"
      {"kung7", 2477},
      {""}, {""}, {""}, {""},
#line 4964 "zhy_symbol_map"
      {"zong1", 4956},
      {""}, {""}, {""}, {""},
#line 2484 "zhy_symbol_map"
      {"kung6", 2476},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2481 "zhy_symbol_map"
      {"kung3", 2473},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2480 "zhy_symbol_map"
      {"kung2", 2472},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2876 "zhy_symbol_map"
      {"loi6", 2868},
      {""},
#line 2873 "zhy_symbol_map"
      {"loi3", 2865},
#line 2483 "zhy_symbol_map"
      {"kung5", 2475},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2872 "zhy_symbol_map"
      {"loi2", 2864},
#line 2479 "zhy_symbol_map"
      {"kung1", 2471},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1232 "zhy_symbol_map"
      {"gaang7", 1224},
      {""}, {""}, {""}, {""},
#line 1231 "zhy_symbol_map"
      {"gaang6", 1223},
#line 1412 "zhy_symbol_map"
      {"goe5", 1404},
      {""},
#line 2667 "zhy_symbol_map"
      {"lai7", 2659},
      {""},
#line 1228 "zhy_symbol_map"
      {"gaang3", 1220},
      {""}, {""},
#line 2871 "zhy_symbol_map"
      {"loi1", 2863},
      {""},
#line 1227 "zhy_symbol_map"
      {"gaang2", 1219},
      {""}, {""}, {""}, {""},
#line 1230 "zhy_symbol_map"
      {"gaang5", 1222},
      {""}, {""}, {""}, {""},
#line 1226 "zhy_symbol_map"
      {"gaang1", 1218},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2849 "zhy_symbol_map"
      {"loei7", 2841},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2848 "zhy_symbol_map"
      {"loei6", 2840},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2845 "zhy_symbol_map"
      {"loei3", 2837},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2844 "zhy_symbol_map"
      {"loei2", 2836},
      {""},
#line 3449 "zhy_symbol_map"
      {"nge5", 3441},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2847 "zhy_symbol_map"
      {"loei5", 2839},
      {""},
#line 4220 "zhy_symbol_map"
      {"suk6", 4212},
      {""},
#line 4217 "zhy_symbol_map"
      {"suk3", 4209},
      {""}, {""}, {""}, {""}, {""},
#line 2843 "zhy_symbol_map"
      {"loei1", 2835},
      {""}, {""}, {""},
#line 4216 "zhy_symbol_map"
      {"suk2", 4208},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4927 "zhy_symbol_map"
      {"zoe6", 4919},
      {""},
#line 4924 "zhy_symbol_map"
      {"zoe3", 4916},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4923 "zhy_symbol_map"
      {"zoe2", 4915},
      {""}, {""}, {""}, {""},
#line 4215 "zhy_symbol_map"
      {"suk1", 4207},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4922 "zhy_symbol_map"
      {"zoe1", 4914},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2877 "zhy_symbol_map"
      {"loi7", 2869},
      {""}, {""}, {""}, {""}, {""},
#line 4942 "zhy_symbol_map"
      {"zoek7", 4934},
      {""},
#line 2935 "zhy_symbol_map"
      {"m2", 2927},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4941 "zhy_symbol_map"
      {"zoek6", 4933},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4938 "zhy_symbol_map"
      {"zoek3", 4930},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4937 "zhy_symbol_map"
      {"zoek2", 4929},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4940 "zhy_symbol_map"
      {"zoek5", 4932},
      {""}, {""}, {""}, {""},
#line 4732 "zhy_symbol_map"
      {"zaan7", 4724},
#line 3423 "zhy_symbol_map"
      {"ngang7", 3415},
      {""}, {""}, {""},
#line 4936 "zhy_symbol_map"
      {"zoek1", 4928},
#line 3422 "zhy_symbol_map"
      {"ngang6", 3414},
      {""}, {""}, {""},
#line 4731 "zhy_symbol_map"
      {"zaan6", 4723},
#line 3419 "zhy_symbol_map"
      {"ngang3", 3411},
      {""}, {""}, {""}, {""},
#line 3418 "zhy_symbol_map"
      {"ngang2", 3410},
      {""}, {""}, {""},
#line 4728 "zhy_symbol_map"
      {"zaan3", 4720},
#line 3421 "zhy_symbol_map"
      {"ngang5", 3413},
      {""}, {""}, {""}, {""},
#line 3417 "zhy_symbol_map"
      {"ngang1", 3409},
      {""}, {""}, {""},
#line 4727 "zhy_symbol_map"
      {"zaan2", 4719},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4730 "zhy_symbol_map"
      {"zaan5", 4722},
      {""}, {""}, {""}, {""}, {""},
#line 2774 "zhy_symbol_map"
      {"li2", 2766},
      {""}, {""},
#line 4221 "zhy_symbol_map"
      {"suk7", 4213},
#line 4726 "zhy_symbol_map"
      {"zaan1", 4718},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4228 "zhy_symbol_map"
      {"sung7", 4220},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4928 "zhy_symbol_map"
      {"zoe7", 4920},
#line 4227 "zhy_symbol_map"
      {"sung6", 4219},
      {""}, {""}, {""}, {""},
#line 1239 "zhy_symbol_map"
      {"gaap7", 1231},
      {""}, {""}, {""}, {""},
#line 4224 "zhy_symbol_map"
      {"sung3", 4216},
      {""}, {""}, {""}, {""},
#line 1238 "zhy_symbol_map"
      {"gaap6", 1230},
      {""}, {""}, {""}, {""},
#line 4223 "zhy_symbol_map"
      {"sung2", 4215},
      {""}, {""}, {""}, {""},
#line 1235 "zhy_symbol_map"
      {"gaap3", 1227},
      {""}, {""}, {""}, {""},
#line 4226 "zhy_symbol_map"
      {"sung5", 4218},
      {""}, {""}, {""}, {""},
#line 1234 "zhy_symbol_map"
      {"gaap2", 1226},
      {""}, {""}, {""}, {""},
#line 4222 "zhy_symbol_map"
      {"sung1", 4214},
      {""}, {""}, {""}, {""},
#line 1237 "zhy_symbol_map"
      {"gaap5", 1229},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4394 "zhy_symbol_map"
      {"tik5", 4386},
      {""}, {""},
#line 1233 "zhy_symbol_map"
      {"gaap1", 1225},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4711 "zhy_symbol_map"
      {"zaai7", 4703},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2779 "zhy_symbol_map"
      {"li7", 2771},
      {""},
#line 4710 "zhy_symbol_map"
      {"zaai6", 4702},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4707 "zhy_symbol_map"
      {"zaai3", 4699},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4706 "zhy_symbol_map"
      {"zaai2", 4698},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4709 "zhy_symbol_map"
      {"zaai5", 4701},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4705 "zhy_symbol_map"
      {"zaai1", 4697},
#line 2830 "zhy_symbol_map"
      {"lo2", 2822},
#line 4254 "zhy_symbol_map"
      {"taa5", 4246},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3149 "zhy_symbol_map"
      {"muk6", 3141},
      {""},
#line 3146 "zhy_symbol_map"
      {"muk3", 3138},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3145 "zhy_symbol_map"
      {"muk2", 3137},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3144 "zhy_symbol_map"
      {"muk1", 3136},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4408 "zhy_symbol_map"
      {"tin5", 4400},
      {""}, {""}, {""}, {""},
#line 4324 "zhy_symbol_map"
      {"tan5", 4316},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1435 "zhy_symbol_map"
      {"goeng7", 1427},
#line 4464 "zhy_symbol_map"
      {"tok5", 4456},
      {""}, {""}, {""},
#line 1434 "zhy_symbol_map"
      {"goeng6", 1426},
      {""}, {""}, {""},
#line 3430 "zhy_symbol_map"
      {"ngap7", 3422},
#line 1431 "zhy_symbol_map"
      {"goeng3", 1423},
      {""}, {""}, {""}, {""},
#line 1430 "zhy_symbol_map"
      {"goeng2", 1422},
      {""}, {""}, {""},
#line 3429 "zhy_symbol_map"
      {"ngap6", 3421},
#line 1433 "zhy_symbol_map"
      {"goeng5", 1425},
      {""},
#line 2835 "zhy_symbol_map"
      {"lo7", 2827},
      {""}, {""},
#line 1429 "zhy_symbol_map"
      {"goeng1", 1421},
      {""}, {""}, {""},
#line 3426 "zhy_symbol_map"
      {"ngap3", 3418},
#line 2773 "zhy_symbol_map"
      {"li1", 2765},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3425 "zhy_symbol_map"
      {"ngap2", 3417},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1258 "zhy_symbol_map"
      {"gai5", 1250},
#line 2778 "zhy_symbol_map"
      {"li6", 2770},
      {""},
#line 3428 "zhy_symbol_map"
      {"ngap5", 3420},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3156 "zhy_symbol_map"
      {"mun6", 3148},
      {""},
#line 3153 "zhy_symbol_map"
      {"mun3", 3145},
#line 3424 "zhy_symbol_map"
      {"ngap1", 3416},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3152 "zhy_symbol_map"
      {"mun2", 3144},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3151 "zhy_symbol_map"
      {"mun1", 3143},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3150 "zhy_symbol_map"
      {"muk7", 3142},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3164 "zhy_symbol_map"
      {"mung7", 3156},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3163 "zhy_symbol_map"
      {"mung6", 3155},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3160 "zhy_symbol_map"
      {"mung3", 3152},
      {""},
#line 4766 "zhy_symbol_map"
      {"zai6", 4758},
      {""},
#line 4763 "zhy_symbol_map"
      {"zai3", 4755},
      {""}, {""}, {""}, {""}, {""},
#line 3159 "zhy_symbol_map"
      {"mung2", 3151},
      {""}, {""}, {""},
#line 4762 "zhy_symbol_map"
      {"zai2", 4754},
      {""}, {""}, {""}, {""}, {""},
#line 3162 "zhy_symbol_map"
      {"mung5", 3154},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3158 "zhy_symbol_map"
      {"mung1", 3150},
      {""}, {""}, {""}, {""}, {""},
#line 2829 "zhy_symbol_map"
      {"lo1", 2821},
      {""}, {""},
#line 4761 "zhy_symbol_map"
      {"zai1", 4753},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1440 "zhy_symbol_map"
      {"goi5", 1432},
      {""}, {""}, {""}, {""}, {""},
#line 2834 "zhy_symbol_map"
      {"lo6", 2826},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3157 "zhy_symbol_map"
      {"mun7", 3149},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3456 "zhy_symbol_map"
      {"ngi5", 3448},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4955 "zhy_symbol_map"
      {"zoi6", 4947},
      {""},
#line 4952 "zhy_symbol_map"
      {"zoi3", 4944},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4951 "zhy_symbol_map"
      {"zoi2", 4943},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4767 "zhy_symbol_map"
      {"zai7", 4759},
      {""}, {""},
#line 19 "zhy_symbol_map"
      {"aai5", 11},
      {""},
#line 4950 "zhy_symbol_map"
      {"zoi1", 4942},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2470 "zhy_symbol_map"
      {"kui6", 2462},
      {""},
#line 2467 "zhy_symbol_map"
      {"kui3", 2459},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2466 "zhy_symbol_map"
      {"kui2", 2458},
#line 4935 "zhy_symbol_map"
      {"zoei7", 4927},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4934 "zhy_symbol_map"
      {"zoei6", 4926},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4931 "zhy_symbol_map"
      {"zoei3", 4923},
      {""}, {""}, {""},
#line 2465 "zhy_symbol_map"
      {"kui1", 2457},
      {""}, {""}, {""}, {""}, {""},
#line 4930 "zhy_symbol_map"
      {"zoei2", 4922},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4933 "zhy_symbol_map"
      {"zoei5", 4925},
      {""},
#line 4689 "zhy_symbol_map"
      {"wun6", 4681},
      {""},
#line 4686 "zhy_symbol_map"
      {"wun3", 4678},
      {""},
#line 4284 "zhy_symbol_map"
      {"taang7", 4276},
      {""}, {""}, {""},
#line 4929 "zhy_symbol_map"
      {"zoei1", 4921},
#line 4283 "zhy_symbol_map"
      {"taang6", 4275},
#line 4450 "zhy_symbol_map"
      {"toe5", 4442},
      {""},
#line 4685 "zhy_symbol_map"
      {"wun2", 4677},
      {""},
#line 4280 "zhy_symbol_map"
      {"taang3", 4272},
      {""}, {""}, {""}, {""},
#line 4279 "zhy_symbol_map"
      {"taang2", 4271},
      {""}, {""}, {""}, {""},
#line 4282 "zhy_symbol_map"
      {"taang5", 4274},
      {""}, {""}, {""}, {""},
#line 4278 "zhy_symbol_map"
      {"taang1", 4270},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4684 "zhy_symbol_map"
      {"wun1", 4676},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4956 "zhy_symbol_map"
      {"zoi7", 4948},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2471 "zhy_symbol_map"
      {"kui7", 2463},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4860 "zhy_symbol_map"
      {"zi2", 4852},
      {""}, {""},
#line 4690 "zhy_symbol_map"
      {"wun7", 4682},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3632 "zhy_symbol_map"
      {"nuk6", 3624},
      {""},
#line 3629 "zhy_symbol_map"
      {"nuk3", 3621},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3628 "zhy_symbol_map"
      {"nuk2", 3620},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3627 "zhy_symbol_map"
      {"nuk1", 3619},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2784 "zhy_symbol_map"
      {"lik5", 2776},
      {""}, {""}, {""}, {""},
#line 2672 "zhy_symbol_map"
      {"lak5", 2664},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4865 "zhy_symbol_map"
      {"zi7", 4857},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4291 "zhy_symbol_map"
      {"taap7", 4283},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4290 "zhy_symbol_map"
      {"taap6", 4282},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4287 "zhy_symbol_map"
      {"taap3", 4279},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4286 "zhy_symbol_map"
      {"taap2", 4278},
#line 4916 "zhy_symbol_map"
      {"zo2", 4908},
#line 2602 "zhy_symbol_map"
      {"laa5", 2594},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4289 "zhy_symbol_map"
      {"taap5", 4281},
      {""},
#line 2407 "zhy_symbol_map"
      {"kiu6", 2399},
      {""},
#line 2404 "zhy_symbol_map"
      {"kiu3", 2396},
      {""}, {""},
#line 2316 "zhy_symbol_map"
      {"kau6", 2308},
      {""},
#line 2313 "zhy_symbol_map"
      {"kau3", 2305},
#line 4285 "zhy_symbol_map"
      {"taap1", 4277},
      {""}, {""}, {""},
#line 2403 "zhy_symbol_map"
      {"kiu2", 2395},
      {""}, {""}, {""}, {""},
#line 2312 "zhy_symbol_map"
      {"kau2", 2304},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3698 "zhy_symbol_map"
      {"ou2", 3690},
      {""}, {""}, {""}, {""},
#line 2459 "zhy_symbol_map"
      {"ku2", 2451},
      {""}, {""},
#line 2402 "zhy_symbol_map"
      {"kiu1", 2394},
      {""}, {""}, {""}, {""},
#line 2311 "zhy_symbol_map"
      {"kau1", 2303},
      {""}, {""},
#line 2798 "zhy_symbol_map"
      {"lin5", 2790},
      {""},
#line 3633 "zhy_symbol_map"
      {"nuk7", 3625},
      {""}, {""},
#line 2686 "zhy_symbol_map"
      {"lan5", 2678},
      {""}, {""}, {""}, {""},
#line 531 "zhy_symbol_map"
      {"cik6", 523},
      {""},
#line 528 "zhy_symbol_map"
      {"cik3", 520},
#line 3640 "zhy_symbol_map"
      {"nung7", 3632},
      {""},
#line 433 "zhy_symbol_map"
      {"cak6", 425},
      {""},
#line 430 "zhy_symbol_map"
      {"cak3", 422},
      {""}, {""},
#line 2882 "zhy_symbol_map"
      {"lok5", 2874},
      {""},
#line 527 "zhy_symbol_map"
      {"cik2", 519},
#line 3639 "zhy_symbol_map"
      {"nung6", 3631},
      {""}, {""}, {""},
#line 429 "zhy_symbol_map"
      {"cak2", 421},
      {""}, {""}, {""}, {""}, {""},
#line 3636 "zhy_symbol_map"
      {"nung3", 3628},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4921 "zhy_symbol_map"
      {"zo7", 4913},
      {""},
#line 3635 "zhy_symbol_map"
      {"nung2", 3627},
      {""}, {""}, {""},
#line 526 "zhy_symbol_map"
      {"cik1", 518},
      {""},
#line 4859 "zhy_symbol_map"
      {"zi1", 4851},
      {""}, {""},
#line 428 "zhy_symbol_map"
      {"cak1", 420},
#line 3638 "zhy_symbol_map"
      {"nung5", 3630},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3634 "zhy_symbol_map"
      {"nung1", 3626},
      {""}, {""},
#line 4864 "zhy_symbol_map"
      {"zi6", 4856},
      {""},
#line 420 "zhy_symbol_map"
      {"caau7", 412},
      {""}, {""}, {""}, {""},
#line 385 "zhy_symbol_map"
      {"caam7", 377},
      {""},
#line 363 "zhy_symbol_map"
      {"caa6", 355},
      {""},
#line 360 "zhy_symbol_map"
      {"caa3", 352},
#line 419 "zhy_symbol_map"
      {"caau6", 411},
      {""}, {""},
#line 3703 "zhy_symbol_map"
      {"ou7", 3695},
      {""},
#line 384 "zhy_symbol_map"
      {"caam6", 376},
      {""}, {""},
#line 2464 "zhy_symbol_map"
      {"ku7", 2456},
#line 359 "zhy_symbol_map"
      {"caa2", 351},
#line 416 "zhy_symbol_map"
      {"caau3", 408},
      {""}, {""}, {""}, {""},
#line 381 "zhy_symbol_map"
      {"caam3", 373},
      {""}, {""}, {""}, {""},
#line 415 "zhy_symbol_map"
      {"caau2", 407},
      {""}, {""}, {""}, {""},
#line 380 "zhy_symbol_map"
      {"caam2", 372},
      {""}, {""}, {""}, {""},
#line 418 "zhy_symbol_map"
      {"caau5", 410},
      {""}, {""}, {""},
#line 358 "zhy_symbol_map"
      {"caa1", 350},
#line 383 "zhy_symbol_map"
      {"caam5", 375},
      {""}, {""}, {""}, {""},
#line 414 "zhy_symbol_map"
      {"caau1", 406},
      {""}, {""}, {""},
#line 2408 "zhy_symbol_map"
      {"kiu7", 2400},
#line 379 "zhy_symbol_map"
      {"caam1", 371},
      {""}, {""}, {""},
#line 2317 "zhy_symbol_map"
      {"kau7", 2309},
#line 378 "zhy_symbol_map"
      {"caak7", 370},
      {""},
#line 545 "zhy_symbol_map"
      {"cin6", 537},
      {""},
#line 542 "zhy_symbol_map"
      {"cin3", 534},
      {""}, {""},
#line 447 "zhy_symbol_map"
      {"can6", 439},
      {""},
#line 444 "zhy_symbol_map"
      {"can3", 436},
#line 377 "zhy_symbol_map"
      {"caak6", 369},
      {""}, {""}, {""},
#line 541 "zhy_symbol_map"
      {"cin2", 533},
      {""}, {""},
#line 4310 "zhy_symbol_map"
      {"tai5", 4302},
      {""},
#line 443 "zhy_symbol_map"
      {"can2", 435},
#line 374 "zhy_symbol_map"
      {"caak3", 366},
      {""},
#line 615 "zhy_symbol_map"
      {"cok6", 607},
      {""},
#line 612 "zhy_symbol_map"
      {"cok3", 604},
      {""}, {""}, {""}, {""}, {""},
#line 373 "zhy_symbol_map"
      {"caak2", 365},
      {""}, {""}, {""},
#line 611 "zhy_symbol_map"
      {"cok2", 603},
      {""}, {""}, {""}, {""},
#line 540 "zhy_symbol_map"
      {"cin1", 532},
#line 376 "zhy_symbol_map"
      {"caak5", 368},
      {""}, {""}, {""},
#line 442 "zhy_symbol_map"
      {"can1", 434},
      {""}, {""}, {""}, {""},
#line 532 "zhy_symbol_map"
      {"cik7", 524},
#line 372 "zhy_symbol_map"
      {"caak1", 364},
      {""}, {""}, {""},
#line 434 "zhy_symbol_map"
      {"cak7", 426},
      {""}, {""}, {""}, {""},
#line 610 "zhy_symbol_map"
      {"cok1", 602},
#line 553 "zhy_symbol_map"
      {"cing7", 545},
#line 4915 "zhy_symbol_map"
      {"zo1", 4907},
      {""}, {""}, {""},
#line 455 "zhy_symbol_map"
      {"cang7", 447},
      {""}, {""}, {""}, {""},
#line 552 "zhy_symbol_map"
      {"cing6", 544},
      {""}, {""}, {""}, {""},
#line 454 "zhy_symbol_map"
      {"cang6", 446},
      {""}, {""},
#line 4920 "zhy_symbol_map"
      {"zo6", 4912},
      {""},
#line 549 "zhy_symbol_map"
      {"cing3", 541},
      {""}, {""}, {""}, {""},
#line 451 "zhy_symbol_map"
      {"cang3", 443},
      {""},
#line 3142 "zhy_symbol_map"
      {"mui6", 3134},
      {""},
#line 3139 "zhy_symbol_map"
      {"mui3", 3131},
#line 548 "zhy_symbol_map"
      {"cing2", 540},
      {""}, {""}, {""}, {""},
#line 450 "zhy_symbol_map"
      {"cang2", 442},
      {""}, {""}, {""},
#line 3138 "zhy_symbol_map"
      {"mui2", 3130},
#line 551 "zhy_symbol_map"
      {"cing5", 543},
#line 3697 "zhy_symbol_map"
      {"ou1", 3689},
      {""}, {""}, {""},
#line 453 "zhy_symbol_map"
      {"cang5", 445},
#line 2458 "zhy_symbol_map"
      {"ku1", 2450},
      {""}, {""}, {""},
#line 547 "zhy_symbol_map"
      {"cing1", 539},
      {""}, {""}, {""},
#line 364 "zhy_symbol_map"
      {"caa7", 356},
#line 449 "zhy_symbol_map"
      {"cang1", 441},
      {""}, {""},
#line 3702 "zhy_symbol_map"
      {"ou6", 3694},
      {""}, {""}, {""}, {""},
#line 2463 "zhy_symbol_map"
      {"ku6", 2455},
#line 3137 "zhy_symbol_map"
      {"mui1", 3129},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4150 "zhy_symbol_map"
      {"siu6", 4142},
      {""},
#line 4147 "zhy_symbol_map"
      {"siu3", 4139},
      {""}, {""},
#line 4045 "zhy_symbol_map"
      {"sau6", 4037},
      {""},
#line 4042 "zhy_symbol_map"
      {"sau3", 4034},
      {""}, {""},
#line 4457 "zhy_symbol_map"
      {"toi5", 4449},
      {""},
#line 4146 "zhy_symbol_map"
      {"siu2", 4138},
      {""}, {""}, {""}, {""},
#line 4041 "zhy_symbol_map"
      {"sau2", 4033},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 546 "zhy_symbol_map"
      {"cin7", 538},
      {""}, {""}, {""}, {""},
#line 448 "zhy_symbol_map"
      {"can7", 440},
      {""}, {""}, {""}, {""},
#line 4145 "zhy_symbol_map"
      {"siu1", 4137},
      {""}, {""}, {""}, {""},
#line 4040 "zhy_symbol_map"
      {"sau1", 4032},
      {""}, {""}, {""}, {""},
#line 616 "zhy_symbol_map"
      {"cok7", 608},
#line 860 "zhy_symbol_map"
      {"dik6", 852},
      {""},
#line 857 "zhy_symbol_map"
      {"dik3", 849},
      {""}, {""},
#line 748 "zhy_symbol_map"
      {"dak6", 740},
      {""},
#line 745 "zhy_symbol_map"
      {"dak3", 737},
      {""}, {""},
#line 623 "zhy_symbol_map"
      {"cong7", 615},
      {""},
#line 856 "zhy_symbol_map"
      {"dik2", 848},
      {""}, {""}, {""}, {""},
#line 744 "zhy_symbol_map"
      {"dak2", 736},
      {""}, {""},
#line 622 "zhy_symbol_map"
      {"cong6", 614},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 619 "zhy_symbol_map"
      {"cong3", 611},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 855 "zhy_symbol_map"
      {"dik1", 847},
      {""}, {""},
#line 618 "zhy_symbol_map"
      {"cong2", 610},
      {""},
#line 743 "zhy_symbol_map"
      {"dak1", 735},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 621 "zhy_symbol_map"
      {"cong5", 613},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 735 "zhy_symbol_map"
      {"daau7", 727},
      {""},
#line 617 "zhy_symbol_map"
      {"cong1", 609},
      {""}, {""},
#line 700 "zhy_symbol_map"
      {"daam7", 692},
#line 3143 "zhy_symbol_map"
      {"mui7", 3135},
#line 678 "zhy_symbol_map"
      {"daa6", 670},
      {""},
#line 675 "zhy_symbol_map"
      {"daa3", 667},
#line 734 "zhy_symbol_map"
      {"daau6", 726},
      {""}, {""}, {""}, {""},
#line 699 "zhy_symbol_map"
      {"daam6", 691},
      {""}, {""},
#line 2639 "zhy_symbol_map"
      {"laang7", 2631},
#line 674 "zhy_symbol_map"
      {"daa2", 666},
#line 731 "zhy_symbol_map"
      {"daau3", 723},
      {""}, {""},
#line 2638 "zhy_symbol_map"
      {"laang6", 2630},
#line 2840 "zhy_symbol_map"
      {"loe5", 2832},
#line 696 "zhy_symbol_map"
      {"daam3", 688},
      {""}, {""},
#line 2635 "zhy_symbol_map"
      {"laang3", 2627},
      {""},
#line 730 "zhy_symbol_map"
      {"daau2", 722},
      {""}, {""},
#line 2634 "zhy_symbol_map"
      {"laang2", 2626},
#line 4213 "zhy_symbol_map"
      {"sou6", 4205},
#line 695 "zhy_symbol_map"
      {"daam2", 687},
#line 4210 "zhy_symbol_map"
      {"sou3", 4202},
      {""},
#line 2637 "zhy_symbol_map"
      {"laang5", 2629},
      {""},
#line 733 "zhy_symbol_map"
      {"daau5", 725},
      {""}, {""},
#line 2633 "zhy_symbol_map"
      {"laang1", 2625},
#line 673 "zhy_symbol_map"
      {"daa1", 665},
#line 698 "zhy_symbol_map"
      {"daam5", 690},
#line 4209 "zhy_symbol_map"
      {"sou2", 4201},
      {""}, {""}, {""},
#line 729 "zhy_symbol_map"
      {"daau1", 721},
      {""}, {""}, {""}, {""},
#line 694 "zhy_symbol_map"
      {"daam1", 686},
      {""}, {""}, {""}, {""},
#line 693 "zhy_symbol_map"
      {"daak7", 685},
#line 4151 "zhy_symbol_map"
      {"siu7", 4143},
#line 874 "zhy_symbol_map"
      {"din6", 866},
      {""},
#line 871 "zhy_symbol_map"
      {"din3", 863},
      {""},
#line 4046 "zhy_symbol_map"
      {"sau7", 4038},
#line 762 "zhy_symbol_map"
      {"dan6", 754},
      {""},
#line 759 "zhy_symbol_map"
      {"dan3", 751},
#line 692 "zhy_symbol_map"
      {"daak6", 684},
#line 4208 "zhy_symbol_map"
      {"sou1", 4200},
      {""}, {""},
#line 870 "zhy_symbol_map"
      {"din2", 862},
      {""}, {""}, {""}, {""},
#line 758 "zhy_symbol_map"
      {"dan2", 750},
#line 689 "zhy_symbol_map"
      {"daak3", 681},
      {""},
#line 944 "zhy_symbol_map"
      {"dok6", 936},
      {""},
#line 941 "zhy_symbol_map"
      {"dok3", 933},
      {""}, {""}, {""}, {""}, {""},
#line 688 "zhy_symbol_map"
      {"daak2", 680},
      {""}, {""}, {""},
#line 940 "zhy_symbol_map"
      {"dok2", 932},
      {""}, {""}, {""}, {""},
#line 869 "zhy_symbol_map"
      {"din1", 861},
#line 691 "zhy_symbol_map"
      {"daak5", 683},
      {""}, {""}, {""},
#line 757 "zhy_symbol_map"
      {"dan1", 749},
      {""}, {""}, {""}, {""},
#line 861 "zhy_symbol_map"
      {"dik7", 853},
#line 687 "zhy_symbol_map"
      {"daak1", 679},
      {""}, {""}, {""},
#line 749 "zhy_symbol_map"
      {"dak7", 741},
      {""}, {""}, {""}, {""},
#line 939 "zhy_symbol_map"
      {"dok1", 931},
#line 882 "zhy_symbol_map"
      {"ding7", 874},
      {""}, {""}, {""}, {""},
#line 770 "zhy_symbol_map"
      {"dang7", 762},
      {""}, {""}, {""}, {""},
#line 881 "zhy_symbol_map"
      {"ding6", 873},
      {""}, {""}, {""}, {""},
#line 769 "zhy_symbol_map"
      {"dang6", 761},
      {""}, {""}, {""}, {""},
#line 878 "zhy_symbol_map"
      {"ding3", 870},
      {""}, {""}, {""}, {""},
#line 766 "zhy_symbol_map"
      {"dang3", 758},
      {""}, {""}, {""}, {""},
#line 877 "zhy_symbol_map"
      {"ding2", 869},
      {""}, {""}, {""},
#line 4682 "zhy_symbol_map"
      {"wui6", 4674},
#line 765 "zhy_symbol_map"
      {"dang2", 757},
#line 4679 "zhy_symbol_map"
      {"wui3", 4671},
      {""}, {""}, {""},
#line 880 "zhy_symbol_map"
      {"ding5", 872},
      {""}, {""}, {""}, {""},
#line 768 "zhy_symbol_map"
      {"dang5", 760},
#line 4678 "zhy_symbol_map"
      {"wui2", 4670},
      {""}, {""}, {""},
#line 876 "zhy_symbol_map"
      {"ding1", 868},
      {""}, {""}, {""},
#line 679 "zhy_symbol_map"
      {"daa7", 671},
#line 764 "zhy_symbol_map"
      {"dang1", 756},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4677 "zhy_symbol_map"
      {"wui1", 4669},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4214 "zhy_symbol_map"
      {"sou7", 4206},
      {""}, {""}, {""}, {""}, {""},
#line 595 "zhy_symbol_map"
      {"coek7", 587},
      {""},
#line 3100 "zhy_symbol_map"
      {"miu6", 3092},
      {""},
#line 3097 "zhy_symbol_map"
      {"miu3", 3089},
      {""}, {""},
#line 3037 "zhy_symbol_map"
      {"mau6", 3029},
      {""},
#line 3034 "zhy_symbol_map"
      {"mau3", 3026},
#line 594 "zhy_symbol_map"
      {"coek6", 586},
      {""}, {""}, {""},
#line 3096 "zhy_symbol_map"
      {"miu2", 3088},
      {""}, {""}, {""}, {""},
#line 3033 "zhy_symbol_map"
      {"mau2", 3025},
#line 591 "zhy_symbol_map"
      {"coek3", 583},
      {""},
#line 875 "zhy_symbol_map"
      {"din7", 867},
      {""}, {""}, {""}, {""},
#line 763 "zhy_symbol_map"
      {"dan7", 755},
      {""}, {""},
#line 590 "zhy_symbol_map"
      {"coek2", 582},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3095 "zhy_symbol_map"
      {"miu1", 3087},
#line 593 "zhy_symbol_map"
      {"coek5", 585},
      {""},
#line 945 "zhy_symbol_map"
      {"dok7", 937},
      {""},
#line 3032 "zhy_symbol_map"
      {"mau1", 3024},
#line 392 "zhy_symbol_map"
      {"caan7", 384},
      {""},
#line 4870 "zhy_symbol_map"
      {"zik5", 4862},
      {""}, {""},
#line 589 "zhy_symbol_map"
      {"coek1", 581},
      {""},
#line 4772 "zhy_symbol_map"
      {"zak5", 4764},
#line 952 "zhy_symbol_map"
      {"dong7", 944},
      {""},
#line 391 "zhy_symbol_map"
      {"caan6", 383},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1497 "zhy_symbol_map"
      {"guk6", 1489},
#line 951 "zhy_symbol_map"
      {"dong6", 943},
#line 1494 "zhy_symbol_map"
      {"guk3", 1486},
#line 388 "zhy_symbol_map"
      {"caan3", 380},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 948 "zhy_symbol_map"
      {"dong3", 940},
#line 1493 "zhy_symbol_map"
      {"guk2", 1485},
#line 387 "zhy_symbol_map"
      {"caan2", 379},
      {""}, {""}, {""}, {""},
#line 2646 "zhy_symbol_map"
      {"laap7", 2638},
      {""}, {""},
#line 947 "zhy_symbol_map"
      {"dong2", 939},
      {""},
#line 390 "zhy_symbol_map"
      {"caan5", 382},
      {""}, {""}, {""}, {""},
#line 2645 "zhy_symbol_map"
      {"laap6", 2637},
      {""}, {""},
#line 950 "zhy_symbol_map"
      {"dong5", 942},
      {""},
#line 386 "zhy_symbol_map"
      {"caan1", 378},
      {""}, {""}, {""},
#line 1492 "zhy_symbol_map"
      {"guk1", 1484},
#line 2642 "zhy_symbol_map"
      {"laap3", 2634},
      {""}, {""},
#line 946 "zhy_symbol_map"
      {"dong1", 938},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2641 "zhy_symbol_map"
      {"laap2", 2633},
      {""},
#line 4702 "zhy_symbol_map"
      {"zaa5", 4694},
      {""},
#line 4683 "zhy_symbol_map"
      {"wui7", 4675},
      {""}, {""}, {""}, {""}, {""},
#line 2644 "zhy_symbol_map"
      {"laap5", 2636},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2640 "zhy_symbol_map"
      {"laap1", 2632},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3135 "zhy_symbol_map"
      {"mou6", 3127},
      {""},
#line 3132 "zhy_symbol_map"
      {"mou3", 3124},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3131 "zhy_symbol_map"
      {"mou2", 3123},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4884 "zhy_symbol_map"
      {"zin5", 4876},
      {""},
#line 3101 "zhy_symbol_map"
      {"miu7", 3093},
      {""}, {""},
#line 4786 "zhy_symbol_map"
      {"zan5", 4778},
      {""},
#line 3038 "zhy_symbol_map"
      {"mau7", 3030},
      {""}, {""}, {""}, {""},
#line 3130 "zhy_symbol_map"
      {"mou1", 3122},
#line 371 "zhy_symbol_map"
      {"caai7", 363},
      {""},
#line 1504 "zhy_symbol_map"
      {"gun6", 1496},
      {""},
#line 1501 "zhy_symbol_map"
      {"gun3", 1493},
      {""}, {""},
#line 4961 "zhy_symbol_map"
      {"zok5", 4953},
      {""}, {""},
#line 370 "zhy_symbol_map"
      {"caai6", 362},
      {""}, {""}, {""},
#line 1500 "zhy_symbol_map"
      {"gun2", 1492},
      {""}, {""}, {""}, {""}, {""},
#line 367 "zhy_symbol_map"
      {"caai3", 359},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 366 "zhy_symbol_map"
      {"caai2", 358},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1499 "zhy_symbol_map"
      {"gun1", 1491},
#line 369 "zhy_symbol_map"
      {"caai5", 361},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1498 "zhy_symbol_map"
      {"guk7", 1490},
#line 365 "zhy_symbol_map"
      {"caai1", 357},
      {""},
#line 2476 "zhy_symbol_map"
      {"kuk5", 2468},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1512 "zhy_symbol_map"
      {"gung7", 1504},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1511 "zhy_symbol_map"
      {"gung6", 1503},
      {""}, {""}, {""}, {""},
#line 916 "zhy_symbol_map"
      {"doe6", 908},
      {""},
#line 913 "zhy_symbol_map"
      {"doe3", 905},
      {""}, {""},
#line 1508 "zhy_symbol_map"
      {"gung3", 1500},
      {""}, {""}, {""}, {""}, {""},
#line 2863 "zhy_symbol_map"
      {"loeng7", 2855},
#line 912 "zhy_symbol_map"
      {"doe2", 904},
      {""}, {""},
#line 1507 "zhy_symbol_map"
      {"gung2", 1499},
#line 2862 "zhy_symbol_map"
      {"loeng6", 2854},
      {""}, {""}, {""}, {""},
#line 2859 "zhy_symbol_map"
      {"loeng3", 2851},
      {""}, {""}, {""},
#line 1510 "zhy_symbol_map"
      {"gung5", 1502},
#line 2858 "zhy_symbol_map"
      {"loeng2", 2850},
      {""}, {""}, {""}, {""},
#line 2861 "zhy_symbol_map"
      {"loeng5", 2853},
      {""}, {""}, {""},
#line 1506 "zhy_symbol_map"
      {"gung1", 1498},
#line 2857 "zhy_symbol_map"
      {"loeng1", 2849},
#line 911 "zhy_symbol_map"
      {"doe1", 903},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 924 "zhy_symbol_map"
      {"doek7", 916},
#line 3136 "zhy_symbol_map"
      {"mou7", 3128},
      {""}, {""},
#line 2665 "zhy_symbol_map"
      {"lai5", 2657},
      {""}, {""}, {""}, {""}, {""},
#line 923 "zhy_symbol_map"
      {"doek6", 915},
      {""}, {""}, {""},
#line 4605 "zhy_symbol_map"
      {"wau6", 4597},
      {""},
#line 4602 "zhy_symbol_map"
      {"wau3", 4594},
      {""}, {""}, {""},
#line 920 "zhy_symbol_map"
      {"doek3", 912},
      {""}, {""}, {""}, {""}, {""},
#line 4601 "zhy_symbol_map"
      {"wau2", 4593},
      {""}, {""}, {""},
#line 919 "zhy_symbol_map"
      {"doek2", 911},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 922 "zhy_symbol_map"
      {"doek5", 914},
#line 1505 "zhy_symbol_map"
      {"gun7", 1497},
      {""},
#line 4671 "zhy_symbol_map"
      {"wu2", 4663},
      {""},
#line 707 "zhy_symbol_map"
      {"daan7", 699},
      {""}, {""}, {""}, {""},
#line 918 "zhy_symbol_map"
      {"doek1", 910},
#line 4600 "zhy_symbol_map"
      {"wau1", 4592},
      {""}, {""}, {""},
#line 706 "zhy_symbol_map"
      {"daan6", 698},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 703 "zhy_symbol_map"
      {"daan3", 695},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 702 "zhy_symbol_map"
      {"daan2", 694},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 705 "zhy_symbol_map"
      {"daan5", 697},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 701 "zhy_symbol_map"
      {"daan1", 693},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 917 "zhy_symbol_map"
      {"doe7", 909},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 426 "zhy_symbol_map"
      {"cai6", 418},
      {""},
#line 423 "zhy_symbol_map"
      {"cai3", 415},
      {""}, {""},
#line 2875 "zhy_symbol_map"
      {"loi5", 2867},
#line 4676 "zhy_symbol_map"
      {"wu7", 4668},
      {""}, {""}, {""}, {""}, {""},
#line 422 "zhy_symbol_map"
      {"cai2", 414},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 421 "zhy_symbol_map"
      {"cai1", 413},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 686 "zhy_symbol_map"
      {"daai7", 678},
#line 4606 "zhy_symbol_map"
      {"wau7", 4598},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 685 "zhy_symbol_map"
      {"daai6", 677},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 682 "zhy_symbol_map"
      {"daai3", 674},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 681 "zhy_symbol_map"
      {"daai2", 673},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 684 "zhy_symbol_map"
      {"daai5", 676},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 680 "zhy_symbol_map"
      {"daai1", 672},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4219 "zhy_symbol_map"
      {"suk5", 4211},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4739 "zhy_symbol_map"
      {"zaang7", 4731},
      {""}, {""}, {""}, {""},
#line 4738 "zhy_symbol_map"
      {"zaang6", 4730},
#line 4926 "zhy_symbol_map"
      {"zoe5", 4918},
      {""}, {""}, {""},
#line 4735 "zhy_symbol_map"
      {"zaang3", 4727},
      {""}, {""}, {""}, {""},
#line 4734 "zhy_symbol_map"
      {"zaang2", 4726},
#line 608 "zhy_symbol_map"
      {"coi6", 600},
      {""},
#line 605 "zhy_symbol_map"
      {"coi3", 597},
      {""},
#line 4737 "zhy_symbol_map"
      {"zaang5", 4729},
      {""}, {""}, {""}, {""},
#line 4733 "zhy_symbol_map"
      {"zaang1", 4725},
      {""}, {""},
#line 604 "zhy_symbol_map"
      {"coi2", 596},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4670 "zhy_symbol_map"
      {"wu1", 4662},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 427 "zhy_symbol_map"
      {"cai7", 419},
      {""}, {""}, {""},
#line 4675 "zhy_symbol_map"
      {"wu6", 4667},
#line 603 "zhy_symbol_map"
      {"coi1", 595},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3576 "zhy_symbol_map"
      {"niu6", 3568},
      {""},
#line 3573 "zhy_symbol_map"
      {"niu3", 3565},
#line 588 "zhy_symbol_map"
      {"coei7", 580},
      {""},
#line 3282 "zhy_symbol_map"
      {"nau6", 3274},
      {""},
#line 3279 "zhy_symbol_map"
      {"nau3", 3271},
      {""}, {""}, {""}, {""},
#line 3572 "zhy_symbol_map"
      {"niu2", 3564},
#line 587 "zhy_symbol_map"
      {"coei6", 579},
      {""}, {""}, {""},
#line 3278 "zhy_symbol_map"
      {"nau2", 3270},
      {""}, {""}, {""}, {""}, {""},
#line 584 "zhy_symbol_map"
      {"coei3", 576},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 583 "zhy_symbol_map"
      {"coei2", 575},
      {""}, {""}, {""},
#line 3571 "zhy_symbol_map"
      {"niu1", 3563},
      {""}, {""}, {""}, {""},
#line 3277 "zhy_symbol_map"
      {"nau1", 3269},
#line 586 "zhy_symbol_map"
      {"coei5", 578},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 582 "zhy_symbol_map"
      {"coei1", 574},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 609 "zhy_symbol_map"
      {"coi7", 601},
      {""}, {""}, {""}, {""}, {""},
#line 741 "zhy_symbol_map"
      {"dai6", 733},
      {""},
#line 738 "zhy_symbol_map"
      {"dai3", 730},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 737 "zhy_symbol_map"
      {"dai2", 729},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3625 "zhy_symbol_map"
      {"nou6", 3617},
      {""},
#line 3622 "zhy_symbol_map"
      {"nou3", 3614},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3621 "zhy_symbol_map"
      {"nou2", 3613},
      {""}, {""},
#line 736 "zhy_symbol_map"
      {"dai1", 728},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3577 "zhy_symbol_map"
      {"niu7", 3569},
      {""}, {""}, {""}, {""},
#line 3283 "zhy_symbol_map"
      {"nau7", 3275},
      {""}, {""}, {""}, {""},
#line 3620 "zhy_symbol_map"
      {"nou1", 3612},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4746 "zhy_symbol_map"
      {"zaap7", 4738},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4745 "zhy_symbol_map"
      {"zaap6", 4737},
#line 520 "zhy_symbol_map"
      {"ci2", 512},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4742 "zhy_symbol_map"
      {"zaap3", 4734},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4741 "zhy_symbol_map"
      {"zaap2", 4733},
      {""},
#line 3148 "zhy_symbol_map"
      {"muk5", 3140},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4744 "zhy_symbol_map"
      {"zaap5", 4736},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4740 "zhy_symbol_map"
      {"zaap1", 4732},
      {""}, {""}, {""}, {""},
#line 937 "zhy_symbol_map"
      {"doi6", 929},
      {""},
#line 934 "zhy_symbol_map"
      {"doi3", 926},
      {""}, {""}, {""}, {""},
#line 4486 "zhy_symbol_map"
      {"tuk6", 4478},
      {""},
#line 4483 "zhy_symbol_map"
      {"tuk3", 4475},
      {""}, {""},
#line 933 "zhy_symbol_map"
      {"doi2", 925},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4482 "zhy_symbol_map"
      {"tuk2", 4474},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 742 "zhy_symbol_map"
      {"dai7", 734},
      {""}, {""}, {""}, {""},
#line 932 "zhy_symbol_map"
      {"doi1", 924},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4481 "zhy_symbol_map"
      {"tuk1", 4473},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 525 "zhy_symbol_map"
      {"ci7", 517},
#line 3626 "zhy_symbol_map"
      {"nou7", 3618},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2400 "zhy_symbol_map"
      {"kit6", 2392},
      {""},
#line 2397 "zhy_symbol_map"
      {"kit3", 2389},
      {""}, {""},
#line 2309 "zhy_symbol_map"
      {"kat6", 2301},
      {""},
#line 2306 "zhy_symbol_map"
      {"kat3", 2298},
      {""}, {""}, {""}, {""},
#line 2396 "zhy_symbol_map"
      {"kit2", 2388},
      {""}, {""}, {""}, {""},
#line 2305 "zhy_symbol_map"
      {"kat2", 2297},
      {""}, {""}, {""}, {""}, {""},
#line 576 "zhy_symbol_map"
      {"co2", 568},
#line 3155 "zhy_symbol_map"
      {"mun5", 3147},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2395 "zhy_symbol_map"
      {"kit1", 2387},
      {""}, {""}, {""}, {""},
#line 2304 "zhy_symbol_map"
      {"kat1", 2296},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4949 "zhy_symbol_map"
      {"zoeng7", 4941},
      {""}, {""}, {""}, {""},
#line 4948 "zhy_symbol_map"
      {"zoeng6", 4940},
      {""}, {""}, {""}, {""},
#line 4945 "zhy_symbol_map"
      {"zoeng3", 4937},
      {""}, {""}, {""}, {""},
#line 4944 "zhy_symbol_map"
      {"zoeng2", 4936},
      {""}, {""}, {""}, {""},
#line 4947 "zhy_symbol_map"
      {"zoeng5", 4939},
      {""}, {""}, {""}, {""},
#line 4943 "zhy_symbol_map"
      {"zoeng1", 4935},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 938 "zhy_symbol_map"
      {"doi7", 930},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4487 "zhy_symbol_map"
      {"tuk7", 4479},
      {""}, {""},
#line 4765 "zhy_symbol_map"
      {"zai5", 4757},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4494 "zhy_symbol_map"
      {"tung7", 4486},
      {""},
#line 1490 "zhy_symbol_map"
      {"gui6", 1482},
      {""},
#line 1487 "zhy_symbol_map"
      {"gui3", 1479},
      {""}, {""}, {""},
#line 581 "zhy_symbol_map"
      {"co7", 573},
      {""},
#line 4493 "zhy_symbol_map"
      {"tung6", 4485},
      {""}, {""}, {""},
#line 1486 "zhy_symbol_map"
      {"gui2", 1478},
      {""},
#line 519 "zhy_symbol_map"
      {"ci1", 511},
      {""}, {""}, {""},
#line 4490 "zhy_symbol_map"
      {"tung3", 4482},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4489 "zhy_symbol_map"
      {"tung2", 4481},
      {""}, {""},
#line 524 "zhy_symbol_map"
      {"ci6", 516},
      {""}, {""}, {""}, {""}, {""},
#line 1485 "zhy_symbol_map"
      {"gui1", 1477},
#line 4492 "zhy_symbol_map"
      {"tung5", 4484},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4488 "zhy_symbol_map"
      {"tung1", 4480},
      {""},
#line 3651 "zhy_symbol_map"
      {"o4", 3643},
      {""}, {""},
#line 2401 "zhy_symbol_map"
      {"kit7", 2393},
      {""}, {""}, {""}, {""},
#line 2310 "zhy_symbol_map"
      {"kat7", 2302},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 849 "zhy_symbol_map"
      {"di2", 841},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 994 "zhy_symbol_map"
      {"e7", 986},
      {""}, {""}, {""}, {""},
#line 993 "zhy_symbol_map"
      {"e6", 985},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4954 "zhy_symbol_map"
      {"zoi5", 4946},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 575 "zhy_symbol_map"
      {"co1", 567},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1491 "zhy_symbol_map"
      {"gui7", 1483},
      {""}, {""},
#line 2469 "zhy_symbol_map"
      {"kui5", 2461},
#line 580 "zhy_symbol_map"
      {"co6", 572},
      {""}, {""},
#line 854 "zhy_symbol_map"
      {"di7", 846},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3650 "zhy_symbol_map"
      {"o3", 3642},
#line 4143 "zhy_symbol_map"
      {"sit6", 4135},
      {""},
#line 4140 "zhy_symbol_map"
      {"sit3", 4132},
      {""}, {""},
#line 4038 "zhy_symbol_map"
      {"sat6", 4030},
      {""},
#line 4035 "zhy_symbol_map"
      {"sat3", 4027},
      {""}, {""},
#line 3666 "zhy_symbol_map"
      {"oi5", 3658},
      {""},
#line 4139 "zhy_symbol_map"
      {"sit2", 4131},
      {""}, {""},
#line 2357 "zhy_symbol_map"
      {"ki5", 2349},
#line 905 "zhy_symbol_map"
      {"do2", 897},
#line 4034 "zhy_symbol_map"
      {"sat2", 4026},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4688 "zhy_symbol_map"
      {"wun5", 4680},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4138 "zhy_symbol_map"
      {"sit1", 4130},
      {""}, {""}, {""}, {""},
#line 4033 "zhy_symbol_map"
      {"sat1", 4025},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 910 "zhy_symbol_map"
      {"do7", 902},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 848 "zhy_symbol_map"
      {"di1", 840},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 853 "zhy_symbol_map"
      {"di6", 845},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2413 "zhy_symbol_map"
      {"ko5", 2405},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4144 "zhy_symbol_map"
      {"sit7", 4136},
      {""},
#line 1399 "zhy_symbol_map"
      {"giu6", 1391},
      {""},
#line 1396 "zhy_symbol_map"
      {"giu3", 1388},
#line 4039 "zhy_symbol_map"
      {"sat7", 4031},
      {""},
#line 1308 "zhy_symbol_map"
      {"gau6", 1300},
      {""},
#line 1305 "zhy_symbol_map"
      {"gau3", 1297},
      {""}, {""}, {""}, {""},
#line 1395 "zhy_symbol_map"
      {"giu2", 1387},
      {""}, {""}, {""}, {""},
#line 1304 "zhy_symbol_map"
      {"gau2", 1296},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1479 "zhy_symbol_map"
      {"gu2", 1471},
      {""}, {""},
#line 1394 "zhy_symbol_map"
      {"giu1", 1386},
      {""}, {""}, {""}, {""},
#line 1303 "zhy_symbol_map"
      {"gau1", 1295},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3631 "zhy_symbol_map"
      {"nuk5", 3623},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 904 "zhy_symbol_map"
      {"do1", 896},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 909 "zhy_symbol_map"
      {"do6", 901},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1484 "zhy_symbol_map"
      {"gu7", 1476},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1476 "zhy_symbol_map"
      {"gou6", 1468},
      {""},
#line 1473 "zhy_symbol_map"
      {"gou3", 1465},
      {""}, {""}, {""},
#line 3093 "zhy_symbol_map"
      {"mit6", 3085},
      {""},
#line 3090 "zhy_symbol_map"
      {"mit3", 3082},
      {""}, {""},
#line 3030 "zhy_symbol_map"
      {"mat6", 3022},
#line 1472 "zhy_symbol_map"
      {"gou2", 1464},
#line 3027 "zhy_symbol_map"
      {"mat3", 3019},
      {""}, {""}, {""}, {""},
#line 3089 "zhy_symbol_map"
      {"mit2", 3081},
      {""}, {""},
#line 4100 "zhy_symbol_map"
      {"si5", 4092},
      {""},
#line 3026 "zhy_symbol_map"
      {"mat2", 3018},
      {""}, {""}, {""},
#line 1400 "zhy_symbol_map"
      {"giu7", 1392},
      {""}, {""},
#line 2406 "zhy_symbol_map"
      {"kiu5", 2398},
      {""},
#line 1309 "zhy_symbol_map"
      {"gau7", 1301},
      {""}, {""},
#line 2315 "zhy_symbol_map"
      {"kau5", 2307},
      {""},
#line 1471 "zhy_symbol_map"
      {"gou1", 1463},
      {""}, {""}, {""}, {""}, {""},
#line 3088 "zhy_symbol_map"
      {"mit1", 3080},
      {""}, {""}, {""}, {""},
#line 3025 "zhy_symbol_map"
      {"mat1", 3017},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2904 "zhy_symbol_map"
      {"luk6", 2896},
      {""},
#line 2901 "zhy_symbol_map"
      {"luk3", 2893},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2900 "zhy_symbol_map"
      {"luk2", 2892},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 530 "zhy_symbol_map"
      {"cik5", 522},
      {""}, {""}, {""}, {""},
#line 432 "zhy_symbol_map"
      {"cak5", 424},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2899 "zhy_symbol_map"
      {"luk1", 2891},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3514 "zhy_symbol_map"
      {"nguk7", 3506},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3513 "zhy_symbol_map"
      {"nguk6", 3505},
      {""},
#line 69 "zhy_symbol_map"
      {"aau6", 61},
      {""},
#line 66 "zhy_symbol_map"
      {"aau3", 58},
      {""}, {""}, {""}, {""}, {""},
#line 3510 "zhy_symbol_map"
      {"nguk3", 3502},
      {""}, {""}, {""},
#line 65 "zhy_symbol_map"
      {"aau2", 57},
      {""},
#line 1478 "zhy_symbol_map"
      {"gu1", 1470},
      {""}, {""}, {""},
#line 3509 "zhy_symbol_map"
      {"nguk2", 3501},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 362 "zhy_symbol_map"
      {"caa5", 354},
      {""}, {""},
#line 3512 "zhy_symbol_map"
      {"nguk5", 3504},
#line 121 "zhy_symbol_map"
      {"au2", 113},
      {""},
#line 1483 "zhy_symbol_map"
      {"gu6", 1475},
      {""}, {""}, {""}, {""},
#line 4156 "zhy_symbol_map"
      {"so5", 4148},
#line 64 "zhy_symbol_map"
      {"aau1", 56},
#line 3508 "zhy_symbol_map"
      {"nguk1", 3500},
      {""}, {""}, {""},
#line 1477 "zhy_symbol_map"
      {"gou7", 1469},
      {""}, {""}, {""}, {""}, {""},
#line 3094 "zhy_symbol_map"
      {"mit7", 3086},
      {""},
#line 2937 "zhy_symbol_map"
      {"m4", 2929},
      {""}, {""},
#line 3031 "zhy_symbol_map"
      {"mat7", 3023},
      {""},
#line 2911 "zhy_symbol_map"
      {"lun6", 2903},
      {""},
#line 2908 "zhy_symbol_map"
      {"lun3", 2900},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2907 "zhy_symbol_map"
      {"lun2", 2899},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 544 "zhy_symbol_map"
      {"cin5", 536},
      {""}, {""}, {""}, {""},
#line 446 "zhy_symbol_map"
      {"can5", 438},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2906 "zhy_symbol_map"
      {"lun1", 2898},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 614 "zhy_symbol_map"
      {"cok5", 606},
      {""},
#line 2905 "zhy_symbol_map"
      {"luk7", 2897},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2919 "zhy_symbol_map"
      {"lung7", 2911},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2918 "zhy_symbol_map"
      {"lung6", 2910},
      {""}, {""},
#line 126 "zhy_symbol_map"
      {"au7", 118},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2915 "zhy_symbol_map"
      {"lung3", 2907},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2914 "zhy_symbol_map"
      {"lung2", 2906},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2917 "zhy_symbol_map"
      {"lung5", 2909},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2913 "zhy_symbol_map"
      {"lung1", 2905},
      {""},
#line 3141 "zhy_symbol_map"
      {"mui5", 3133},
      {""},
#line 70 "zhy_symbol_map"
      {"aau7", 62},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2936 "zhy_symbol_map"
      {"m3", 2928},
#line 4598 "zhy_symbol_map"
      {"wat6", 4590},
      {""},
#line 4595 "zhy_symbol_map"
      {"wat3", 4587},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3064 "zhy_symbol_map"
      {"mi5", 3056},
      {""},
#line 4594 "zhy_symbol_map"
      {"wat2", 4586},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4149 "zhy_symbol_map"
      {"siu5", 4141},
      {""},
#line 2912 "zhy_symbol_map"
      {"lun7", 2904},
      {""}, {""},
#line 4044 "zhy_symbol_map"
      {"sau5", 4036},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4593 "zhy_symbol_map"
      {"wat1", 4585},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 859 "zhy_symbol_map"
      {"dik5", 851},
      {""}, {""}, {""}, {""},
#line 747 "zhy_symbol_map"
      {"dak5", 739},
#line 120 "zhy_symbol_map"
      {"au1", 112},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 125 "zhy_symbol_map"
      {"au6", 117},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 677 "zhy_symbol_map"
      {"daa5", 669},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3106 "zhy_symbol_map"
      {"mo5", 3098},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4212 "zhy_symbol_map"
      {"sou5", 4204},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4599 "zhy_symbol_map"
      {"wat7", 4591},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 873 "zhy_symbol_map"
      {"din5", 865},
      {""}, {""}, {""}, {""},
#line 761 "zhy_symbol_map"
      {"dan5", 753},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 943 "zhy_symbol_map"
      {"dok5", 935},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 399 "zhy_symbol_map"
      {"caang7", 391},
      {""}, {""}, {""}, {""},
#line 398 "zhy_symbol_map"
      {"caang6", 390},
#line 4681 "zhy_symbol_map"
      {"wui5", 4673},
      {""}, {""}, {""},
#line 395 "zhy_symbol_map"
      {"caang3", 387},
      {""}, {""}, {""}, {""},
#line 394 "zhy_symbol_map"
      {"caang2", 386},
      {""}, {""}, {""}, {""},
#line 397 "zhy_symbol_map"
      {"caang5", 389},
      {""}, {""}, {""}, {""},
#line 393 "zhy_symbol_map"
      {"caang1", 385},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3569 "zhy_symbol_map"
      {"nit6", 3561},
      {""},
#line 3566 "zhy_symbol_map"
      {"nit3", 3558},
      {""}, {""},
#line 3275 "zhy_symbol_map"
      {"nat6", 3267},
      {""},
#line 3272 "zhy_symbol_map"
      {"nat3", 3264},
      {""}, {""}, {""}, {""},
#line 3565 "zhy_symbol_map"
      {"nit2", 3557},
      {""}, {""},
#line 4632 "zhy_symbol_map"
      {"wi5", 4624},
      {""},
#line 3271 "zhy_symbol_map"
      {"nat2", 3263},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3099 "zhy_symbol_map"
      {"miu5", 3091},
      {""}, {""}, {""}, {""},
#line 3036 "zhy_symbol_map"
      {"mau5", 3028},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3564 "zhy_symbol_map"
      {"nit1", 3556},
      {""}, {""}, {""}, {""},
#line 3270 "zhy_symbol_map"
      {"nat1", 3262},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4437 "zhy_symbol_map"
      {"tiu6", 4429},
      {""},
#line 4434 "zhy_symbol_map"
      {"tiu3", 4426},
      {""}, {""},
#line 4353 "zhy_symbol_map"
      {"tau6", 4345},
      {""},
#line 4350 "zhy_symbol_map"
      {"tau3", 4342},
      {""}, {""}, {""}, {""},
#line 4433 "zhy_symbol_map"
      {"tiu2", 4425},
      {""}, {""}, {""}, {""},
#line 4349 "zhy_symbol_map"
      {"tau2", 4341},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1496 "zhy_symbol_map"
      {"guk5", 1488},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4432 "zhy_symbol_map"
      {"tiu1", 4424},
      {""}, {""}, {""}, {""},
#line 4348 "zhy_symbol_map"
      {"tau1", 4340},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4653 "zhy_symbol_map"
      {"wo5", 4645},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3134 "zhy_symbol_map"
      {"mou5", 3126},
      {""}, {""},
#line 3570 "zhy_symbol_map"
      {"nit7", 3562},
      {""}, {""}, {""}, {""},
#line 3276 "zhy_symbol_map"
      {"nat7", 3268},
      {""},
#line 4990 "zhy_symbol_map"
      {"zuk6", 4982},
      {""},
#line 4987 "zhy_symbol_map"
      {"zuk3", 4979},
      {""}, {""}, {""}, {""},
#line 2337 "zhy_symbol_map"
      {"kek6", 2329},
      {""},
#line 2334 "zhy_symbol_map"
      {"kek3", 2326},
      {""}, {""},
#line 4986 "zhy_symbol_map"
      {"zuk2", 4978},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2333 "zhy_symbol_map"
      {"kek2", 2325},
#line 4479 "zhy_symbol_map"
      {"tou6", 4471},
      {""},
#line 4476 "zhy_symbol_map"
      {"tou3", 4468},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1503 "zhy_symbol_map"
      {"gun5", 1495},
      {""},
#line 4475 "zhy_symbol_map"
      {"tou2", 4467},
      {""}, {""}, {""}, {""},
#line 4985 "zhy_symbol_map"
      {"zuk1", 4977},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2332 "zhy_symbol_map"
      {"kek1", 2324},
      {""}, {""},
#line 4438 "zhy_symbol_map"
      {"tiu7", 4430},
      {""}, {""}, {""}, {""},
#line 4354 "zhy_symbol_map"
      {"tau7", 4346},
      {""}, {""}, {""}, {""},
#line 4474 "zhy_symbol_map"
      {"tou1", 4466},
#line 406 "zhy_symbol_map"
      {"caap7", 398},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 405 "zhy_symbol_map"
      {"caap6", 397},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 402 "zhy_symbol_map"
      {"caap3", 394},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 401 "zhy_symbol_map"
      {"caap2", 393},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 714 "zhy_symbol_map"
      {"daang7", 706},
#line 404 "zhy_symbol_map"
      {"caap5", 396},
      {""}, {""}, {""},
#line 713 "zhy_symbol_map"
      {"daang6", 705},
#line 915 "zhy_symbol_map"
      {"doe5", 907},
      {""}, {""}, {""},
#line 710 "zhy_symbol_map"
      {"daang3", 702},
#line 400 "zhy_symbol_map"
      {"caap1", 392},
      {""}, {""}, {""},
#line 709 "zhy_symbol_map"
      {"daang2", 701},
      {""}, {""}, {""}, {""},
#line 712 "zhy_symbol_map"
      {"daang5", 704},
      {""}, {""}, {""}, {""},
#line 708 "zhy_symbol_map"
      {"daang1", 700},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3966 "zhy_symbol_map"
      {"saang4", 3958},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3526 "zhy_symbol_map"
      {"ni5", 3518},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4991 "zhy_symbol_map"
      {"zuk7", 4983},
      {""}, {""},
#line 4604 "zhy_symbol_map"
      {"wau5", 4596},
      {""}, {""}, {""},
#line 2338 "zhy_symbol_map"
      {"kek7", 2330},
      {""}, {""}, {""},
#line 4998 "zhy_symbol_map"
      {"zung7", 4990},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4480 "zhy_symbol_map"
      {"tou7", 4472},
#line 4997 "zhy_symbol_map"
      {"zung6", 4989},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4994 "zhy_symbol_map"
      {"zung3", 4986},
      {""}, {""},
#line 996 "zhy_symbol_map"
      {"ei2", 988},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4993 "zhy_symbol_map"
      {"zung2", 4985},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4996 "zhy_symbol_map"
      {"zung5", 4988},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4992 "zhy_symbol_map"
      {"zung1", 4984},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 602 "zhy_symbol_map"
      {"coeng7", 594},
      {""}, {""}, {""}, {""},
#line 601 "zhy_symbol_map"
      {"coeng6", 593},
      {""}, {""}, {""}, {""},
#line 598 "zhy_symbol_map"
      {"coeng3", 590},
      {""}, {""}, {""}, {""},
#line 597 "zhy_symbol_map"
      {"coeng2", 589},
      {""}, {""}, {""}, {""},
#line 600 "zhy_symbol_map"
      {"coeng5", 592},
      {""}, {""}, {""}, {""},
#line 596 "zhy_symbol_map"
      {"coeng1", 588},
      {""}, {""}, {""}, {""},
#line 2433 "zhy_symbol_map"
      {"koeng4", 2425},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 425 "zhy_symbol_map"
      {"cai5", 417},
      {""}, {""}, {""}, {""}, {""},
#line 3582 "zhy_symbol_map"
      {"no5", 3574},
      {""},
#line 1001 "zhy_symbol_map"
      {"ei7", 993},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 992 "zhy_symbol_map"
      {"e5", 984},
      {""}, {""}, {""}, {""},
#line 4066 "zhy_symbol_map"
      {"sek6", 4058},
      {""},
#line 4063 "zhy_symbol_map"
      {"sek3", 4055},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4062 "zhy_symbol_map"
      {"sek2", 4054},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4061 "zhy_symbol_map"
      {"sek1", 4053},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 721 "zhy_symbol_map"
      {"daap7", 713},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 720 "zhy_symbol_map"
      {"daap6", 712},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 717 "zhy_symbol_map"
      {"daap3", 709},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 716 "zhy_symbol_map"
      {"daap2", 708},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 719 "zhy_symbol_map"
      {"daap5", 711},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 715 "zhy_symbol_map"
      {"daap1", 707},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 607 "zhy_symbol_map"
      {"coi5", 599},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 995 "zhy_symbol_map"
      {"ei1", 987},
      {""}, {""},
#line 2972 "zhy_symbol_map"
      {"maang4", 2964},
      {""}, {""}, {""}, {""},
#line 3677 "zhy_symbol_map"
      {"om2", 3669},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1000 "zhy_symbol_map"
      {"ei6", 992},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3575 "zhy_symbol_map"
      {"niu5", 3567},
      {""}, {""}, {""}, {""},
#line 3281 "zhy_symbol_map"
      {"nau5", 3273},
      {""}, {""}, {""},
#line 4067 "zhy_symbol_map"
      {"sek7", 4059},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4074 "zhy_symbol_map"
      {"seng7", 4066},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4073 "zhy_symbol_map"
      {"seng6", 4065},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4070 "zhy_symbol_map"
      {"seng3", 4062},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4069 "zhy_symbol_map"
      {"seng2", 4061},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4072 "zhy_symbol_map"
      {"seng5", 4064},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4068 "zhy_symbol_map"
      {"seng1", 4060},
#line 3682 "zhy_symbol_map"
      {"om7", 3674},
#line 931 "zhy_symbol_map"
      {"doeng7", 923},
      {""}, {""}, {""}, {""},
#line 930 "zhy_symbol_map"
      {"doeng6", 922},
      {""}, {""}, {""}, {""},
#line 927 "zhy_symbol_map"
      {"doeng3", 919},
      {""}, {""}, {""}, {""},
#line 926 "zhy_symbol_map"
      {"doeng2", 918},
      {""}, {""}, {""}, {""},
#line 929 "zhy_symbol_map"
      {"doeng5", 921},
      {""}, {""}, {""}, {""},
#line 925 "zhy_symbol_map"
      {"doeng1", 917},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4176 "zhy_symbol_map"
      {"soeng4", 4168},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 740 "zhy_symbol_map"
      {"dai5", 732},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3624 "zhy_symbol_map"
      {"nou5", 3616},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2827 "zhy_symbol_map"
      {"liu6", 2819},
      {""},
#line 2824 "zhy_symbol_map"
      {"liu3", 2816},
      {""}, {""},
#line 2715 "zhy_symbol_map"
      {"lau6", 2707},
#line 1392 "zhy_symbol_map"
      {"git6", 1384},
#line 2712 "zhy_symbol_map"
      {"lau3", 2704},
#line 1389 "zhy_symbol_map"
      {"git3", 1381},
      {""}, {""},
#line 1301 "zhy_symbol_map"
      {"gat6", 1293},
#line 2823 "zhy_symbol_map"
      {"liu2", 2815},
#line 1298 "zhy_symbol_map"
      {"gat3", 1290},
      {""}, {""}, {""},
#line 2711 "zhy_symbol_map"
      {"lau2", 2703},
#line 1388 "zhy_symbol_map"
      {"git2", 1380},
      {""}, {""}, {""}, {""},
#line 1297 "zhy_symbol_map"
      {"gat2", 1289},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2822 "zhy_symbol_map"
      {"liu1", 2814},
      {""}, {""}, {""}, {""},
#line 2710 "zhy_symbol_map"
      {"lau1", 2702},
#line 1387 "zhy_symbol_map"
      {"git1", 1379},
      {""}, {""}, {""}, {""},
#line 1296 "zhy_symbol_map"
      {"gat1", 1288},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3676 "zhy_symbol_map"
      {"om1", 3668},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3681 "zhy_symbol_map"
      {"om6", 3673},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 936 "zhy_symbol_map"
      {"doi5", 928},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4485 "zhy_symbol_map"
      {"tuk5", 4477},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4554 "zhy_symbol_map"
      {"waang4", 4546},
      {""}, {""}, {""}, {""}, {""},
#line 2897 "zhy_symbol_map"
      {"lou6", 2889},
      {""},
#line 2894 "zhy_symbol_map"
      {"lou3", 2886},
      {""}, {""}, {""},
#line 1469 "zhy_symbol_map"
      {"got6", 1461},
      {""},
#line 1466 "zhy_symbol_map"
      {"got3", 1458},
      {""}, {""}, {""},
#line 2893 "zhy_symbol_map"
      {"lou2", 2885},
      {""}, {""}, {""}, {""}, {""},
#line 1465 "zhy_symbol_map"
      {"got2", 1457},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2828 "zhy_symbol_map"
      {"liu7", 2820},
      {""}, {""}, {""}, {""},
#line 2716 "zhy_symbol_map"
      {"lau7", 2708},
#line 1393 "zhy_symbol_map"
      {"git7", 1385},
      {""}, {""},
#line 2399 "zhy_symbol_map"
      {"kit5", 2391},
#line 2892 "zhy_symbol_map"
      {"lou1", 2884},
#line 1302 "zhy_symbol_map"
      {"gat7", 1294},
      {""}, {""},
#line 2308 "zhy_symbol_map"
      {"kat5", 2300},
      {""},
#line 1464 "zhy_symbol_map"
      {"got1", 1456},
      {""},
#line 3059 "zhy_symbol_map"
      {"meng7", 3051},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3058 "zhy_symbol_map"
      {"meng6", 3050},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3055 "zhy_symbol_map"
      {"meng3", 3047},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3054 "zhy_symbol_map"
      {"meng2", 3046},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3057 "zhy_symbol_map"
      {"meng5", 3049},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3053 "zhy_symbol_map"
      {"meng1", 3045},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 62 "zhy_symbol_map"
      {"aat6", 54},
      {""},
#line 59 "zhy_symbol_map"
      {"aat3", 51},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 58 "zhy_symbol_map"
      {"aat2", 50},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1489 "zhy_symbol_map"
      {"gui5", 1481},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2898 "zhy_symbol_map"
      {"lou7", 2890},
#line 57 "zhy_symbol_map"
      {"aat1", 49},
      {""}, {""}, {""}, {""},
#line 1470 "zhy_symbol_map"
      {"got7", 1462},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1349 "zhy_symbol_map"
      {"gi5", 1341},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4983 "zhy_symbol_map"
      {"zui6", 4975},
      {""},
#line 4980 "zhy_symbol_map"
      {"zui3", 4972},
      {""}, {""}, {""}, {""},
#line 2330 "zhy_symbol_map"
      {"kei6", 2322},
      {""},
#line 2327 "zhy_symbol_map"
      {"kei3", 2319},
      {""}, {""},
#line 4979 "zhy_symbol_map"
      {"zui2", 4971},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2326 "zhy_symbol_map"
      {"kei2", 2318},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4978 "zhy_symbol_map"
      {"zui1", 4970},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2325 "zhy_symbol_map"
      {"kei1", 2317},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 63 "zhy_symbol_map"
      {"aat7", 55},
      {""}, {""}, {""},
#line 4619 "zhy_symbol_map"
      {"wen6", 4611},
      {""},
#line 4616 "zhy_symbol_map"
      {"wen3", 4608},
      {""}, {""}, {""}, {""},
#line 3203 "zhy_symbol_map"
      {"naang4", 3195},
      {""}, {""}, {""}, {""},
#line 4615 "zhy_symbol_map"
      {"wen2", 4607},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1405 "zhy_symbol_map"
      {"go5", 1397},
      {""}, {""},
#line 4614 "zhy_symbol_map"
      {"wen1", 4606},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4142 "zhy_symbol_map"
      {"sit5", 4134},
      {""}, {""}, {""}, {""},
#line 4037 "zhy_symbol_map"
      {"sat5", 4029},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3323 "zhy_symbol_map"
      {"ng5", 3315},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3332 "zhy_symbol_map"
      {"ngaa7", 3324},
      {""},
#line 4984 "zhy_symbol_map"
      {"zui7", 4976},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2331 "zhy_symbol_map"
      {"kei7", 2323},
#line 3331 "zhy_symbol_map"
      {"ngaa6", 3323},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3328 "zhy_symbol_map"
      {"ngaa3", 3320},
#line 3339 "zhy_symbol_map"
      {"ngaai7", 3331},
      {""}, {""}, {""}, {""},
#line 3338 "zhy_symbol_map"
      {"ngaai6", 3330},
      {""}, {""}, {""},
#line 3327 "zhy_symbol_map"
      {"ngaa2", 3319},
#line 3335 "zhy_symbol_map"
      {"ngaai3", 3327},
      {""}, {""}, {""}, {""},
#line 3334 "zhy_symbol_map"
      {"ngaai2", 3326},
      {""}, {""}, {""},
#line 3330 "zhy_symbol_map"
      {"ngaa5", 3322},
#line 3337 "zhy_symbol_map"
      {"ngaai5", 3329},
      {""}, {""}, {""}, {""},
#line 3333 "zhy_symbol_map"
      {"ngaai1", 3325},
      {""}, {""}, {""},
#line 3326 "zhy_symbol_map"
      {"ngaa1", 3318},
#line 75 "zhy_symbol_map"
      {"ai5", 67},
      {""}, {""}, {""}, {""},
#line 12 "zhy_symbol_map"
      {"aa5", 4},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4620 "zhy_symbol_map"
      {"wen7", 4612},
      {""},
#line 3346 "zhy_symbol_map"
      {"ngaak7", 3338},
      {""}, {""}, {""}, {""},
#line 3345 "zhy_symbol_map"
      {"ngaak6", 3337},
      {""}, {""}, {""}, {""},
#line 3342 "zhy_symbol_map"
      {"ngaak3", 3334},
      {""}, {""}, {""}, {""},
#line 3341 "zhy_symbol_map"
      {"ngaak2", 3333},
      {""}, {""}, {""}, {""},
#line 3344 "zhy_symbol_map"
      {"ngaak5", 3336},
      {""}, {""}, {""}, {""},
#line 3340 "zhy_symbol_map"
      {"ngaak1", 3332},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 117 "zhy_symbol_map"
      {"at5", 109},
      {""}, {""}, {""},
#line 1398 "zhy_symbol_map"
      {"giu5", 1390},
#line 110 "zhy_symbol_map"
      {"ap5", 102},
      {""}, {""}, {""},
#line 1307 "zhy_symbol_map"
      {"gau5", 1299},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3507 "zhy_symbol_map"
      {"ngou7", 3499},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3471 "zhy_symbol_map"
      {"ngo6", 3463},
      {""},
#line 3468 "zhy_symbol_map"
      {"ngo3", 3460},
#line 3506 "zhy_symbol_map"
      {"ngou6", 3498},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3467 "zhy_symbol_map"
      {"ngo2", 3459},
#line 3503 "zhy_symbol_map"
      {"ngou3", 3495},
      {""}, {""}, {""},
#line 4059 "zhy_symbol_map"
      {"sei6", 4051},
      {""},
#line 4056 "zhy_symbol_map"
      {"sei3", 4048},
      {""}, {""}, {""},
#line 3502 "zhy_symbol_map"
      {"ngou2", 3494},
      {""}, {""}, {""}, {""}, {""},
#line 4055 "zhy_symbol_map"
      {"sei2", 4047},
      {""}, {""}, {""},
#line 3505 "zhy_symbol_map"
      {"ngou5", 3497},
      {""}, {""}, {""},
#line 3466 "zhy_symbol_map"
      {"ngo1", 3458},
      {""}, {""}, {""}, {""}, {""},
#line 3501 "zhy_symbol_map"
      {"ngou1", 3493},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3486 "zhy_symbol_map"
      {"ngok7", 3478},
#line 4054 "zhy_symbol_map"
      {"sei1", 4046},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3485 "zhy_symbol_map"
      {"ngok6", 3477},
      {""},
#line 4913 "zhy_symbol_map"
      {"ziu6", 4905},
      {""},
#line 4910 "zhy_symbol_map"
      {"ziu3", 4902},
      {""}, {""},
#line 4815 "zhy_symbol_map"
      {"zau6", 4807},
      {""},
#line 4812 "zhy_symbol_map"
      {"zau3", 4804},
#line 3482 "zhy_symbol_map"
      {"ngok3", 3474},
      {""}, {""},
#line 3665 "zhy_symbol_map"
      {"oi4", 3657},
#line 4909 "zhy_symbol_map"
      {"ziu2", 4901},
      {""}, {""}, {""},
#line 2356 "zhy_symbol_map"
      {"ki4", 2348},
#line 4811 "zhy_symbol_map"
      {"zau2", 4803},
#line 3481 "zhy_symbol_map"
      {"ngok2", 3473},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3484 "zhy_symbol_map"
      {"ngok5", 3476},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3656 "zhy_symbol_map"
      {"oe2", 3648},
#line 4908 "zhy_symbol_map"
      {"ziu1", 4900},
#line 3480 "zhy_symbol_map"
      {"ngok1", 3472},
      {""}, {""},
#line 2319 "zhy_symbol_map"
      {"ke2", 2311},
#line 4810 "zhy_symbol_map"
      {"zau1", 4802},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1475 "zhy_symbol_map"
      {"gou5", 1467},
      {""}, {""}, {""}, {""}, {""},
#line 3092 "zhy_symbol_map"
      {"mit5", 3084},
      {""}, {""}, {""}, {""},
#line 3029 "zhy_symbol_map"
      {"mat5", 3021},
      {""}, {""}, {""},
#line 3304 "zhy_symbol_map"
      {"neng7", 3296},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3303 "zhy_symbol_map"
      {"neng6", 3295},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3300 "zhy_symbol_map"
      {"neng3", 3292},
#line 4430 "zhy_symbol_map"
      {"tit6", 4422},
      {""},
#line 4427 "zhy_symbol_map"
      {"tit3", 4419},
      {""}, {""},
#line 4346 "zhy_symbol_map"
      {"tat6", 4338},
      {""},
#line 4343 "zhy_symbol_map"
      {"tat3", 4335},
      {""},
#line 3299 "zhy_symbol_map"
      {"neng2", 3291},
      {""}, {""},
#line 4426 "zhy_symbol_map"
      {"tit2", 4418},
      {""}, {""}, {""},
#line 3472 "zhy_symbol_map"
      {"ngo7", 3464},
#line 4342 "zhy_symbol_map"
      {"tat2", 4334},
      {""},
#line 3302 "zhy_symbol_map"
      {"neng5", 3294},
      {""}, {""}, {""}, {""},
#line 2903 "zhy_symbol_map"
      {"luk5", 2895},
      {""}, {""}, {""}, {""},
#line 3298 "zhy_symbol_map"
      {"neng1", 3290},
      {""}, {""}, {""},
#line 4060 "zhy_symbol_map"
      {"sei7", 4052},
      {""}, {""}, {""},
#line 4425 "zhy_symbol_map"
      {"tit1", 4417},
      {""}, {""}, {""}, {""},
#line 4341 "zhy_symbol_map"
      {"tat1", 4333},
      {""}, {""}, {""}, {""},
#line 3661 "zhy_symbol_map"
      {"oe7", 3653},
      {""}, {""}, {""}, {""},
#line 2324 "zhy_symbol_map"
      {"ke7", 2316},
      {""},
#line 4976 "zhy_symbol_map"
      {"zou6", 4968},
      {""},
#line 4973 "zhy_symbol_map"
      {"zou3", 4965},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4972 "zhy_symbol_map"
      {"zou2", 4964},
      {""},
#line 3595 "zhy_symbol_map"
      {"noeng4", 3587},
      {""},
#line 2412 "zhy_symbol_map"
      {"ko4", 2404},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4914 "zhy_symbol_map"
      {"ziu7", 4906},
      {""}, {""}, {""}, {""},
#line 4816 "zhy_symbol_map"
      {"zau7", 4808},
      {""}, {""},
#line 68 "zhy_symbol_map"
      {"aau5", 60},
      {""},
#line 4971 "zhy_symbol_map"
      {"zou1", 4963},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1742 "zhy_symbol_map"
      {"hak6", 1734},
      {""},
#line 1739 "zhy_symbol_map"
      {"hak3", 1731},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1738 "zhy_symbol_map"
      {"hak2", 1730},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2910 "zhy_symbol_map"
      {"lun5", 2902},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1737 "zhy_symbol_map"
      {"hak1", 1729},
#line 636 "zhy_symbol_map"
      {"cuk6", 628},
      {""},
#line 633 "zhy_symbol_map"
      {"cuk3", 625},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 632 "zhy_symbol_map"
      {"cuk2", 624},
#line 4431 "zhy_symbol_map"
      {"tit7", 4423},
      {""},
#line 1729 "zhy_symbol_map"
      {"haau7", 1721},
      {""}, {""},
#line 4347 "zhy_symbol_map"
      {"tat7", 4339},
      {""},
#line 1694 "zhy_symbol_map"
      {"haam7", 1686},
      {""},
#line 1672 "zhy_symbol_map"
      {"haa6", 1664},
      {""},
#line 1669 "zhy_symbol_map"
      {"haa3", 1661},
#line 1728 "zhy_symbol_map"
      {"haau6", 1720},
      {""},
#line 3051 "zhy_symbol_map"
      {"mei6", 3043},
      {""},
#line 3048 "zhy_symbol_map"
      {"mei3", 3040},
#line 1693 "zhy_symbol_map"
      {"haam6", 1685},
      {""}, {""}, {""},
#line 1668 "zhy_symbol_map"
      {"haa2", 1660},
#line 1725 "zhy_symbol_map"
      {"haau3", 1717},
      {""},
#line 631 "zhy_symbol_map"
      {"cuk1", 623},
      {""},
#line 3047 "zhy_symbol_map"
      {"mei2", 3039},
#line 1690 "zhy_symbol_map"
      {"haam3", 1682},
#line 3655 "zhy_symbol_map"
      {"oe1", 3647},
      {""}, {""}, {""},
#line 1724 "zhy_symbol_map"
      {"haau2", 1716},
#line 2318 "zhy_symbol_map"
      {"ke1", 2310},
      {""}, {""}, {""},
#line 1689 "zhy_symbol_map"
      {"haam2", 1681},
      {""}, {""}, {""}, {""},
#line 1727 "zhy_symbol_map"
      {"haau5", 1719},
      {""}, {""},
#line 3660 "zhy_symbol_map"
      {"oe6", 3652},
#line 1667 "zhy_symbol_map"
      {"haa1", 1659},
#line 1692 "zhy_symbol_map"
      {"haam5", 1684},
      {""}, {""},
#line 2323 "zhy_symbol_map"
      {"ke6", 2315},
#line 3046 "zhy_symbol_map"
      {"mei1", 3038},
#line 1723 "zhy_symbol_map"
      {"haau1", 1715},
      {""},
#line 4977 "zhy_symbol_map"
      {"zou7", 4969},
      {""}, {""},
#line 1688 "zhy_symbol_map"
      {"haam1", 1680},
      {""}, {""}, {""}, {""},
#line 1687 "zhy_symbol_map"
      {"haak7", 1679},
      {""},
#line 1847 "zhy_symbol_map"
      {"hin6", 1839},
      {""},
#line 1844 "zhy_symbol_map"
      {"hin3", 1836},
      {""}, {""},
#line 1756 "zhy_symbol_map"
      {"han6", 1748},
      {""},
#line 1753 "zhy_symbol_map"
      {"han3", 1745},
#line 1686 "zhy_symbol_map"
      {"haak6", 1678},
      {""}, {""}, {""},
#line 1843 "zhy_symbol_map"
      {"hin2", 1835},
      {""},
#line 4099 "zhy_symbol_map"
      {"si4", 4091},
      {""}, {""},
#line 1752 "zhy_symbol_map"
      {"han2", 1744},
#line 1683 "zhy_symbol_map"
      {"haak3", 1675},
      {""},
#line 1924 "zhy_symbol_map"
      {"hok6", 1916},
      {""},
#line 1921 "zhy_symbol_map"
      {"hok3", 1913},
      {""}, {""}, {""}, {""}, {""},
#line 1682 "zhy_symbol_map"
      {"haak2", 1674},
      {""}, {""}, {""},
#line 1920 "zhy_symbol_map"
      {"hok2", 1912},
      {""}, {""}, {""}, {""},
#line 1842 "zhy_symbol_map"
      {"hin1", 1834},
#line 1685 "zhy_symbol_map"
      {"haak5", 1677},
#line 4048 "zhy_symbol_map"
      {"se2", 4040},
      {""}, {""},
#line 1751 "zhy_symbol_map"
      {"han1", 1743},
#line 643 "zhy_symbol_map"
      {"cun6", 635},
      {""},
#line 640 "zhy_symbol_map"
      {"cun3", 632},
      {""}, {""},
#line 1681 "zhy_symbol_map"
      {"haak1", 1673},
      {""}, {""}, {""},
#line 1743 "zhy_symbol_map"
      {"hak7", 1735},
      {""}, {""},
#line 639 "zhy_symbol_map"
      {"cun2", 631},
      {""},
#line 1919 "zhy_symbol_map"
      {"hok1", 1911},
#line 1855 "zhy_symbol_map"
      {"hing7", 1847},
#line 4597 "zhy_symbol_map"
      {"wat5", 4589},
      {""}, {""}, {""},
#line 1764 "zhy_symbol_map"
      {"hang7", 1756},
      {""}, {""}, {""}, {""},
#line 1854 "zhy_symbol_map"
      {"hing6", 1846},
      {""}, {""}, {""}, {""},
#line 1763 "zhy_symbol_map"
      {"hang6", 1755},
      {""}, {""}, {""}, {""},
#line 1851 "zhy_symbol_map"
      {"hing3", 1843},
      {""},
#line 638 "zhy_symbol_map"
      {"cun1", 630},
      {""}, {""},
#line 1760 "zhy_symbol_map"
      {"hang3", 1752},
      {""}, {""}, {""}, {""},
#line 1850 "zhy_symbol_map"
      {"hing2", 1842},
      {""},
#line 637 "zhy_symbol_map"
      {"cuk7", 629},
      {""}, {""},
#line 1759 "zhy_symbol_map"
      {"hang2", 1751},
      {""}, {""}, {""}, {""},
#line 1853 "zhy_symbol_map"
      {"hing5", 1845},
#line 4387 "zhy_symbol_map"
      {"ti5", 4379},
      {""},
#line 651 "zhy_symbol_map"
      {"cung7", 643},
      {""},
#line 1762 "zhy_symbol_map"
      {"hang5", 1754},
      {""}, {""}, {""}, {""},
#line 1849 "zhy_symbol_map"
      {"hing1", 1841},
      {""}, {""},
#line 650 "zhy_symbol_map"
      {"cung6", 642},
#line 1673 "zhy_symbol_map"
      {"haa7", 1665},
#line 1758 "zhy_symbol_map"
      {"hang1", 1750},
      {""}, {""}, {""},
#line 3052 "zhy_symbol_map"
      {"mei7", 3044},
      {""}, {""}, {""},
#line 647 "zhy_symbol_map"
      {"cung3", 639},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 646 "zhy_symbol_map"
      {"cung2", 638},
      {""}, {""}, {""}, {""},
#line 4053 "zhy_symbol_map"
      {"se7", 4045},
      {""}, {""}, {""},
#line 1931 "zhy_symbol_map"
      {"hon6", 1923},
#line 649 "zhy_symbol_map"
      {"cung5", 641},
#line 1928 "zhy_symbol_map"
      {"hon3", 1920},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 645 "zhy_symbol_map"
      {"cung1", 637},
#line 1927 "zhy_symbol_map"
      {"hon2", 1919},
      {""},
#line 4155 "zhy_symbol_map"
      {"so4", 4147},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1848 "zhy_symbol_map"
      {"hin7", 1840},
      {""}, {""}, {""},
#line 3493 "zhy_symbol_map"
      {"ngon7", 3485},
#line 1757 "zhy_symbol_map"
      {"han7", 1749},
      {""},
#line 3353 "zhy_symbol_map"
      {"ngaam7", 3345},
      {""}, {""},
#line 1926 "zhy_symbol_map"
      {"hon1", 1918},
      {""},
#line 3352 "zhy_symbol_map"
      {"ngaam6", 3344},
      {""},
#line 3492 "zhy_symbol_map"
      {"ngon6", 3484},
      {""}, {""},
#line 3349 "zhy_symbol_map"
      {"ngaam3", 3341},
      {""}, {""},
#line 1925 "zhy_symbol_map"
      {"hok7", 1917},
      {""},
#line 3348 "zhy_symbol_map"
      {"ngaam2", 3340},
      {""},
#line 3489 "zhy_symbol_map"
      {"ngon3", 3481},
#line 1229 "zhy_symbol_map"
      {"gaang4", 1221},
      {""},
#line 3351 "zhy_symbol_map"
      {"ngaam5", 3343},
      {""}, {""}, {""},
#line 1939 "zhy_symbol_map"
      {"hong7", 1931},
#line 3347 "zhy_symbol_map"
      {"ngaam1", 3339},
      {""},
#line 3488 "zhy_symbol_map"
      {"ngon2", 3480},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1938 "zhy_symbol_map"
      {"hong6", 1930},
      {""},
#line 644 "zhy_symbol_map"
      {"cun7", 636},
#line 3491 "zhy_symbol_map"
      {"ngon5", 3483},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1935 "zhy_symbol_map"
      {"hong3", 1927},
      {""}, {""},
#line 3487 "zhy_symbol_map"
      {"ngon1", 3479},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1934 "zhy_symbol_map"
      {"hong2", 1926},
      {""}, {""},
#line 965 "zhy_symbol_map"
      {"duk6", 957},
      {""},
#line 962 "zhy_symbol_map"
      {"duk3", 954},
      {""}, {""}, {""}, {""},
#line 1937 "zhy_symbol_map"
      {"hong5", 1929},
#line 4443 "zhy_symbol_map"
      {"to5", 4435},
      {""}, {""},
#line 3521 "zhy_symbol_map"
      {"ngung7", 3513},
#line 961 "zhy_symbol_map"
      {"duk2", 953},
      {""}, {""}, {""},
#line 3520 "zhy_symbol_map"
      {"ngung6", 3512},
#line 1933 "zhy_symbol_map"
      {"hong1", 1925},
      {""}, {""}, {""},
#line 3517 "zhy_symbol_map"
      {"ngung3", 3509},
      {""}, {""}, {""}, {""},
#line 3516 "zhy_symbol_map"
      {"ngung2", 3508},
      {""}, {""}, {""}, {""},
#line 3519 "zhy_symbol_map"
      {"ngung5", 3511},
      {""}, {""}, {""}, {""},
#line 3515 "zhy_symbol_map"
      {"ngung1", 3507},
#line 960 "zhy_symbol_map"
      {"duk1", 952},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4047 "zhy_symbol_map"
      {"se1", 4039},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4052 "zhy_symbol_map"
      {"se6", 4044},
      {""}, {""}, {""}, {""},
#line 3479 "zhy_symbol_map"
      {"ngoi7", 3471},
#line 1932 "zhy_symbol_map"
      {"hon7", 1924},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3478 "zhy_symbol_map"
      {"ngoi6", 3470},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3475 "zhy_symbol_map"
      {"ngoi3", 3467},
      {""}, {""},
#line 3063 "zhy_symbol_map"
      {"mi4", 3055},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3474 "zhy_symbol_map"
      {"ngoi2", 3466},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3477 "zhy_symbol_map"
      {"ngoi5", 3469},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3040 "zhy_symbol_map"
      {"me2", 3032},
      {""},
#line 3473 "zhy_symbol_map"
      {"ngoi1", 3465},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3568 "zhy_symbol_map"
      {"nit5", 3560},
      {""}, {""}, {""}, {""},
#line 3274 "zhy_symbol_map"
      {"nat5", 3266},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3420 "zhy_symbol_map"
      {"ngang4", 3412},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 966 "zhy_symbol_map"
      {"duk7", 958},
      {""},
#line 1903 "zhy_symbol_map"
      {"hoe6", 1895},
      {""},
#line 1900 "zhy_symbol_map"
      {"hoe3", 1892},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 973 "zhy_symbol_map"
      {"dung7", 965},
      {""}, {""},
#line 1899 "zhy_symbol_map"
      {"hoe2", 1891},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 972 "zhy_symbol_map"
      {"dung6", 964},
      {""}, {""}, {""},
#line 4436 "zhy_symbol_map"
      {"tiu5", 4428},
      {""}, {""}, {""}, {""},
#line 4352 "zhy_symbol_map"
      {"tau5", 4344},
#line 969 "zhy_symbol_map"
      {"dung3", 961},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1898 "zhy_symbol_map"
      {"hoe1", 1890},
      {""},
#line 968 "zhy_symbol_map"
      {"dung2", 960},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 971 "zhy_symbol_map"
      {"dung5", 963},
      {""},
#line 3045 "zhy_symbol_map"
      {"me7", 3037},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 967 "zhy_symbol_map"
      {"dung1", 959},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3105 "zhy_symbol_map"
      {"mo4", 3097},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1701 "zhy_symbol_map"
      {"haan7", 1693},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1700 "zhy_symbol_map"
      {"haan6", 1692},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1697 "zhy_symbol_map"
      {"haan3", 1689},
#line 2820 "zhy_symbol_map"
      {"lit6", 2812},
      {""},
#line 2817 "zhy_symbol_map"
      {"lit3", 2809},
      {""}, {""},
#line 2708 "zhy_symbol_map"
      {"lat6", 2700},
      {""},
#line 2705 "zhy_symbol_map"
      {"lat3", 2697},
      {""},
#line 1696 "zhy_symbol_map"
      {"haan2", 1688},
      {""}, {""},
#line 2816 "zhy_symbol_map"
      {"lit2", 2808},
      {""}, {""}, {""}, {""},
#line 2704 "zhy_symbol_map"
      {"lat2", 2696},
      {""},
#line 1699 "zhy_symbol_map"
      {"haan5", 1691},
      {""}, {""}, {""}, {""},
#line 4989 "zhy_symbol_map"
      {"zuk5", 4981},
      {""}, {""}, {""}, {""},
#line 1695 "zhy_symbol_map"
      {"haan1", 1687},
      {""},
#line 2336 "zhy_symbol_map"
      {"kek5", 2328},
      {""}, {""}, {""}, {""}, {""},
#line 2815 "zhy_symbol_map"
      {"lit1", 2807},
      {""},
#line 1330 "zhy_symbol_map"
      {"geng7", 1322},
      {""}, {""},
#line 2703 "zhy_symbol_map"
      {"lat1", 2695},
      {""},
#line 4478 "zhy_symbol_map"
      {"tou5", 4470},
      {""}, {""}, {""},
#line 1904 "zhy_symbol_map"
      {"hoe7", 1896},
#line 1329 "zhy_symbol_map"
      {"geng6", 1321},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1326 "zhy_symbol_map"
      {"geng3", 1318},
      {""},
#line 3296 "zhy_symbol_map"
      {"nei6", 3288},
      {""},
#line 3293 "zhy_symbol_map"
      {"nei3", 3285},
      {""}, {""}, {""}, {""}, {""},
#line 1325 "zhy_symbol_map"
      {"geng2", 1317},
      {""}, {""}, {""},
#line 3292 "zhy_symbol_map"
      {"nei2", 3284},
      {""}, {""}, {""}, {""}, {""},
#line 1328 "zhy_symbol_map"
      {"geng5", 1320},
#line 3039 "zhy_symbol_map"
      {"me1", 3031},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1324 "zhy_symbol_map"
      {"geng1", 1316},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3044 "zhy_symbol_map"
      {"me6", 3036},
#line 3291 "zhy_symbol_map"
      {"nei1", 3283},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1680 "zhy_symbol_map"
      {"haai7", 1672},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1679 "zhy_symbol_map"
      {"haai6", 1671},
#line 4631 "zhy_symbol_map"
      {"wi4", 4623},
      {""}, {""},
#line 1432 "zhy_symbol_map"
      {"goeng4", 1424},
      {""}, {""}, {""}, {""}, {""},
#line 1676 "zhy_symbol_map"
      {"haai3", 1668},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1675 "zhy_symbol_map"
      {"haai2", 1667},
      {""}, {""}, {""}, {""}, {""},
#line 4608 "zhy_symbol_map"
      {"we2", 4600},
      {""}, {""}, {""},
#line 1678 "zhy_symbol_map"
      {"haai5", 1670},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1674 "zhy_symbol_map"
      {"haai1", 1666},
      {""}, {""},
#line 2821 "zhy_symbol_map"
      {"lit7", 2813},
      {""}, {""}, {""}, {""},
#line 2709 "zhy_symbol_map"
      {"lat7", 2701},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3297 "zhy_symbol_map"
      {"nei7", 3289},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4613 "zhy_symbol_map"
      {"we7", 4605},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4652 "zhy_symbol_map"
      {"wo4", 4644},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3360 "zhy_symbol_map"
      {"ngaan7", 3352},
#line 3367 "zhy_symbol_map"
      {"ngaang7", 3359},
      {""}, {""}, {""},
#line 3359 "zhy_symbol_map"
      {"ngaan6", 3351},
#line 3366 "zhy_symbol_map"
      {"ngaang6", 3358},
      {""}, {""}, {""},
#line 3356 "zhy_symbol_map"
      {"ngaan3", 3348},
#line 3363 "zhy_symbol_map"
      {"ngaang3", 3355},
      {""}, {""}, {""},
#line 3355 "zhy_symbol_map"
      {"ngaan2", 3347},
#line 3362 "zhy_symbol_map"
      {"ngaang2", 3354},
      {""}, {""}, {""},
#line 3358 "zhy_symbol_map"
      {"ngaan5", 3350},
#line 3365 "zhy_symbol_map"
      {"ngaang5", 3357},
      {""}, {""}, {""},
#line 3354 "zhy_symbol_map"
      {"ngaan1", 3346},
#line 3361 "zhy_symbol_map"
      {"ngaang1", 3353},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2777 "zhy_symbol_map"
      {"li5", 2769},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4065 "zhy_symbol_map"
      {"sek5", 4057},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1735 "zhy_symbol_map"
      {"hai6", 1727},
      {""},
#line 1732 "zhy_symbol_map"
      {"hai3", 1724},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1731 "zhy_symbol_map"
      {"hai2", 1723},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4607 "zhy_symbol_map"
      {"we1", 4599},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4612 "zhy_symbol_map"
      {"we6", 4604},
#line 1730 "zhy_symbol_map"
      {"hai1", 1722},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3525 "zhy_symbol_map"
      {"ni4", 3517},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3285 "zhy_symbol_map"
      {"ne2", 3277},
      {""}, {""}, {""}, {""},
#line 2833 "zhy_symbol_map"
      {"lo5", 2825},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4281 "zhy_symbol_map"
      {"taang4", 4273},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1917 "zhy_symbol_map"
      {"hoi6", 1909},
      {""},
#line 1914 "zhy_symbol_map"
      {"hoi3", 1906},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1913 "zhy_symbol_map"
      {"hoi2", 1905},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1736 "zhy_symbol_map"
      {"hai7", 1728},
      {""}, {""}, {""}, {""},
#line 1912 "zhy_symbol_map"
      {"hoi1", 1904},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3290 "zhy_symbol_map"
      {"ne7", 3282},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 86 "zhy_symbol_map"
      {"am2", 78},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3581 "zhy_symbol_map"
      {"no4", 3573},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2826 "zhy_symbol_map"
      {"liu5", 2818},
      {""}, {""}, {""}, {""},
#line 2714 "zhy_symbol_map"
      {"lau5", 2706},
#line 1391 "zhy_symbol_map"
      {"git5", 1383},
      {""}, {""}, {""}, {""},
#line 1300 "zhy_symbol_map"
      {"gat5", 1292},
      {""}, {""}, {""}, {""},
#line 91 "zhy_symbol_map"
      {"am7", 83},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1918 "zhy_symbol_map"
      {"hoi7", 1910},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4360 "zhy_symbol_map"
      {"tek6", 4352},
      {""},
#line 4357 "zhy_symbol_map"
      {"tek3", 4349},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4356 "zhy_symbol_map"
      {"tek2", 4348},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3284 "zhy_symbol_map"
      {"ne1", 3276},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3289 "zhy_symbol_map"
      {"ne6", 3281},
#line 4355 "zhy_symbol_map"
      {"tek1", 4347},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4906 "zhy_symbol_map"
      {"zit6", 4898},
      {""},
#line 4903 "zhy_symbol_map"
      {"zit3", 4895},
      {""}, {""},
#line 4808 "zhy_symbol_map"
      {"zat6", 4800},
      {""},
#line 4805 "zhy_symbol_map"
      {"zat3", 4797},
      {""}, {""}, {""}, {""},
#line 4902 "zhy_symbol_map"
      {"zit2", 4894},
      {""}, {""}, {""}, {""},
#line 4804 "zhy_symbol_map"
      {"zat2", 4796},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1829 "zhy_symbol_map"
      {"hi2", 1821},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 573 "zhy_symbol_map"
      {"ciu6", 565},
      {""},
#line 570 "zhy_symbol_map"
      {"ciu3", 562},
#line 4901 "zhy_symbol_map"
      {"zit1", 4893},
      {""},
#line 475 "zhy_symbol_map"
      {"cau6", 467},
      {""},
#line 472 "zhy_symbol_map"
      {"cau3", 464},
#line 4803 "zhy_symbol_map"
      {"zat1", 4795},
      {""},
#line 2896 "zhy_symbol_map"
      {"lou5", 2888},
      {""},
#line 569 "zhy_symbol_map"
      {"ciu2", 561},
      {""}, {""}, {""},
#line 1468 "zhy_symbol_map"
      {"got5", 1460},
#line 471 "zhy_symbol_map"
      {"cau2", 463},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 85 "zhy_symbol_map"
      {"am1", 77},
      {""}, {""},
#line 568 "zhy_symbol_map"
      {"ciu1", 560},
      {""}, {""}, {""}, {""},
#line 470 "zhy_symbol_map"
      {"cau1", 462},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 90 "zhy_symbol_map"
      {"am6", 82},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4361 "zhy_symbol_map"
      {"tek7", 4353},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4368 "zhy_symbol_map"
      {"teng7", 4360},
      {""},
#line 1322 "zhy_symbol_map"
      {"gei6", 1314},
#line 1834 "zhy_symbol_map"
      {"hi7", 1826},
#line 1319 "zhy_symbol_map"
      {"gei3", 1311},
      {""}, {""}, {""}, {""}, {""},
#line 4367 "zhy_symbol_map"
      {"teng6", 4359},
      {""}, {""}, {""},
#line 1318 "zhy_symbol_map"
      {"gei2", 1310},
      {""}, {""}, {""}, {""}, {""},
#line 4364 "zhy_symbol_map"
      {"teng3", 4356},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4363 "zhy_symbol_map"
      {"teng2", 4355},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1317 "zhy_symbol_map"
      {"gei1", 1309},
#line 4366 "zhy_symbol_map"
      {"teng5", 4358},
      {""}, {""}, {""}, {""}, {""},
#line 1892 "zhy_symbol_map"
      {"ho2", 1884},
#line 989 "zhy_symbol_map"
      {"e2", 981},
#line 4907 "zhy_symbol_map"
      {"zit7", 4899},
      {""},
#line 4362 "zhy_symbol_map"
      {"teng1", 4354},
      {""}, {""},
#line 4809 "zhy_symbol_map"
      {"zat7", 4801},
      {""},
#line 629 "zhy_symbol_map"
      {"cou6", 621},
#line 61 "zhy_symbol_map"
      {"aat5", 53},
#line 626 "zhy_symbol_map"
      {"cou3", 618},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2491 "zhy_symbol_map"
      {"kut6", 2483},
#line 625 "zhy_symbol_map"
      {"cou2", 617},
#line 2488 "zhy_symbol_map"
      {"kut3", 2480},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2487 "zhy_symbol_map"
      {"kut2", 2479},
      {""}, {""}, {""},
#line 574 "zhy_symbol_map"
      {"ciu7", 566},
      {""}, {""}, {""}, {""},
#line 476 "zhy_symbol_map"
      {"cau7", 468},
#line 2258 "zhy_symbol_map"
      {"kaau4", 2250},
      {""}, {""}, {""},
#line 624 "zhy_symbol_map"
      {"cou1", 616},
#line 2230 "zhy_symbol_map"
      {"kaam4", 2222},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2486 "zhy_symbol_map"
      {"kut1", 2478},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1897 "zhy_symbol_map"
      {"ho7", 1889},
      {""}, {""}, {""},
#line 2363 "zhy_symbol_map"
      {"kik4", 2355},
#line 2223 "zhy_symbol_map"
      {"kaak4", 2215},
      {""}, {""},
#line 1828 "zhy_symbol_map"
      {"hi1", 1820},
#line 2272 "zhy_symbol_map"
      {"kak4", 2264},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4982 "zhy_symbol_map"
      {"zui5", 4974},
      {""}, {""},
#line 1833 "zhy_symbol_map"
      {"hi6", 1825},
#line 1323 "zhy_symbol_map"
      {"gei7", 1315},
      {""}, {""},
#line 2329 "zhy_symbol_map"
      {"kei5", 2321},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2384 "zhy_symbol_map"
      {"king4", 2376},
      {""}, {""},
#line 4863 "zhy_symbol_map"
      {"zi5", 4855},
#line 2209 "zhy_symbol_map"
      {"kaa4", 2201},
#line 2293 "zhy_symbol_map"
      {"kang4", 2285},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 630 "zhy_symbol_map"
      {"cou7", 622},
#line 902 "zhy_symbol_map"
      {"diu6", 894},
      {""},
#line 899 "zhy_symbol_map"
      {"diu3", 891},
      {""},
#line 4618 "zhy_symbol_map"
      {"wen5", 4610},
#line 790 "zhy_symbol_map"
      {"dau6", 782},
      {""},
#line 787 "zhy_symbol_map"
      {"dau3", 779},
      {""}, {""},
#line 2492 "zhy_symbol_map"
      {"kut7", 2484},
      {""},
#line 898 "zhy_symbol_map"
      {"diu2", 890},
      {""}, {""}, {""}, {""},
#line 786 "zhy_symbol_map"
      {"dau2", 778},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 897 "zhy_symbol_map"
      {"diu1", 889},
      {""}, {""}, {""}, {""},
#line 785 "zhy_symbol_map"
      {"dau1", 777},
      {""},
#line 2377 "zhy_symbol_map"
      {"kin4", 2369},
      {""}, {""}, {""}, {""},
#line 2286 "zhy_symbol_map"
      {"kan4", 2278},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2447 "zhy_symbol_map"
      {"kok4", 2439},
      {""}, {""}, {""},
#line 1891 "zhy_symbol_map"
      {"ho1", 1883},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1896 "zhy_symbol_map"
      {"ho6", 1888},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2454 "zhy_symbol_map"
      {"kong4", 2446},
      {""}, {""},
#line 4919 "zhy_symbol_map"
      {"zo5", 4911},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 958 "zhy_symbol_map"
      {"dou6", 950},
      {""},
#line 955 "zhy_symbol_map"
      {"dou3", 947},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 954 "zhy_symbol_map"
      {"dou2", 946},
      {""}, {""}, {""}, {""}, {""},
#line 1348 "zhy_symbol_map"
      {"gi4", 1340},
      {""}, {""},
#line 2636 "zhy_symbol_map"
      {"laang4", 2628},
      {""}, {""}, {""}, {""}, {""},
#line 903 "zhy_symbol_map"
      {"diu7", 895},
#line 3701 "zhy_symbol_map"
      {"ou5", 3693},
      {""}, {""}, {""},
#line 791 "zhy_symbol_map"
      {"dau7", 783},
#line 2462 "zhy_symbol_map"
      {"ku5", 2454},
      {""}, {""}, {""},
#line 953 "zhy_symbol_map"
      {"dou1", 945},
      {""}, {""},
#line 3987 "zhy_symbol_map"
      {"saau4", 3979},
      {""}, {""},
#line 1311 "zhy_symbol_map"
      {"ge2", 1303},
      {""},
#line 3952 "zhy_symbol_map"
      {"saam4", 3944},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4106 "zhy_symbol_map"
      {"sik4", 4098},
#line 3945 "zhy_symbol_map"
      {"saak4", 3937},
      {""}, {""}, {""},
#line 4001 "zhy_symbol_map"
      {"sak4", 3993},
      {""}, {""},
#line 3470 "zhy_symbol_map"
      {"ngo5", 3462},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4058 "zhy_symbol_map"
      {"sei5", 4050},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3835 "zhy_symbol_map"
      {"pik6", 3827},
#line 1316 "zhy_symbol_map"
      {"ge7", 1308},
#line 3832 "zhy_symbol_map"
      {"pik3", 3824},
      {""}, {""},
#line 3765 "zhy_symbol_map"
      {"pak6", 3757},
      {""},
#line 3762 "zhy_symbol_map"
      {"pak3", 3754},
      {""}, {""}, {""}, {""},
#line 3831 "zhy_symbol_map"
      {"pik2", 3823},
      {""}, {""}, {""}, {""},
#line 3761 "zhy_symbol_map"
      {"pak2", 3753},
      {""},
#line 1404 "zhy_symbol_map"
      {"go4", 1396},
      {""},
#line 4127 "zhy_symbol_map"
      {"sing4", 4119},
      {""}, {""}, {""},
#line 3931 "zhy_symbol_map"
      {"saa4", 3923},
#line 4022 "zhy_symbol_map"
      {"sang4", 4014},
      {""},
#line 959 "zhy_symbol_map"
      {"dou7", 951},
      {""}, {""}, {""}, {""},
#line 4912 "zhy_symbol_map"
      {"ziu5", 4904},
      {""}, {""}, {""},
#line 3830 "zhy_symbol_map"
      {"pik1", 3822},
#line 4814 "zhy_symbol_map"
      {"zau5", 4806},
      {""}, {""}, {""},
#line 3760 "zhy_symbol_map"
      {"pak1", 3752},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3752 "zhy_symbol_map"
      {"paau7", 3744},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3709 "zhy_symbol_map"
      {"paa6", 3701},
      {""},
#line 3706 "zhy_symbol_map"
      {"paa3", 3698},
#line 3751 "zhy_symbol_map"
      {"paau6", 3743},
      {""},
#line 2736 "zhy_symbol_map"
      {"lek6", 2728},
      {""},
#line 2733 "zhy_symbol_map"
      {"lek3", 2725},
      {""}, {""}, {""}, {""},
#line 3705 "zhy_symbol_map"
      {"paa2", 3697},
#line 3748 "zhy_symbol_map"
      {"paau3", 3740},
#line 3322 "zhy_symbol_map"
      {"ng4", 3314},
#line 4120 "zhy_symbol_map"
      {"sin4", 4112},
      {""},
#line 2732 "zhy_symbol_map"
      {"lek2", 2724},
      {""}, {""},
#line 4015 "zhy_symbol_map"
      {"san4", 4007},
      {""}, {""},
#line 3747 "zhy_symbol_map"
      {"paau2", 3739},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3750 "zhy_symbol_map"
      {"paau5", 3742},
      {""},
#line 4197 "zhy_symbol_map"
      {"sok4", 4189},
#line 2426 "zhy_symbol_map"
      {"koek4", 2418},
#line 3704 "zhy_symbol_map"
      {"paa1", 3696},
      {""}, {""}, {""}, {""},
#line 2731 "zhy_symbol_map"
      {"lek1", 2723},
#line 3746 "zhy_symbol_map"
      {"paau1", 3738},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3724 "zhy_symbol_map"
      {"paak7", 3716},
#line 4429 "zhy_symbol_map"
      {"tit5", 4421},
#line 3842 "zhy_symbol_map"
      {"pin6", 3834},
      {""},
#line 3839 "zhy_symbol_map"
      {"pin3", 3831},
      {""},
#line 4345 "zhy_symbol_map"
      {"tat5", 4337},
#line 3772 "zhy_symbol_map"
      {"pan6", 3764},
      {""},
#line 3769 "zhy_symbol_map"
      {"pan3", 3761},
#line 3723 "zhy_symbol_map"
      {"paak6", 3715},
      {""}, {""}, {""},
#line 3838 "zhy_symbol_map"
      {"pin2", 3830},
      {""}, {""}, {""}, {""},
#line 3768 "zhy_symbol_map"
      {"pan2", 3760},
#line 3720 "zhy_symbol_map"
      {"paak3", 3712},
#line 74 "zhy_symbol_map"
      {"ai4", 66},
#line 3877 "zhy_symbol_map"
      {"pok6", 3869},
      {""},
#line 3874 "zhy_symbol_map"
      {"pok3", 3866},
      {""},
#line 11 "zhy_symbol_map"
      {"aa4", 3},
      {""},
#line 2237 "zhy_symbol_map"
      {"kaan4", 2229},
      {""},
#line 3719 "zhy_symbol_map"
      {"paak2", 3711},
#line 1310 "zhy_symbol_map"
      {"ge1", 1302},
      {""}, {""},
#line 3873 "zhy_symbol_map"
      {"pok2", 3865},
      {""}, {""}, {""}, {""},
#line 3837 "zhy_symbol_map"
      {"pin1", 3829},
#line 3722 "zhy_symbol_map"
      {"paak5", 3714},
      {""}, {""},
#line 4204 "zhy_symbol_map"
      {"song4", 4196},
#line 3767 "zhy_symbol_map"
      {"pan1", 3759},
      {""}, {""},
#line 2419 "zhy_symbol_map"
      {"koe4", 2411},
#line 1315 "zhy_symbol_map"
      {"ge6", 1307},
#line 3836 "zhy_symbol_map"
      {"pik7", 3828},
#line 3718 "zhy_symbol_map"
      {"paak1", 3710},
      {""}, {""}, {""},
#line 3766 "zhy_symbol_map"
      {"pak7", 3758},
#line 4975 "zhy_symbol_map"
      {"zou5", 4967},
      {""}, {""}, {""},
#line 3872 "zhy_symbol_map"
      {"pok1", 3864},
#line 3850 "zhy_symbol_map"
      {"ping7", 3842},
      {""}, {""}, {""}, {""},
#line 3780 "zhy_symbol_map"
      {"pang7", 3772},
      {""}, {""}, {""}, {""},
#line 3849 "zhy_symbol_map"
      {"ping6", 3841},
      {""}, {""}, {""}, {""},
#line 3779 "zhy_symbol_map"
      {"pang6", 3771},
#line 3170 "zhy_symbol_map"
      {"mut6", 3162},
      {""},
#line 3167 "zhy_symbol_map"
      {"mut3", 3159},
      {""},
#line 3846 "zhy_symbol_map"
      {"ping3", 3838},
#line 116 "zhy_symbol_map"
      {"at4", 108},
      {""}, {""}, {""},
#line 3776 "zhy_symbol_map"
      {"pang3", 3768},
#line 109 "zhy_symbol_map"
      {"ap4", 101},
      {""},
#line 3166 "zhy_symbol_map"
      {"mut2", 3158},
      {""},
#line 3845 "zhy_symbol_map"
      {"ping2", 3837},
      {""}, {""}, {""}, {""},
#line 3775 "zhy_symbol_map"
      {"pang2", 3767},
      {""}, {""},
#line 2986 "zhy_symbol_map"
      {"maau4", 2978},
      {""},
#line 3848 "zhy_symbol_map"
      {"ping5", 3840},
      {""}, {""}, {""}, {""},
#line 3778 "zhy_symbol_map"
      {"pang5", 3770},
      {""}, {""}, {""}, {""},
#line 3844 "zhy_symbol_map"
      {"ping1", 3836},
      {""}, {""},
#line 3165 "zhy_symbol_map"
      {"mut1", 3157},
#line 3710 "zhy_symbol_map"
      {"paa7", 3702},
#line 3774 "zhy_symbol_map"
      {"pang1", 3766},
      {""},
#line 1741 "zhy_symbol_map"
      {"hak5", 1733},
      {""},
#line 2737 "zhy_symbol_map"
      {"lek7", 2729},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2751 "zhy_symbol_map"
      {"leng7", 2743},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2750 "zhy_symbol_map"
      {"leng6", 2742},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2747 "zhy_symbol_map"
      {"leng3", 2739},
      {""}, {""}, {""}, {""},
#line 635 "zhy_symbol_map"
      {"cuk5", 627},
      {""},
#line 3070 "zhy_symbol_map"
      {"mik4", 3062},
#line 2958 "zhy_symbol_map"
      {"maak4", 2950},
      {""},
#line 2746 "zhy_symbol_map"
      {"leng2", 2738},
      {""},
#line 3000 "zhy_symbol_map"
      {"mak4", 2992},
#line 2216 "zhy_symbol_map"
      {"kaai4", 2208},
      {""}, {""}, {""}, {""}, {""},
#line 3843 "zhy_symbol_map"
      {"pin7", 3835},
#line 2749 "zhy_symbol_map"
      {"leng5", 2741},
      {""}, {""}, {""},
#line 3773 "zhy_symbol_map"
      {"pan7", 3765},
      {""}, {""},
#line 1671 "zhy_symbol_map"
      {"haa5", 1663},
      {""}, {""},
#line 2745 "zhy_symbol_map"
      {"leng1", 2737},
      {""},
#line 3050 "zhy_symbol_map"
      {"mei5", 3042},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3878 "zhy_symbol_map"
      {"pok7", 3870},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3885 "zhy_symbol_map"
      {"pong7", 3877},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3884 "zhy_symbol_map"
      {"pong6", 3876},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3084 "zhy_symbol_map"
      {"ming4", 3076},
#line 2860 "zhy_symbol_map"
      {"loeng4", 2852},
#line 3881 "zhy_symbol_map"
      {"pong3", 3873},
      {""},
#line 2944 "zhy_symbol_map"
      {"maa4", 2936},
#line 3021 "zhy_symbol_map"
      {"mang4", 3013},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3880 "zhy_symbol_map"
      {"pong2", 3872},
      {""},
#line 1846 "zhy_symbol_map"
      {"hin5", 1838},
      {""}, {""}, {""}, {""},
#line 1755 "zhy_symbol_map"
      {"han5", 1747},
      {""}, {""},
#line 3883 "zhy_symbol_map"
      {"pong5", 3875},
      {""}, {""},
#line 3171 "zhy_symbol_map"
      {"mut7", 3163},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3879 "zhy_symbol_map"
      {"pong1", 3871},
      {""},
#line 1923 "zhy_symbol_map"
      {"hok5", 1915},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 642 "zhy_symbol_map"
      {"cun5", 634},
      {""},
#line 3077 "zhy_symbol_map"
      {"min4", 3069},
      {""}, {""}, {""}, {""},
#line 3014 "zhy_symbol_map"
      {"man4", 3006},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3119 "zhy_symbol_map"
      {"mok4", 3111},
#line 4169 "zhy_symbol_map"
      {"soek4", 4161},
#line 3500 "zhy_symbol_map"
      {"ngong7", 3492},
      {""}, {""}, {""}, {""},
#line 3499 "zhy_symbol_map"
      {"ngong6", 3491},
      {""}, {""}, {""}, {""},
#line 3496 "zhy_symbol_map"
      {"ngong3", 3488},
      {""}, {""}, {""}, {""},
#line 3495 "zhy_symbol_map"
      {"ngong2", 3487},
      {""}, {""}, {""}, {""},
#line 3498 "zhy_symbol_map"
      {"ngong5", 3490},
      {""}, {""}, {""}, {""},
#line 3494 "zhy_symbol_map"
      {"ngong1", 3486},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3959 "zhy_symbol_map"
      {"saan4", 3951},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3126 "zhy_symbol_map"
      {"mong4", 3118},
      {""}, {""}, {""},
#line 4162 "zhy_symbol_map"
      {"soe4", 4154},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1930 "zhy_symbol_map"
      {"hon5", 1922},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4696 "zhy_symbol_map"
      {"wut6", 4688},
      {""},
#line 4693 "zhy_symbol_map"
      {"wut3", 4685},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4692 "zhy_symbol_map"
      {"wut2", 4684},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4691 "zhy_symbol_map"
      {"wut1", 4683},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3381 "zhy_symbol_map"
      {"ngaat7", 3373},
      {""}, {""}, {""}, {""},
#line 3380 "zhy_symbol_map"
      {"ngaat6", 3372},
      {""}, {""}, {""}, {""},
#line 3377 "zhy_symbol_map"
      {"ngaat3", 3369},
      {""}, {""}, {""}, {""},
#line 3376 "zhy_symbol_map"
      {"ngaat2", 3368},
      {""}, {""}, {""}, {""},
#line 3379 "zhy_symbol_map"
      {"ngaat5", 3371},
      {""}, {""}, {""}, {""},
#line 3375 "zhy_symbol_map"
      {"ngaat1", 3367},
      {""},
#line 964 "zhy_symbol_map"
      {"duk5", 956},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4638 "zhy_symbol_map"
      {"wik4", 4630},
#line 4540 "zhy_symbol_map"
      {"waak4", 4532},
      {""}, {""}, {""},
#line 4575 "zhy_symbol_map"
      {"wak4", 4567},
#line 3938 "zhy_symbol_map"
      {"saai4", 3930},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3731 "zhy_symbol_map"
      {"paan7", 3723},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3730 "zhy_symbol_map"
      {"paan6", 3722},
      {""}, {""}, {""},
#line 4736 "zhy_symbol_map"
      {"zaang4", 4728},
      {""}, {""}, {""}, {""}, {""},
#line 3727 "zhy_symbol_map"
      {"paan3", 3719},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3726 "zhy_symbol_map"
      {"paan2", 3718},
      {""}, {""},
#line 4645 "zhy_symbol_map"
      {"wing4", 4637},
      {""}, {""}, {""},
#line 4526 "zhy_symbol_map"
      {"waa4", 4518},
#line 4589 "zhy_symbol_map"
      {"wang4", 4581},
      {""},
#line 3729 "zhy_symbol_map"
      {"paan5", 3721},
      {""},
#line 2265 "zhy_symbol_map"
      {"kai4", 2257},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3725 "zhy_symbol_map"
      {"paan1", 3717},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4697 "zhy_symbol_map"
      {"wut7", 4689},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4386 "zhy_symbol_map"
      {"ti4", 4378},
      {""}, {""}, {""}, {""}, {""},
#line 4582 "zhy_symbol_map"
      {"wan4", 4574},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4659 "zhy_symbol_map"
      {"wok4", 4651},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1708 "zhy_symbol_map"
      {"haang7", 1700},
      {""}, {""}, {""}, {""},
#line 1707 "zhy_symbol_map"
      {"haang6", 1699},
#line 1902 "zhy_symbol_map"
      {"hoe5", 1894},
      {""}, {""},
#line 3717 "zhy_symbol_map"
      {"paai7", 3709},
#line 1704 "zhy_symbol_map"
      {"haang3", 1696},
      {""}, {""}, {""}, {""},
#line 1703 "zhy_symbol_map"
      {"haang2", 1695},
      {""}, {""}, {""},
#line 3716 "zhy_symbol_map"
      {"paai6", 3708},
#line 1706 "zhy_symbol_map"
      {"haang5", 1698},
      {""}, {""}, {""}, {""},
#line 1702 "zhy_symbol_map"
      {"haang1", 1694},
      {""}, {""}, {""},
#line 3713 "zhy_symbol_map"
      {"paai3", 3705},
      {""}, {""},
#line 2965 "zhy_symbol_map"
      {"maan4", 2957},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3712 "zhy_symbol_map"
      {"paai2", 3704},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4666 "zhy_symbol_map"
      {"wong4", 4658},
      {""},
#line 3715 "zhy_symbol_map"
      {"paai5", 3707},
      {""},
#line 2440 "zhy_symbol_map"
      {"koi4", 2432},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3711 "zhy_symbol_map"
      {"paai1", 3703},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4829 "zhy_symbol_map"
      {"zek6", 4821},
      {""},
#line 4826 "zhy_symbol_map"
      {"zek3", 4818},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4825 "zhy_symbol_map"
      {"zek2", 4817},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4674 "zhy_symbol_map"
      {"wu5", 4666},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3224 "zhy_symbol_map"
      {"naau4", 3216},
      {""}, {""},
#line 4442 "zhy_symbol_map"
      {"to4", 4434},
      {""},
#line 3189 "zhy_symbol_map"
      {"naam4", 3181},
      {""}, {""}, {""}, {""}, {""},
#line 4824 "zhy_symbol_map"
      {"zek1", 4816},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2819 "zhy_symbol_map"
      {"lit5", 2811},
      {""}, {""}, {""}, {""},
#line 2707 "zhy_symbol_map"
      {"lat5", 2699},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3532 "zhy_symbol_map"
      {"nik4", 3524},
      {""}, {""}, {""}, {""},
#line 3238 "zhy_symbol_map"
      {"nak4", 3230},
#line 2951 "zhy_symbol_map"
      {"maai4", 2943},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3295 "zhy_symbol_map"
      {"nei5", 3287},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3553 "zhy_symbol_map"
      {"ning4", 3545},
      {""}, {""}, {""},
#line 3175 "zhy_symbol_map"
      {"naa4", 3167},
#line 3259 "zhy_symbol_map"
      {"nang4", 3251},
      {""}, {""}, {""},
#line 3994 "zhy_symbol_map"
      {"sai4", 3986},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4830 "zhy_symbol_map"
      {"zek7", 4822},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4837 "zhy_symbol_map"
      {"zeng7", 4829},
#line 566 "zhy_symbol_map"
      {"cit6", 558},
      {""},
#line 563 "zhy_symbol_map"
      {"cit3", 555},
      {""}, {""},
#line 468 "zhy_symbol_map"
      {"cat6", 460},
      {""},
#line 465 "zhy_symbol_map"
      {"cat3", 457},
      {""},
#line 4836 "zhy_symbol_map"
      {"zeng6", 4828},
      {""}, {""},
#line 562 "zhy_symbol_map"
      {"cit2", 554},
      {""},
#line 1715 "zhy_symbol_map"
      {"haap7", 1707},
      {""},
#line 3758 "zhy_symbol_map"
      {"pai6", 3750},
#line 464 "zhy_symbol_map"
      {"cat2", 456},
#line 3755 "zhy_symbol_map"
      {"pai3", 3747},
#line 4833 "zhy_symbol_map"
      {"zeng3", 4825},
      {""}, {""}, {""}, {""},
#line 1714 "zhy_symbol_map"
      {"haap6", 1706},
      {""},
#line 3546 "zhy_symbol_map"
      {"nin4", 3538},
      {""},
#line 3754 "zhy_symbol_map"
      {"pai2", 3746},
#line 4832 "zhy_symbol_map"
      {"zeng2", 4824},
      {""},
#line 3252 "zhy_symbol_map"
      {"nan4", 3244},
      {""}, {""},
#line 1711 "zhy_symbol_map"
      {"haap3", 1703},
      {""}, {""},
#line 561 "zhy_symbol_map"
      {"cit1", 553},
      {""},
#line 4835 "zhy_symbol_map"
      {"zeng5", 4827},
      {""}, {""},
#line 463 "zhy_symbol_map"
      {"cat1", 455},
      {""},
#line 1710 "zhy_symbol_map"
      {"haap2", 1702},
      {""},
#line 3609 "zhy_symbol_map"
      {"nok4", 3601},
      {""}, {""},
#line 4831 "zhy_symbol_map"
      {"zeng1", 4823},
      {""}, {""}, {""},
#line 3753 "zhy_symbol_map"
      {"pai1", 3745},
#line 1713 "zhy_symbol_map"
      {"haap5", 1705},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1709 "zhy_symbol_map"
      {"haap1", 1701},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2729 "zhy_symbol_map"
      {"lei6", 2721},
      {""},
#line 2726 "zhy_symbol_map"
      {"lei3", 2718},
      {""}, {""}, {""}, {""},
#line 4946 "zhy_symbol_map"
      {"zoeng4", 4938},
      {""}, {""}, {""},
#line 4547 "zhy_symbol_map"
      {"waan4", 4539},
#line 2725 "zhy_symbol_map"
      {"lei2", 2717},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3616 "zhy_symbol_map"
      {"nong4", 3608},
      {""}, {""}, {""},
#line 4190 "zhy_symbol_map"
      {"soi4", 4182},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2724 "zhy_symbol_map"
      {"lei1", 2716},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 567 "zhy_symbol_map"
      {"cit7", 559},
      {""}, {""}, {""}, {""},
#line 469 "zhy_symbol_map"
      {"cat7", 461},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3759 "zhy_symbol_map"
      {"pai7", 3751},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1911 "zhy_symbol_map"
      {"hoeng7", 1903},
      {""}, {""}, {""}, {""},
#line 1910 "zhy_symbol_map"
      {"hoeng6", 1902},
      {""}, {""}, {""}, {""},
#line 1907 "zhy_symbol_map"
      {"hoeng3", 1899},
      {""}, {""}, {""}, {""},
#line 1906 "zhy_symbol_map"
      {"hoeng2", 1898},
      {""}, {""}, {""}, {""},
#line 1909 "zhy_symbol_map"
      {"hoeng5", 1901},
      {""}, {""}, {""}, {""},
#line 1905 "zhy_symbol_map"
      {"hoeng1", 1897},
      {""},
#line 4533 "zhy_symbol_map"
      {"waai4", 4525},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1734 "zhy_symbol_map"
      {"hai5", 1726},
      {""},
#line 2730 "zhy_symbol_map"
      {"lei7", 2722},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2993 "zhy_symbol_map"
      {"mai4", 2985},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3664 "zhy_symbol_map"
      {"oi3", 3656},
      {""}, {""}, {""}, {""},
#line 2355 "zhy_symbol_map"
      {"ki3", 2347},
#line 895 "zhy_symbol_map"
      {"dit6", 887},
      {""},
#line 892 "zhy_symbol_map"
      {"dit3", 884},
      {""}, {""},
#line 783 "zhy_symbol_map"
      {"dat6", 775},
      {""},
#line 780 "zhy_symbol_map"
      {"dat3", 772},
#line 3648 "zhy_symbol_map"
      {"o1", 3640},
      {""}, {""}, {""},
#line 891 "zhy_symbol_map"
      {"dit2", 883},
      {""}, {""}, {""}, {""},
#line 779 "zhy_symbol_map"
      {"dat2", 771},
      {""}, {""}, {""}, {""},
#line 523 "zhy_symbol_map"
      {"ci5", 515},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 890 "zhy_symbol_map"
      {"dit1", 882},
      {""}, {""}, {""}, {""},
#line 778 "zhy_symbol_map"
      {"dat1", 770},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1916 "zhy_symbol_map"
      {"hoi5", 1908},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3196 "zhy_symbol_map"
      {"naan4", 3188},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3112 "zhy_symbol_map"
      {"moi4", 3104},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2411 "zhy_symbol_map"
      {"ko3", 2403},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2776 "zhy_symbol_map"
      {"li4", 2768},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 579 "zhy_symbol_map"
      {"co5", 571},
      {""}, {""}, {""}, {""},
#line 896 "zhy_symbol_map"
      {"dit7", 888},
      {""}, {""}, {""}, {""},
#line 784 "zhy_symbol_map"
      {"dat7", 776},
      {""}, {""}, {""}, {""},
#line 2718 "zhy_symbol_map"
      {"le2", 2710},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3182 "zhy_symbol_map"
      {"naai4", 3174},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3866 "zhy_symbol_map"
      {"po2", 3858},
      {""}, {""}, {""}, {""}, {""},
#line 4359 "zhy_symbol_map"
      {"tek5", 4351},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2723 "zhy_symbol_map"
      {"le7", 2715},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2832 "zhy_symbol_map"
      {"lo4", 2824},
      {""}, {""}, {""}, {""}, {""},
#line 4568 "zhy_symbol_map"
      {"wai4", 4560},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4905 "zhy_symbol_map"
      {"zit5", 4897},
      {""},
#line 4098 "zhy_symbol_map"
      {"si3", 4090},
      {""}, {""},
#line 4807 "zhy_symbol_map"
      {"zat5", 4799},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1518 "zhy_symbol_map"
      {"gut6", 1510},
      {""},
#line 1515 "zhy_symbol_map"
      {"gut3", 1507},
#line 852 "zhy_symbol_map"
      {"di5", 844},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1514 "zhy_symbol_map"
      {"gut2", 1506},
      {""}, {""}, {""}, {""},
#line 3871 "zhy_symbol_map"
      {"po7", 3863},
      {""},
#line 572 "zhy_symbol_map"
      {"ciu5", 564},
      {""}, {""},
#line 1250 "zhy_symbol_map"
      {"gaau4", 1242},
      {""},
#line 474 "zhy_symbol_map"
      {"cau5", 466},
      {""}, {""},
#line 1215 "zhy_symbol_map"
      {"gaam4", 1207},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1513 "zhy_symbol_map"
      {"gut1", 1505},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1355 "zhy_symbol_map"
      {"gik4", 1347},
#line 1208 "zhy_symbol_map"
      {"gaak4", 1200},
      {""}, {""}, {""},
#line 1264 "zhy_symbol_map"
      {"gak4", 1256},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2717 "zhy_symbol_map"
      {"le1", 2709},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1321 "zhy_symbol_map"
      {"gei5", 1313},
      {""}, {""}, {""}, {""}, {""},
#line 2722 "zhy_symbol_map"
      {"le6", 2714},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 258 "zhy_symbol_map"
      {"bik6", 250},
      {""},
#line 255 "zhy_symbol_map"
      {"bik3", 247},
      {""}, {""},
#line 188 "zhy_symbol_map"
      {"bak6", 180},
#line 4154 "zhy_symbol_map"
      {"so3", 4146},
#line 185 "zhy_symbol_map"
      {"bak3", 177},
      {""}, {""}, {""}, {""},
#line 254 "zhy_symbol_map"
      {"bik2", 246},
      {""}, {""}, {""}, {""},
#line 184 "zhy_symbol_map"
      {"bak2", 176},
      {""}, {""}, {""},
#line 1376 "zhy_symbol_map"
      {"ging4", 1368},
#line 908 "zhy_symbol_map"
      {"do5", 900},
      {""}, {""},
#line 1194 "zhy_symbol_map"
      {"gaa4", 1186},
#line 1285 "zhy_symbol_map"
      {"gang4", 1277},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 253 "zhy_symbol_map"
      {"bik1", 245},
#line 628 "zhy_symbol_map"
      {"cou5", 620},
      {""}, {""}, {""},
#line 183 "zhy_symbol_map"
      {"bak1", 175},
      {""},
#line 3865 "zhy_symbol_map"
      {"po1", 3857},
      {""},
#line 1519 "zhy_symbol_map"
      {"gut7", 1511},
      {""}, {""},
#line 2490 "zhy_symbol_map"
      {"kut5", 2482},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 175 "zhy_symbol_map"
      {"baau7", 167},
      {""}, {""},
#line 3870 "zhy_symbol_map"
      {"po6", 3862},
      {""}, {""}, {""},
#line 132 "zhy_symbol_map"
      {"baa6", 124},
      {""},
#line 129 "zhy_symbol_map"
      {"baa3", 121},
#line 174 "zhy_symbol_map"
      {"baau6", 166},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 128 "zhy_symbol_map"
      {"baa2", 120},
#line 171 "zhy_symbol_map"
      {"baau3", 163},
      {""},
#line 1369 "zhy_symbol_map"
      {"gin4", 1361},
      {""}, {""}, {""}, {""},
#line 1278 "zhy_symbol_map"
      {"gan4", 1270},
      {""}, {""},
#line 170 "zhy_symbol_map"
      {"baau2", 162},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3441 "zhy_symbol_map"
      {"ngau4", 3433},
      {""},
#line 173 "zhy_symbol_map"
      {"baau5", 165},
      {""},
#line 1446 "zhy_symbol_map"
      {"gok4", 1438},
#line 3406 "zhy_symbol_map"
      {"ngam4", 3398},
#line 127 "zhy_symbol_map"
      {"baa1", 119},
      {""}, {""}, {""}, {""}, {""},
#line 169 "zhy_symbol_map"
      {"baau1", 161},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 147 "zhy_symbol_map"
      {"baak7", 139},
      {""},
#line 265 "zhy_symbol_map"
      {"bin6", 257},
      {""},
#line 262 "zhy_symbol_map"
      {"bin3", 254},
      {""}, {""},
#line 202 "zhy_symbol_map"
      {"ban6", 194},
      {""},
#line 199 "zhy_symbol_map"
      {"ban3", 191},
#line 146 "zhy_symbol_map"
      {"baak6", 138},
      {""}, {""}, {""},
#line 261 "zhy_symbol_map"
      {"bin2", 253},
      {""}, {""}, {""}, {""},
#line 198 "zhy_symbol_map"
      {"ban2", 190},
#line 143 "zhy_symbol_map"
      {"baak3", 135},
      {""},
#line 300 "zhy_symbol_map"
      {"bok6", 292},
      {""},
#line 297 "zhy_symbol_map"
      {"bok3", 289},
      {""}, {""}, {""}, {""}, {""},
#line 142 "zhy_symbol_map"
      {"baak2", 134},
      {""}, {""}, {""},
#line 296 "zhy_symbol_map"
      {"bok2", 288},
      {""}, {""}, {""},
#line 3399 "zhy_symbol_map"
      {"ngak4", 3391},
#line 260 "zhy_symbol_map"
      {"bin1", 252},
#line 145 "zhy_symbol_map"
      {"baak5", 137},
      {""}, {""},
#line 1460 "zhy_symbol_map"
      {"gong4", 1452},
#line 197 "zhy_symbol_map"
      {"ban1", 189},
      {""}, {""},
#line 3231 "zhy_symbol_map"
      {"nai4", 3223},
      {""},
#line 259 "zhy_symbol_map"
      {"bik7", 251},
#line 141 "zhy_symbol_map"
      {"baak1", 133},
      {""}, {""}, {""},
#line 189 "zhy_symbol_map"
      {"bak7", 181},
      {""}, {""}, {""}, {""},
#line 295 "zhy_symbol_map"
      {"bok1", 287},
#line 273 "zhy_symbol_map"
      {"bing7", 265},
      {""}, {""},
#line 3062 "zhy_symbol_map"
      {"mi3", 3054},
      {""},
#line 210 "zhy_symbol_map"
      {"bang7", 202},
      {""}, {""}, {""}, {""},
#line 272 "zhy_symbol_map"
      {"bing6", 264},
      {""}, {""}, {""}, {""},
#line 209 "zhy_symbol_map"
      {"bang6", 201},
      {""},
#line 2934 "zhy_symbol_map"
      {"m1", 2926},
      {""}, {""},
#line 269 "zhy_symbol_map"
      {"bing3", 261},
      {""}, {""}, {""}, {""},
#line 206 "zhy_symbol_map"
      {"bang3", 198},
      {""}, {""},
#line 901 "zhy_symbol_map"
      {"diu5", 893},
      {""},
#line 268 "zhy_symbol_map"
      {"bing2", 260},
#line 1482 "zhy_symbol_map"
      {"gu5", 1474},
      {""},
#line 789 "zhy_symbol_map"
      {"dau5", 781},
      {""},
#line 205 "zhy_symbol_map"
      {"bang2", 197},
      {""}, {""}, {""}, {""},
#line 271 "zhy_symbol_map"
      {"bing5", 263},
      {""},
#line 1453 "zhy_symbol_map"
      {"gon4", 1445},
      {""}, {""},
#line 208 "zhy_symbol_map"
      {"bang5", 200},
      {""},
#line 25 "zhy_symbol_map"
      {"aak4", 17},
      {""}, {""},
#line 267 "zhy_symbol_map"
      {"bing1", 259},
      {""}, {""}, {""},
#line 133 "zhy_symbol_map"
      {"baa7", 125},
#line 204 "zhy_symbol_map"
      {"bang1", 196},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3588 "zhy_symbol_map"
      {"noei4", 3580},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 46 "zhy_symbol_map"
      {"aang4", 38},
#line 266 "zhy_symbol_map"
      {"bin7", 258},
      {""}, {""}, {""}, {""},
#line 203 "zhy_symbol_map"
      {"ban7", 195},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3602 "zhy_symbol_map"
      {"noi4", 3594},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 301 "zhy_symbol_map"
      {"bok7", 293},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 308 "zhy_symbol_map"
      {"bong7", 300},
      {""}, {""},
#line 3104 "zhy_symbol_map"
      {"mo3", 3096},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 307 "zhy_symbol_map"
      {"bong6", 299},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 304 "zhy_symbol_map"
      {"bong3", 296},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 39 "zhy_symbol_map"
      {"aan4", 31},
#line 957 "zhy_symbol_map"
      {"dou5", 949},
      {""},
#line 303 "zhy_symbol_map"
      {"bong2", 295},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 306 "zhy_symbol_map"
      {"bong5", 298},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 302 "zhy_symbol_map"
      {"bong1", 294},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1425 "zhy_symbol_map"
      {"goek4", 1417},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4862 "zhy_symbol_map"
      {"zi4", 4854},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1222 "zhy_symbol_map"
      {"gaan4", 1214},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 124 "zhy_symbol_map"
      {"au5", 116},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4818 "zhy_symbol_map"
      {"ze2", 4810},
#line 1411 "zhy_symbol_map"
      {"goe4", 1403},
      {""},
#line 2372 "zhy_symbol_map"
      {"kim6", 2364},
      {""},
#line 2369 "zhy_symbol_map"
      {"kim3", 2361},
#line 3834 "zhy_symbol_map"
      {"pik5", 3826},
      {""},
#line 2281 "zhy_symbol_map"
      {"kam6", 2273},
      {""},
#line 2278 "zhy_symbol_map"
      {"kam3", 2270},
#line 3764 "zhy_symbol_map"
      {"pak5", 3756},
      {""}, {""}, {""},
#line 2368 "zhy_symbol_map"
      {"kim2", 2360},
      {""},
#line 4630 "zhy_symbol_map"
      {"wi3", 4622},
#line 396 "zhy_symbol_map"
      {"caang4", 388},
      {""},
#line 2277 "zhy_symbol_map"
      {"kam2", 2269},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2367 "zhy_symbol_map"
      {"kim1", 2359},
      {""}, {""}, {""}, {""},
#line 2276 "zhy_symbol_map"
      {"kam1", 2268},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3448 "zhy_symbol_map"
      {"nge4", 3440},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3708 "zhy_symbol_map"
      {"paa5", 3700},
      {""}, {""}, {""}, {""},
#line 2735 "zhy_symbol_map"
      {"lek5", 2727},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4823 "zhy_symbol_map"
      {"ze7", 4815},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4918 "zhy_symbol_map"
      {"zo4", 4910},
      {""},
#line 1201 "zhy_symbol_map"
      {"gaai4", 1193},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3841 "zhy_symbol_map"
      {"pin5", 3833},
      {""}, {""}, {""}, {""},
#line 3771 "zhy_symbol_map"
      {"pan5", 3763},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 154 "zhy_symbol_map"
      {"baan7", 146},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3876 "zhy_symbol_map"
      {"pok5", 3868},
      {""}, {""},
#line 153 "zhy_symbol_map"
      {"baan6", 145},
      {""}, {""}, {""}, {""}, {""},
#line 3700 "zhy_symbol_map"
      {"ou4", 3692},
      {""},
#line 4651 "zhy_symbol_map"
      {"wo3", 4643},
      {""},
#line 150 "zhy_symbol_map"
      {"baan3", 142},
#line 2461 "zhy_symbol_map"
      {"ku4", 2453},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 149 "zhy_symbol_map"
      {"baan2", 141},
#line 2373 "zhy_symbol_map"
      {"kim7", 2365},
      {""}, {""}, {""}, {""},
#line 2282 "zhy_symbol_map"
      {"kam7", 2274},
      {""},
#line 3413 "zhy_symbol_map"
      {"ngan4", 3405},
      {""},
#line 152 "zhy_symbol_map"
      {"baan5", 144},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 148 "zhy_symbol_map"
      {"baan1", 140},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3169 "zhy_symbol_map"
      {"mut5", 3161},
      {""}, {""}, {""}, {""}, {""},
#line 489 "zhy_symbol_map"
      {"cek6", 481},
      {""},
#line 486 "zhy_symbol_map"
      {"cek3", 478},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 485 "zhy_symbol_map"
      {"cek2", 477},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 484 "zhy_symbol_map"
      {"cek1", 476},
      {""},
#line 4817 "zhy_symbol_map"
      {"ze1", 4809},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4302 "zhy_symbol_map"
      {"taau4", 4294},
      {""}, {""}, {""}, {""},
#line 4267 "zhy_symbol_map"
      {"taam4", 4259},
      {""}, {""}, {""}, {""},
#line 4822 "zhy_symbol_map"
      {"ze6", 4814},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 140 "zhy_symbol_map"
      {"baai7", 132},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 139 "zhy_symbol_map"
      {"baai6", 131},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 136 "zhy_symbol_map"
      {"baai3", 128},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 135 "zhy_symbol_map"
      {"baai2", 127},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4393 "zhy_symbol_map"
      {"tik4", 4385},
#line 3392 "zhy_symbol_map"
      {"ngai4", 3384},
      {""},
#line 138 "zhy_symbol_map"
      {"baai5", 130},
      {""}, {""},
#line 2244 "zhy_symbol_map"
      {"kaap4", 2236},
#line 4115 "zhy_symbol_map"
      {"sim6", 4107},
      {""},
#line 4112 "zhy_symbol_map"
      {"sim3", 4104},
      {""}, {""},
#line 4010 "zhy_symbol_map"
      {"sam6", 4002},
#line 134 "zhy_symbol_map"
      {"baai1", 126},
#line 4007 "zhy_symbol_map"
      {"sam3", 3999},
#line 711 "zhy_symbol_map"
      {"daang4", 703},
      {""}, {""}, {""},
#line 4111 "zhy_symbol_map"
      {"sim2", 4103},
      {""},
#line 3524 "zhy_symbol_map"
      {"ni3", 3516},
      {""}, {""},
#line 4006 "zhy_symbol_map"
      {"sam2", 3998},
      {""}, {""}, {""}, {""}, {""},
#line 1959 "zhy_symbol_map"
      {"huk6", 1951},
      {""},
#line 1956 "zhy_symbol_map"
      {"huk3", 1948},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1955 "zhy_symbol_map"
      {"huk2", 1947},
      {""},
#line 4110 "zhy_symbol_map"
      {"sim1", 4102},
      {""}, {""}, {""}, {""},
#line 4005 "zhy_symbol_map"
      {"sam1", 3997},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4414 "zhy_symbol_map"
      {"ting4", 4406},
#line 490 "zhy_symbol_map"
      {"cek7", 482},
      {""}, {""},
#line 4253 "zhy_symbol_map"
      {"taa4", 4245},
#line 4330 "zhy_symbol_map"
      {"tang4", 4322},
#line 1954 "zhy_symbol_map"
      {"huk1", 1946},
      {""}, {""}, {""}, {""}, {""},
#line 497 "zhy_symbol_map"
      {"ceng7", 489},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 496 "zhy_symbol_map"
      {"ceng6", 488},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 493 "zhy_symbol_map"
      {"ceng3", 485},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 492 "zhy_symbol_map"
      {"ceng2", 484},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 495 "zhy_symbol_map"
      {"ceng5", 487},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4407 "zhy_symbol_map"
      {"tin4", 4399},
      {""}, {""},
#line 491 "zhy_symbol_map"
      {"ceng1", 483},
      {""},
#line 4323 "zhy_symbol_map"
      {"tan4", 4315},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4463 "zhy_symbol_map"
      {"tok4", 4455},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3580 "zhy_symbol_map"
      {"no3", 3572},
#line 599 "zhy_symbol_map"
      {"coeng4", 591},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4116 "zhy_symbol_map"
      {"sim7", 4108},
      {""}, {""}, {""}, {""},
#line 4011 "zhy_symbol_map"
      {"sam7", 4003},
      {""}, {""}, {""}, {""}, {""},
#line 1257 "zhy_symbol_map"
      {"gai4", 1249},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1960 "zhy_symbol_map"
      {"huk7", 1952},
      {""}, {""}, {""},
#line 4470 "zhy_symbol_map"
      {"tong4", 4462},
      {""},
#line 811 "zhy_symbol_map"
      {"dek6", 803},
#line 4695 "zhy_symbol_map"
      {"wut5", 4687},
#line 808 "zhy_symbol_map"
      {"dek3", 800},
      {""}, {""},
#line 1967 "zhy_symbol_map"
      {"hung7", 1959},
#line 3738 "zhy_symbol_map"
      {"paang7", 3730},
      {""}, {""}, {""}, {""},
#line 3737 "zhy_symbol_map"
      {"paang6", 3729},
#line 807 "zhy_symbol_map"
      {"dek2", 799},
      {""}, {""},
#line 1966 "zhy_symbol_map"
      {"hung6", 1958},
#line 3734 "zhy_symbol_map"
      {"paang3", 3726},
      {""}, {""}, {""}, {""},
#line 3733 "zhy_symbol_map"
      {"paang2", 3725},
#line 181 "zhy_symbol_map"
      {"bai6", 173},
      {""},
#line 178 "zhy_symbol_map"
      {"bai3", 170},
#line 1963 "zhy_symbol_map"
      {"hung3", 1955},
#line 3736 "zhy_symbol_map"
      {"paang5", 3728},
      {""}, {""}, {""}, {""},
#line 3732 "zhy_symbol_map"
      {"paang1", 3724},
      {""}, {""},
#line 177 "zhy_symbol_map"
      {"bai2", 169},
#line 1962 "zhy_symbol_map"
      {"hung2", 1954},
      {""},
#line 806 "zhy_symbol_map"
      {"dek1", 798},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1965 "zhy_symbol_map"
      {"hung5", 1957},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1961 "zhy_symbol_map"
      {"hung1", 1953},
      {""}, {""},
#line 1418 "zhy_symbol_map"
      {"goei4", 1410},
#line 176 "zhy_symbol_map"
      {"bai1", 168},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1439 "zhy_symbol_map"
      {"goi4", 1431},
#line 3973 "zhy_symbol_map"
      {"saap4", 3965},
      {""}, {""}, {""}, {""}, {""},
#line 3009 "zhy_symbol_map"
      {"mam6", 3001},
      {""},
#line 3006 "zhy_symbol_map"
      {"mam3", 2998},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3005 "zhy_symbol_map"
      {"mam2", 2997},
#line 2106 "zhy_symbol_map"
      {"jik6", 2098},
      {""},
#line 2103 "zhy_symbol_map"
      {"jik3", 2095},
      {""}, {""}, {""},
#line 1007 "zhy_symbol_map"
      {"eot6", 999},
      {""},
#line 1004 "zhy_symbol_map"
      {"eot3", 996},
      {""}, {""}, {""},
#line 2102 "zhy_symbol_map"
      {"jik2", 2094},
      {""}, {""}, {""}, {""}, {""},
#line 1003 "zhy_symbol_map"
      {"eot2", 995},
      {""}, {""}, {""}, {""}, {""},
#line 3004 "zhy_symbol_map"
      {"mam1", 2996},
      {""}, {""}, {""}, {""}, {""},
#line 812 "zhy_symbol_map"
      {"dek7", 804},
      {""}, {""}, {""}, {""},
#line 991 "zhy_symbol_map"
      {"e4", 983},
      {""},
#line 2101 "zhy_symbol_map"
      {"jik1", 2093},
      {""}, {""},
#line 3455 "zhy_symbol_map"
      {"ngi4", 3447},
#line 819 "zhy_symbol_map"
      {"deng7", 811},
      {""},
#line 1002 "zhy_symbol_map"
      {"eot1", 994},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 818 "zhy_symbol_map"
      {"deng6", 810},
#line 182 "zhy_symbol_map"
      {"bai7", 174},
      {""}, {""}, {""}, {""}, {""},
#line 2023 "zhy_symbol_map"
      {"jaau7", 2015},
      {""}, {""},
#line 815 "zhy_symbol_map"
      {"deng3", 807},
      {""}, {""}, {""},
#line 1987 "zhy_symbol_map"
      {"jaa6", 1979},
      {""},
#line 1984 "zhy_symbol_map"
      {"jaa3", 1976},
#line 2022 "zhy_symbol_map"
      {"jaau6", 2014},
      {""}, {""},
#line 814 "zhy_symbol_map"
      {"deng2", 806},
      {""}, {""}, {""}, {""}, {""},
#line 1983 "zhy_symbol_map"
      {"jaa2", 1975},
#line 2019 "zhy_symbol_map"
      {"jaau3", 2011},
      {""}, {""},
#line 817 "zhy_symbol_map"
      {"deng5", 809},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2018 "zhy_symbol_map"
      {"jaau2", 2010},
      {""}, {""},
#line 813 "zhy_symbol_map"
      {"deng1", 805},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2021 "zhy_symbol_map"
      {"jaau5", 2013},
      {""}, {""}, {""},
#line 1982 "zhy_symbol_map"
      {"jaa1", 1974},
      {""}, {""},
#line 18 "zhy_symbol_map"
      {"aai4", 10},
      {""}, {""},
#line 2017 "zhy_symbol_map"
      {"jaau1", 2009},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2002 "zhy_symbol_map"
      {"jaak7", 1994},
      {""},
#line 2120 "zhy_symbol_map"
      {"jin6", 2112},
      {""},
#line 2117 "zhy_symbol_map"
      {"jin3", 2109},
      {""}, {""},
#line 2043 "zhy_symbol_map"
      {"jan6", 2035},
      {""},
#line 2040 "zhy_symbol_map"
      {"jan3", 2032},
#line 2001 "zhy_symbol_map"
      {"jaak6", 1993},
      {""},
#line 928 "zhy_symbol_map"
      {"doeng4", 920},
      {""},
#line 2116 "zhy_symbol_map"
      {"jin2", 2108},
      {""}, {""}, {""}, {""},
#line 2039 "zhy_symbol_map"
      {"jan2", 2031},
#line 1998 "zhy_symbol_map"
      {"jaak3", 1990},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1997 "zhy_symbol_map"
      {"jaak2", 1989},
      {""}, {""}, {""}, {""}, {""},
#line 3010 "zhy_symbol_map"
      {"mam7", 3002},
      {""}, {""},
#line 2115 "zhy_symbol_map"
      {"jin1", 2107},
#line 2000 "zhy_symbol_map"
      {"jaak5", 1992},
      {""}, {""},
#line 4274 "zhy_symbol_map"
      {"taan4", 4266},
#line 2038 "zhy_symbol_map"
      {"jan1", 2030},
      {""}, {""}, {""}, {""},
#line 2107 "zhy_symbol_map"
      {"jik7", 2099},
#line 1996 "zhy_symbol_map"
      {"jaak1", 1988},
      {""}, {""}, {""}, {""},
#line 1008 "zhy_symbol_map"
      {"eot7", 1000},
      {""},
#line 4828 "zhy_symbol_map"
      {"zek5", 4820},
      {""}, {""},
#line 2128 "zhy_symbol_map"
      {"jing7", 2120},
      {""},
#line 4449 "zhy_symbol_map"
      {"toe4", 4441},
      {""}, {""}, {""}, {""},
#line 990 "zhy_symbol_map"
      {"e3", 982},
      {""}, {""},
#line 2127 "zhy_symbol_map"
      {"jing6", 2119},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 999 "zhy_symbol_map"
      {"ei5", 991},
      {""},
#line 2124 "zhy_symbol_map"
      {"jing3", 2116},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2123 "zhy_symbol_map"
      {"jing2", 2115},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2126 "zhy_symbol_map"
      {"jing5", 2118},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2122 "zhy_symbol_map"
      {"jing1", 2114},
      {""}, {""}, {""},
#line 1988 "zhy_symbol_map"
      {"jaa7", 1980},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2121 "zhy_symbol_map"
      {"jin7", 2113},
      {""}, {""}, {""}, {""},
#line 2044 "zhy_symbol_map"
      {"jan7", 2036},
      {""}, {""}, {""},
#line 4260 "zhy_symbol_map"
      {"taai4", 4252},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2657 "zhy_symbol_map"
      {"laau4", 2649},
      {""}, {""},
#line 565 "zhy_symbol_map"
      {"cit5", 557},
      {""},
#line 2622 "zhy_symbol_map"
      {"laam4", 2614},
      {""}, {""},
#line 467 "zhy_symbol_map"
      {"cat5", 459},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3757 "zhy_symbol_map"
      {"pai5", 3749},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2393 "zhy_symbol_map"
      {"kip6", 2385},
      {""},
#line 2390 "zhy_symbol_map"
      {"kip3", 2382},
      {""}, {""},
#line 2302 "zhy_symbol_map"
      {"kap6", 2294},
      {""},
#line 2299 "zhy_symbol_map"
      {"kap3", 2291},
      {""}, {""}, {""}, {""},
#line 2389 "zhy_symbol_map"
      {"kip2", 2381},
      {""}, {""}, {""}, {""},
#line 2298 "zhy_symbol_map"
      {"kap2", 2290},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2783 "zhy_symbol_map"
      {"lik4", 2775},
#line 2615 "zhy_symbol_map"
      {"laak4", 2607},
      {""}, {""}, {""},
#line 2671 "zhy_symbol_map"
      {"lak4", 2663},
      {""},
#line 2388 "zhy_symbol_map"
      {"kip1", 2380},
      {""}, {""}, {""}, {""},
#line 2297 "zhy_symbol_map"
      {"kap1", 2289},
      {""},
#line 289 "zhy_symbol_map"
      {"bo2", 281},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2728 "zhy_symbol_map"
      {"lei5", 2720},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3680 "zhy_symbol_map"
      {"om5", 3672},
      {""}, {""}, {""}, {""},
#line 4673 "zhy_symbol_map"
      {"wu4", 4665},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2254 "zhy_symbol_map"
      {"kaat7", 2246},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2804 "zhy_symbol_map"
      {"ling4", 2796},
      {""},
#line 2253 "zhy_symbol_map"
      {"kaat6", 2245},
      {""},
#line 2601 "zhy_symbol_map"
      {"laa4", 2593},
#line 2692 "zhy_symbol_map"
      {"lang4", 2684},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2250 "zhy_symbol_map"
      {"kaat3", 2242},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1347 "zhy_symbol_map"
      {"gi3", 1339},
      {""},
#line 2249 "zhy_symbol_map"
      {"kaat2", 2241},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2252 "zhy_symbol_map"
      {"kaat5", 2244},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2248 "zhy_symbol_map"
      {"kaat1", 2240},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 294 "zhy_symbol_map"
      {"bo7", 286},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2797 "zhy_symbol_map"
      {"lin4", 2789},
      {""}, {""}, {""}, {""},
#line 2685 "zhy_symbol_map"
      {"lan4", 2677},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2394 "zhy_symbol_map"
      {"kip7", 2386},
      {""}, {""},
#line 2881 "zhy_symbol_map"
      {"lok4", 2873},
      {""},
#line 2303 "zhy_symbol_map"
      {"kap7", 2295},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2163 "zhy_symbol_map"
      {"joek7", 2155},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2162 "zhy_symbol_map"
      {"joek6", 2154},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2159 "zhy_symbol_map"
      {"joek3", 2151},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2158 "zhy_symbol_map"
      {"joek2", 2150},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2888 "zhy_symbol_map"
      {"long4", 2880},
#line 3541 "zhy_symbol_map"
      {"nim6", 3533},
#line 2161 "zhy_symbol_map"
      {"joek5", 2153},
#line 3538 "zhy_symbol_map"
      {"nim3", 3530},
      {""}, {""},
#line 3247 "zhy_symbol_map"
      {"nam6", 3239},
      {""},
#line 3244 "zhy_symbol_map"
      {"nam3", 3236},
      {""}, {""}, {""},
#line 2157 "zhy_symbol_map"
      {"joek1", 2149},
#line 3537 "zhy_symbol_map"
      {"nim2", 3529},
      {""}, {""}, {""}, {""},
#line 3243 "zhy_symbol_map"
      {"nam2", 3235},
      {""},
#line 1403 "zhy_symbol_map"
      {"go3", 1395},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3536 "zhy_symbol_map"
      {"nim1", 3528},
      {""}, {""}, {""}, {""},
#line 3242 "zhy_symbol_map"
      {"nam1", 3234},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 894 "zhy_symbol_map"
      {"dit5", 886},
      {""}, {""}, {""}, {""},
#line 782 "zhy_symbol_map"
      {"dat5", 774},
      {""},
#line 288 "zhy_symbol_map"
      {"bo1", 280},
      {""}, {""}, {""}, {""}, {""},
#line 4309 "zhy_symbol_map"
      {"tai4", 4301},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 293 "zhy_symbol_map"
      {"bo6", 285},
      {""}, {""}, {""}, {""},
#line 3321 "zhy_symbol_map"
      {"ng3", 3313},
      {""}, {""}, {""},
#line 4136 "zhy_symbol_map"
      {"sip6", 4128},
      {""},
#line 4133 "zhy_symbol_map"
      {"sip3", 4125},
      {""}, {""},
#line 4031 "zhy_symbol_map"
      {"sap6", 4023},
      {""},
#line 4028 "zhy_symbol_map"
      {"sap3", 4020},
      {""}, {""}, {""}, {""},
#line 4132 "zhy_symbol_map"
      {"sip2", 4124},
      {""}, {""}, {""}, {""},
#line 4027 "zhy_symbol_map"
      {"sap2", 4019},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4131 "zhy_symbol_map"
      {"sip1", 4123},
      {""}, {""}, {""}, {""},
#line 4026 "zhy_symbol_map"
      {"sap1", 4018},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 73 "zhy_symbol_map"
      {"ai3", 65},
      {""},
#line 1995 "zhy_symbol_map"
      {"jaai7", 1987},
      {""}, {""},
#line 10 "zhy_symbol_map"
      {"aa3", 2},
      {""}, {""}, {""}, {""},
#line 3670 "zhy_symbol_map"
      {"ok2", 3662},
      {""},
#line 1994 "zhy_symbol_map"
      {"jaai6", 1986},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1991 "zhy_symbol_map"
      {"jaai3", 1983},
#line 3336 "zhy_symbol_map"
      {"ngaai4", 3328},
      {""}, {""}, {""},
#line 3983 "zhy_symbol_map"
      {"saat7", 3975},
#line 3542 "zhy_symbol_map"
      {"nim7", 3534},
      {""}, {""}, {""},
#line 1990 "zhy_symbol_map"
      {"jaai2", 1982},
#line 3248 "zhy_symbol_map"
      {"nam7", 3240},
      {""}, {""}, {""},
#line 3982 "zhy_symbol_map"
      {"saat6", 3974},
      {""}, {""}, {""}, {""},
#line 1993 "zhy_symbol_map"
      {"jaai5", 1985},
      {""}, {""}, {""}, {""},
#line 3979 "zhy_symbol_map"
      {"saat3", 3971},
      {""}, {""}, {""}, {""},
#line 1989 "zhy_symbol_map"
      {"jaai1", 1981},
      {""}, {""}, {""}, {""},
#line 3978 "zhy_symbol_map"
      {"saat2", 3970},
      {""},
#line 4456 "zhy_symbol_map"
      {"toi4", 4448},
#line 115 "zhy_symbol_map"
      {"at3", 107},
      {""}, {""}, {""}, {""},
#line 108 "zhy_symbol_map"
      {"ap3", 100},
      {""},
#line 3981 "zhy_symbol_map"
      {"saat5", 3973},
#line 3343 "zhy_symbol_map"
      {"ngaak4", 3335},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3977 "zhy_symbol_map"
      {"saat1", 3969},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3675 "zhy_symbol_map"
      {"ok7", 3667},
      {""}, {""}, {""},
#line 4137 "zhy_symbol_map"
      {"sip7", 4129},
      {""}, {""}, {""},
#line 2853 "zhy_symbol_map"
      {"loek4", 2845},
#line 4032 "zhy_symbol_map"
      {"sap7", 4024},
      {""}, {""}, {""}, {""}, {""},
#line 804 "zhy_symbol_map"
      {"dei6", 796},
      {""},
#line 801 "zhy_symbol_map"
      {"dei3", 793},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 800 "zhy_symbol_map"
      {"dei2", 792},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 799 "zhy_symbol_map"
      {"dei1", 791},
#line 2629 "zhy_symbol_map"
      {"laan4", 2621},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3210 "zhy_symbol_map"
      {"naap4", 3202},
      {""}, {""}, {""},
#line 2839 "zhy_symbol_map"
      {"loe4", 2831},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 522 "zhy_symbol_map"
      {"ci4", 514},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 478 "zhy_symbol_map"
      {"ce2", 470},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1517 "zhy_symbol_map"
      {"gut5", 1509},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3669 "zhy_symbol_map"
      {"ok1", 3661},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2029 "zhy_symbol_map"
      {"jai6", 2021},
      {""},
#line 2026 "zhy_symbol_map"
      {"jai3", 2018},
      {""}, {""}, {""}, {""}, {""},
#line 3674 "zhy_symbol_map"
      {"ok6", 3666},
      {""}, {""}, {""},
#line 2025 "zhy_symbol_map"
      {"jai2", 2017},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 805 "zhy_symbol_map"
      {"dei7", 797},
      {""}, {""}, {""}, {""}, {""},
#line 2608 "zhy_symbol_map"
      {"laai4", 2600},
      {""}, {""}, {""}, {""}, {""},
#line 2024 "zhy_symbol_map"
      {"jai1", 2016},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 483 "zhy_symbol_map"
      {"ce7", 475},
      {""}, {""}, {""},
#line 1875 "zhy_symbol_map"
      {"hiu6", 1867},
      {""},
#line 1872 "zhy_symbol_map"
      {"hiu3", 1864},
      {""}, {""},
#line 1791 "zhy_symbol_map"
      {"hau6", 1783},
      {""},
#line 1788 "zhy_symbol_map"
      {"hau3", 1780},
      {""}, {""}, {""}, {""},
#line 1871 "zhy_symbol_map"
      {"hiu2", 1863},
      {""},
#line 578 "zhy_symbol_map"
      {"co4", 570},
      {""}, {""},
#line 1787 "zhy_symbol_map"
      {"hau2", 1779},
      {""}, {""}, {""}, {""}, {""},
#line 2982 "zhy_symbol_map"
      {"maat7", 2974},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4757 "zhy_symbol_map"
      {"zaau4", 4749},
      {""},
#line 2981 "zhy_symbol_map"
      {"maat6", 2973},
      {""}, {""},
#line 4722 "zhy_symbol_map"
      {"zaam4", 4714},
#line 1870 "zhy_symbol_map"
      {"hiu1", 1862},
      {""}, {""},
#line 257 "zhy_symbol_map"
      {"bik5", 249},
      {""},
#line 1786 "zhy_symbol_map"
      {"hau1", 1778},
#line 2978 "zhy_symbol_map"
      {"maat3", 2970},
      {""},
#line 187 "zhy_symbol_map"
      {"bak5", 179},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2977 "zhy_symbol_map"
      {"maat2", 2969},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2980 "zhy_symbol_map"
      {"maat5", 2972},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2976 "zhy_symbol_map"
      {"maat1", 2968},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4869 "zhy_symbol_map"
      {"zik4", 4861},
#line 4715 "zhy_symbol_map"
      {"zaak4", 4707},
      {""}, {""}, {""},
#line 4771 "zhy_symbol_map"
      {"zak4", 4763},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 131 "zhy_symbol_map"
      {"baa5", 123},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2030 "zhy_symbol_map"
      {"jai7", 2022},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1952 "zhy_symbol_map"
      {"hou6", 1944},
      {""},
#line 1949 "zhy_symbol_map"
      {"hou3", 1941},
      {""},
#line 477 "zhy_symbol_map"
      {"ce1", 469},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4890 "zhy_symbol_map"
      {"zing4", 4882},
#line 1948 "zhy_symbol_map"
      {"hou2", 1940},
      {""}, {""},
#line 4701 "zhy_symbol_map"
      {"zaa4", 4693},
#line 4792 "zhy_symbol_map"
      {"zang4", 4784},
      {""}, {""}, {""},
#line 264 "zhy_symbol_map"
      {"bin5", 256},
#line 482 "zhy_symbol_map"
      {"ce6", 474},
      {""},
#line 4186 "zhy_symbol_map"
      {"soet7", 4178},
      {""},
#line 201 "zhy_symbol_map"
      {"ban5", 193},
      {""},
#line 1876 "zhy_symbol_map"
      {"hiu7", 1868},
      {""}, {""}, {""}, {""},
#line 1792 "zhy_symbol_map"
      {"hau7", 1784},
#line 4185 "zhy_symbol_map"
      {"soet6", 4177},
      {""}, {""}, {""},
#line 1947 "zhy_symbol_map"
      {"hou1", 1939},
      {""}, {""},
#line 299 "zhy_symbol_map"
      {"bok5", 291},
      {""},
#line 851 "zhy_symbol_map"
      {"di4", 843},
#line 4182 "zhy_symbol_map"
      {"soet3", 4174},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4181 "zhy_symbol_map"
      {"soet2", 4173},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4184 "zhy_symbol_map"
      {"soet5", 4176},
      {""}, {""}, {""},
#line 793 "zhy_symbol_map"
      {"de2", 785},
      {""}, {""},
#line 4883 "zhy_symbol_map"
      {"zin4", 4875},
      {""}, {""},
#line 4180 "zhy_symbol_map"
      {"soet1", 4172},
      {""},
#line 4785 "zhy_symbol_map"
      {"zan4", 4777},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4960 "zhy_symbol_map"
      {"zok4", 4952},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4385 "zhy_symbol_map"
      {"ti3", 4377},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2475 "zhy_symbol_map"
      {"kuk4", 2467},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4967 "zhy_symbol_map"
      {"zong4", 4959},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 798 "zhy_symbol_map"
      {"de7", 790},
      {""}, {""},
#line 1953 "zhy_symbol_map"
      {"hou7", 1945},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 907 "zhy_symbol_map"
      {"do4", 899},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2482 "zhy_symbol_map"
      {"kung4", 2474},
      {""},
#line 4564 "zhy_symbol_map"
      {"waat7", 4556},
      {""}, {""},
#line 3659 "zhy_symbol_map"
      {"oe5", 3651},
      {""}, {""},
#line 2095 "zhy_symbol_map"
      {"ji2", 2087},
      {""},
#line 2322 "zhy_symbol_map"
      {"ke5", 2314},
      {""},
#line 4563 "zhy_symbol_map"
      {"waat6", 4555},
      {""}, {""}, {""},
#line 1364 "zhy_symbol_map"
      {"gim6", 1356},
      {""},
#line 1361 "zhy_symbol_map"
      {"gim3", 1353},
#line 2664 "zhy_symbol_map"
      {"lai4", 2656},
      {""},
#line 1273 "zhy_symbol_map"
      {"gam6", 1265},
#line 4560 "zhy_symbol_map"
      {"waat3", 4552},
#line 1270 "zhy_symbol_map"
      {"gam3", 1262},
      {""}, {""}, {""}, {""},
#line 1360 "zhy_symbol_map"
      {"gim2", 1352},
      {""}, {""}, {""},
#line 4559 "zhy_symbol_map"
      {"waat2", 4551},
#line 1269 "zhy_symbol_map"
      {"gam2", 1261},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4562 "zhy_symbol_map"
      {"waat5", 4554},
#line 3350 "zhy_symbol_map"
      {"ngaam4", 3342},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4558 "zhy_symbol_map"
      {"waat1", 4550},
#line 1359 "zhy_symbol_map"
      {"gim1", 1351},
      {""},
#line 4441 "zhy_symbol_map"
      {"to3", 4433},
      {""}, {""},
#line 1268 "zhy_symbol_map"
      {"gam1", 1260},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2846 "zhy_symbol_map"
      {"loei4", 2838},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2100 "zhy_symbol_map"
      {"ji7", 2092},
      {""}, {""}, {""}, {""}, {""},
#line 3518 "zhy_symbol_map"
      {"ngung4", 3510},
      {""}, {""}, {""}, {""},
#line 792 "zhy_symbol_map"
      {"de1", 784},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 797 "zhy_symbol_map"
      {"de6", 789},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2151 "zhy_symbol_map"
      {"jo2", 2143},
      {""}, {""}, {""}, {""}, {""},
#line 2874 "zhy_symbol_map"
      {"loi4", 2866},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1481 "zhy_symbol_map"
      {"gu4", 1473},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1365 "zhy_symbol_map"
      {"gim7", 1357},
      {""}, {""},
#line 2371 "zhy_symbol_map"
      {"kim5", 2363},
      {""},
#line 1274 "zhy_symbol_map"
      {"gam7", 1266},
      {""}, {""},
#line 2280 "zhy_symbol_map"
      {"kam5", 2272},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4939 "zhy_symbol_map"
      {"zoek4", 4931},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 161 "zhy_symbol_map"
      {"baang7", 153},
      {""}, {""}, {""}, {""},
#line 160 "zhy_symbol_map"
      {"baang6", 152},
      {""}, {""}, {""}, {""},
#line 157 "zhy_symbol_map"
      {"baang3", 149},
      {""}, {""}, {""}, {""},
#line 156 "zhy_symbol_map"
      {"baang2", 148},
#line 3562 "zhy_symbol_map"
      {"nip6", 3554},
      {""},
#line 3559 "zhy_symbol_map"
      {"nip3", 3551},
      {""},
#line 159 "zhy_symbol_map"
      {"baang5", 151},
#line 3268 "zhy_symbol_map"
      {"nap6", 3260},
      {""},
#line 3265 "zhy_symbol_map"
      {"nap3", 3257},
      {""},
#line 155 "zhy_symbol_map"
      {"baang1", 147},
      {""},
#line 2156 "zhy_symbol_map"
      {"jo7", 2148},
#line 3558 "zhy_symbol_map"
      {"nip2", 3550},
      {""}, {""}, {""}, {""},
#line 3264 "zhy_symbol_map"
      {"nap2", 3256},
      {""},
#line 2094 "zhy_symbol_map"
      {"ji1", 2086},
#line 4218 "zhy_symbol_map"
      {"suk4", 4210},
#line 4729 "zhy_symbol_map"
      {"zaan4", 4721},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2099 "zhy_symbol_map"
      {"ji6", 2091},
#line 3557 "zhy_symbol_map"
      {"nip1", 3549},
      {""}, {""},
#line 4925 "zhy_symbol_map"
      {"zoe4", 4917},
      {""},
#line 3263 "zhy_symbol_map"
      {"nap1", 3255},
      {""}, {""}, {""}, {""},
#line 34 "zhy_symbol_map"
      {"aam6", 26},
      {""},
#line 31 "zhy_symbol_map"
      {"aam3", 23},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 30 "zhy_symbol_map"
      {"aam2", 22},
#line 3905 "zhy_symbol_map"
      {"puk6", 3897},
      {""},
#line 3902 "zhy_symbol_map"
      {"puk3", 3894},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3901 "zhy_symbol_map"
      {"puk2", 3893},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4225 "zhy_symbol_map"
      {"sung4", 4217},
      {""},
#line 3220 "zhy_symbol_map"
      {"naat7", 3212},
#line 29 "zhy_symbol_map"
      {"aam1", 21},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4051 "zhy_symbol_map"
      {"se5", 4043},
      {""},
#line 3219 "zhy_symbol_map"
      {"naat6", 3211},
      {""}, {""},
#line 1236 "zhy_symbol_map"
      {"gaap4", 1228},
#line 3900 "zhy_symbol_map"
      {"puk1", 3892},
      {""}, {""}, {""}, {""}, {""},
#line 3216 "zhy_symbol_map"
      {"naat3", 3208},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3215 "zhy_symbol_map"
      {"naat2", 3207},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3218 "zhy_symbol_map"
      {"naat5", 3210},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3214 "zhy_symbol_map"
      {"naat1", 3206},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3684 "zhy_symbol_map"
      {"on2", 3676},
      {""}, {""}, {""}, {""}, {""},
#line 488 "zhy_symbol_map"
      {"cek5", 480},
#line 4708 "zhy_symbol_map"
      {"zaai4", 4700},
      {""}, {""},
#line 2150 "zhy_symbol_map"
      {"jo1", 2142},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3563 "zhy_symbol_map"
      {"nip7", 3555},
      {""}, {""}, {""}, {""},
#line 3269 "zhy_symbol_map"
      {"nap7", 3261},
      {""}, {""}, {""},
#line 2155 "zhy_symbol_map"
      {"jo6", 2147},
      {""}, {""}, {""},
#line 3912 "zhy_symbol_map"
      {"pun6", 3904},
      {""},
#line 3909 "zhy_symbol_map"
      {"pun3", 3901},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3908 "zhy_symbol_map"
      {"pun2", 3900},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 123 "zhy_symbol_map"
      {"au4", 115},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 35 "zhy_symbol_map"
      {"aam7", 27},
      {""}, {""},
#line 3907 "zhy_symbol_map"
      {"pun1", 3899},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3906 "zhy_symbol_map"
      {"puk7", 3898},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3920 "zhy_symbol_map"
      {"pung7", 3912},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3689 "zhy_symbol_map"
      {"on7", 3681},
      {""},
#line 3919 "zhy_symbol_map"
      {"pung6", 3911},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3916 "zhy_symbol_map"
      {"pung3", 3908},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4114 "zhy_symbol_map"
      {"sim5", 4106},
#line 3915 "zhy_symbol_map"
      {"pung2", 3907},
      {""}, {""}, {""},
#line 4009 "zhy_symbol_map"
      {"sam5", 4001},
      {""}, {""}, {""}, {""}, {""},
#line 3918 "zhy_symbol_map"
      {"pung5", 3910},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3914 "zhy_symbol_map"
      {"pung1", 3906},
      {""},
#line 1958 "zhy_symbol_map"
      {"huk5", 1950},
#line 3427 "zhy_symbol_map"
      {"ngap4", 3419},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3147 "zhy_symbol_map"
      {"muk4", 3139},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3913 "zhy_symbol_map"
      {"pun7", 3905},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4626 "zhy_symbol_map"
      {"wet6", 4618},
      {""},
#line 4623 "zhy_symbol_map"
      {"wet3", 4615},
#line 3683 "zhy_symbol_map"
      {"on1", 3675},
      {""},
#line 3161 "zhy_symbol_map"
      {"mung4", 3153},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4622 "zhy_symbol_map"
      {"wet2", 4614},
      {""}, {""},
#line 3043 "zhy_symbol_map"
      {"me5", 3035},
      {""}, {""}, {""}, {""},
#line 3688 "zhy_symbol_map"
      {"on6", 3680},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4621 "zhy_symbol_map"
      {"wet1", 4613},
      {""}, {""},
#line 2775 "zhy_symbol_map"
      {"li3", 2767},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3357 "zhy_symbol_map"
      {"ngaan4", 3349},
#line 3364 "zhy_symbol_map"
      {"ngaang4", 3356},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3154 "zhy_symbol_map"
      {"mun4", 3146},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 810 "zhy_symbol_map"
      {"dek5", 802},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 180 "zhy_symbol_map"
      {"bai5", 172},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4764 "zhy_symbol_map"
      {"zai4", 4756},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4627 "zhy_symbol_map"
      {"wet7", 4619},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2831 "zhy_symbol_map"
      {"lo3", 2823},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3008 "zhy_symbol_map"
      {"mam5", 3000},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2105 "zhy_symbol_map"
      {"jik5", 2097},
      {""}, {""}, {""}, {""}, {""},
#line 1006 "zhy_symbol_map"
      {"eot5", 998},
      {""}, {""}, {""}, {""},
#line 4932 "zhy_symbol_map"
      {"zoei4", 4924},
#line 4402 "zhy_symbol_map"
      {"tim6", 4394},
#line 2345 "zhy_symbol_map"
      {"keoi7", 2337},
#line 4399 "zhy_symbol_map"
      {"tim3", 4391},
      {""}, {""},
#line 4318 "zhy_symbol_map"
      {"tam6", 4310},
      {""},
#line 4315 "zhy_symbol_map"
      {"tam3", 4307},
      {""}, {""}, {""},
#line 2344 "zhy_symbol_map"
      {"keoi6", 2336},
#line 4398 "zhy_symbol_map"
      {"tim2", 4390},
      {""}, {""}, {""}, {""},
#line 4314 "zhy_symbol_map"
      {"tam2", 4306},
      {""}, {""}, {""},
#line 2341 "zhy_symbol_map"
      {"keoi3", 2333},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2340 "zhy_symbol_map"
      {"keoi2", 2332},
      {""}, {""}, {""}, {""}, {""},
#line 4397 "zhy_symbol_map"
      {"tim1", 4389},
      {""}, {""}, {""},
#line 2343 "zhy_symbol_map"
      {"keoi5", 2335},
#line 4313 "zhy_symbol_map"
      {"tam1", 4305},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2339 "zhy_symbol_map"
      {"keoi1", 2331},
      {""},
#line 1986 "zhy_symbol_map"
      {"jaa5", 1978},
      {""}, {""}, {""}, {""},
#line 4953 "zhy_symbol_map"
      {"zoi4", 4945},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2468 "zhy_symbol_map"
      {"kui4", 2460},
      {""}, {""}, {""}, {""},
#line 2119 "zhy_symbol_map"
      {"jin5", 2111},
#line 4611 "zhy_symbol_map"
      {"we5", 4603},
      {""}, {""}, {""},
#line 2042 "zhy_symbol_map"
      {"jan5", 2034},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4687 "zhy_symbol_map"
      {"wun4", 4679},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4403 "zhy_symbol_map"
      {"tim7", 4395},
      {""}, {""}, {""}, {""},
#line 4319 "zhy_symbol_map"
      {"tam7", 4311},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4088 "zhy_symbol_map"
      {"seon7", 4080},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4087 "zhy_symbol_map"
      {"seon6", 4079},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1385 "zhy_symbol_map"
      {"gip6", 1377},
      {""},
#line 1382 "zhy_symbol_map"
      {"gip3", 1374},
#line 4084 "zhy_symbol_map"
      {"seon3", 4076},
      {""},
#line 1294 "zhy_symbol_map"
      {"gap6", 1286},
      {""},
#line 1291 "zhy_symbol_map"
      {"gap3", 1283},
      {""}, {""}, {""}, {""},
#line 1381 "zhy_symbol_map"
      {"gip2", 1373},
#line 4083 "zhy_symbol_map"
      {"seon2", 4075},
      {""}, {""}, {""},
#line 1290 "zhy_symbol_map"
      {"gap2", 1282},
      {""}, {""}, {""}, {""}, {""},
#line 4086 "zhy_symbol_map"
      {"seon5", 4078},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4082 "zhy_symbol_map"
      {"seon1", 4074},
      {""}, {""}, {""},
#line 1380 "zhy_symbol_map"
      {"gip1", 1372},
      {""}, {""}, {""}, {""},
#line 1289 "zhy_symbol_map"
      {"gap1", 1281},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1246 "zhy_symbol_map"
      {"gaat7", 1238},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1245 "zhy_symbol_map"
      {"gaat6", 1237},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1242 "zhy_symbol_map"
      {"gaat3", 1234},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1241 "zhy_symbol_map"
      {"gaat2", 1233},
      {""}, {""},
#line 4288 "zhy_symbol_map"
      {"taap4", 4280},
      {""},
#line 4081 "zhy_symbol_map"
      {"seoi7", 4073},
      {""}, {""}, {""}, {""},
#line 1244 "zhy_symbol_map"
      {"gaat5", 1236},
      {""}, {""}, {""}, {""},
#line 4080 "zhy_symbol_map"
      {"seoi6", 4072},
      {""}, {""}, {""}, {""},
#line 1240 "zhy_symbol_map"
      {"gaat1", 1232},
      {""}, {""}, {""}, {""},
#line 4077 "zhy_symbol_map"
      {"seoi3", 4069},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4076 "zhy_symbol_map"
      {"seoi2", 4068},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3630 "zhy_symbol_map"
      {"nuk4", 3622},
      {""}, {""},
#line 4079 "zhy_symbol_map"
      {"seoi5", 4071},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1386 "zhy_symbol_map"
      {"gip7", 1378},
#line 4075 "zhy_symbol_map"
      {"seoi1", 4067},
      {""},
#line 2392 "zhy_symbol_map"
      {"kip5", 2384},
      {""},
#line 1295 "zhy_symbol_map"
      {"gap7", 1287},
      {""}, {""},
#line 2301 "zhy_symbol_map"
      {"kap5", 2293},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1868 "zhy_symbol_map"
      {"hit6", 1860},
      {""},
#line 1865 "zhy_symbol_map"
      {"hit3", 1857},
      {""}, {""},
#line 1784 "zhy_symbol_map"
      {"hat6", 1776},
      {""},
#line 1781 "zhy_symbol_map"
      {"hat3", 1773},
      {""}, {""},
#line 3637 "zhy_symbol_map"
      {"nung4", 3629},
      {""},
#line 1864 "zhy_symbol_map"
      {"hit2", 1856},
      {""}, {""}, {""}, {""},
#line 1780 "zhy_symbol_map"
      {"hat2", 1772},
      {""}, {""},
#line 3288 "zhy_symbol_map"
      {"ne5", 3280},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1863 "zhy_symbol_map"
      {"hit1", 1855},
      {""}, {""},
#line 998 "zhy_symbol_map"
      {"ei4", 990},
      {""},
#line 1779 "zhy_symbol_map"
      {"hat1", 1771},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3898 "zhy_symbol_map"
      {"pui6", 3890},
      {""},
#line 3895 "zhy_symbol_map"
      {"pui3", 3887},
      {""}, {""},
#line 55 "zhy_symbol_map"
      {"aap6", 47},
      {""},
#line 52 "zhy_symbol_map"
      {"aap3", 44},
      {""}, {""}, {""},
#line 417 "zhy_symbol_map"
      {"caau4", 409},
#line 3894 "zhy_symbol_map"
      {"pui2", 3886},
      {""}, {""},
#line 2405 "zhy_symbol_map"
      {"kiu4", 2397},
#line 382 "zhy_symbol_map"
      {"caam4", 374},
#line 51 "zhy_symbol_map"
      {"aap2", 43},
#line 3437 "zhy_symbol_map"
      {"ngat7", 3429},
      {""},
#line 2314 "zhy_symbol_map"
      {"kau4", 2306},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3436 "zhy_symbol_map"
      {"ngat6", 3428},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3893 "zhy_symbol_map"
      {"pui1", 3885},
#line 3433 "zhy_symbol_map"
      {"ngat3", 3425},
      {""}, {""}, {""},
#line 50 "zhy_symbol_map"
      {"aap1", 42},
      {""},
#line 2009 "zhy_symbol_map"
      {"jaang7", 2001},
      {""}, {""}, {""},
#line 3432 "zhy_symbol_map"
      {"ngat2", 3424},
#line 2008 "zhy_symbol_map"
      {"jaang6", 2000},
      {""}, {""}, {""}, {""},
#line 2005 "zhy_symbol_map"
      {"jaang3", 1997},
      {""}, {""}, {""},
#line 3435 "zhy_symbol_map"
      {"ngat5", 3427},
#line 2004 "zhy_symbol_map"
      {"jaang2", 1996},
      {""}, {""}, {""}, {""},
#line 2007 "zhy_symbol_map"
      {"jaang5", 1999},
      {""}, {""}, {""},
#line 3431 "zhy_symbol_map"
      {"ngat1", 3423},
#line 2003 "zhy_symbol_map"
      {"jaang1", 1995},
#line 529 "zhy_symbol_map"
      {"cik4", 521},
#line 375 "zhy_symbol_map"
      {"caak4", 367},
      {""}, {""},
#line 89 "zhy_symbol_map"
      {"am5", 81},
#line 431 "zhy_symbol_map"
      {"cak4", 423},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1945 "zhy_symbol_map"
      {"hot6", 1937},
      {""},
#line 1942 "zhy_symbol_map"
      {"hot3", 1934},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1941 "zhy_symbol_map"
      {"hot2", 1933},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4861 "zhy_symbol_map"
      {"zi3", 4853},
      {""},
#line 1869 "zhy_symbol_map"
      {"hit7", 1861},
      {""}, {""}, {""}, {""},
#line 1785 "zhy_symbol_map"
      {"hat7", 1777},
      {""}, {""}, {""}, {""},
#line 1940 "zhy_symbol_map"
      {"hot1", 1932},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 550 "zhy_symbol_map"
      {"cing4", 542},
      {""}, {""}, {""},
#line 361 "zhy_symbol_map"
      {"caa4", 353},
#line 452 "zhy_symbol_map"
      {"cang4", 444},
      {""}, {""}, {""}, {""}, {""},
#line 3540 "zhy_symbol_map"
      {"nim5", 3532},
      {""}, {""}, {""}, {""},
#line 3246 "zhy_symbol_map"
      {"nam5", 3238},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3899 "zhy_symbol_map"
      {"pui7", 3891},
      {""}, {""}, {""}, {""},
#line 56 "zhy_symbol_map"
      {"aap7", 48},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 543 "zhy_symbol_map"
      {"cin4", 535},
      {""}, {""}, {""}, {""},
#line 445 "zhy_symbol_map"
      {"can4", 437},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 613 "zhy_symbol_map"
      {"cok4", 605},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4135 "zhy_symbol_map"
      {"sip5", 4127},
      {""}, {""}, {""}, {""},
#line 4030 "zhy_symbol_map"
      {"sap5", 4022},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3679 "zhy_symbol_map"
      {"om4", 3671},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4917 "zhy_symbol_map"
      {"zo3", 4909},
      {""},
#line 1946 "zhy_symbol_map"
      {"hot7", 1938},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 620 "zhy_symbol_map"
      {"cong4", 612},
      {""}, {""}, {""},
#line 3140 "zhy_symbol_map"
      {"mui4", 3132},
#line 1832 "zhy_symbol_map"
      {"hi5", 1824},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3699 "zhy_symbol_map"
      {"ou3", 3691},
#line 2792 "zhy_symbol_map"
      {"lim6", 2784},
      {""},
#line 2789 "zhy_symbol_map"
      {"lim3", 2781},
      {""},
#line 2460 "zhy_symbol_map"
      {"ku3", 2452},
#line 2680 "zhy_symbol_map"
      {"lam6", 2672},
      {""},
#line 2677 "zhy_symbol_map"
      {"lam3", 2669},
      {""}, {""}, {""}, {""},
#line 2788 "zhy_symbol_map"
      {"lim2", 2780},
      {""}, {""}, {""}, {""},
#line 2676 "zhy_symbol_map"
      {"lam2", 2668},
      {""}, {""}, {""},
#line 2016 "zhy_symbol_map"
      {"jaap7", 2008},
      {""}, {""}, {""}, {""}, {""},
#line 732 "zhy_symbol_map"
      {"daau4", 724},
      {""}, {""}, {""},
#line 2015 "zhy_symbol_map"
      {"jaap6", 2007},
#line 697 "zhy_symbol_map"
      {"daam4", 689},
      {""}, {""}, {""}, {""},
#line 2787 "zhy_symbol_map"
      {"lim1", 2779},
#line 4148 "zhy_symbol_map"
      {"siu4", 4140},
      {""}, {""},
#line 2012 "zhy_symbol_map"
      {"jaap3", 2004},
#line 2675 "zhy_symbol_map"
      {"lam1", 2667},
#line 4043 "zhy_symbol_map"
      {"sau4", 4035},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2011 "zhy_symbol_map"
      {"jaap2", 2003},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2014 "zhy_symbol_map"
      {"jaap5", 2006},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2010 "zhy_symbol_map"
      {"jaap1", 2002},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3863 "zhy_symbol_map"
      {"piu6", 3855},
      {""},
#line 3860 "zhy_symbol_map"
      {"piu3", 3852},
      {""}, {""},
#line 3793 "zhy_symbol_map"
      {"pau6", 3785},
      {""},
#line 3790 "zhy_symbol_map"
      {"pau3", 3782},
#line 858 "zhy_symbol_map"
      {"dik4", 850},
#line 690 "zhy_symbol_map"
      {"daak4", 682},
      {""}, {""},
#line 3859 "zhy_symbol_map"
      {"piu2", 3851},
#line 746 "zhy_symbol_map"
      {"dak4", 738},
      {""}, {""},
#line 79 "zhy_symbol_map"
      {"ak2", 71},
#line 3789 "zhy_symbol_map"
      {"pau2", 3781},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 803 "zhy_symbol_map"
      {"dei5", 795},
      {""}, {""}, {""},
#line 3858 "zhy_symbol_map"
      {"piu1", 3850},
      {""}, {""}, {""}, {""},
#line 3788 "zhy_symbol_map"
      {"pau1", 3780},
      {""}, {""}, {""},
#line 1895 "zhy_symbol_map"
      {"ho5", 1887},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1770 "zhy_symbol_map"
      {"hao6", 1762},
      {""},
#line 1767 "zhy_symbol_map"
      {"hao3", 1759},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 879 "zhy_symbol_map"
      {"ding4", 871},
      {""}, {""},
#line 1766 "zhy_symbol_map"
      {"hao2", 1758},
#line 676 "zhy_symbol_map"
      {"daa4", 668},
#line 767 "zhy_symbol_map"
      {"dang4", 759},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2793 "zhy_symbol_map"
      {"lim7", 2785},
      {""}, {""}, {""}, {""},
#line 2681 "zhy_symbol_map"
      {"lam7", 2673},
      {""}, {""},
#line 1765 "zhy_symbol_map"
      {"hao1", 1757},
      {""}, {""},
#line 4211 "zhy_symbol_map"
      {"sou4", 4203},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 84 "zhy_symbol_map"
      {"ak7", 76},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 872 "zhy_symbol_map"
      {"din4", 864},
#line 2170 "zhy_symbol_map"
      {"joeng7", 2162},
      {""}, {""}, {""},
#line 760 "zhy_symbol_map"
      {"dan4", 752},
#line 2169 "zhy_symbol_map"
      {"joeng6", 2161},
      {""}, {""}, {""}, {""},
#line 2166 "zhy_symbol_map"
      {"joeng3", 2158},
#line 3891 "zhy_symbol_map"
      {"pou6", 3883},
      {""},
#line 3888 "zhy_symbol_map"
      {"pou3", 3880},
      {""},
#line 2165 "zhy_symbol_map"
      {"joeng2", 2157},
      {""}, {""}, {""},
#line 942 "zhy_symbol_map"
      {"dok4", 934},
#line 2168 "zhy_symbol_map"
      {"joeng5", 2160},
      {""}, {""},
#line 3887 "zhy_symbol_map"
      {"pou2", 3879},
      {""},
#line 2164 "zhy_symbol_map"
      {"joeng1", 2156},
      {""},
#line 592 "zhy_symbol_map"
      {"coek4", 584},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3864 "zhy_symbol_map"
      {"piu7", 3856},
      {""}, {""}, {""}, {""},
#line 3794 "zhy_symbol_map"
      {"pau7", 3786},
      {""}, {""},
#line 2028 "zhy_symbol_map"
      {"jai5", 2020},
      {""},
#line 3886 "zhy_symbol_map"
      {"pou1", 3878},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4423 "zhy_symbol_map"
      {"tip6", 4415},
#line 389 "zhy_symbol_map"
      {"caan4", 381},
#line 4420 "zhy_symbol_map"
      {"tip3", 4412},
      {""}, {""},
#line 4339 "zhy_symbol_map"
      {"tap6", 4331},
      {""},
#line 4336 "zhy_symbol_map"
      {"tap3", 4328},
      {""},
#line 949 "zhy_symbol_map"
      {"dong4", 941},
      {""}, {""},
#line 4419 "zhy_symbol_map"
      {"tip2", 4411},
      {""}, {""}, {""}, {""},
#line 4335 "zhy_symbol_map"
      {"tap2", 4327},
      {""}, {""},
#line 4680 "zhy_symbol_map"
      {"wui4", 4672},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1771 "zhy_symbol_map"
      {"hao7", 1763},
      {""}, {""},
#line 1874 "zhy_symbol_map"
      {"hiu5", 1866},
      {""}, {""}, {""}, {""},
#line 1790 "zhy_symbol_map"
      {"hau5", 1782},
#line 2643 "zhy_symbol_map"
      {"laap4", 2635},
#line 4418 "zhy_symbol_map"
      {"tip1", 4410},
      {""}, {""}, {""}, {""},
#line 4334 "zhy_symbol_map"
      {"tap1", 4326},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 78 "zhy_symbol_map"
      {"ak1", 70},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3098 "zhy_symbol_map"
      {"miu4", 3090},
      {""}, {""},
#line 83 "zhy_symbol_map"
      {"ak6", 75},
      {""},
#line 3035 "zhy_symbol_map"
      {"mau4", 3027},
      {""}, {""},
#line 4298 "zhy_symbol_map"
      {"taat7", 4290},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4297 "zhy_symbol_map"
      {"taat6", 4289},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4294 "zhy_symbol_map"
      {"taat3", 4286},
      {""}, {""}, {""},
#line 3892 "zhy_symbol_map"
      {"pou7", 3884},
      {""}, {""}, {""}, {""}, {""},
#line 4293 "zhy_symbol_map"
      {"taat2", 4285},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4296 "zhy_symbol_map"
      {"taat5", 4288},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4292 "zhy_symbol_map"
      {"taat1", 4284},
      {""},
#line 1495 "zhy_symbol_map"
      {"guk4", 1487},
#line 368 "zhy_symbol_map"
      {"caai4", 360},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4424 "zhy_symbol_map"
      {"tip7", 4416},
      {""}, {""},
#line 1951 "zhy_symbol_map"
      {"hou5", 1943},
      {""},
#line 4340 "zhy_symbol_map"
      {"tap7", 4332},
      {""}, {""},
#line 335 "zhy_symbol_map"
      {"buk6", 327},
      {""},
#line 332 "zhy_symbol_map"
      {"buk3", 324},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 331 "zhy_symbol_map"
      {"buk2", 323},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1509 "zhy_symbol_map"
      {"gung4", 1501},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1314 "zhy_symbol_map"
      {"ge5", 1306},
      {""}, {""}, {""}, {""}, {""},
#line 330 "zhy_symbol_map"
      {"buk1", 322},
      {""}, {""},
#line 3133 "zhy_symbol_map"
      {"mou4", 3125},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3311 "zhy_symbol_map"
      {"neoi7", 3303},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3310 "zhy_symbol_map"
      {"neoi6", 3302},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3307 "zhy_symbol_map"
      {"neoi3", 3299},
      {""},
#line 1502 "zhy_symbol_map"
      {"gun4", 1494},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3306 "zhy_symbol_map"
      {"neoi2", 3298},
#line 921 "zhy_symbol_map"
      {"doek4", 913},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3309 "zhy_symbol_map"
      {"neoi5", 3301},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3305 "zhy_symbol_map"
      {"neoi1", 3297},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 342 "zhy_symbol_map"
      {"bun6", 334},
      {""},
#line 339 "zhy_symbol_map"
      {"bun3", 331},
      {""}, {""}, {""}, {""},
#line 3497 "zhy_symbol_map"
      {"ngong4", 3489},
      {""}, {""}, {""}, {""},
#line 338 "zhy_symbol_map"
      {"bun2", 330},
      {""},
#line 704 "zhy_symbol_map"
      {"daan4", 696},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 914 "zhy_symbol_map"
      {"doe4", 906},
      {""}, {""}, {""},
#line 337 "zhy_symbol_map"
      {"bun1", 329},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 336 "zhy_symbol_map"
      {"buk7", 328},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 350 "zhy_symbol_map"
      {"bung7", 342},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 349 "zhy_symbol_map"
      {"bung6", 341},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 346 "zhy_symbol_map"
      {"bung3", 338},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1363 "zhy_symbol_map"
      {"gim5", 1355},
#line 345 "zhy_symbol_map"
      {"bung2", 337},
      {""}, {""}, {""},
#line 1272 "zhy_symbol_map"
      {"gam5", 1264},
      {""}, {""}, {""}, {""}, {""},
#line 348 "zhy_symbol_map"
      {"bung5", 340},
      {""},
#line 4603 "zhy_symbol_map"
      {"wau4", 4595},
      {""}, {""},
#line 3465 "zhy_symbol_map"
      {"ngit7", 3457},
      {""}, {""}, {""}, {""},
#line 344 "zhy_symbol_map"
      {"bung1", 336},
      {""}, {""}, {""}, {""},
#line 3464 "zhy_symbol_map"
      {"ngit6", 3456},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3388 "zhy_symbol_map"
      {"ngaau7", 3380},
      {""},
#line 3461 "zhy_symbol_map"
      {"ngit3", 3453},
      {""}, {""},
#line 3387 "zhy_symbol_map"
      {"ngaau6", 3379},
      {""}, {""}, {""}, {""},
#line 3384 "zhy_symbol_map"
      {"ngaau3", 3376},
      {""},
#line 3460 "zhy_symbol_map"
      {"ngit2", 3452},
      {""}, {""},
#line 3383 "zhy_symbol_map"
      {"ngaau2", 3375},
      {""}, {""}, {""}, {""},
#line 3386 "zhy_symbol_map"
      {"ngaau5", 3378},
      {""},
#line 3463 "zhy_symbol_map"
      {"ngit5", 3455},
#line 3378 "zhy_symbol_map"
      {"ngaat4", 3370},
      {""},
#line 3382 "zhy_symbol_map"
      {"ngaau1", 3374},
      {""}, {""},
#line 683 "zhy_symbol_map"
      {"daai4", 675},
      {""}, {""}, {""},
#line 3459 "zhy_symbol_map"
      {"ngit1", 3451},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 343 "zhy_symbol_map"
      {"bun7", 335},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4878 "zhy_symbol_map"
      {"zim6", 4870},
      {""},
#line 4875 "zhy_symbol_map"
      {"zim3", 4867},
      {""}, {""},
#line 4780 "zhy_symbol_map"
      {"zam6", 4772},
      {""},
#line 4777 "zhy_symbol_map"
      {"zam3", 4769},
      {""}, {""}, {""}, {""},
#line 4874 "zhy_symbol_map"
      {"zim2", 4866},
      {""}, {""}, {""}, {""},
#line 4776 "zhy_symbol_map"
      {"zam2", 4768},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4873 "zhy_symbol_map"
      {"zim1", 4865},
      {""}, {""}, {""}, {""},
#line 4775 "zhy_symbol_map"
      {"zam1", 4767},
#line 424 "zhy_symbol_map"
      {"cai4", 416},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4234 "zhy_symbol_map"
      {"syu6", 4226},
      {""},
#line 4231 "zhy_symbol_map"
      {"syu3", 4223},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4230 "zhy_symbol_map"
      {"syu2", 4222},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 585 "zhy_symbol_map"
      {"coei4", 577},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3561 "zhy_symbol_map"
      {"nip5", 3553},
      {""}, {""}, {""}, {""},
#line 3267 "zhy_symbol_map"
      {"nap5", 3259},
      {""},
#line 4229 "zhy_symbol_map"
      {"syu1", 4221},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1705 "zhy_symbol_map"
      {"haang4", 1697},
      {""},
#line 3658 "zhy_symbol_map"
      {"oe4", 3650},
      {""}, {""}, {""}, {""},
#line 2321 "zhy_symbol_map"
      {"ke4", 2313},
      {""}, {""},
#line 4879 "zhy_symbol_map"
      {"zim7", 4871},
      {""}, {""}, {""}, {""},
#line 4781 "zhy_symbol_map"
      {"zam7", 4773},
      {""}, {""},
#line 33 "zhy_symbol_map"
      {"aam5", 25},
      {""}, {""},
#line 606 "zhy_symbol_map"
      {"coi4", 598},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3904 "zhy_symbol_map"
      {"puk5", 3896},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4672 "zhy_symbol_map"
      {"wu3", 4664},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3574 "zhy_symbol_map"
      {"niu4", 3566},
      {""}, {""}, {""}, {""},
#line 3280 "zhy_symbol_map"
      {"nau4", 3272},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4235 "zhy_symbol_map"
      {"syu7", 4227},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2813 "zhy_symbol_map"
      {"lip6", 2805},
      {""},
#line 2810 "zhy_symbol_map"
      {"lip3", 2802},
      {""}, {""},
#line 2701 "zhy_symbol_map"
      {"lap6", 2693},
      {""},
#line 2698 "zhy_symbol_map"
      {"lap3", 2690},
      {""}, {""}, {""}, {""},
#line 2809 "zhy_symbol_map"
      {"lip2", 2801},
      {""}, {""}, {""}, {""},
#line 2697 "zhy_symbol_map"
      {"lap2", 2689},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3911 "zhy_symbol_map"
      {"pun5", 3903},
      {""}, {""}, {""}, {""}, {""},
#line 4743 "zhy_symbol_map"
      {"zaap4", 4735},
#line 2808 "zhy_symbol_map"
      {"lip1", 2800},
      {""}, {""}, {""}, {""},
#line 2696 "zhy_symbol_map"
      {"lap1", 2688},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2590 "zhy_symbol_map"
      {"kyun7", 2582},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1812 "zhy_symbol_map"
      {"hek6", 1804},
#line 2589 "zhy_symbol_map"
      {"kyun6", 2581},
#line 1809 "zhy_symbol_map"
      {"hek3", 1801},
      {""}, {""}, {""},
#line 739 "zhy_symbol_map"
      {"dai4", 731},
      {""}, {""}, {""}, {""},
#line 2586 "zhy_symbol_map"
      {"kyun3", 2578},
#line 1808 "zhy_symbol_map"
      {"hek2", 1800},
      {""}, {""}, {""},
#line 2653 "zhy_symbol_map"
      {"laat7", 2645},
      {""}, {""}, {""}, {""},
#line 2585 "zhy_symbol_map"
      {"kyun2", 2577},
      {""}, {""}, {""}, {""},
#line 2652 "zhy_symbol_map"
      {"laat6", 2644},
#line 93 "zhy_symbol_map"
      {"an2", 85},
#line 3623 "zhy_symbol_map"
      {"nou4", 3615},
      {""}, {""},
#line 2588 "zhy_symbol_map"
      {"kyun5", 2580},
      {""}, {""}, {""}, {""},
#line 2649 "zhy_symbol_map"
      {"laat3", 2641},
#line 1807 "zhy_symbol_map"
      {"hek1", 1799},
      {""}, {""}, {""},
#line 2584 "zhy_symbol_map"
      {"kyun1", 2576},
      {""}, {""}, {""}, {""},
#line 2648 "zhy_symbol_map"
      {"laat2", 2640},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2651 "zhy_symbol_map"
      {"laat5", 2643},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2647 "zhy_symbol_map"
      {"laat1", 2639},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2814 "zhy_symbol_map"
      {"lip7", 2806},
      {""}, {""}, {""}, {""},
#line 2702 "zhy_symbol_map"
      {"lap7", 2694},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 98 "zhy_symbol_map"
      {"an7", 90},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4050 "zhy_symbol_map"
      {"se4", 4042},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 935 "zhy_symbol_map"
      {"doi4", 927},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4484 "zhy_symbol_map"
      {"tuk4", 4476},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1813 "zhy_symbol_map"
      {"hek7", 1805},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4625 "zhy_symbol_map"
      {"wet5", 4617},
      {""}, {""}, {""},
#line 1820 "zhy_symbol_map"
      {"heng7", 1812},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1819 "zhy_symbol_map"
      {"heng6", 1811},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1816 "zhy_symbol_map"
      {"heng3", 1808},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1815 "zhy_symbol_map"
      {"heng2", 1807},
#line 4491 "zhy_symbol_map"
      {"tung4", 4483},
      {""}, {""}, {""}, {""},
#line 2398 "zhy_symbol_map"
      {"kit4", 2390},
      {""}, {""}, {""},
#line 1818 "zhy_symbol_map"
      {"heng5", 1810},
#line 2307 "zhy_symbol_map"
      {"kat4", 2299},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1337 "zhy_symbol_map"
      {"geoi7", 1329},
      {""},
#line 1814 "zhy_symbol_map"
      {"heng1", 1806},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1336 "zhy_symbol_map"
      {"geoi6", 1328},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1333 "zhy_symbol_map"
      {"geoi3", 1325},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1332 "zhy_symbol_map"
      {"geoi2", 1324},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1335 "zhy_symbol_map"
      {"geoi5", 1327},
#line 1908 "zhy_symbol_map"
      {"hoeng4", 1900},
      {""}, {""}, {""}, {""},
#line 92 "zhy_symbol_map"
      {"an1", 84},
      {""}, {""}, {""},
#line 1331 "zhy_symbol_map"
      {"geoi1", 1323},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 97 "zhy_symbol_map"
      {"an6", 89},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4242 "zhy_symbol_map"
      {"syun7", 4234},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4241 "zhy_symbol_map"
      {"syun6", 4233},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4238 "zhy_symbol_map"
      {"syun3", 4230},
      {""},
#line 1488 "zhy_symbol_map"
      {"gui4", 1480},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4237 "zhy_symbol_map"
      {"syun2", 4229},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 521 "zhy_symbol_map"
      {"ci3", 513},
      {""},
#line 4240 "zhy_symbol_map"
      {"syun5", 4232},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4236 "zhy_symbol_map"
      {"syun1", 4228},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 328 "zhy_symbol_map"
      {"bui6", 320},
      {""},
#line 325 "zhy_symbol_map"
      {"bui3", 317},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 324 "zhy_symbol_map"
      {"bui2", 316},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4401 "zhy_symbol_map"
      {"tim5", 4393},
      {""}, {""}, {""}, {""},
#line 4317 "zhy_symbol_map"
      {"tam5", 4309},
      {""}, {""}, {""}, {""},
#line 323 "zhy_symbol_map"
      {"bui1", 315},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3042 "zhy_symbol_map"
      {"me4", 3034},
      {""}, {""},
#line 1878 "zhy_symbol_map"
      {"hm2", 1870},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2870 "zhy_symbol_map"
      {"loet7", 2862},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2869 "zhy_symbol_map"
      {"loet6", 2861},
      {""}, {""},
#line 577 "zhy_symbol_map"
      {"co3", 569},
      {""}, {""}, {""},
#line 2176 "zhy_symbol_map"
      {"juk6", 2168},
      {""},
#line 2173 "zhy_symbol_map"
      {"juk3", 2165},
#line 2866 "zhy_symbol_map"
      {"loet3", 2858},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2172 "zhy_symbol_map"
      {"juk2", 2164},
#line 2865 "zhy_symbol_map"
      {"loet2", 2857},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2868 "zhy_symbol_map"
      {"loet5", 2860},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2864 "zhy_symbol_map"
      {"loet1", 2856},
      {""}, {""}, {""},
#line 2171 "zhy_symbol_map"
      {"juk1", 2163},
      {""}, {""}, {""},
#line 4141 "zhy_symbol_map"
      {"sit4", 4133},
      {""}, {""}, {""}, {""},
#line 4036 "zhy_symbol_map"
      {"sat4", 4028},
#line 329 "zhy_symbol_map"
      {"bui7", 321},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1883 "zhy_symbol_map"
      {"hm7", 1875},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3856 "zhy_symbol_map"
      {"pit6", 3848},
      {""},
#line 3853 "zhy_symbol_map"
      {"pit3", 3845},
      {""}, {""},
#line 3786 "zhy_symbol_map"
      {"pat6", 3778},
      {""},
#line 3783 "zhy_symbol_map"
      {"pat3", 3775},
      {""}, {""}, {""}, {""},
#line 3852 "zhy_symbol_map"
      {"pit2", 3844},
      {""}, {""}, {""}, {""},
#line 3782 "zhy_symbol_map"
      {"pat2", 3774},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1384 "zhy_symbol_map"
      {"gip5", 1376},
      {""}, {""}, {""}, {""},
#line 1293 "zhy_symbol_map"
      {"gap5", 1285},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3851 "zhy_symbol_map"
      {"pit1", 3843},
      {""}, {""}, {""}, {""},
#line 3781 "zhy_symbol_map"
      {"pat1", 3773},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 850 "zhy_symbol_map"
      {"di3", 842},
      {""}, {""},
#line 2177 "zhy_symbol_map"
      {"juk7", 2169},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2184 "zhy_symbol_map"
      {"jung7", 2176},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2183 "zhy_symbol_map"
      {"jung6", 2175},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4899 "zhy_symbol_map"
      {"zip6", 4891},
      {""},
#line 4896 "zhy_symbol_map"
      {"zip3", 4888},
#line 2180 "zhy_symbol_map"
      {"jung3", 2172},
      {""},
#line 4801 "zhy_symbol_map"
      {"zap6", 4793},
      {""},
#line 4798 "zhy_symbol_map"
      {"zap3", 4790},
      {""}, {""}, {""}, {""},
#line 4895 "zhy_symbol_map"
      {"zip2", 4887},
#line 2179 "zhy_symbol_map"
      {"jung2", 2171},
#line 1877 "zhy_symbol_map"
      {"hm1", 1869},
#line 1397 "zhy_symbol_map"
      {"giu4", 1389},
      {""},
#line 4797 "zhy_symbol_map"
      {"zap2", 4789},
      {""}, {""},
#line 1306 "zhy_symbol_map"
      {"gau4", 1298},
      {""}, {""},
#line 2182 "zhy_symbol_map"
      {"jung5", 2174},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1882 "zhy_symbol_map"
      {"hm6", 1874},
      {""},
#line 2178 "zhy_symbol_map"
      {"jung1", 2170},
      {""}, {""}, {""},
#line 4894 "zhy_symbol_map"
      {"zip1", 4886},
      {""}, {""}, {""}, {""},
#line 4796 "zhy_symbol_map"
      {"zap1", 4788},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3857 "zhy_symbol_map"
      {"pit7", 3849},
      {""},
#line 286 "zhy_symbol_map"
      {"biu6", 278},
      {""},
#line 283 "zhy_symbol_map"
      {"biu3", 275},
#line 3787 "zhy_symbol_map"
      {"pat7", 3779},
      {""},
#line 223 "zhy_symbol_map"
      {"bau6", 215},
      {""},
#line 220 "zhy_symbol_map"
      {"bau3", 212},
      {""}, {""}, {""}, {""},
#line 282 "zhy_symbol_map"
      {"biu2", 274},
      {""}, {""}, {""}, {""},
#line 219 "zhy_symbol_map"
      {"bau2", 211},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4610 "zhy_symbol_map"
      {"we4", 4602},
      {""},
#line 4753 "zhy_symbol_map"
      {"zaat7", 4745},
      {""}, {""}, {""}, {""}, {""},
#line 317 "zhy_symbol_map"
      {"bu2", 309},
      {""}, {""},
#line 281 "zhy_symbol_map"
      {"biu1", 273},
#line 4752 "zhy_symbol_map"
      {"zaat6", 4744},
      {""}, {""}, {""},
#line 218 "zhy_symbol_map"
      {"bau1", 210},
      {""}, {""}, {""}, {""}, {""},
#line 4749 "zhy_symbol_map"
      {"zaat3", 4741},
      {""}, {""}, {""}, {""}, {""},
#line 906 "zhy_symbol_map"
      {"do3", 898},
      {""}, {""}, {""},
#line 4748 "zhy_symbol_map"
      {"zaat2", 4740},
      {""}, {""},
#line 1867 "zhy_symbol_map"
      {"hit5", 1859},
      {""}, {""}, {""}, {""},
#line 1783 "zhy_symbol_map"
      {"hat5", 1775},
      {""},
#line 4751 "zhy_symbol_map"
      {"zaat5", 4743},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4747 "zhy_symbol_map"
      {"zaat1", 4739},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3511 "zhy_symbol_map"
      {"nguk4", 3503},
      {""}, {""}, {""},
#line 1474 "zhy_symbol_map"
      {"gou4", 1466},
      {""}, {""}, {""}, {""}, {""},
#line 3091 "zhy_symbol_map"
      {"mit4", 3083},
      {""}, {""}, {""}, {""},
#line 3028 "zhy_symbol_map"
      {"mat4", 3020},
#line 4900 "zhy_symbol_map"
      {"zip7", 4892},
      {""}, {""},
#line 3897 "zhy_symbol_map"
      {"pui5", 3889},
      {""},
#line 4802 "zhy_symbol_map"
      {"zap7", 4794},
      {""}, {""},
#line 54 "zhy_symbol_map"
      {"aap5", 46},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 322 "zhy_symbol_map"
      {"bu7", 314},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 314 "zhy_symbol_map"
      {"bou6", 306},
      {""},
#line 311 "zhy_symbol_map"
      {"bou3", 303},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 310 "zhy_symbol_map"
      {"bou2", 302},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2902 "zhy_symbol_map"
      {"luk4", 2894},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 287 "zhy_symbol_map"
      {"biu7", 279},
      {""}, {""}, {""}, {""},
#line 224 "zhy_symbol_map"
      {"bau7", 216},
      {""}, {""}, {""}, {""},
#line 309 "zhy_symbol_map"
      {"bou1", 301},
      {""}, {""}, {""}, {""}, {""},
#line 4382 "zhy_symbol_map"
      {"teon7", 4374},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4381 "zhy_symbol_map"
      {"teon6", 4373},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1944 "zhy_symbol_map"
      {"hot5", 1936},
      {""},
#line 4378 "zhy_symbol_map"
      {"teon3", 4370},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4377 "zhy_symbol_map"
      {"teon2", 4369},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2916 "zhy_symbol_map"
      {"lung4", 2908},
      {""},
#line 4380 "zhy_symbol_map"
      {"teon5", 4372},
      {""},
#line 67 "zhy_symbol_map"
      {"aau4", 59},
      {""}, {""}, {""}, {""}, {""},
#line 2721 "zhy_symbol_map"
      {"le5", 2713},
      {""},
#line 4376 "zhy_symbol_map"
      {"teon1", 4368},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1480 "zhy_symbol_map"
      {"gu3", 1472},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1805 "zhy_symbol_map"
      {"hei6", 1797},
      {""},
#line 1802 "zhy_symbol_map"
      {"hei3", 1794},
      {""}, {""}, {""}, {""},
#line 316 "zhy_symbol_map"
      {"bu1", 308},
#line 2909 "zhy_symbol_map"
      {"lun4", 2901},
      {""}, {""}, {""},
#line 1801 "zhy_symbol_map"
      {"hei2", 1793},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 321 "zhy_symbol_map"
      {"bu6", 313},
      {""}, {""}, {""}, {""},
#line 3869 "zhy_symbol_map"
      {"po5", 3861},
      {""}, {""}, {""}, {""}, {""},
#line 315 "zhy_symbol_map"
      {"bou7", 307},
      {""},
#line 1800 "zhy_symbol_map"
      {"hei1", 1792},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4375 "zhy_symbol_map"
      {"teoi7", 4367},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4374 "zhy_symbol_map"
      {"teoi6", 4366},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4371 "zhy_symbol_map"
      {"teoi3", 4363},
      {""}, {""},
#line 3287 "zhy_symbol_map"
      {"ne4", 3279},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4370 "zhy_symbol_map"
      {"teoi2", 4362},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4373 "zhy_symbol_map"
      {"teoi5", 4365},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4369 "zhy_symbol_map"
      {"teoi1", 4361},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2791 "zhy_symbol_map"
      {"lim5", 2783},
      {""}, {""}, {""}, {""},
#line 2679 "zhy_symbol_map"
      {"lam5", 2671},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4596 "zhy_symbol_map"
      {"wat4", 4588},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1806 "zhy_symbol_map"
      {"hei7", 1798},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 88 "zhy_symbol_map"
      {"am4", 80},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3862 "zhy_symbol_map"
      {"piu5", 3854},
      {""}, {""}, {""}, {""},
#line 3792 "zhy_symbol_map"
      {"pau5", 3784},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 122 "zhy_symbol_map"
      {"au3", 114},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3647 "zhy_symbol_map"
      {"nyun7", 3639},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3646 "zhy_symbol_map"
      {"nyun6", 3638},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3643 "zhy_symbol_map"
      {"nyun3", 3635},
      {""}, {""}, {""},
#line 538 "zhy_symbol_map"
      {"cim6", 530},
      {""},
#line 535 "zhy_symbol_map"
      {"cim3", 527},
      {""}, {""},
#line 440 "zhy_symbol_map"
      {"cam6", 432},
#line 3642 "zhy_symbol_map"
      {"nyun2", 3634},
#line 437 "zhy_symbol_map"
      {"cam3", 429},
#line 1769 "zhy_symbol_map"
      {"hao5", 1761},
      {""}, {""}, {""},
#line 534 "zhy_symbol_map"
      {"cim2", 526},
      {""}, {""}, {""},
#line 3645 "zhy_symbol_map"
      {"nyun5", 3637},
#line 436 "zhy_symbol_map"
      {"cam2", 428},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3641 "zhy_symbol_map"
      {"nyun1", 3633},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 533 "zhy_symbol_map"
      {"cim1", 525},
      {""}, {""}, {""}, {""},
#line 435 "zhy_symbol_map"
      {"cam1", 427},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3890 "zhy_symbol_map"
      {"pou5", 3882},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1831 "zhy_symbol_map"
      {"hi4", 1823},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1794 "zhy_symbol_map"
      {"he2", 1786},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4422 "zhy_symbol_map"
      {"tip5", 4414},
      {""}, {""}, {""}, {""},
#line 4338 "zhy_symbol_map"
      {"tap5", 4330},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 539 "zhy_symbol_map"
      {"cim7", 531},
      {""}, {""}, {""}, {""},
#line 441 "zhy_symbol_map"
      {"cam7", 433},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3567 "zhy_symbol_map"
      {"nit4", 3559},
      {""}, {""}, {""}, {""},
#line 3273 "zhy_symbol_map"
      {"nat4", 3265},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1799 "zhy_symbol_map"
      {"he7", 1791},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4435 "zhy_symbol_map"
      {"tiu4", 4427},
#line 1894 "zhy_symbol_map"
      {"ho4", 1886},
      {""}, {""}, {""},
#line 4351 "zhy_symbol_map"
      {"tau4", 4343},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 867 "zhy_symbol_map"
      {"dim6", 859},
      {""},
#line 864 "zhy_symbol_map"
      {"dim3", 856},
      {""}, {""},
#line 755 "zhy_symbol_map"
      {"dam6", 747},
#line 403 "zhy_symbol_map"
      {"caap4", 395},
#line 752 "zhy_symbol_map"
      {"dam3", 744},
      {""}, {""}, {""}, {""},
#line 863 "zhy_symbol_map"
      {"dim2", 855},
      {""}, {""},
#line 334 "zhy_symbol_map"
      {"buk5", 326},
      {""},
#line 751 "zhy_symbol_map"
      {"dam2", 743},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 862 "zhy_symbol_map"
      {"dim1", 854},
      {""}, {""}, {""}, {""},
#line 750 "zhy_symbol_map"
      {"dam1", 742},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4988 "zhy_symbol_map"
      {"zuk4", 4980},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2335 "zhy_symbol_map"
      {"kek4", 2327},
      {""}, {""}, {""},
#line 1793 "zhy_symbol_map"
      {"he1", 1785},
      {""}, {""}, {""},
#line 2548 "zhy_symbol_map"
      {"kwan7", 2540},
      {""}, {""}, {""}, {""},
#line 4477 "zhy_symbol_map"
      {"tou4", 4469},
      {""}, {""},
#line 2765 "zhy_symbol_map"
      {"leon7", 2757},
      {""},
#line 2547 "zhy_symbol_map"
      {"kwan6", 2539},
      {""}, {""},
#line 1798 "zhy_symbol_map"
      {"he6", 1790},
      {""}, {""}, {""}, {""},
#line 2764 "zhy_symbol_map"
      {"leon6", 2756},
      {""},
#line 2544 "zhy_symbol_map"
      {"kwan3", 2536},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2761 "zhy_symbol_map"
      {"leon3", 2753},
      {""},
#line 2543 "zhy_symbol_map"
      {"kwan2", 2535},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2760 "zhy_symbol_map"
      {"leon2", 2752},
      {""},
#line 2546 "zhy_symbol_map"
      {"kwan5", 2538},
      {""}, {""}, {""}, {""}, {""},
#line 4995 "zhy_symbol_map"
      {"zung4", 4987},
      {""},
#line 2763 "zhy_symbol_map"
      {"leon5", 2755},
      {""},
#line 2542 "zhy_symbol_map"
      {"kwan1", 2534},
      {""}, {""}, {""}, {""},
#line 341 "zhy_symbol_map"
      {"bun5", 333},
#line 4821 "zhy_symbol_map"
      {"ze5", 4813},
      {""},
#line 2759 "zhy_symbol_map"
      {"leon1", 2751},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 846 "zhy_symbol_map"
      {"deu6", 838},
      {""},
#line 843 "zhy_symbol_map"
      {"deu3", 835},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 842 "zhy_symbol_map"
      {"deu2", 834},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 868 "zhy_symbol_map"
      {"dim7", 860},
      {""}, {""}, {""}, {""},
#line 756 "zhy_symbol_map"
      {"dam7", 748},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 841 "zhy_symbol_map"
      {"deu1", 833},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2541 "zhy_symbol_map"
      {"kwai7", 2533},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2758 "zhy_symbol_map"
      {"leoi7", 2750},
      {""},
#line 2540 "zhy_symbol_map"
      {"kwai6", 2532},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2757 "zhy_symbol_map"
      {"leoi6", 2749},
      {""},
#line 2537 "zhy_symbol_map"
      {"kwai3", 2529},
      {""}, {""}, {""},
#line 3735 "zhy_symbol_map"
      {"paang4", 3727},
      {""}, {""}, {""},
#line 2754 "zhy_symbol_map"
      {"leoi3", 2746},
      {""},
#line 2536 "zhy_symbol_map"
      {"kwai2", 2528},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2753 "zhy_symbol_map"
      {"leoi2", 2745},
      {""},
#line 2539 "zhy_symbol_map"
      {"kwai5", 2531},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2756 "zhy_symbol_map"
      {"leoi5", 2748},
      {""},
#line 2535 "zhy_symbol_map"
      {"kwai1", 2527},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2752 "zhy_symbol_map"
      {"leoi1", 2744},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2148 "zhy_symbol_map"
      {"jiu6", 2140},
      {""},
#line 2145 "zhy_symbol_map"
      {"jiu3", 2137},
      {""}, {""},
#line 2064 "zhy_symbol_map"
      {"jau6", 2056},
      {""},
#line 2061 "zhy_symbol_map"
      {"jau3", 2053},
      {""}, {""}, {""}, {""},
#line 2144 "zhy_symbol_map"
      {"jiu2", 2136},
      {""}, {""}, {""}, {""},
#line 2060 "zhy_symbol_map"
      {"jau2", 2052},
      {""}, {""}, {""},
#line 1313 "zhy_symbol_map"
      {"ge4", 1305},
      {""}, {""}, {""}, {""}, {""},
#line 4877 "zhy_symbol_map"
      {"zim5", 4869},
      {""},
#line 718 "zhy_symbol_map"
      {"daap4", 710},
#line 847 "zhy_symbol_map"
      {"deu7", 839},
      {""},
#line 4779 "zhy_symbol_map"
      {"zam5", 4771},
      {""}, {""}, {""}, {""},
#line 2143 "zhy_symbol_map"
      {"jiu1", 2135},
      {""}, {""}, {""}, {""},
#line 2059 "zhy_symbol_map"
      {"jau1", 2051},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4064 "zhy_symbol_map"
      {"sek4", 4056},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4233 "zhy_symbol_map"
      {"syu5", 4225},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3814 "zhy_symbol_map"
      {"pek6", 3806},
      {""},
#line 3811 "zhy_symbol_map"
      {"pek3", 3803},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3810 "zhy_symbol_map"
      {"pek2", 3802},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4071 "zhy_symbol_map"
      {"seng4", 4063},
      {""}, {""}, {""},
#line 2149 "zhy_symbol_map"
      {"jiu7", 2141},
      {""}, {""}, {""}, {""},
#line 2065 "zhy_symbol_map"
      {"jau7", 2057},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3809 "zhy_symbol_map"
      {"pek1", 3801},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1659 "zhy_symbol_map"
      {"gyun7", 1651},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1658 "zhy_symbol_map"
      {"gyun6", 1650},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1655 "zhy_symbol_map"
      {"gyun3", 1647},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1654 "zhy_symbol_map"
      {"gyun2", 1646},
      {""}, {""}, {""}, {""},
#line 997 "zhy_symbol_map"
      {"ei3", 989},
      {""}, {""}, {""}, {""},
#line 1657 "zhy_symbol_map"
      {"gyun5", 1649},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 988 "zhy_symbol_map"
      {"e1", 980},
#line 1653 "zhy_symbol_map"
      {"gyun1", 1645},
      {""},
#line 2555 "zhy_symbol_map"
      {"kwik7", 2547},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2554 "zhy_symbol_map"
      {"kwik6", 2546},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2551 "zhy_symbol_map"
      {"kwik3", 2543},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2550 "zhy_symbol_map"
      {"kwik2", 2542},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2553 "zhy_symbol_map"
      {"kwik5", 2545},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2549 "zhy_symbol_map"
      {"kwik1", 2541},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2812 "zhy_symbol_map"
      {"lip5", 2804},
      {""}, {""}, {""},
#line 3815 "zhy_symbol_map"
      {"pek7", 3807},
#line 2700 "zhy_symbol_map"
      {"lap5", 2692},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3822 "zhy_symbol_map"
      {"peng7", 3814},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3821 "zhy_symbol_map"
      {"peng6", 3813},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3818 "zhy_symbol_map"
      {"peng3", 3810},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3817 "zhy_symbol_map"
      {"peng2", 3809},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3820 "zhy_symbol_map"
      {"peng5", 3812},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3816 "zhy_symbol_map"
      {"peng1", 3808},
      {""},
#line 1811 "zhy_symbol_map"
      {"hek5", 1803},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2825 "zhy_symbol_map"
      {"liu4", 2817},
      {""}, {""}, {""}, {""},
#line 2713 "zhy_symbol_map"
      {"lau4", 2705},
#line 1390 "zhy_symbol_map"
      {"git4", 1382},
      {""}, {""}, {""}, {""},
#line 1299 "zhy_symbol_map"
      {"gat4", 1291},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 559 "zhy_symbol_map"
      {"cip6", 551},
      {""},
#line 556 "zhy_symbol_map"
      {"cip3", 548},
      {""}, {""},
#line 461 "zhy_symbol_map"
      {"cap6", 453},
      {""},
#line 458 "zhy_symbol_map"
      {"cap3", 450},
      {""}, {""}, {""}, {""},
#line 555 "zhy_symbol_map"
      {"cip2", 547},
      {""}, {""}, {""}, {""},
#line 457 "zhy_symbol_map"
      {"cap2", 449},
      {""}, {""}, {""},
#line 279 "zhy_symbol_map"
      {"bit6", 271},
      {""},
#line 276 "zhy_symbol_map"
      {"bit3", 268},
      {""}, {""},
#line 216 "zhy_symbol_map"
      {"bat6", 208},
      {""},
#line 213 "zhy_symbol_map"
      {"bat3", 205},
      {""}, {""}, {""}, {""},
#line 275 "zhy_symbol_map"
      {"bit2", 267},
      {""}, {""}, {""},
#line 554 "zhy_symbol_map"
      {"cip1", 546},
#line 212 "zhy_symbol_map"
      {"bat2", 204},
      {""}, {""}, {""},
#line 456 "zhy_symbol_map"
      {"cap1", 448},
#line 3056 "zhy_symbol_map"
      {"meng4", 3048},
      {""}, {""},
#line 3678 "zhy_symbol_map"
      {"om3", 3670},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 274 "zhy_symbol_map"
      {"bit1", 266},
      {""}, {""}, {""}, {""},
#line 211 "zhy_symbol_map"
      {"bat1", 203},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 413 "zhy_symbol_map"
      {"caat7", 405},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 412 "zhy_symbol_map"
      {"caat6", 404},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 409 "zhy_symbol_map"
      {"caat3", 401},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 408 "zhy_symbol_map"
      {"caat2", 400},
      {""},
#line 2895 "zhy_symbol_map"
      {"lou4", 2887},
      {""}, {""},
#line 4851 "zhy_symbol_map"
      {"zeon7", 4843},
      {""}, {""},
#line 1467 "zhy_symbol_map"
      {"got4", 1459},
      {""},
#line 411 "zhy_symbol_map"
      {"caat5", 403},
      {""}, {""}, {""}, {""},
#line 4850 "zhy_symbol_map"
      {"zeon6", 4842},
      {""}, {""}, {""}, {""},
#line 407 "zhy_symbol_map"
      {"caat1", 399},
      {""}, {""}, {""}, {""},
#line 4847 "zhy_symbol_map"
      {"zeon3", 4839},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4846 "zhy_symbol_map"
      {"zeon2", 4838},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4849 "zhy_symbol_map"
      {"zeon5", 4841},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 560 "zhy_symbol_map"
      {"cip7", 552},
#line 4845 "zhy_symbol_map"
      {"zeon1", 4837},
      {""}, {""}, {""},
#line 462 "zhy_symbol_map"
      {"cap7", 454},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 280 "zhy_symbol_map"
      {"bit7", 272},
      {""}, {""}, {""}, {""},
#line 217 "zhy_symbol_map"
      {"bat7", 209},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 60 "zhy_symbol_map"
      {"aat4", 52},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4844 "zhy_symbol_map"
      {"zeoi7", 4836},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4843 "zhy_symbol_map"
      {"zeoi6", 4835},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4840 "zhy_symbol_map"
      {"zeoi3", 4832},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4839 "zhy_symbol_map"
      {"zeoi2", 4831},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4842 "zhy_symbol_map"
      {"zeoi5", 4834},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4838 "zhy_symbol_map"
      {"zeoi1", 4830},
      {""},
#line 327 "zhy_symbol_map"
      {"bui5", 319},
      {""}, {""},
#line 888 "zhy_symbol_map"
      {"dip6", 880},
      {""},
#line 885 "zhy_symbol_map"
      {"dip3", 877},
      {""}, {""},
#line 776 "zhy_symbol_map"
      {"dap6", 768},
      {""},
#line 773 "zhy_symbol_map"
      {"dap3", 765},
      {""}, {""}, {""}, {""},
#line 884 "zhy_symbol_map"
      {"dip2", 876},
      {""}, {""}, {""}, {""},
#line 772 "zhy_symbol_map"
      {"dap2", 764},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 883 "zhy_symbol_map"
      {"dip1", 875},
      {""}, {""}, {""}, {""},
#line 771 "zhy_symbol_map"
      {"dap1", 763},
      {""}, {""}, {""}, {""},
#line 4981 "zhy_symbol_map"
      {"zui4", 4973},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2328 "zhy_symbol_map"
      {"kei4", 2320},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 728 "zhy_symbol_map"
      {"daat7", 720},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 727 "zhy_symbol_map"
      {"daat6", 719},
      {""},
#line 3329 "zhy_symbol_map"
      {"ngaa4", 3321},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 724 "zhy_symbol_map"
      {"daat3", 716},
      {""}, {""}, {""}, {""}, {""},
#line 4617 "zhy_symbol_map"
      {"wen4", 4609},
      {""}, {""}, {""},
#line 723 "zhy_symbol_map"
      {"daat2", 715},
      {""}, {""}, {""},
#line 2175 "zhy_symbol_map"
      {"juk5", 2167},
      {""}, {""}, {""}, {""}, {""},
#line 726 "zhy_symbol_map"
      {"daat5", 718},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 722 "zhy_symbol_map"
      {"daat1", 714},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 889 "zhy_symbol_map"
      {"dip7", 881},
      {""}, {""}, {""}, {""},
#line 777 "zhy_symbol_map"
      {"dap7", 769},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 292 "zhy_symbol_map"
      {"bo5", 284},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3855 "zhy_symbol_map"
      {"pit5", 3847},
      {""}, {""}, {""}, {""},
#line 3785 "zhy_symbol_map"
      {"pat5", 3777},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3504 "zhy_symbol_map"
      {"ngou4", 3496},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4501 "zhy_symbol_map"
      {"tyun7", 4493},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3483 "zhy_symbol_map"
      {"ngok4", 3475},
      {""},
#line 4500 "zhy_symbol_map"
      {"tyun6", 4492},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4497 "zhy_symbol_map"
      {"tyun3", 4489},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4496 "zhy_symbol_map"
      {"tyun2", 4488},
      {""},
#line 4898 "zhy_symbol_map"
      {"zip5", 4890},
      {""}, {""}, {""}, {""},
#line 4800 "zhy_symbol_map"
      {"zap5", 4792},
      {""}, {""},
#line 4499 "zhy_symbol_map"
      {"tyun5", 4491},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4495 "zhy_symbol_map"
      {"tyun1", 4487},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3469 "zhy_symbol_map"
      {"ngo4", 3461},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3301 "zhy_symbol_map"
      {"neng4", 3293},
      {""},
#line 285 "zhy_symbol_map"
      {"biu5", 277},
      {""},
#line 4057 "zhy_symbol_map"
      {"sei4", 4049},
      {""}, {""},
#line 222 "zhy_symbol_map"
      {"bau5", 214},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3807 "zhy_symbol_map"
      {"pei6", 3799},
      {""},
#line 3804 "zhy_symbol_map"
      {"pei3", 3796},
      {""}, {""}, {""}, {""}, {""},
#line 4911 "zhy_symbol_map"
      {"ziu4", 4903},
      {""}, {""}, {""},
#line 3803 "zhy_symbol_map"
      {"pei2", 3795},
#line 4813 "zhy_symbol_map"
      {"zau4", 4805},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 3802 "zhy_symbol_map"
      {"pei1", 3794},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4428 "zhy_symbol_map"
      {"tit4", 4420},
      {""}, {""}, {""},
#line 313 "zhy_symbol_map"
      {"bou5", 305},
#line 4344 "zhy_symbol_map"
      {"tat4", 4336},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3673 "zhy_symbol_map"
      {"ok5", 3665},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1726 "zhy_symbol_map"
      {"haau4", 1718},
      {""},
#line 4974 "zhy_symbol_map"
      {"zou4", 4966},
      {""}, {""},
#line 1691 "zhy_symbol_map"
      {"haam4", 1683},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3808 "zhy_symbol_map"
      {"pei7", 3800},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1684 "zhy_symbol_map"
      {"haak4", 1676},
      {""}, {""}, {""},
#line 1740 "zhy_symbol_map"
      {"hak4", 1732},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3374 "zhy_symbol_map"
      {"ngaap7", 3366},
      {""}, {""}, {""}, {""},
#line 3373 "zhy_symbol_map"
      {"ngaap6", 3365},
#line 1804 "zhy_symbol_map"
      {"hei5", 1796},
      {""}, {""}, {""},
#line 3370 "zhy_symbol_map"
      {"ngaap3", 3362},
      {""}, {""}, {""}, {""},
#line 3369 "zhy_symbol_map"
      {"ngaap2", 3361},
      {""}, {""}, {""}, {""},
#line 3372 "zhy_symbol_map"
      {"ngaap5", 3364},
#line 1105 "zhy_symbol_map"
      {"fik6", 1097},
      {""},
#line 1102 "zhy_symbol_map"
      {"fik3", 1094},
#line 634 "zhy_symbol_map"
      {"cuk4", 626},
#line 3368 "zhy_symbol_map"
      {"ngaap1", 3360},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1101 "zhy_symbol_map"
      {"fik2", 1093},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1852 "zhy_symbol_map"
      {"hing4", 1844},
      {""}, {""}, {""},
#line 1670 "zhy_symbol_map"
      {"haa4", 1662},
#line 1761 "zhy_symbol_map"
      {"hang4", 1753},
      {""}, {""}, {""},
#line 3049 "zhy_symbol_map"
      {"mei4", 3041},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1100 "zhy_symbol_map"
      {"fik1", 1092},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3657 "zhy_symbol_map"
      {"oe3", 3649},
      {""}, {""}, {""}, {""},
#line 2320 "zhy_symbol_map"
      {"ke3", 2312},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1050 "zhy_symbol_map"
      {"faau7", 1042},
      {""}, {""}, {""}, {""}, {""},
#line 648 "zhy_symbol_map"
      {"cung4", 640},
#line 1014 "zhy_symbol_map"
      {"faa6", 1006},
      {""},
#line 1011 "zhy_symbol_map"
      {"faa3", 1003},
#line 1049 "zhy_symbol_map"
      {"faau6", 1041},
      {""}, {""}, {""}, {""}, {""},
#line 481 "zhy_symbol_map"
      {"ce5", 473},
      {""}, {""},
#line 1010 "zhy_symbol_map"
      {"faa2", 1002},
#line 1046 "zhy_symbol_map"
      {"faau3", 1038},
      {""},
#line 1845 "zhy_symbol_map"
      {"hin4", 1837},
      {""}, {""}, {""}, {""},
#line 1754 "zhy_symbol_map"
      {"han4", 1746},
      {""}, {""},
#line 1045 "zhy_symbol_map"
      {"faau2", 1037},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1048 "zhy_symbol_map"
      {"faau5", 1040},
      {""},
#line 1922 "zhy_symbol_map"
      {"hok4", 1914},
      {""},
#line 1009 "zhy_symbol_map"
      {"faa1", 1001},
      {""}, {""}, {""}, {""}, {""},
#line 1044 "zhy_symbol_map"
      {"faau1", 1036},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1029 "zhy_symbol_map"
      {"faak7", 1021},
      {""}, {""}, {""}, {""},
#line 641 "zhy_symbol_map"
      {"cun4", 633},
      {""},
#line 1063 "zhy_symbol_map"
      {"fan6", 1055},
      {""},
#line 1060 "zhy_symbol_map"
      {"fan3", 1052},
#line 1028 "zhy_symbol_map"
      {"faak6", 1020},
      {""}, {""}, {""}, {""}, {""},
#line 3490 "zhy_symbol_map"
      {"ngon4", 3482},
      {""}, {""},
#line 1059 "zhy_symbol_map"
      {"fan2", 1051},
#line 1025 "zhy_symbol_map"
      {"faak3", 1017},
      {""},
#line 1140 "zhy_symbol_map"
      {"fok6", 1132},
      {""},
#line 1137 "zhy_symbol_map"
      {"fok3", 1129},
      {""}, {""}, {""}, {""}, {""},
#line 1024 "zhy_symbol_map"
      {"faak2", 1016},
      {""}, {""}, {""},
#line 1136 "zhy_symbol_map"
      {"fok2", 1128},
      {""}, {""}, {""}, {""}, {""},
#line 1027 "zhy_symbol_map"
      {"faak5", 1019},
#line 3796 "zhy_symbol_map"
      {"pe2", 3788},
      {""},
#line 1936 "zhy_symbol_map"
      {"hong4", 1928},
#line 1058 "zhy_symbol_map"
      {"fan1", 1050},
      {""}, {""}, {""}, {""},
#line 1106 "zhy_symbol_map"
      {"fik7", 1098},
#line 1023 "zhy_symbol_map"
      {"faak1", 1015},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1135 "zhy_symbol_map"
      {"fok1", 1127},
#line 1113 "zhy_symbol_map"
      {"fing7", 1105},
      {""}, {""}, {""}, {""},
#line 1071 "zhy_symbol_map"
      {"fang7", 1063},
      {""}, {""}, {""}, {""},
#line 1112 "zhy_symbol_map"
      {"fing6", 1104},
#line 2141 "zhy_symbol_map"
      {"jit6", 2133},
      {""},
#line 2138 "zhy_symbol_map"
      {"jit3", 2130},
      {""},
#line 1070 "zhy_symbol_map"
      {"fang6", 1062},
#line 2057 "zhy_symbol_map"
      {"jat6", 2049},
      {""},
#line 2054 "zhy_symbol_map"
      {"jat3", 2046},
      {""},
#line 1109 "zhy_symbol_map"
      {"fing3", 1101},
      {""}, {""},
#line 2137 "zhy_symbol_map"
      {"jit2", 2129},
      {""},
#line 1067 "zhy_symbol_map"
      {"fang3", 1059},
#line 2720 "zhy_symbol_map"
      {"le4", 2712},
      {""},
#line 2053 "zhy_symbol_map"
      {"jat2", 2045},
      {""},
#line 1108 "zhy_symbol_map"
      {"fing2", 1100},
      {""}, {""}, {""}, {""},
#line 1066 "zhy_symbol_map"
      {"fang2", 1058},
      {""}, {""}, {""}, {""},
#line 1111 "zhy_symbol_map"
      {"fing5", 1103},
      {""},
#line 1929 "zhy_symbol_map"
      {"hon4", 1921},
      {""}, {""},
#line 1069 "zhy_symbol_map"
      {"fang5", 1061},
      {""}, {""},
#line 2136 "zhy_symbol_map"
      {"jit1", 2128},
      {""},
#line 1107 "zhy_symbol_map"
      {"fing1", 1099},
      {""}, {""},
#line 2052 "zhy_symbol_map"
      {"jat1", 2044},
#line 1015 "zhy_symbol_map"
      {"faa7", 1007},
#line 1065 "zhy_symbol_map"
      {"fang1", 1057},
      {""},
#line 1595 "zhy_symbol_map"
      {"gwe6", 1587},
      {""},
#line 1592 "zhy_symbol_map"
      {"gwe3", 1584},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1591 "zhy_symbol_map"
      {"gwe2", 1583},
      {""}, {""},
#line 537 "zhy_symbol_map"
      {"cim5", 529},
      {""}, {""}, {""}, {""},
#line 439 "zhy_symbol_map"
      {"cam5", 431},
#line 3801 "zhy_symbol_map"
      {"pe7", 3793},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3476 "zhy_symbol_map"
      {"ngoi4", 3468},
      {""}, {""},
#line 1590 "zhy_symbol_map"
      {"gwe1", 1582},
      {""},
#line 3868 "zhy_symbol_map"
      {"po4", 3860},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1064 "zhy_symbol_map"
      {"fan7", 1056},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1141 "zhy_symbol_map"
      {"fok7", 1133},
      {""}, {""}, {""},
#line 963 "zhy_symbol_map"
      {"duk4", 955},
#line 158 "zhy_symbol_map"
      {"baang4", 150},
      {""}, {""}, {""}, {""}, {""},
#line 1148 "zhy_symbol_map"
      {"fong7", 1140},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1147 "zhy_symbol_map"
      {"fong6", 1139},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1144 "zhy_symbol_map"
      {"fong3", 1136},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1143 "zhy_symbol_map"
      {"fong2", 1135},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2142 "zhy_symbol_map"
      {"jit7", 2134},
      {""},
#line 1146 "zhy_symbol_map"
      {"fong5", 1138},
      {""}, {""},
#line 2058 "zhy_symbol_map"
      {"jat7", 2050},
      {""}, {""}, {""}, {""},
#line 4049 "zhy_symbol_map"
      {"se3", 4041},
      {""},
#line 1142 "zhy_symbol_map"
      {"fong1", 1134},
      {""}, {""}, {""},
#line 970 "zhy_symbol_map"
      {"dung4", 962},
      {""}, {""}, {""},
#line 2926 "zhy_symbol_map"
      {"lyun7", 2918},
      {""}, {""}, {""}, {""}, {""},
#line 796 "zhy_symbol_map"
      {"de5", 788},
      {""}, {""}, {""},
#line 2925 "zhy_symbol_map"
      {"lyun6", 2917},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2922 "zhy_symbol_map"
      {"lyun3", 2914},
      {""}, {""}, {""}, {""}, {""},
#line 1596 "zhy_symbol_map"
      {"gwe7", 1588},
      {""},
#line 3795 "zhy_symbol_map"
      {"pe1", 3787},
      {""},
#line 2921 "zhy_symbol_map"
      {"lyun2", 2913},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2924 "zhy_symbol_map"
      {"lyun5", 2916},
      {""}, {""}, {""}, {""},
#line 3800 "zhy_symbol_map"
      {"pe6", 3792},
      {""}, {""}, {""}, {""},
#line 2920 "zhy_symbol_map"
      {"lyun1", 2912},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1698 "zhy_symbol_map"
      {"haan4", 1690},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1575 "zhy_symbol_map"
      {"gwan7", 1567},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1574 "zhy_symbol_map"
      {"gwan6", 1566},
      {""},
#line 1901 "zhy_symbol_map"
      {"hoe4", 1893},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1571 "zhy_symbol_map"
      {"gwan3", 1563},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1570 "zhy_symbol_map"
      {"gwan2", 1562},
      {""},
#line 244 "zhy_symbol_map"
      {"bek6", 236},
      {""},
#line 241 "zhy_symbol_map"
      {"bek3", 233},
      {""}, {""}, {""}, {""}, {""},
#line 1573 "zhy_symbol_map"
      {"gwan5", 1565},
      {""}, {""}, {""},
#line 240 "zhy_symbol_map"
      {"bek2", 232},
      {""}, {""}, {""}, {""}, {""},
#line 1569 "zhy_symbol_map"
      {"gwan1", 1561},
#line 2098 "zhy_symbol_map"
      {"ji5", 2090},
      {""},
#line 1327 "zhy_symbol_map"
      {"geng4", 1319},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 239 "zhy_symbol_map"
      {"bek1", 231},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 866 "zhy_symbol_map"
      {"dim5", 858},
      {""}, {""}, {""}, {""},
#line 754 "zhy_symbol_map"
      {"dam5", 746},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5004 "zhy_symbol_map"
      {"zyu6", 4996},
      {""},
#line 5001 "zhy_symbol_map"
      {"zyu3", 4993},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5000 "zhy_symbol_map"
      {"zyu2", 4992},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1677 "zhy_symbol_map"
      {"haai4", 1669},
      {""}, {""},
#line 2818 "zhy_symbol_map"
      {"lit4", 2810},
      {""}, {""}, {""},
#line 1568 "zhy_symbol_map"
      {"gwai7", 1560},
#line 2706 "zhy_symbol_map"
      {"lat4", 2698},
      {""}, {""}, {""}, {""}, {""},
#line 4999 "zhy_symbol_map"
      {"zyu1", 4991},
      {""}, {""},
#line 1567 "zhy_symbol_map"
      {"gwai6", 1559},
      {""}, {""}, {""}, {""},
#line 1036 "zhy_symbol_map"
      {"faan7", 1028},
      {""}, {""}, {""}, {""},
#line 1564 "zhy_symbol_map"
      {"gwai3", 1556},
      {""}, {""}, {""}, {""},
#line 1035 "zhy_symbol_map"
      {"faan6", 1027},
      {""}, {""}, {""}, {""},
#line 1563 "zhy_symbol_map"
      {"gwai2", 1555},
      {""}, {""}, {""}, {""},
#line 1032 "zhy_symbol_map"
      {"faan3", 1024},
      {""}, {""}, {""}, {""},
#line 1566 "zhy_symbol_map"
      {"gwai5", 1558},
      {""}, {""}, {""}, {""},
#line 1031 "zhy_symbol_map"
      {"faan2", 1023},
      {""}, {""}, {""}, {""},
#line 1562 "zhy_symbol_map"
      {"gwai1", 1554},
      {""}, {""}, {""}, {""},
#line 1034 "zhy_symbol_map"
      {"faan5", 1026},
#line 2154 "zhy_symbol_map"
      {"jo5", 2146},
#line 3294 "zhy_symbol_map"
      {"nei4", 3286},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 245 "zhy_symbol_map"
      {"bek7", 237},
#line 1030 "zhy_symbol_map"
      {"faan1", 1022},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 252 "zhy_symbol_map"
      {"beng7", 244},
      {""}, {""},
#line 3041 "zhy_symbol_map"
      {"me3", 3033},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 251 "zhy_symbol_map"
      {"beng6", 243},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 248 "zhy_symbol_map"
      {"beng3", 240},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 845 "zhy_symbol_map"
      {"deu5", 837},
      {""},
#line 247 "zhy_symbol_map"
      {"beng2", 239},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 250 "zhy_symbol_map"
      {"beng5", 242},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 246 "zhy_symbol_map"
      {"beng1", 238},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 5005 "zhy_symbol_map"
      {"zyu7", 4997},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1022 "zhy_symbol_map"
      {"faai7", 1014},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1021 "zhy_symbol_map"
      {"faai6", 1013},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1018 "zhy_symbol_map"
      {"faai3", 1010},
      {""}, {""},
#line 511 "zhy_symbol_map"
      {"ceon7", 503},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1017 "zhy_symbol_map"
      {"faai2", 1009},
      {""}, {""},
#line 510 "zhy_symbol_map"
      {"ceon6", 502},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1020 "zhy_symbol_map"
      {"faai5", 1012},
      {""}, {""},
#line 507 "zhy_symbol_map"
      {"ceon3", 499},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1016 "zhy_symbol_map"
      {"faai1", 1008},
      {""}, {""},
#line 506 "zhy_symbol_map"
      {"ceon2", 498},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 509 "zhy_symbol_map"
      {"ceon5", 501},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 505 "zhy_symbol_map"
      {"ceon1", 497},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2147 "zhy_symbol_map"
      {"jiu5", 2139},
      {""}, {""}, {""}, {""},
#line 2063 "zhy_symbol_map"
      {"jau5", 2055},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1602 "zhy_symbol_map"
      {"gwi6", 1594},
      {""},
#line 1599 "zhy_symbol_map"
      {"gwi3", 1591},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1598 "zhy_symbol_map"
      {"gwi2", 1590},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3687 "zhy_symbol_map"
      {"on5", 3679},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1597 "zhy_symbol_map"
      {"gwi1", 1589},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 504 "zhy_symbol_map"
      {"ceoi7", 496},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1610 "zhy_symbol_map"
      {"gwik7", 1602},
      {""}, {""},
#line 503 "zhy_symbol_map"
      {"ceoi6", 495},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1609 "zhy_symbol_map"
      {"gwik6", 1601},
      {""}, {""},
#line 500 "zhy_symbol_map"
      {"ceoi3", 492},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1606 "zhy_symbol_map"
      {"gwik3", 1598},
      {""}, {""},
#line 499 "zhy_symbol_map"
      {"ceoi2", 491},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1605 "zhy_symbol_map"
      {"gwik2", 1597},
      {""}, {""},
#line 502 "zhy_symbol_map"
      {"ceoi5", 494},
      {""}, {""},
#line 4820 "zhy_symbol_map"
      {"ze4", 4812},
      {""}, {""}, {""},
#line 1608 "zhy_symbol_map"
      {"gwik5", 1600},
      {""}, {""},
#line 498 "zhy_symbol_map"
      {"ceoi1", 490},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1604 "zhy_symbol_map"
      {"gwik1", 1596},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1733 "zhy_symbol_map"
      {"hai4", 1725},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3813 "zhy_symbol_map"
      {"pek5", 3805},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4609 "zhy_symbol_map"
      {"we3", 4601},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1056 "zhy_symbol_map"
      {"fai6", 1048},
      {""},
#line 1053 "zhy_symbol_map"
      {"fai3", 1045},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1052 "zhy_symbol_map"
      {"fai2", 1044},
      {""}, {""}, {""}, {""},
#line 1603 "zhy_symbol_map"
      {"gwi7", 1595},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1051 "zhy_symbol_map"
      {"fai1", 1043},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 833 "zhy_symbol_map"
      {"deon7", 825},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 832 "zhy_symbol_map"
      {"deon6", 824},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 829 "zhy_symbol_map"
      {"deon3", 821},
      {""}, {""}, {""}, {""}, {""},
#line 1915 "zhy_symbol_map"
      {"hoi4", 1907},
      {""}, {""}, {""},
#line 828 "zhy_symbol_map"
      {"deon2", 820},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 831 "zhy_symbol_map"
      {"deon5", 823},
      {""},
#line 5012 "zhy_symbol_map"
      {"zyun7", 5004},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 827 "zhy_symbol_map"
      {"deon1", 819},
      {""},
#line 5011 "zhy_symbol_map"
      {"zyun6", 5003},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5008 "zhy_symbol_map"
      {"zyun3", 5000},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5007 "zhy_symbol_map"
      {"zyun2", 4999},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5010 "zhy_symbol_map"
      {"zyun5", 5002},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5006 "zhy_symbol_map"
      {"zyun1", 4998},
      {""}, {""}, {""}, {""}, {""},
#line 1057 "zhy_symbol_map"
      {"fai7", 1049},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 826 "zhy_symbol_map"
      {"deoi7", 818},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 825 "zhy_symbol_map"
      {"deoi6", 817},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 822 "zhy_symbol_map"
      {"deoi3", 814},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 821 "zhy_symbol_map"
      {"deoi2", 813},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 824 "zhy_symbol_map"
      {"deoi5", 816},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 820 "zhy_symbol_map"
      {"deoi1", 812},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 558 "zhy_symbol_map"
      {"cip5", 550},
      {""}, {""}, {""}, {""},
#line 460 "zhy_symbol_map"
      {"cap5", 452},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4358 "zhy_symbol_map"
      {"tek4", 4350},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 278 "zhy_symbol_map"
      {"bit5", 270},
      {""}, {""}, {""}, {""},
#line 215 "zhy_symbol_map"
      {"bat5", 207},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3286 "zhy_symbol_map"
      {"ne3", 3278},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4904 "zhy_symbol_map"
      {"zit4", 4896},
      {""},
#line 4365 "zhy_symbol_map"
      {"teng4", 4357},
      {""}, {""},
#line 4806 "zhy_symbol_map"
      {"zat4", 4798},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 571 "zhy_symbol_map"
      {"ciu4", 563},
      {""}, {""}, {""}, {""},
#line 473 "zhy_symbol_map"
      {"cau4", 465},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 87 "zhy_symbol_map"
      {"am3", 79},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2006 "zhy_symbol_map"
      {"jaang4", 1998},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1320 "zhy_symbol_map"
      {"gei4", 1312},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 237 "zhy_symbol_map"
      {"bei6", 229},
      {""},
#line 234 "zhy_symbol_map"
      {"bei3", 226},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 233 "zhy_symbol_map"
      {"bei2", 225},
#line 627 "zhy_symbol_map"
      {"cou4", 619},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2489 "zhy_symbol_map"
      {"kut4", 2481},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 232 "zhy_symbol_map"
      {"bei1", 224},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1129 "zhy_symbol_map"
      {"fo2", 1121},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 887 "zhy_symbol_map"
      {"dip5", 879},
      {""}, {""}, {""}, {""},
#line 775 "zhy_symbol_map"
      {"dap5", 767},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1830 "zhy_symbol_map"
      {"hi3", 1822},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1134 "zhy_symbol_map"
      {"fo7", 1126},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 238 "zhy_symbol_map"
      {"bei7", 230},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 900 "zhy_symbol_map"
      {"diu4", 892},
      {""}, {""}, {""}, {""},
#line 788 "zhy_symbol_map"
      {"dau4", 780},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1893 "zhy_symbol_map"
      {"ho3", 1885},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1128 "zhy_symbol_map"
      {"fo1", 1120},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2079 "zhy_symbol_map"
      {"jeng7", 2071},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1133 "zhy_symbol_map"
      {"fo6", 1125},
      {""},
#line 2078 "zhy_symbol_map"
      {"jeng6", 2070},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2075 "zhy_symbol_map"
      {"jeng3", 2067},
      {""}, {""},
#line 956 "zhy_symbol_map"
      {"dou4", 948},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2074 "zhy_symbol_map"
      {"jeng2", 2066},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2077 "zhy_symbol_map"
      {"jeng5", 2069},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2073 "zhy_symbol_map"
      {"jeng1", 2065},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3926 "zhy_symbol_map"
      {"put6", 3918},
      {""},
#line 3923 "zhy_symbol_map"
      {"put3", 3915},
      {""}, {""}, {""}, {""}, {""},
#line 2167 "zhy_symbol_map"
      {"joeng4", 2159},
      {""}, {""}, {""},
#line 3922 "zhy_symbol_map"
      {"put2", 3914},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3749 "zhy_symbol_map"
      {"paau4", 3741},
      {""}, {""},
#line 226 "zhy_symbol_map"
      {"be2", 218},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3921 "zhy_symbol_map"
      {"put1", 3913},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3833 "zhy_symbol_map"
      {"pik4", 3825},
#line 3721 "zhy_symbol_map"
      {"paak4", 3713},
      {""}, {""}, {""},
#line 3763 "zhy_symbol_map"
      {"pak4", 3755},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3806 "zhy_symbol_map"
      {"pei5", 3798},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3695 "zhy_symbol_map"
      {"ong6", 3687},
      {""},
#line 3692 "zhy_symbol_map"
      {"ong3", 3684},
      {""}, {""},
#line 2351 "zhy_symbol_map"
      {"kep6", 2343},
#line 231 "zhy_symbol_map"
      {"be7", 223},
#line 2348 "zhy_symbol_map"
      {"kep3", 2340},
      {""}, {""}, {""}, {""},
#line 3691 "zhy_symbol_map"
      {"ong2", 3683},
      {""}, {""}, {""}, {""},
#line 2347 "zhy_symbol_map"
      {"kep2", 2339},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 291 "zhy_symbol_map"
      {"bo4", 283},
      {""},
#line 3847 "zhy_symbol_map"
      {"ping4", 3839},
      {""}, {""}, {""},
#line 3707 "zhy_symbol_map"
      {"paa4", 3699},
#line 3777 "zhy_symbol_map"
      {"pang4", 3769},
      {""}, {""}, {""},
#line 2734 "zhy_symbol_map"
      {"lek4", 2726},
      {""},
#line 3690 "zhy_symbol_map"
      {"ong1", 3682},
      {""}, {""}, {""}, {""},
#line 2346 "zhy_symbol_map"
      {"kep1", 2338},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3927 "zhy_symbol_map"
      {"put7", 3919},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3840 "zhy_symbol_map"
      {"pin4", 3832},
      {""}, {""}, {""}, {""},
#line 3770 "zhy_symbol_map"
      {"pan4", 3762},
      {""}, {""}, {""}, {""}, {""},
#line 2748 "zhy_symbol_map"
      {"leng4", 2740},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3875 "zhy_symbol_map"
      {"pok4", 3867},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1312 "zhy_symbol_map"
      {"ge3", 1304},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 225 "zhy_symbol_map"
      {"be1", 217},
      {""}, {""}, {""}, {""},
#line 3168 "zhy_symbol_map"
      {"mut4", 3160},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3882 "zhy_symbol_map"
      {"pong4", 3874},
#line 3696 "zhy_symbol_map"
      {"ong7", 3688},
      {""}, {""}, {""},
#line 230 "zhy_symbol_map"
      {"be6", 222},
#line 2352 "zhy_symbol_map"
      {"kep7", 2344},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 657 "zhy_symbol_map"
      {"cyu6", 649},
      {""},
#line 654 "zhy_symbol_map"
      {"cyu3", 646},
      {""}, {""}, {""}, {""}, {""},
#line 82 "zhy_symbol_map"
      {"ak5", 74},
      {""}, {""}, {""},
#line 653 "zhy_symbol_map"
      {"cyu2", 645},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 652 "zhy_symbol_map"
      {"cyu1", 644},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1104 "zhy_symbol_map"
      {"fik5", 1096},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1013 "zhy_symbol_map"
      {"faa5", 1005},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4510 "zhy_symbol_map"
      {"uk2", 4502},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 658 "zhy_symbol_map"
      {"cyu7", 650},
      {""}, {""}, {""}, {""},
#line 1062 "zhy_symbol_map"
      {"fan5", 1054},
#line 3672 "zhy_symbol_map"
      {"ok4", 3664},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1139 "zhy_symbol_map"
      {"fok5", 1131},
      {""}, {""}, {""},
#line 3385 "zhy_symbol_map"
      {"ngaau4", 3377},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4515 "zhy_symbol_map"
      {"uk7", 4507},
#line 2140 "zhy_symbol_map"
      {"jit5", 2132},
      {""}, {""}, {""}, {""},
#line 2056 "zhy_symbol_map"
      {"jat5", 2048},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3728 "zhy_symbol_map"
      {"paan4", 3720},
      {""}, {""}, {""},
#line 1594 "zhy_symbol_map"
      {"gwe5", 1586},
      {""}, {""}, {""},
#line 4694 "zhy_symbol_map"
      {"wut4", 4686},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1840 "zhy_symbol_map"
      {"him6", 1832},
      {""},
#line 1837 "zhy_symbol_map"
      {"him3", 1829},
      {""}, {""},
#line 1749 "zhy_symbol_map"
      {"ham6", 1741},
      {""},
#line 1746 "zhy_symbol_map"
      {"ham3", 1738},
      {""}, {""}, {""}, {""},
#line 1836 "zhy_symbol_map"
      {"him2", 1828},
      {""}, {""}, {""}, {""},
#line 1745 "zhy_symbol_map"
      {"ham2", 1737},
      {""}, {""}, {""}, {""},
#line 480 "zhy_symbol_map"
      {"ce4", 472},
      {""},
#line 4509 "zhy_symbol_map"
      {"uk1", 4501},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1835 "zhy_symbol_map"
      {"him1", 1827},
      {""}, {""}, {""},
#line 4514 "zhy_symbol_map"
      {"uk6", 4506},
#line 1744 "zhy_symbol_map"
      {"ham1", 1736},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1582 "zhy_symbol_map"
      {"gwang7", 1574},
      {""},
#line 3714 "zhy_symbol_map"
      {"paai4", 3706},
      {""}, {""},
#line 1581 "zhy_symbol_map"
      {"gwang6", 1573},
      {""}, {""}, {""}, {""},
#line 1578 "zhy_symbol_map"
      {"gwang3", 1570},
      {""}, {""}, {""}, {""},
#line 1577 "zhy_symbol_map"
      {"gwang2", 1569},
      {""}, {""}, {""}, {""},
#line 1580 "zhy_symbol_map"
      {"gwang5", 1572},
      {""}, {""}, {""}, {""},
#line 1576 "zhy_symbol_map"
      {"gwang1", 1568},
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
#line 1841 "zhy_symbol_map"
      {"him7", 1833},
      {""}, {""}, {""}, {""},
#line 1750 "zhy_symbol_map"
      {"ham7", 1742},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 243 "zhy_symbol_map"
      {"bek5", 235},
      {""}, {""}, {""}, {""}, {""},
#line 665 "zhy_symbol_map"
      {"cyun7", 657},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 664 "zhy_symbol_map"
      {"cyun6", 656},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 661 "zhy_symbol_map"
      {"cyun3", 653},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 660 "zhy_symbol_map"
      {"cyun2", 652},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 663 "zhy_symbol_map"
      {"cyun5", 655},
      {""}, {""}, {""},
#line 4827 "zhy_symbol_map"
      {"zek4", 4819},
      {""}, {""}, {""}, {""}, {""},
#line 659 "zhy_symbol_map"
      {"cyun1", 651},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5003 "zhy_symbol_map"
      {"zyu5", 4995},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 4834 "zhy_symbol_map"
      {"zeng4", 4826},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1712 "zhy_symbol_map"
      {"haap4", 1704},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 795 "zhy_symbol_map"
      {"de4", 787},
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
#line 564 "zhy_symbol_map"
      {"cit4", 556},
      {""}, {""}, {""}, {""},
#line 466 "zhy_symbol_map"
      {"cat4", 458},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3756 "zhy_symbol_map"
      {"pai4", 3748},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2097 "zhy_symbol_map"
      {"ji4", 2089},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2067 "zhy_symbol_map"
      {"je2", 2059},
      {""}, {""}, {""}, {""}, {""},
#line 2727 "zhy_symbol_map"
      {"lei4", 2719},
      {""}, {""}, {""},
#line 980 "zhy_symbol_map"
      {"dyun7", 972},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 979 "zhy_symbol_map"
      {"dyun6", 971},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 976 "zhy_symbol_map"
      {"dyun3", 968},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 975 "zhy_symbol_map"
      {"dyun2", 967},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 978 "zhy_symbol_map"
      {"dyun5", 970},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 974 "zhy_symbol_map"
      {"dyun1", 966},
#line 1601 "zhy_symbol_map"
      {"gwi5", 1593},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2072 "zhy_symbol_map"
      {"je7", 2064},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2153 "zhy_symbol_map"
      {"jo4", 2145},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 96 "zhy_symbol_map"
      {"an5", 88},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1055 "zhy_symbol_map"
      {"fai5", 1047},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 893 "zhy_symbol_map"
      {"dit4", 885},
      {""}, {""}, {""}, {""},
#line 781 "zhy_symbol_map"
      {"dat4", 773},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2066 "zhy_symbol_map"
      {"je1", 2058},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2071 "zhy_symbol_map"
      {"je6", 2063},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3686 "zhy_symbol_map"
      {"on4", 3678},
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
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1617 "zhy_symbol_map"
      {"gwing7", 1609},
      {""}, {""}, {""}, {""},
#line 1616 "zhy_symbol_map"
      {"gwing6", 1608},
      {""}, {""}, {""}, {""},
#line 1613 "zhy_symbol_map"
      {"gwing3", 1605},
      {""}, {""}, {""}, {""},
#line 1612 "zhy_symbol_map"
      {"gwing2", 1604},
      {""}, {""}, {""}, {""},
#line 1615 "zhy_symbol_map"
      {"gwing5", 1607},
      {""}, {""}, {""}, {""},
#line 1611 "zhy_symbol_map"
      {"gwing1", 1603},
#line 1861 "zhy_symbol_map"
      {"hip6", 1853},
      {""},
#line 1858 "zhy_symbol_map"
      {"hip3", 1850},
      {""}, {""},
#line 1777 "zhy_symbol_map"
      {"hap6", 1769},
      {""},
#line 1774 "zhy_symbol_map"
      {"hap3", 1766},
      {""}, {""}, {""}, {""},
#line 1857 "zhy_symbol_map"
      {"hip2", 1849},
      {""}, {""}, {""}, {""},
#line 1773 "zhy_symbol_map"
      {"hap2", 1765},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1516 "zhy_symbol_map"
      {"gut4", 1508},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1856 "zhy_symbol_map"
      {"hip1", 1848},
      {""}, {""}, {""}, {""},
#line 1772 "zhy_symbol_map"
      {"hap1", 1764},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 356 "zhy_symbol_map"
      {"but6", 348},
      {""},
#line 353 "zhy_symbol_map"
      {"but3", 345},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1881 "zhy_symbol_map"
      {"hm5", 1873},
      {""},
#line 352 "zhy_symbol_map"
      {"but2", 344},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1722 "zhy_symbol_map"
      {"haat7", 1714},
      {""}, {""},
#line 172 "zhy_symbol_map"
      {"baau4", 164},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1721 "zhy_symbol_map"
      {"haat6", 1713},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 351 "zhy_symbol_map"
      {"but1", 343},
      {""},
#line 1718 "zhy_symbol_map"
      {"haat3", 1710},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1717 "zhy_symbol_map"
      {"haat2", 1709},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2719 "zhy_symbol_map"
      {"le3", 2711},
      {""},
#line 1720 "zhy_symbol_map"
      {"haat5", 1712},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1716 "zhy_symbol_map"
      {"haat1", 1708},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 256 "zhy_symbol_map"
      {"bik4", 248},
#line 144 "zhy_symbol_map"
      {"baak4", 136},
      {""}, {""}, {""},
#line 186 "zhy_symbol_map"
      {"bak4", 178},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1862 "zhy_symbol_map"
      {"hip7", 1854},
      {""}, {""},
#line 236 "zhy_symbol_map"
      {"bei5", 228},
      {""},
#line 1778 "zhy_symbol_map"
      {"hap7", 1770},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3867 "zhy_symbol_map"
      {"po3", 3859},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 270 "zhy_symbol_map"
      {"bing4", 262},
      {""}, {""}, {""},
#line 130 "zhy_symbol_map"
      {"baa4", 122},
#line 207 "zhy_symbol_map"
      {"bang4", 199},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 357 "zhy_symbol_map"
      {"but7", 349},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 263 "zhy_symbol_map"
      {"bin4", 255},
      {""}, {""}, {""}, {""},
#line 200 "zhy_symbol_map"
      {"ban4", 192},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 298 "zhy_symbol_map"
      {"bok4", 290},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 305 "zhy_symbol_map"
      {"bong4", 297},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 320 "zhy_symbol_map"
      {"bu5", 312},
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
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1343 "zhy_symbol_map"
      {"gep6", 1335},
      {""},
#line 1340 "zhy_symbol_map"
      {"gep3", 1332},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1339 "zhy_symbol_map"
      {"gep2", 1331},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1338 "zhy_symbol_map"
      {"gep1", 1330},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 3925 "zhy_symbol_map"
      {"put5", 3917},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2370 "zhy_symbol_map"
      {"kim4", 2362},
      {""}, {""}, {""}, {""},
#line 2279 "zhy_symbol_map"
      {"kam4", 2271},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 151 "zhy_symbol_map"
      {"baan4", 143},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3694 "zhy_symbol_map"
      {"ong5", 3686},
      {""},
#line 1344 "zhy_symbol_map"
      {"gep7", 1336},
      {""}, {""},
#line 2350 "zhy_symbol_map"
      {"kep5", 2342},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2583 "zhy_symbol_map"
      {"kwun7", 2575},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2582 "zhy_symbol_map"
      {"kwun6", 2574},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2579 "zhy_symbol_map"
      {"kwun3", 2571},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2578 "zhy_symbol_map"
      {"kwun2", 2570},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 104 "zhy_symbol_map"
      {"ang6", 96},
      {""},
#line 101 "zhy_symbol_map"
      {"ang3", 93},
#line 2581 "zhy_symbol_map"
      {"kwun5", 2573},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 100 "zhy_symbol_map"
      {"ang2", 92},
#line 2577 "zhy_symbol_map"
      {"kwun1", 2569},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 137 "zhy_symbol_map"
      {"baai4", 129},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 99 "zhy_symbol_map"
      {"ang1", 91},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 487 "zhy_symbol_map"
      {"cek4", 479},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 656 "zhy_symbol_map"
      {"cyu5", 648},
      {""}, {""}, {""}, {""},
#line 2576 "zhy_symbol_map"
      {"kwui7", 2568},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2575 "zhy_symbol_map"
      {"kwui6", 2567},
      {""}, {""},
#line 4819 "zhy_symbol_map"
      {"ze3", 4811},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2572 "zhy_symbol_map"
      {"kwui3", 2564},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2571 "zhy_symbol_map"
      {"kwui2", 2563},
      {""}, {""},
#line 494 "zhy_symbol_map"
      {"ceng4", 486},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2574 "zhy_symbol_map"
      {"kwui5", 2566},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2570 "zhy_symbol_map"
      {"kwui1", 2562},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 105 "zhy_symbol_map"
      {"ang7", 97},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4113 "zhy_symbol_map"
      {"sim4", 4105},
      {""}, {""}, {""}, {""},
#line 4008 "zhy_symbol_map"
      {"sam4", 4000},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1957 "zhy_symbol_map"
      {"huk4", 1949},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1168 "zhy_symbol_map"
      {"fuk6", 1160},
      {""},
#line 1165 "zhy_symbol_map"
      {"fuk3", 1157},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1164 "zhy_symbol_map"
      {"fuk2", 1156},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1964 "zhy_symbol_map"
      {"hung4", 1956},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1797 "zhy_symbol_map"
      {"he5", 1789},
      {""}, {""}, {""}, {""}, {""},
#line 1163 "zhy_symbol_map"
      {"fuk1", 1155},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2743 "zhy_symbol_map"
      {"lem6", 2735},
      {""},
#line 2740 "zhy_symbol_map"
      {"lem3", 2732},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2739 "zhy_symbol_map"
      {"lem2", 2731},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2738 "zhy_symbol_map"
      {"lem1", 2730},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 81 "zhy_symbol_map"
      {"ak4", 73},
      {""}, {""}, {""},
#line 1175 "zhy_symbol_map"
      {"fun6", 1167},
      {""},
#line 1172 "zhy_symbol_map"
      {"fun3", 1164},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1171 "zhy_symbol_map"
      {"fun2", 1163},
#line 809 "zhy_symbol_map"
      {"dek4", 801},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 179 "zhy_symbol_map"
      {"bai4", 171},
      {""},
#line 1170 "zhy_symbol_map"
      {"fun1", 1162},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1169 "zhy_symbol_map"
      {"fuk7", 1161},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1183 "zhy_symbol_map"
      {"fung7", 1175},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1182 "zhy_symbol_map"
      {"fung6", 1174},
      {""}, {""}, {""}, {""}, {""},
#line 816 "zhy_symbol_map"
      {"deng4", 808},
      {""}, {""}, {""},
#line 1179 "zhy_symbol_map"
      {"fung3", 1171},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1839 "zhy_symbol_map"
      {"him5", 1831},
#line 1178 "zhy_symbol_map"
      {"fung2", 1170},
      {""}, {""},
#line 2020 "zhy_symbol_map"
      {"jaau4", 2012},
#line 1748 "zhy_symbol_map"
      {"ham5", 1740},
      {""},
#line 2744 "zhy_symbol_map"
      {"lem7", 2736},
      {""}, {""}, {""},
#line 1181 "zhy_symbol_map"
      {"fung5", 1173},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1177 "zhy_symbol_map"
      {"fung1", 1169},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3007 "zhy_symbol_map"
      {"mam4", 2999},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2104 "zhy_symbol_map"
      {"jik4", 2096},
#line 1999 "zhy_symbol_map"
      {"jaak4", 1991},
      {""}, {""}, {""}, {""},
#line 1005 "zhy_symbol_map"
      {"eot4", 997},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1176 "zhy_symbol_map"
      {"fun7", 1168},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2125 "zhy_symbol_map"
      {"jing4", 2117},
      {""}, {""}, {""},
#line 1985 "zhy_symbol_map"
      {"jaa4", 1977},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2118 "zhy_symbol_map"
      {"jin4", 2110},
      {""}, {""}, {""}, {""},
#line 2041 "zhy_symbol_map"
      {"jan4", 2033},
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
#line 2251 "zhy_symbol_map"
      {"kaat4", 2243},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2391 "zhy_symbol_map"
      {"kip4", 2383},
      {""}, {""}, {""}, {""},
#line 2300 "zhy_symbol_map"
      {"kap4", 2292},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2160 "zhy_symbol_map"
      {"joek4", 2152},
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
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1827 "zhy_symbol_map"
      {"heoi7", 1819},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1826 "zhy_symbol_map"
      {"heoi6", 1818},
      {""}, {""}, {""},
#line 3539 "zhy_symbol_map"
      {"nim4", 3531},
      {""}, {""}, {""}, {""},
#line 3245 "zhy_symbol_map"
      {"nam4", 3237},
#line 1823 "zhy_symbol_map"
      {"heoi3", 1815},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1822 "zhy_symbol_map"
      {"heoi2", 1814},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1992 "zhy_symbol_map"
      {"jaai4", 1984},
      {""},
#line 1825 "zhy_symbol_map"
      {"heoi5", 1817},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1821 "zhy_symbol_map"
      {"heoi1", 1813},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3980 "zhy_symbol_map"
      {"saat4", 3972},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 290 "zhy_symbol_map"
      {"bo3", 282},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4134 "zhy_symbol_map"
      {"sip4", 4126},
      {""}, {""}, {""}, {""},
#line 4029 "zhy_symbol_map"
      {"sap4", 4021},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1161 "zhy_symbol_map"
      {"fui6", 1153},
      {""},
#line 1158 "zhy_symbol_map"
      {"fui3", 1150},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1157 "zhy_symbol_map"
      {"fui2", 1149},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1156 "zhy_symbol_map"
      {"fui1", 1148},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3745 "zhy_symbol_map"
      {"paat7", 3737},
      {""},
#line 2499 "zhy_symbol_map"
      {"kwaa7", 2491},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3744 "zhy_symbol_map"
      {"paat6", 3736},
      {""},
#line 2498 "zhy_symbol_map"
      {"kwaa6", 2490},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3741 "zhy_symbol_map"
      {"paat3", 3733},
      {""},
#line 2495 "zhy_symbol_map"
      {"kwaa3", 2487},
#line 2506 "zhy_symbol_map"
      {"kwaai7", 2498},
      {""}, {""}, {""}, {""},
#line 2505 "zhy_symbol_map"
      {"kwaai6", 2497},
      {""},
#line 3740 "zhy_symbol_map"
      {"paat2", 3732},
      {""},
#line 2494 "zhy_symbol_map"
      {"kwaa2", 2486},
#line 2502 "zhy_symbol_map"
      {"kwaai3", 2494},
      {""}, {""}, {""}, {""},
#line 2501 "zhy_symbol_map"
      {"kwaai2", 2493},
      {""},
#line 3743 "zhy_symbol_map"
      {"paat5", 3735},
      {""},
#line 2497 "zhy_symbol_map"
      {"kwaa5", 2489},
#line 2504 "zhy_symbol_map"
      {"kwaai5", 2496},
      {""}, {""}, {""}, {""},
#line 2500 "zhy_symbol_map"
      {"kwaai1", 2492},
      {""},
#line 3739 "zhy_symbol_map"
      {"paat1", 3731},
      {""},
#line 2493 "zhy_symbol_map"
      {"kwaa1", 2485},
      {""}, {""},
#line 802 "zhy_symbol_map"
      {"dei4", 794},
#line 95 "zhy_symbol_map"
      {"an4", 87},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2513 "zhy_symbol_map"
      {"kwaak7", 2505},
      {""}, {""}, {""}, {""},
#line 2512 "zhy_symbol_map"
      {"kwaak6", 2504},
      {""}, {""}, {""}, {""},
#line 2509 "zhy_symbol_map"
      {"kwaak3", 2501},
      {""}, {""}, {""}, {""},
#line 2508 "zhy_symbol_map"
      {"kwaak2", 2500},
      {""}, {""}, {""}, {""},
#line 2511 "zhy_symbol_map"
      {"kwaak5", 2503},
#line 1162 "zhy_symbol_map"
      {"fui7", 1154},
      {""}, {""}, {""},
#line 2507 "zhy_symbol_map"
      {"kwaak1", 2499},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1860 "zhy_symbol_map"
      {"hip5", 1852},
      {""}, {""}, {""}, {""},
#line 1776 "zhy_symbol_map"
      {"hap5", 1768},
      {""}, {""}, {""}, {""}, {""},
#line 2979 "zhy_symbol_map"
      {"maat4", 2971},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3671 "zhy_symbol_map"
      {"ok3", 3663},
      {""}, {""}, {""}, {""},
#line 2562 "zhy_symbol_map"
      {"kwok7", 2554},
      {""},
#line 2027 "zhy_symbol_map"
      {"jai4", 2019},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2561 "zhy_symbol_map"
      {"kwok6", 2553},
      {""}, {""}, {""}, {""}, {""},
#line 355 "zhy_symbol_map"
      {"but5", 347},
      {""}, {""}, {""},
#line 2558 "zhy_symbol_map"
      {"kwok3", 2550},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2557 "zhy_symbol_map"
      {"kwok2", 2549},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2560 "zhy_symbol_map"
      {"kwok5", 2552},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2556 "zhy_symbol_map"
      {"kwok1", 2548},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1873 "zhy_symbol_map"
      {"hiu4", 1865},
      {""}, {""}, {""}, {""},
#line 1789 "zhy_symbol_map"
      {"hau4", 1781},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1126 "zhy_symbol_map"
      {"fiu6", 1118},
      {""},
#line 1123 "zhy_symbol_map"
      {"fiu3", 1115},
      {""}, {""},
#line 1084 "zhy_symbol_map"
      {"fau6", 1076},
#line 4183 "zhy_symbol_map"
      {"soet4", 4175},
#line 1081 "zhy_symbol_map"
      {"fau3", 1073},
      {""}, {""}, {""}, {""},
#line 1122 "zhy_symbol_map"
      {"fiu2", 1114},
      {""}, {""}, {""}, {""},
#line 1080 "zhy_symbol_map"
      {"fau2", 1072},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1150 "zhy_symbol_map"
      {"fu2", 1142},
      {""}, {""},
#line 1121 "zhy_symbol_map"
      {"fiu1", 1113},
      {""}, {""}, {""}, {""},
#line 1079 "zhy_symbol_map"
      {"fau1", 1071},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3371 "zhy_symbol_map"
      {"ngaap4", 3363},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1950 "zhy_symbol_map"
      {"hou4", 1942},
      {""}, {""}, {""}, {""}, {""},
#line 479 "zhy_symbol_map"
      {"ce3", 471},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1155 "zhy_symbol_map"
      {"fu7", 1147},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1127 "zhy_symbol_map"
      {"fiu7", 1119},
      {""}, {""}, {""}, {""},
#line 1085 "zhy_symbol_map"
      {"fau7", 1077},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4561 "zhy_symbol_map"
      {"waat4", 4553},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1880 "zhy_symbol_map"
      {"hm4", 1872},
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
#line 1149 "zhy_symbol_map"
      {"fu1", 1141},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1154 "zhy_symbol_map"
      {"fu6", 1146},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1362 "zhy_symbol_map"
      {"gim4", 1354},
      {""}, {""}, {""}, {""},
#line 1271 "zhy_symbol_map"
      {"gam4", 1263},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3828 "zhy_symbol_map"
      {"pet6", 3820},
      {""},
#line 3825 "zhy_symbol_map"
      {"pet3", 3817},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3824 "zhy_symbol_map"
      {"pet2", 3816},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1342 "zhy_symbol_map"
      {"gep5", 1334},
      {""},
#line 195 "zhy_symbol_map"
      {"bam6", 187},
      {""},
#line 192 "zhy_symbol_map"
      {"bam3", 184},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3823 "zhy_symbol_map"
      {"pet1", 3815},
#line 191 "zhy_symbol_map"
      {"bam2", 183},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 190 "zhy_symbol_map"
      {"bam1", 182},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1652 "zhy_symbol_map"
      {"gwun7", 1644},
      {""}, {""}, {""}, {""}, {""},
#line 794 "zhy_symbol_map"
      {"de3", 786},
      {""}, {""}, {""},
#line 1651 "zhy_symbol_map"
      {"gwun6", 1643},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1648 "zhy_symbol_map"
      {"gwun3", 1640},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1647 "zhy_symbol_map"
      {"gwun2", 1639},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1650 "zhy_symbol_map"
      {"gwun5", 1642},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1646 "zhy_symbol_map"
      {"gwun1", 1638},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3217 "zhy_symbol_map"
      {"naat4", 3209},
      {""},
#line 3829 "zhy_symbol_map"
      {"pet7", 3821},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 319 "zhy_symbol_map"
      {"bu4", 311},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 196 "zhy_symbol_map"
      {"bam7", 188},
      {""}, {""}, {""}, {""}, {""},
#line 3560 "zhy_symbol_map"
      {"nip4", 3552},
      {""}, {""}, {""}, {""},
#line 3266 "zhy_symbol_map"
      {"nap4", 3258},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2096 "zhy_symbol_map"
      {"ji3", 2088},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 32 "zhy_symbol_map"
      {"aam4", 24},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3903 "zhy_symbol_map"
      {"puk4", 3895},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 103 "zhy_symbol_map"
      {"ang5", 95},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3917 "zhy_symbol_map"
      {"pung4", 3909},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3799 "zhy_symbol_map"
      {"pe5", 3791},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2152 "zhy_symbol_map"
      {"jo3", 2144},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3910 "zhy_symbol_map"
      {"pun4", 3902},
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
#line 1167 "zhy_symbol_map"
      {"fuk5", 1159},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2742 "zhy_symbol_map"
      {"lem5", 2734},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4624 "zhy_symbol_map"
      {"wet4", 4616},
      {""}, {""}, {""}, {""},
#line 3685 "zhy_symbol_map"
      {"on3", 3677},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1174 "zhy_symbol_map"
      {"fun5", 1166},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2342 "zhy_symbol_map"
      {"keoi4", 2334},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2520 "zhy_symbol_map"
      {"kwaan7", 2512},
#line 2527 "zhy_symbol_map"
      {"kwaang7", 2519},
      {""}, {""}, {""},
#line 2519 "zhy_symbol_map"
      {"kwaan6", 2511},
#line 2526 "zhy_symbol_map"
      {"kwaang6", 2518},
      {""}, {""}, {""},
#line 2516 "zhy_symbol_map"
      {"kwaan3", 2508},
#line 2523 "zhy_symbol_map"
      {"kwaang3", 2515},
      {""}, {""}, {""},
#line 2515 "zhy_symbol_map"
      {"kwaan2", 2507},
#line 2522 "zhy_symbol_map"
      {"kwaang2", 2514},
      {""}, {""}, {""},
#line 2518 "zhy_symbol_map"
      {"kwaan5", 2510},
#line 2525 "zhy_symbol_map"
      {"kwaang5", 2517},
      {""}, {""}, {""},
#line 2514 "zhy_symbol_map"
      {"kwaan1", 2506},
#line 2521 "zhy_symbol_map"
      {"kwaang1", 2513},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4400 "zhy_symbol_map"
      {"tim4", 4392},
      {""}, {""}, {""}, {""},
#line 4316 "zhy_symbol_map"
      {"tam4", 4308},
      {""}, {""}, {""},
#line 1796 "zhy_symbol_map"
      {"he4", 1788},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4085 "zhy_symbol_map"
      {"seon4", 4077},
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
#line 1243 "zhy_symbol_map"
      {"gaat4", 1235},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1974 "zhy_symbol_map"
      {"hyun7", 1966},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1973 "zhy_symbol_map"
      {"hyun6", 1965},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1383 "zhy_symbol_map"
      {"gip4", 1375},
#line 4078 "zhy_symbol_map"
      {"seoi4", 4070},
      {""},
#line 1970 "zhy_symbol_map"
      {"hyun3", 1962},
      {""},
#line 1292 "zhy_symbol_map"
      {"gap4", 1284},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1969 "zhy_symbol_map"
      {"hyun2", 1961},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1972 "zhy_symbol_map"
      {"hyun5", 1964},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1968 "zhy_symbol_map"
      {"hyun1", 1960},
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
#line 168 "zhy_symbol_map"
      {"baat7", 160},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 167 "zhy_symbol_map"
      {"baat6", 159},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2113 "zhy_symbol_map"
      {"jim6", 2105},
#line 164 "zhy_symbol_map"
      {"baat3", 156},
#line 2110 "zhy_symbol_map"
      {"jim3", 2102},
      {""}, {""},
#line 2036 "zhy_symbol_map"
      {"jam6", 2028},
      {""},
#line 2033 "zhy_symbol_map"
      {"jam3", 2025},
      {""}, {""}, {""},
#line 163 "zhy_symbol_map"
      {"baat2", 155},
#line 2109 "zhy_symbol_map"
      {"jim2", 2101},
      {""}, {""}, {""}, {""},
#line 2032 "zhy_symbol_map"
      {"jam2", 2024},
      {""},
#line 3434 "zhy_symbol_map"
      {"ngat4", 3426},
      {""},
#line 166 "zhy_symbol_map"
      {"baat5", 158},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 162 "zhy_symbol_map"
      {"baat1", 154},
      {""}, {""}, {""}, {""}, {""},
#line 2108 "zhy_symbol_map"
      {"jim1", 2100},
      {""}, {""}, {""}, {""},
#line 2031 "zhy_symbol_map"
      {"jam1", 2023},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1866 "zhy_symbol_map"
      {"hit4", 1858},
      {""}, {""}, {""}, {""},
#line 1782 "zhy_symbol_map"
      {"hat4", 1774},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1119 "zhy_symbol_map"
      {"fit6", 1111},
      {""},
#line 1116 "zhy_symbol_map"
      {"fit3", 1108},
      {""}, {""},
#line 1077 "zhy_symbol_map"
      {"fat6", 1069},
      {""},
#line 1074 "zhy_symbol_map"
      {"fat3", 1066},
      {""}, {""}, {""}, {""},
#line 1115 "zhy_symbol_map"
      {"fit2", 1107},
      {""},
#line 3896 "zhy_symbol_map"
      {"pui4", 3888},
      {""}, {""},
#line 1073 "zhy_symbol_map"
      {"fat2", 1065},
      {""},
#line 53 "zhy_symbol_map"
      {"aap4", 45},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1114 "zhy_symbol_map"
      {"fit1", 1106},
      {""}, {""}, {""}, {""},
#line 1072 "zhy_symbol_map"
      {"fat1", 1064},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2114 "zhy_symbol_map"
      {"jim7", 2106},
      {""}, {""}, {""}, {""},
#line 2037 "zhy_symbol_map"
      {"jam7", 2029},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1943 "zhy_symbol_map"
      {"hot4", 1935},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1120 "zhy_symbol_map"
      {"fit7", 1112},
      {""}, {""}, {""}, {""},
#line 1078 "zhy_symbol_map"
      {"fat7", 1070},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1526 "zhy_symbol_map"
      {"gwaa7", 1518},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1525 "zhy_symbol_map"
      {"gwaa6", 1517},
      {""}, {""}, {""}, {""}, {""},
#line 2013 "zhy_symbol_map"
      {"jaap4", 2005},
      {""}, {""}, {""},
#line 1522 "zhy_symbol_map"
      {"gwaa3", 1514},
#line 1533 "zhy_symbol_map"
      {"gwaai7", 1525},
      {""}, {""}, {""}, {""},
#line 1532 "zhy_symbol_map"
      {"gwaai6", 1524},
      {""}, {""}, {""},
#line 1521 "zhy_symbol_map"
      {"gwaa2", 1513},
#line 1529 "zhy_symbol_map"
      {"gwaai3", 1521},
      {""}, {""}, {""}, {""},
#line 1528 "zhy_symbol_map"
      {"gwaai2", 1520},
      {""}, {""}, {""},
#line 1524 "zhy_symbol_map"
      {"gwaa5", 1516},
#line 1531 "zhy_symbol_map"
      {"gwaai5", 1523},
      {""}, {""}, {""}, {""},
#line 1527 "zhy_symbol_map"
      {"gwaai1", 1519},
      {""}, {""}, {""},
#line 1520 "zhy_symbol_map"
      {"gwaa1", 1512},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1540 "zhy_symbol_map"
      {"gwaak7", 1532},
      {""}, {""}, {""}, {""},
#line 1539 "zhy_symbol_map"
      {"gwaak6", 1531},
      {""}, {""}, {""}, {""},
#line 1536 "zhy_symbol_map"
      {"gwaak3", 1528},
      {""}, {""}, {""}, {""},
#line 1535 "zhy_symbol_map"
      {"gwaak2", 1527},
      {""}, {""}, {""}, {""},
#line 1538 "zhy_symbol_map"
      {"gwaak5", 1530},
      {""}, {""}, {""},
#line 1160 "zhy_symbol_map"
      {"fui5", 1152},
#line 1534 "zhy_symbol_map"
      {"gwaak1", 1526},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2790 "zhy_symbol_map"
      {"lim4", 2782},
      {""}, {""}, {""}, {""},
#line 2678 "zhy_symbol_map"
      {"lam4", 2670},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1630 "zhy_symbol_map"
      {"gwo6", 1622},
      {""},
#line 1627 "zhy_symbol_map"
      {"gwo3", 1619},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1626 "zhy_symbol_map"
      {"gwo2", 1618},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1625 "zhy_symbol_map"
      {"gwo1", 1617},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3861 "zhy_symbol_map"
      {"piu4", 3853},
      {""}, {""},
#line 1638 "zhy_symbol_map"
      {"gwok7", 1630},
      {""},
#line 3791 "zhy_symbol_map"
      {"pau4", 3783},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1637 "zhy_symbol_map"
      {"gwok6", 1629},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1634 "zhy_symbol_map"
      {"gwok3", 1626},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1633 "zhy_symbol_map"
      {"gwok2", 1625},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1636 "zhy_symbol_map"
      {"gwok5", 1628},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1632 "zhy_symbol_map"
      {"gwok1", 1624},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1768 "zhy_symbol_map"
      {"hao4", 1760},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1132 "zhy_symbol_map"
      {"fo5", 1124},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1631 "zhy_symbol_map"
      {"gwo7", 1623},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3889 "zhy_symbol_map"
      {"pou4", 3881},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4295 "zhy_symbol_map"
      {"taat4", 4287},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4421 "zhy_symbol_map"
      {"tip4", 4413},
      {""}, {""}, {""}, {""},
#line 4337 "zhy_symbol_map"
      {"tap4", 4329},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 80 "zhy_symbol_map"
      {"ak3", 72},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1125 "zhy_symbol_map"
      {"fiu5", 1117},
      {""}, {""}, {""}, {""},
#line 1083 "zhy_symbol_map"
      {"fau5", 1075},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3308 "zhy_symbol_map"
      {"neoi4", 3300},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 333 "zhy_symbol_map"
      {"buk4", 325},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 347 "zhy_symbol_map"
      {"bung4", 339},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 229 "zhy_symbol_map"
      {"be5", 221},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3462 "zhy_symbol_map"
      {"ngit4", 3454},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 340 "zhy_symbol_map"
      {"bun4", 332},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2134 "zhy_symbol_map"
      {"jip6", 2126},
      {""},
#line 2131 "zhy_symbol_map"
      {"jip3", 2123},
      {""}, {""},
#line 2050 "zhy_symbol_map"
      {"jap6", 2042},
      {""},
#line 2047 "zhy_symbol_map"
      {"jap3", 2039},
      {""}, {""}, {""}, {""},
#line 2130 "zhy_symbol_map"
      {"jip2", 2122},
      {""}, {""}, {""}, {""},
#line 2046 "zhy_symbol_map"
      {"jap2", 2038},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2569 "zhy_symbol_map"
      {"kwong7", 2561},
      {""}, {""}, {""}, {""},
#line 2568 "zhy_symbol_map"
      {"kwong6", 2560},
      {""}, {""},
#line 2129 "zhy_symbol_map"
      {"jip1", 2121},
      {""},
#line 2565 "zhy_symbol_map"
      {"kwong3", 2557},
      {""}, {""},
#line 2045 "zhy_symbol_map"
      {"jap1", 2037},
      {""},
#line 2564 "zhy_symbol_map"
      {"kwong2", 2556},
      {""}, {""}, {""}, {""},
#line 2567 "zhy_symbol_map"
      {"kwong5", 2559},
      {""}, {""}, {""}, {""},
#line 2563 "zhy_symbol_map"
      {"kwong1", 2555},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 3827 "zhy_symbol_map"
      {"pet5", 3819},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 194 "zhy_symbol_map"
      {"bam5", 186},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4876 "zhy_symbol_map"
      {"zim4", 4868},
      {""}, {""}, {""}, {""},
#line 4778 "zhy_symbol_map"
      {"zam4", 4770},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2534 "zhy_symbol_map"
      {"kwaat7", 2526},
#line 2135 "zhy_symbol_map"
      {"jip7", 2127},
      {""}, {""}, {""},
#line 2533 "zhy_symbol_map"
      {"kwaat6", 2525},
#line 2051 "zhy_symbol_map"
      {"jap7", 2043},
      {""}, {""}, {""},
#line 2530 "zhy_symbol_map"
      {"kwaat3", 2522},
      {""}, {""},
#line 1579 "zhy_symbol_map"
      {"gwang4", 1571},
      {""},
#line 2529 "zhy_symbol_map"
      {"kwaat2", 2521},
      {""}, {""}, {""}, {""},
#line 2532 "zhy_symbol_map"
      {"kwaat5", 2524},
      {""}, {""}, {""}, {""},
#line 2528 "zhy_symbol_map"
      {"kwaat1", 2520},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4232 "zhy_symbol_map"
      {"syu4", 4224},
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
      {""}, {""},
#line 4513 "zhy_symbol_map"
      {"uk5", 4505},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2587 "zhy_symbol_map"
      {"kyun4", 2579},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2650 "zhy_symbol_map"
      {"laat4", 2642},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2811 "zhy_symbol_map"
      {"lip4", 2803},
      {""}, {""}, {""}, {""},
#line 2699 "zhy_symbol_map"
      {"lap4", 2691},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1810 "zhy_symbol_map"
      {"hek4", 1802},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1817 "zhy_symbol_map"
      {"heng4", 1809},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1334 "zhy_symbol_map"
      {"geoi4", 1326},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1547 "zhy_symbol_map"
      {"gwaan7", 1539},
#line 1554 "zhy_symbol_map"
      {"gwaang7", 1546},
      {""}, {""}, {""},
#line 1546 "zhy_symbol_map"
      {"gwaan6", 1538},
#line 1553 "zhy_symbol_map"
      {"gwaang6", 1545},
      {""}, {""}, {""},
#line 1543 "zhy_symbol_map"
      {"gwaan3", 1535},
#line 1550 "zhy_symbol_map"
      {"gwaang3", 1542},
      {""}, {""}, {""},
#line 1542 "zhy_symbol_map"
      {"gwaan2", 1534},
#line 1549 "zhy_symbol_map"
      {"gwaang2", 1541},
      {""}, {""}, {""},
#line 1545 "zhy_symbol_map"
      {"gwaan5", 1537},
#line 1552 "zhy_symbol_map"
      {"gwaang5", 1544},
      {""}, {""}, {""},
#line 1541 "zhy_symbol_map"
      {"gwaan1", 1533},
#line 1548 "zhy_symbol_map"
      {"gwaang1", 1540},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3798 "zhy_symbol_map"
      {"pe4", 3790},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 4239 "zhy_symbol_map"
      {"syun4", 4231},
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
#line 94 "zhy_symbol_map"
      {"an3", 86},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2867 "zhy_symbol_map"
      {"loet4", 2859},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 326 "zhy_symbol_map"
      {"bui4", 318},
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
      {""}, {""},
#line 2174 "zhy_symbol_map"
      {"juk4", 2166},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2181 "zhy_symbol_map"
      {"jung4", 2173},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2070 "zhy_symbol_map"
      {"je5", 2062},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3854 "zhy_symbol_map"
      {"pit4", 3846},
      {""}, {""}, {""}, {""},
#line 3784 "zhy_symbol_map"
      {"pat4", 3776},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4750 "zhy_symbol_map"
      {"zaat4", 4742},
      {""}, {""},
#line 1614 "zhy_symbol_map"
      {"gwing4", 1606},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 4897 "zhy_symbol_map"
      {"zip4", 4889},
      {""}, {""}, {""}, {""},
#line 4799 "zhy_symbol_map"
      {"zap4", 4791},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1879 "zhy_symbol_map"
      {"hm3", 1871},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2112 "zhy_symbol_map"
      {"jim5", 2104},
      {""}, {""},
#line 284 "zhy_symbol_map"
      {"biu4", 276},
      {""},
#line 2035 "zhy_symbol_map"
      {"jam5", 2027},
      {""}, {""},
#line 221 "zhy_symbol_map"
      {"bau4", 213},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4379 "zhy_symbol_map"
      {"teon4", 4371},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1118 "zhy_symbol_map"
      {"fit5", 1110},
      {""}, {""}, {""}, {""},
#line 1076 "zhy_symbol_map"
      {"fat5", 1068},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 312 "zhy_symbol_map"
      {"bou4", 304},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4372 "zhy_symbol_map"
      {"teoi4", 4364},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1803 "zhy_symbol_map"
      {"hei4", 1795},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 318 "zhy_symbol_map"
      {"bu3", 310},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1098 "zhy_symbol_map"
      {"fei6", 1090},
      {""},
#line 1095 "zhy_symbol_map"
      {"fei3", 1087},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1094 "zhy_symbol_map"
      {"fei2", 1086},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1093 "zhy_symbol_map"
      {"fei1", 1085},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3644 "zhy_symbol_map"
      {"nyun4", 3636},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1629 "zhy_symbol_map"
      {"gwo5", 1621},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1099 "zhy_symbol_map"
      {"fei7", 1091},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2093 "zhy_symbol_map"
      {"jeon7", 2085},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2092 "zhy_symbol_map"
      {"jeon6", 2084},
      {""}, {""}, {""},
#line 536 "zhy_symbol_map"
      {"cim4", 528},
      {""}, {""}, {""}, {""},
#line 438 "zhy_symbol_map"
      {"cam4", 430},
#line 2089 "zhy_symbol_map"
      {"jeon3", 2081},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2088 "zhy_symbol_map"
      {"jeon2", 2080},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2091 "zhy_symbol_map"
      {"jeon5", 2083},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2087 "zhy_symbol_map"
      {"jeon1", 2079},
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
#line 2086 "zhy_symbol_map"
      {"jeoi7", 2078},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2085 "zhy_symbol_map"
      {"jeoi6", 2077},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2082 "zhy_symbol_map"
      {"jeoi3", 2074},
      {""}, {""}, {""}, {""},
#line 4095 "zhy_symbol_map"
      {"seot7", 4087},
      {""}, {""}, {""}, {""},
#line 2081 "zhy_symbol_map"
      {"jeoi2", 2073},
      {""}, {""}, {""}, {""},
#line 4094 "zhy_symbol_map"
      {"seot6", 4086},
      {""}, {""}, {""}, {""},
#line 2084 "zhy_symbol_map"
      {"jeoi5", 2076},
      {""}, {""}, {""}, {""},
#line 4091 "zhy_symbol_map"
      {"seot3", 4083},
      {""}, {""}, {""}, {""},
#line 2080 "zhy_symbol_map"
      {"jeoi1", 2072},
      {""}, {""}, {""}, {""},
#line 4090 "zhy_symbol_map"
      {"seot2", 4082},
      {""}, {""},
#line 1087 "zhy_symbol_map"
      {"fe2", 1079},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4093 "zhy_symbol_map"
      {"seot5", 4085},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4089 "zhy_symbol_map"
      {"seot1", 4081},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2545 "zhy_symbol_map"
      {"kwan4", 2537},
      {""}, {""}, {""}, {""},
#line 1092 "zhy_symbol_map"
      {"fe7", 1084},
      {""}, {""},
#line 2762 "zhy_symbol_map"
      {"leon4", 2754},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1131 "zhy_symbol_map"
      {"fo4", 1123},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 865 "zhy_symbol_map"
      {"dim4", 857},
      {""}, {""}, {""}, {""},
#line 753 "zhy_symbol_map"
      {"dam4", 745},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1645 "zhy_symbol_map"
      {"gwong7", 1637},
      {""}, {""}, {""}, {""},
#line 1644 "zhy_symbol_map"
      {"gwong6", 1636},
      {""}, {""}, {""}, {""},
#line 1641 "zhy_symbol_map"
      {"gwong3", 1633},
      {""}, {""}, {""}, {""},
#line 1640 "zhy_symbol_map"
      {"gwong2", 1632},
      {""}, {""}, {""}, {""},
#line 1643 "zhy_symbol_map"
      {"gwong5", 1635},
      {""}, {""}, {""}, {""},
#line 1639 "zhy_symbol_map"
      {"gwong1", 1631},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1795 "zhy_symbol_map"
      {"he3", 1787},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2538 "zhy_symbol_map"
      {"kwai4", 2530},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2755 "zhy_symbol_map"
      {"leoi4", 2747},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1086 "zhy_symbol_map"
      {"fe1", 1078},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1091 "zhy_symbol_map"
      {"fe6", 1083},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 844 "zhy_symbol_map"
      {"deu4", 836},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1561 "zhy_symbol_map"
      {"gwaat7", 1553},
      {""}, {""}, {""},
#line 2133 "zhy_symbol_map"
      {"jip5", 2125},
#line 1560 "zhy_symbol_map"
      {"gwaat6", 1552},
      {""}, {""}, {""},
#line 2049 "zhy_symbol_map"
      {"jap5", 2041},
#line 1557 "zhy_symbol_map"
      {"gwaat3", 1549},
      {""}, {""}, {""}, {""},
#line 1556 "zhy_symbol_map"
      {"gwaat2", 1548},
      {""}, {""}, {""}, {""},
#line 1559 "zhy_symbol_map"
      {"gwaat5", 1551},
      {""}, {""}, {""}, {""},
#line 1555 "zhy_symbol_map"
      {"gwaat1", 1547},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2146 "zhy_symbol_map"
      {"jiu4", 2138},
      {""}, {""}, {""}, {""},
#line 2062 "zhy_symbol_map"
      {"jau4", 2054},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 228 "zhy_symbol_map"
      {"be4", 220},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1656 "zhy_symbol_map"
      {"gyun4", 1648},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4521 "zhy_symbol_map"
      {"ung6", 4513},
      {""},
#line 4518 "zhy_symbol_map"
      {"ung3", 4510},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2552 "zhy_symbol_map"
      {"kwik4", 2544},
#line 4517 "zhy_symbol_map"
      {"ung2", 4509},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3812 "zhy_symbol_map"
      {"pek4", 3804},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4516 "zhy_symbol_map"
      {"ung1", 4508},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3819 "zhy_symbol_map"
      {"peng4", 3811},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4522 "zhy_symbol_map"
      {"ung7", 4514},
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
#line 410 "zhy_symbol_map"
      {"caat4", 402},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 557 "zhy_symbol_map"
      {"cip4", 549},
#line 4848 "zhy_symbol_map"
      {"zeon4", 4840},
      {""}, {""}, {""},
#line 459 "zhy_symbol_map"
      {"cap4", 451},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 277 "zhy_symbol_map"
      {"bit4", 269},
      {""}, {""}, {""}, {""},
#line 214 "zhy_symbol_map"
      {"bat4", 206},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4512 "zhy_symbol_map"
      {"uk4", 4504},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3318 "zhy_symbol_map"
      {"neot7", 3310},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3317 "zhy_symbol_map"
      {"neot6", 3309},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3314 "zhy_symbol_map"
      {"neot3", 3306},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3313 "zhy_symbol_map"
      {"neot2", 3305},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3316 "zhy_symbol_map"
      {"neot5", 3308},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3312 "zhy_symbol_map"
      {"neot1", 3304},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4841 "zhy_symbol_map"
      {"zeoi4", 4833},
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
      {""}, {""},
#line 725 "zhy_symbol_map"
      {"daat4", 717},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 886 "zhy_symbol_map"
      {"dip4", 878},
      {""}, {""}, {""}, {""},
#line 774 "zhy_symbol_map"
      {"dap4", 766},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2190 "zhy_symbol_map"
      {"jyu6", 2182},
      {""},
#line 2187 "zhy_symbol_map"
      {"jyu3", 2179},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2186 "zhy_symbol_map"
      {"jyu2", 2178},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2185 "zhy_symbol_map"
      {"jyu1", 2177},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 4498 "zhy_symbol_map"
      {"tyun4", 4490},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2503 "zhy_symbol_map"
      {"kwaai4", 2495},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2510 "zhy_symbol_map"
      {"kwaak4", 2502},
      {""}, {""}, {""}, {""}, {""},
#line 2191 "zhy_symbol_map"
      {"jyu7", 2183},
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
#line 3805 "zhy_symbol_map"
      {"pei4", 3797},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2069 "zhy_symbol_map"
      {"je4", 2061},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2597 "zhy_symbol_map"
      {"kyut7", 2589},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2596 "zhy_symbol_map"
      {"kyut6", 2588},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2593 "zhy_symbol_map"
      {"kyut3", 2585},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2592 "zhy_symbol_map"
      {"kyut2", 2584},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2595 "zhy_symbol_map"
      {"kyut5", 2587},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2591 "zhy_symbol_map"
      {"kyut1", 2583},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1189 "zhy_symbol_map"
      {"fut6", 1181},
      {""},
#line 1186 "zhy_symbol_map"
      {"fut3", 1178},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1185 "zhy_symbol_map"
      {"fut2", 1177},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1047 "zhy_symbol_map"
      {"faau4", 1039},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1184 "zhy_symbol_map"
      {"fut1", 1176},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2198 "zhy_symbol_map"
      {"jyun7", 2190},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2197 "zhy_symbol_map"
      {"jyun6", 2189},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2194 "zhy_symbol_map"
      {"jyun3", 2186},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2193 "zhy_symbol_map"
      {"jyun2", 2185},
      {""}, {""}, {""},
#line 1103 "zhy_symbol_map"
      {"fik4", 1095},
#line 1026 "zhy_symbol_map"
      {"faak4", 1018},
      {""}, {""}, {""}, {""},
#line 2196 "zhy_symbol_map"
      {"jyun5", 2188},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2192 "zhy_symbol_map"
      {"jyun1", 2184},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1097 "zhy_symbol_map"
      {"fei5", 1089},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1110 "zhy_symbol_map"
      {"fing4", 1102},
      {""}, {""}, {""},
#line 1012 "zhy_symbol_map"
      {"faa4", 1004},
#line 1068 "zhy_symbol_map"
      {"fang4", 1060},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1190 "zhy_symbol_map"
      {"fut7", 1182},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1061 "zhy_symbol_map"
      {"fan4", 1053},
#line 4249 "zhy_symbol_map"
      {"syut7", 4241},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4248 "zhy_symbol_map"
      {"syut6", 4240},
      {""}, {""}, {""},
#line 1138 "zhy_symbol_map"
      {"fok4", 1130},
      {""}, {""}, {""}, {""}, {""},
#line 4245 "zhy_symbol_map"
      {"syut3", 4237},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4244 "zhy_symbol_map"
      {"syut2", 4236},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4247 "zhy_symbol_map"
      {"syut5", 4239},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4243 "zhy_symbol_map"
      {"syut1", 4235},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2139 "zhy_symbol_map"
      {"jit4", 2131},
      {""}, {""}, {""}, {""},
#line 2055 "zhy_symbol_map"
      {"jat4", 2047},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1145 "zhy_symbol_map"
      {"fong4", 1137},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1593 "zhy_symbol_map"
      {"gwe4", 1585},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1153 "zhy_symbol_map"
      {"fu5", 1145},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2923 "zhy_symbol_map"
      {"lyun4", 2915},
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
#line 1572 "zhy_symbol_map"
      {"gwan4", 1564},
      {""}, {""}, {""},
#line 1889 "zhy_symbol_map"
      {"hng6", 1881},
      {""},
#line 1886 "zhy_symbol_map"
      {"hng3", 1878},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1885 "zhy_symbol_map"
      {"hng2", 1877},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1884 "zhy_symbol_map"
      {"hng1", 1876},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 3797 "zhy_symbol_map"
      {"pe3", 3789},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1565 "zhy_symbol_map"
      {"gwai4", 1557},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 242 "zhy_symbol_map"
      {"bek4", 234},
#line 1033 "zhy_symbol_map"
      {"faan4", 1025},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1890 "zhy_symbol_map"
      {"hng7", 1882},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 249 "zhy_symbol_map"
      {"beng4", 241},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 5002 "zhy_symbol_map"
      {"zyu4", 4994},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1019 "zhy_symbol_map"
      {"faai4", 1011},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 508 "zhy_symbol_map"
      {"ceon4", 500},
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
#line 501 "zhy_symbol_map"
      {"ceoi4", 493},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1607 "zhy_symbol_map"
      {"gwik4", 1599},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2517 "zhy_symbol_map"
      {"kwaan4", 2509},
#line 2524 "zhy_symbol_map"
      {"kwaang4", 2516},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1600 "zhy_symbol_map"
      {"gwi4", 1592},
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
#line 4520 "zhy_symbol_map"
      {"ung5", 4512},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 830 "zhy_symbol_map"
      {"deon4", 822},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 5009 "zhy_symbol_map"
      {"zyun4", 5001},
      {""}, {""}, {""}, {""}, {""},
#line 1054 "zhy_symbol_map"
      {"fai4", 1046},
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
#line 823 "zhy_symbol_map"
      {"deoi4", 815},
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
      {""},
#line 2772 "zhy_symbol_map"
      {"leot7", 2764},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2771 "zhy_symbol_map"
      {"leot6", 2763},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 235 "zhy_symbol_map"
      {"bei4", 227},
#line 2768 "zhy_symbol_map"
      {"leot3", 2760},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2767 "zhy_symbol_map"
      {"leot2", 2759},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2770 "zhy_symbol_map"
      {"leot5", 2762},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2766 "zhy_symbol_map"
      {"leot1", 2758},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1530 "zhy_symbol_map"
      {"gwaai4", 1522},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1537 "zhy_symbol_map"
      {"gwaak4", 1529},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2189 "zhy_symbol_map"
      {"jyu5", 2181},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2076 "zhy_symbol_map"
      {"jeng4", 2068},
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
#line 1130 "zhy_symbol_map"
      {"fo3", 1122},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1666 "zhy_symbol_map"
      {"gyut7", 1658},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 3924 "zhy_symbol_map"
      {"put4", 3916},
      {""},
#line 1665 "zhy_symbol_map"
      {"gyut6", 1657},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1662 "zhy_symbol_map"
      {"gyut3", 1654},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1661 "zhy_symbol_map"
      {"gyut2", 1653},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1664 "zhy_symbol_map"
      {"gyut5", 1656},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1660 "zhy_symbol_map"
      {"gyut1", 1652},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 3693 "zhy_symbol_map"
      {"ong4", 3685},
      {""}, {""}, {""}, {""},
#line 2349 "zhy_symbol_map"
      {"kep4", 2341},
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
#line 1188 "zhy_symbol_map"
      {"fut5", 1180},
      {""},
#line 227 "zhy_symbol_map"
      {"be3", 219},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 655 "zhy_symbol_map"
      {"cyu4", 647},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4858 "zhy_symbol_map"
      {"zeot7", 4850},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4857 "zhy_symbol_map"
      {"zeot6", 4849},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4854 "zhy_symbol_map"
      {"zeot3", 4846},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4853 "zhy_symbol_map"
      {"zeot2", 4845},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4856 "zhy_symbol_map"
      {"zeot5", 4848},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4852 "zhy_symbol_map"
      {"zeot1", 4844},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2566 "zhy_symbol_map"
      {"kwong4", 2558},
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
#line 2531 "zhy_symbol_map"
      {"kwaat4", 2523},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1838 "zhy_symbol_map"
      {"him4", 1830},
      {""}, {""}, {""}, {""},
#line 1747 "zhy_symbol_map"
      {"ham4", 1739},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 4511 "zhy_symbol_map"
      {"uk3", 4503},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1888 "zhy_symbol_map"
      {"hng5", 1880},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 662 "zhy_symbol_map"
      {"cyun4", 654},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4508 "zhy_symbol_map"
      {"tyut7", 4500},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4507 "zhy_symbol_map"
      {"tyut6", 4499},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4504 "zhy_symbol_map"
      {"tyut3", 4496},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4503 "zhy_symbol_map"
      {"tyut2", 4495},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1152 "zhy_symbol_map"
      {"fu4", 1144},
      {""},
#line 4506 "zhy_symbol_map"
      {"tyut5", 4498},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4502 "zhy_symbol_map"
      {"tyut1", 4494},
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
#line 1544 "zhy_symbol_map"
      {"gwaan4", 1536},
#line 1551 "zhy_symbol_map"
      {"gwaang4", 1543},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 977 "zhy_symbol_map"
      {"dyun4", 969},
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
      {""}, {""},
#line 2068 "zhy_symbol_map"
      {"je3", 2060},
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
#line 2933 "zhy_symbol_map"
      {"lyut7", 2925},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2932 "zhy_symbol_map"
      {"lyut6", 2924},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2929 "zhy_symbol_map"
      {"lyut3", 2921},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2928 "zhy_symbol_map"
      {"lyut2", 2920},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2931 "zhy_symbol_map"
      {"lyut5", 2923},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2927 "zhy_symbol_map"
      {"lyut1", 2919},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1719 "zhy_symbol_map"
      {"haat4", 1711},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1589 "zhy_symbol_map"
      {"gwat7", 1581},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1588 "zhy_symbol_map"
      {"gwat6", 1580},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1585 "zhy_symbol_map"
      {"gwat3", 1577},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1859 "zhy_symbol_map"
      {"hip4", 1851},
      {""}, {""},
#line 1584 "zhy_symbol_map"
      {"gwat2", 1576},
      {""},
#line 1775 "zhy_symbol_map"
      {"hap4", 1767},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1587 "zhy_symbol_map"
      {"gwat5", 1579},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1583 "zhy_symbol_map"
      {"gwat1", 1575},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 354 "zhy_symbol_map"
      {"but4", 346},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1043 "zhy_symbol_map"
      {"faat7", 1035},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1042 "zhy_symbol_map"
      {"faat6", 1034},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1039 "zhy_symbol_map"
      {"faat3", 1031},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1038 "zhy_symbol_map"
      {"faat2", 1030},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1041 "zhy_symbol_map"
      {"faat5", 1033},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1037 "zhy_symbol_map"
      {"faat1", 1029},
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
#line 518 "zhy_symbol_map"
      {"ceot7", 510},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 517 "zhy_symbol_map"
      {"ceot6", 509},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 514 "zhy_symbol_map"
      {"ceot3", 506},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 513 "zhy_symbol_map"
      {"ceot2", 505},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 516 "zhy_symbol_map"
      {"ceot5", 508},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 512 "zhy_symbol_map"
      {"ceot1", 504},
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
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1341 "zhy_symbol_map"
      {"gep4", 1333},
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
#line 840 "zhy_symbol_map"
      {"deot7", 832},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 839 "zhy_symbol_map"
      {"deot6", 831},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2580 "zhy_symbol_map"
      {"kwun4", 2572},
      {""}, {""},
#line 836 "zhy_symbol_map"
      {"deot3", 828},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 835 "zhy_symbol_map"
      {"deot2", 827},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 838 "zhy_symbol_map"
      {"deot5", 830},
      {""},
#line 5019 "zhy_symbol_map"
      {"zyut7", 5011},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 834 "zhy_symbol_map"
      {"deot1", 826},
      {""},
#line 5018 "zhy_symbol_map"
      {"zyut6", 5010},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5015 "zhy_symbol_map"
      {"zyut3", 5007},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5014 "zhy_symbol_map"
      {"zyut2", 5006},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5017 "zhy_symbol_map"
      {"zyut5", 5009},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 5013 "zhy_symbol_map"
      {"zyut1", 5005},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2573 "zhy_symbol_map"
      {"kwui4", 2565},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 102 "zhy_symbol_map"
      {"ang4", 94},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1642 "zhy_symbol_map"
      {"gwong4", 1634},
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
#line 1624 "zhy_symbol_map"
      {"gwit7", 1616},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1623 "zhy_symbol_map"
      {"gwit6", 1615},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1620 "zhy_symbol_map"
      {"gwit3", 1612},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1619 "zhy_symbol_map"
      {"gwit2", 1611},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1622 "zhy_symbol_map"
      {"gwit5", 1614},
#line 1558 "zhy_symbol_map"
      {"gwaat4", 1550},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1618 "zhy_symbol_map"
      {"gwit1", 1610},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1166 "zhy_symbol_map"
      {"fuk4", 1158},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2741 "zhy_symbol_map"
      {"lem4", 2733},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1180 "zhy_symbol_map"
      {"fung4", 1172},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1090 "zhy_symbol_map"
      {"fe5", 1082},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1173 "zhy_symbol_map"
      {"fun4", 1165},
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
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1824 "zhy_symbol_map"
      {"heoi4", 1816},
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
#line 3742 "zhy_symbol_map"
      {"paat4", 3734},
      {""},
#line 2496 "zhy_symbol_map"
      {"kwaa4", 2488},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1159 "zhy_symbol_map"
      {"fui4", 1151},
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
      {""}, {""},
#line 2559 "zhy_symbol_map"
      {"kwok4", 2551},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 672 "zhy_symbol_map"
      {"cyut7", 664},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 671 "zhy_symbol_map"
      {"cyut6", 663},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 668 "zhy_symbol_map"
      {"cyut3", 660},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 667 "zhy_symbol_map"
      {"cyut2", 659},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 670 "zhy_symbol_map"
      {"cyut5", 662},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 666 "zhy_symbol_map"
      {"cyut1", 658},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1124 "zhy_symbol_map"
      {"fiu4", 1116},
      {""}, {""}, {""}, {""},
#line 1082 "zhy_symbol_map"
      {"fau4", 1074},
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
      {""}, {""}, {""}, {""}, {""}, {""},
#line 987 "zhy_symbol_map"
      {"dyut7", 979},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 986 "zhy_symbol_map"
      {"dyut6", 978},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 983 "zhy_symbol_map"
      {"dyut3", 975},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 982 "zhy_symbol_map"
      {"dyut2", 974},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 985 "zhy_symbol_map"
      {"dyut5", 977},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 981 "zhy_symbol_map"
      {"dyut1", 973},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1151 "zhy_symbol_map"
      {"fu3", 1143},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1649 "zhy_symbol_map"
      {"gwun4", 1641},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 3826 "zhy_symbol_map"
      {"pet4", 3818},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 193 "zhy_symbol_map"
      {"bam4", 185},
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
#line 1089 "zhy_symbol_map"
      {"fe4", 1081},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1971 "zhy_symbol_map"
      {"hyun4", 1963},
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
#line 165 "zhy_symbol_map"
      {"baat4", 157},
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
#line 2111 "zhy_symbol_map"
      {"jim4", 2103},
      {""}, {""}, {""}, {""},
#line 2034 "zhy_symbol_map"
      {"jam4", 2026},
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
#line 1117 "zhy_symbol_map"
      {"fit4", 1109},
      {""}, {""}, {""}, {""},
#line 1075 "zhy_symbol_map"
      {"fat4", 1067},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1523 "zhy_symbol_map"
      {"gwaa4", 1515},
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
#line 1635 "zhy_symbol_map"
      {"gwok4", 1627},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1628 "zhy_symbol_map"
      {"gwo4", 1620},
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
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2132 "zhy_symbol_map"
      {"jip4", 2124},
      {""}, {""}, {""}, {""},
#line 2048 "zhy_symbol_map"
      {"jap4", 2040},
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
#line 1096 "zhy_symbol_map"
      {"fei4", 1088},
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
#line 2090 "zhy_symbol_map"
      {"jeon4", 2082},
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
#line 2083 "zhy_symbol_map"
      {"jeoi4", 2075},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 4092 "zhy_symbol_map"
      {"seot4", 4084},
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
#line 1981 "zhy_symbol_map"
      {"hyut7", 1973},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1980 "zhy_symbol_map"
      {"hyut6", 1972},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1977 "zhy_symbol_map"
      {"hyut3", 1969},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1976 "zhy_symbol_map"
      {"hyut2", 1968},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1979 "zhy_symbol_map"
      {"hyut5", 1971},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1975 "zhy_symbol_map"
      {"hyut1", 1967},
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
#line 1088 "zhy_symbol_map"
      {"fe3", 1080},
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
#line 4519 "zhy_symbol_map"
      {"ung4", 4511},
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
      {""}, {""},
#line 3315 "zhy_symbol_map"
      {"neot4", 3307},
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
      {""}, {""}, {""}, {""}, {""},
#line 2188 "zhy_symbol_map"
      {"jyu4", 2180},
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
      {""},
#line 2594 "zhy_symbol_map"
      {"kyut4", 2586},
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
#line 2195 "zhy_symbol_map"
      {"jyun4", 2187},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1187 "zhy_symbol_map"
      {"fut4", 1179},
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
#line 4246 "zhy_symbol_map"
      {"syut4", 4238},
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
      {""}, {""}, {""},
#line 1887 "zhy_symbol_map"
      {"hng4", 1879},
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
      {""},
#line 2769 "zhy_symbol_map"
      {"leot4", 2761},
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
      {""}, {""},
#line 1663 "zhy_symbol_map"
      {"gyut4", 1655},
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
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 4855 "zhy_symbol_map"
      {"zeot4", 4847},
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
      {""},
#line 4505 "zhy_symbol_map"
      {"tyut4", 4497},
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
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2205 "zhy_symbol_map"
      {"jyut7", 2197},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2204 "zhy_symbol_map"
      {"jyut6", 2196},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2201 "zhy_symbol_map"
      {"jyut3", 2193},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2200 "zhy_symbol_map"
      {"jyut2", 2192},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2203 "zhy_symbol_map"
      {"jyut5", 2195},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2199 "zhy_symbol_map"
      {"jyut1", 2191},
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
#line 2930 "zhy_symbol_map"
      {"lyut4", 2922},
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
#line 1586 "zhy_symbol_map"
      {"gwat4", 1578},
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
#line 1040 "zhy_symbol_map"
      {"faat4", 1032},
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
#line 515 "zhy_symbol_map"
      {"ceot4", 507},
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
      {""},
#line 837 "zhy_symbol_map"
      {"deot4", 829},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 5016 "zhy_symbol_map"
      {"zyut4", 5008},
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
      {""}, {""}, {""},
#line 1621 "zhy_symbol_map"
      {"gwit4", 1613},
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
#line 669 "zhy_symbol_map"
      {"cyut4", 661},
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
      {""},
#line 984 "zhy_symbol_map"
      {"dyut4", 976},
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
#line 1978 "zhy_symbol_map"
      {"hyut4", 1970},
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
#line 2202 "zhy_symbol_map"
      {"jyut4", 2194}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
