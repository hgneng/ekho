/* C++ code produced by gperf version 3.0.3 */
/* Command-line: gperf -L C++ -t zh_symbol_map  */
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

#line 1 "zh_symbol_map"

#include "stdafx.h"
#include "phonetic_symbol.h"
#include "zh_symbol_map.h"
#line 6 "zh_symbol_map"
struct ekho::SymbolCode;

#define TOTAL_KEYWORDS 2474
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 8
#define MIN_HASH_VALUE 17
#define MAX_HASH_VALUE 24239
/* maximum key range = 24223, duplicates = 0 */

inline unsigned int
ZH_PHash::hash (register const char *str, register unsigned int len){
  static unsigned short asso_values[] =
    {
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,  1300,
          5,    15,     0,    10,   420, 10257, 24240,     0,     0,     0,
          0,     0,     0, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240,  3570,    80,  3797,
       7262,   540,  3640,     0,  9237,    95,  1820, 10652,  7596,   520,
         40,    15,   585,   200,  7886,  2488, 11912,   180,  4241,  2392,
       8039, 11167,  3400, 24240, 24240,  1635,   535, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240, 24240,
      24240, 24240, 24240, 24240
    };
  register int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[4]];
      /*FALLTHROUGH*/
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      /*FALLTHROUGH*/
      case 3:
        hval += asso_values[(unsigned char)str[2]+1];
      /*FALLTHROUGH*/
      case 2:
        hval += asso_values[(unsigned char)str[1]+8];
      /*FALLTHROUGH*/
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

struct ekho::SymbolCode *
ZH_PHash::in_word_set (register const char *str, register unsigned int len){
  static struct ekho::SymbolCode wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1421 "zh_symbol_map"
      {"o4", 6425},
      {""}, {""}, {""}, {""},
#line 1419 "zh_symbol_map"
      {"o2", 6423},
      {""}, {""}, {""}, {""},
#line 1422 "zh_symbol_map"
      {"o5", 6426},
      {""}, {""}, {""}, {""},
#line 1420 "zh_symbol_map"
      {"o3", 6424},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1319 "zh_symbol_map"
      {"ng4", 6323},
      {""}, {""}, {""}, {""},
#line 1318 "zh_symbol_map"
      {"ng3", 6322},
      {""}, {""}, {""}, {""},
#line 1317 "zh_symbol_map"
      {"ng2", 6321},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 569 "zh_symbol_map"
      {"ga4", 5573},
      {""}, {""}, {""}, {""},
#line 568 "zh_symbol_map"
      {"ga3", 5572},
#line 581 "zh_symbol_map"
      {"gan4", 5585},
#line 587 "zh_symbol_map"
      {"gang4", 5591},
      {""}, {""},
#line 567 "zh_symbol_map"
      {"ga2", 5571},
      {""}, {""}, {""}, {""}, {""},
#line 579 "zh_symbol_map"
      {"gan2", 5583},
#line 585 "zh_symbol_map"
      {"gang2", 5589},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 582 "zh_symbol_map"
      {"gan5", 5586},
#line 588 "zh_symbol_map"
      {"gang5", 5592},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 580 "zh_symbol_map"
      {"gan3", 5584},
#line 586 "zh_symbol_map"
      {"gang3", 5590},
      {""}, {""},
#line 1265 "zh_symbol_map"
      {"na4", 6269},
      {""}, {""}, {""}, {""},
#line 1264 "zh_symbol_map"
      {"na3", 6268},
#line 1277 "zh_symbol_map"
      {"nan4", 6281},
#line 1283 "zh_symbol_map"
      {"nang4", 6287},
      {""}, {""},
#line 1263 "zh_symbol_map"
      {"na2", 6267},
      {""}, {""}, {""}, {""}, {""},
#line 1275 "zh_symbol_map"
      {"nan2", 6279},
#line 1281 "zh_symbol_map"
      {"nang2", 6285},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1278 "zh_symbol_map"
      {"nan5", 6282},
#line 1284 "zh_symbol_map"
      {"nang5", 6288},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1276 "zh_symbol_map"
      {"nan3", 6280},
#line 1282 "zh_symbol_map"
      {"nang3", 6286},
      {""}, {""},
#line 41 "zh_symbol_map"
      {"ba4", 5045},
      {""}, {""}, {""}, {""},
#line 40 "zh_symbol_map"
      {"ba3", 5044},
#line 53 "zh_symbol_map"
      {"ban4", 5057},
#line 59 "zh_symbol_map"
      {"bang4", 5063},
      {""}, {""},
#line 39 "zh_symbol_map"
      {"ba2", 5043},
      {""}, {""}, {""}, {""}, {""},
#line 51 "zh_symbol_map"
      {"ban2", 5055},
#line 57 "zh_symbol_map"
      {"bang2", 5061},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 54 "zh_symbol_map"
      {"ban5", 5058},
#line 60 "zh_symbol_map"
      {"bang5", 5064},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 52 "zh_symbol_map"
      {"ban3", 5056},
#line 58 "zh_symbol_map"
      {"bang3", 5062},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1325 "zh_symbol_map"
      {"ni4", 6329},
      {""}, {""}, {""}, {""},
#line 1324 "zh_symbol_map"
      {"ni3", 6328},
#line 1361 "zh_symbol_map"
      {"nin4", 6365},
#line 1367 "zh_symbol_map"
      {"ning4", 6371},
      {""}, {""},
#line 1323 "zh_symbol_map"
      {"ni2", 6327},
      {""}, {""}, {""}, {""}, {""},
#line 1359 "zh_symbol_map"
      {"nin2", 6363},
#line 1365 "zh_symbol_map"
      {"ning2", 6369},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1362 "zh_symbol_map"
      {"nin5", 6366},
#line 1368 "zh_symbol_map"
      {"ning5", 6372},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1360 "zh_symbol_map"
      {"nin3", 6364},
#line 1366 "zh_symbol_map"
      {"ning3", 6370},
      {""}, {""},
#line 89 "zh_symbol_map"
      {"bi4", 5093},
      {""}, {""}, {""}, {""},
#line 88 "zh_symbol_map"
      {"bi3", 5092},
#line 113 "zh_symbol_map"
      {"bin4", 5117},
#line 119 "zh_symbol_map"
      {"bing4", 5123},
      {""}, {""},
#line 87 "zh_symbol_map"
      {"bi2", 5091},
      {""}, {""}, {""}, {""}, {""},
#line 111 "zh_symbol_map"
      {"bin2", 5115},
#line 117 "zh_symbol_map"
      {"bing2", 5121},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 114 "zh_symbol_map"
      {"bin5", 5118},
#line 120 "zh_symbol_map"
      {"bing5", 5124},
      {""}, {""}, {""},
#line 1331 "zh_symbol_map"
      {"nia4", 6335},
      {""}, {""}, {""}, {""},
#line 112 "zh_symbol_map"
      {"bin3", 5116},
#line 118 "zh_symbol_map"
      {"bing3", 5122},
      {""}, {""}, {""},
#line 1329 "zh_symbol_map"
      {"nia2", 6333},
      {""}, {""}, {""}, {""}, {""},
#line 1349 "zh_symbol_map"
      {"niao4", 6353},
      {""}, {""}, {""},
#line 1332 "zh_symbol_map"
      {"nia5", 6336},
      {""}, {""}, {""}, {""}, {""},
#line 1347 "zh_symbol_map"
      {"niao2", 6351},
      {""}, {""}, {""},
#line 1330 "zh_symbol_map"
      {"nia3", 6334},
      {""}, {""}, {""}, {""}, {""},
#line 1350 "zh_symbol_map"
      {"niao5", 6354},
      {""}, {""}, {""}, {""},
#line 1337 "zh_symbol_map"
      {"nian4", 6341},
#line 1343 "zh_symbol_map"
      {"niang4", 6347},
      {""}, {""}, {""},
#line 1348 "zh_symbol_map"
      {"niao3", 6352},
#line 1341 "zh_symbol_map"
      {"niang2", 6345},
      {""}, {""}, {""},
#line 1335 "zh_symbol_map"
      {"nian2", 6339},
#line 1344 "zh_symbol_map"
      {"niang5", 6348},
      {""}, {""}, {""},
#line 101 "zh_symbol_map"
      {"biao4", 5105},
#line 1342 "zh_symbol_map"
      {"niang3", 6346},
      {""}, {""}, {""},
#line 1338 "zh_symbol_map"
      {"nian5", 6342},
      {""}, {""}, {""}, {""},
#line 99 "zh_symbol_map"
      {"biao2", 5103},
      {""}, {""}, {""}, {""},
#line 1336 "zh_symbol_map"
      {"nian3", 6340},
      {""}, {""}, {""}, {""},
#line 102 "zh_symbol_map"
      {"biao5", 5106},
      {""}, {""}, {""}, {""},
#line 95 "zh_symbol_map"
      {"bian4", 5099},
      {""}, {""}, {""}, {""},
#line 100 "zh_symbol_map"
      {"biao3", 5104},
      {""}, {""},
#line 1535 "zh_symbol_map"
      {"qi4", 6539},
      {""},
#line 93 "zh_symbol_map"
      {"bian2", 5097},
      {""}, {""},
#line 1534 "zh_symbol_map"
      {"qi3", 6538},
#line 1571 "zh_symbol_map"
      {"qin4", 6575},
#line 1577 "zh_symbol_map"
      {"qing4", 6581},
      {""}, {""},
#line 1533 "zh_symbol_map"
      {"qi2", 6537},
      {""},
#line 96 "zh_symbol_map"
      {"bian5", 5100},
      {""}, {""}, {""},
#line 1569 "zh_symbol_map"
      {"qin2", 6573},
#line 1575 "zh_symbol_map"
      {"qing2", 6579},
      {""}, {""}, {""}, {""},
#line 94 "zh_symbol_map"
      {"bian3", 5098},
      {""},
#line 1423 "zh_symbol_map"
      {"o6", 6427},
      {""},
#line 1572 "zh_symbol_map"
      {"qin5", 6576},
#line 1578 "zh_symbol_map"
      {"qing5", 6582},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1570 "zh_symbol_map"
      {"qin3", 6574},
#line 1576 "zh_symbol_map"
      {"qing3", 6580},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1541 "zh_symbol_map"
      {"qia4", 6545},
      {""}, {""}, {""},
#line 1320 "zh_symbol_map"
      {"ng5", 6324},
      {""}, {""}, {""}, {""}, {""},
#line 1539 "zh_symbol_map"
      {"qia2", 6543},
      {""}, {""}, {""}, {""}, {""},
#line 1559 "zh_symbol_map"
      {"qiao4", 6563},
      {""}, {""}, {""},
#line 1542 "zh_symbol_map"
      {"qia5", 6546},
      {""}, {""}, {""}, {""}, {""},
#line 1557 "zh_symbol_map"
      {"qiao2", 6561},
      {""}, {""}, {""},
#line 1540 "zh_symbol_map"
      {"qia3", 6544},
      {""}, {""}, {""}, {""}, {""},
#line 1560 "zh_symbol_map"
      {"qiao5", 6564},
      {""}, {""}, {""}, {""},
#line 1547 "zh_symbol_map"
      {"qian4", 6551},
#line 1553 "zh_symbol_map"
      {"qiang4", 6557},
      {""},
#line 570 "zh_symbol_map"
      {"ga5", 5574},
      {""},
#line 1558 "zh_symbol_map"
      {"qiao3", 6562},
#line 1551 "zh_symbol_map"
      {"qiang2", 6555},
      {""},
#line 599 "zh_symbol_map"
      {"ge4", 5603},
      {""},
#line 1545 "zh_symbol_map"
      {"qian2", 6549},
#line 1554 "zh_symbol_map"
      {"qiang5", 6558},
      {""},
#line 598 "zh_symbol_map"
      {"ge3", 5602},
#line 611 "zh_symbol_map"
      {"gen4", 5615},
#line 617 "zh_symbol_map"
      {"geng4", 5621},
#line 1552 "zh_symbol_map"
      {"qiang3", 6556},
#line 485 "zh_symbol_map"
      {"e4", 5489},
#line 597 "zh_symbol_map"
      {"ge2", 5601},
      {""},
#line 1548 "zh_symbol_map"
      {"qian5", 6552},
      {""},
#line 483 "zh_symbol_map"
      {"e2", 5487},
      {""},
#line 609 "zh_symbol_map"
      {"gen2", 5613},
#line 615 "zh_symbol_map"
      {"geng2", 5619},
      {""},
#line 486 "zh_symbol_map"
      {"e5", 5490},
      {""}, {""},
#line 1546 "zh_symbol_map"
      {"qian3", 6550},
      {""},
#line 484 "zh_symbol_map"
      {"e3", 5488},
      {""},
#line 612 "zh_symbol_map"
      {"gen5", 5616},
#line 618 "zh_symbol_map"
      {"geng5", 5622},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1266 "zh_symbol_map"
      {"na5", 6270},
#line 610 "zh_symbol_map"
      {"gen3", 5614},
#line 616 "zh_symbol_map"
      {"geng3", 5620},
      {""}, {""},
#line 1295 "zh_symbol_map"
      {"ne4", 6299},
      {""}, {""}, {""}, {""},
#line 1294 "zh_symbol_map"
      {"ne3", 6298},
#line 1307 "zh_symbol_map"
      {"nen4", 6311},
#line 1313 "zh_symbol_map"
      {"neng4", 6317},
      {""}, {""},
#line 1293 "zh_symbol_map"
      {"ne2", 6297},
      {""}, {""}, {""}, {""},
#line 1409 "zh_symbol_map"
      {"nv4", 6413},
#line 1305 "zh_symbol_map"
      {"nen2", 6309},
#line 1311 "zh_symbol_map"
      {"neng2", 6315},
      {""}, {""},
#line 1408 "zh_symbol_map"
      {"nv3", 6412},
      {""}, {""}, {""}, {""},
#line 1407 "zh_symbol_map"
      {"nv2", 6411},
#line 1308 "zh_symbol_map"
      {"nen5", 6312},
#line 1314 "zh_symbol_map"
      {"neng5", 6318},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 42 "zh_symbol_map"
      {"ba5", 5046},
#line 1306 "zh_symbol_map"
      {"nen3", 6310},
#line 1312 "zh_symbol_map"
      {"neng3", 6316},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 77 "zh_symbol_map"
      {"ben4", 5081},
#line 83 "zh_symbol_map"
      {"beng4", 5087},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1151 "zh_symbol_map"
      {"ma4", 6155},
#line 75 "zh_symbol_map"
      {"ben2", 5079},
#line 81 "zh_symbol_map"
      {"beng2", 5085},
      {""}, {""},
#line 1150 "zh_symbol_map"
      {"ma3", 6154},
#line 1163 "zh_symbol_map"
      {"man4", 6167},
#line 1169 "zh_symbol_map"
      {"mang4", 6173},
      {""}, {""},
#line 1149 "zh_symbol_map"
      {"ma2", 6153},
#line 78 "zh_symbol_map"
      {"ben5", 5082},
#line 84 "zh_symbol_map"
      {"beng5", 5088},
      {""}, {""}, {""},
#line 1161 "zh_symbol_map"
      {"man2", 6165},
#line 1167 "zh_symbol_map"
      {"mang2", 6171},
      {""}, {""}, {""},
#line 76 "zh_symbol_map"
      {"ben3", 5080},
#line 82 "zh_symbol_map"
      {"beng3", 5086},
      {""}, {""}, {""},
#line 1164 "zh_symbol_map"
      {"man5", 6168},
#line 1170 "zh_symbol_map"
      {"mang5", 6174},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1162 "zh_symbol_map"
      {"man3", 6166},
#line 1168 "zh_symbol_map"
      {"mang3", 6172},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1326 "zh_symbol_map"
      {"ni5", 6330},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 593 "zh_symbol_map"
      {"gao4", 5597},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1433 "zh_symbol_map"
      {"pa4", 6437},
#line 591 "zh_symbol_map"
      {"gao2", 5595},
      {""}, {""}, {""},
#line 1432 "zh_symbol_map"
      {"pa3", 6436},
#line 1445 "zh_symbol_map"
      {"pan4", 6449},
#line 1451 "zh_symbol_map"
      {"pang4", 6455},
      {""}, {""},
#line 1431 "zh_symbol_map"
      {"pa2", 6435},
#line 594 "zh_symbol_map"
      {"gao5", 5598},
      {""}, {""}, {""}, {""},
#line 1443 "zh_symbol_map"
      {"pan2", 6447},
#line 1449 "zh_symbol_map"
      {"pang2", 6453},
      {""}, {""},
#line 90 "zh_symbol_map"
      {"bi5", 5094},
#line 592 "zh_symbol_map"
      {"gao3", 5596},
      {""}, {""}, {""}, {""},
#line 1446 "zh_symbol_map"
      {"pan5", 6450},
#line 1452 "zh_symbol_map"
      {"pang5", 6456},
      {""}, {""}, {""},
#line 1289 "zh_symbol_map"
      {"nao4", 6293},
      {""}, {""}, {""}, {""},
#line 1444 "zh_symbol_map"
      {"pan3", 6448},
#line 1450 "zh_symbol_map"
      {"pang3", 6454},
      {""}, {""},
#line 1205 "zh_symbol_map"
      {"mi4", 6209},
#line 1287 "zh_symbol_map"
      {"nao2", 6291},
      {""}, {""}, {""},
#line 1204 "zh_symbol_map"
      {"mi3", 6208},
#line 1229 "zh_symbol_map"
      {"min4", 6233},
#line 1235 "zh_symbol_map"
      {"ming4", 6239},
      {""}, {""},
#line 1203 "zh_symbol_map"
      {"mi2", 6207},
#line 1290 "zh_symbol_map"
      {"nao5", 6294},
      {""}, {""}, {""}, {""},
#line 1227 "zh_symbol_map"
      {"min2", 6231},
#line 1233 "zh_symbol_map"
      {"ming2", 6237},
      {""}, {""},
#line 491 "zh_symbol_map"
      {"ei4", 5495},
#line 1288 "zh_symbol_map"
      {"nao3", 6292},
      {""}, {""}, {""},
#line 490 "zh_symbol_map"
      {"ei3", 5494},
#line 1230 "zh_symbol_map"
      {"min5", 6234},
#line 1236 "zh_symbol_map"
      {"ming5", 6240},
      {""}, {""},
#line 489 "zh_symbol_map"
      {"ei2", 5493},
#line 65 "zh_symbol_map"
      {"bao4", 5069},
      {""}, {""}, {""}, {""},
#line 1228 "zh_symbol_map"
      {"min3", 6232},
#line 1234 "zh_symbol_map"
      {"ming3", 6238},
      {""}, {""}, {""},
#line 63 "zh_symbol_map"
      {"bao2", 5067},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 66 "zh_symbol_map"
      {"bao5", 5070},
      {""},
#line 1345 "zh_symbol_map"
      {"niang6", 6349},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 64 "zh_symbol_map"
      {"bao3", 5068},
      {""}, {""}, {""},
#line 1481 "zh_symbol_map"
      {"pi4", 6485},
      {""}, {""}, {""}, {""},
#line 1480 "zh_symbol_map"
      {"pi3", 6484},
#line 1505 "zh_symbol_map"
      {"pin4", 6509},
#line 1511 "zh_symbol_map"
      {"ping4", 6515},
      {""}, {""},
#line 1479 "zh_symbol_map"
      {"pi2", 6483},
      {""}, {""}, {""}, {""}, {""},
#line 1503 "zh_symbol_map"
      {"pin2", 6507},
#line 1509 "zh_symbol_map"
      {"ping2", 6513},
      {""}, {""}, {""}, {""},
#line 1217 "zh_symbol_map"
      {"miao4", 6221},
      {""}, {""}, {""},
#line 1506 "zh_symbol_map"
      {"pin5", 6510},
#line 1512 "zh_symbol_map"
      {"ping5", 6516},
      {""}, {""}, {""}, {""},
#line 1215 "zh_symbol_map"
      {"miao2", 6219},
      {""}, {""},
#line 1536 "zh_symbol_map"
      {"qi5", 6540},
#line 1504 "zh_symbol_map"
      {"pin3", 6508},
#line 1510 "zh_symbol_map"
      {"ping3", 6514},
      {""}, {""}, {""}, {""},
#line 1218 "zh_symbol_map"
      {"miao5", 6222},
      {""}, {""}, {""}, {""},
#line 1211 "zh_symbol_map"
      {"mian4", 6215},
      {""}, {""}, {""}, {""},
#line 1216 "zh_symbol_map"
      {"miao3", 6220},
      {""}, {""}, {""}, {""},
#line 1209 "zh_symbol_map"
      {"mian2", 6213},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1212 "zh_symbol_map"
      {"mian5", 6216},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1210 "zh_symbol_map"
      {"mian3", 6214},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1493 "zh_symbol_map"
      {"piao4", 6497},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1491 "zh_symbol_map"
      {"piao2", 6495},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1494 "zh_symbol_map"
      {"piao5", 6498},
      {""}, {""}, {""}, {""},
#line 1487 "zh_symbol_map"
      {"pian4", 6491},
      {""}, {""}, {""}, {""},
#line 1492 "zh_symbol_map"
      {"piao3", 6496},
      {""}, {""}, {""}, {""},
#line 1485 "zh_symbol_map"
      {"pian2", 6489},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1488 "zh_symbol_map"
      {"pian5", 6492},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1486 "zh_symbol_map"
      {"pian3", 6490},
      {""}, {""}, {""}, {""}, {""},
#line 1555 "zh_symbol_map"
      {"qiang6", 6559},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 600 "zh_symbol_map"
      {"ge5", 5604},
#line 583 "zh_symbol_map"
      {"gan6", 5587},
#line 589 "zh_symbol_map"
      {"gang6", 5593},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 487 "zh_symbol_map"
      {"e6", 5491},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1296 "zh_symbol_map"
      {"ne5", 6300},
#line 1279 "zh_symbol_map"
      {"nan6", 6283},
#line 1285 "zh_symbol_map"
      {"nang6", 6289},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1410 "zh_symbol_map"
      {"nv5", 6414},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1583 "zh_symbol_map"
      {"qiong4", 6587},
      {""}, {""},
#line 55 "zh_symbol_map"
      {"ban6", 5059},
#line 61 "zh_symbol_map"
      {"bang6", 5065},
#line 1581 "zh_symbol_map"
      {"qiong2", 6585},
      {""}, {""}, {""}, {""},
#line 1584 "zh_symbol_map"
      {"qiong5", 6588},
      {""}, {""}, {""}, {""},
#line 1582 "zh_symbol_map"
      {"qiong3", 6586},
      {""},
#line 1152 "zh_symbol_map"
      {"ma5", 6156},
      {""}, {""}, {""}, {""},
#line 1181 "zh_symbol_map"
      {"me4", 6185},
      {""}, {""}, {""}, {""},
#line 1180 "zh_symbol_map"
      {"me3", 6184},
#line 1193 "zh_symbol_map"
      {"men4", 6197},
#line 1199 "zh_symbol_map"
      {"meng4", 6203},
      {""}, {""},
#line 1179 "zh_symbol_map"
      {"me2", 6183},
      {""}, {""}, {""}, {""}, {""},
#line 1191 "zh_symbol_map"
      {"men2", 6195},
#line 1197 "zh_symbol_map"
      {"meng2", 6201},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1194 "zh_symbol_map"
      {"men5", 6198},
#line 1200 "zh_symbol_map"
      {"meng5", 6204},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1192 "zh_symbol_map"
      {"men3", 6196},
#line 1198 "zh_symbol_map"
      {"meng3", 6202},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1363 "zh_symbol_map"
      {"nin6", 6367},
#line 1369 "zh_symbol_map"
      {"ning6", 6373},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1434 "zh_symbol_map"
      {"pa5", 6438},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1469 "zh_symbol_map"
      {"pen4", 6473},
#line 1475 "zh_symbol_map"
      {"peng4", 6479},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1467 "zh_symbol_map"
      {"pen2", 6471},
#line 1473 "zh_symbol_map"
      {"peng2", 6477},
      {""}, {""}, {""},
#line 115 "zh_symbol_map"
      {"bin6", 5119},
#line 121 "zh_symbol_map"
      {"bing6", 5125},
      {""}, {""}, {""},
#line 1470 "zh_symbol_map"
      {"pen5", 6474},
#line 1476 "zh_symbol_map"
      {"peng5", 6480},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1206 "zh_symbol_map"
      {"mi5", 6210},
#line 1468 "zh_symbol_map"
      {"pen3", 6472},
#line 1474 "zh_symbol_map"
      {"peng3", 6478},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1333 "zh_symbol_map"
      {"nia6", 6337},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 492 "zh_symbol_map"
      {"ei5", 5496},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1351 "zh_symbol_map"
      {"niao6", 6355},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1175 "zh_symbol_map"
      {"mao4", 6179},
#line 1339 "zh_symbol_map"
      {"nian6", 6343},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1173 "zh_symbol_map"
      {"mao2", 6177},
      {""}, {""}, {""},
#line 1482 "zh_symbol_map"
      {"pi5", 6486},
      {""},
#line 103 "zh_symbol_map"
      {"biao6", 5107},
      {""}, {""}, {""},
#line 1176 "zh_symbol_map"
      {"mao5", 6180},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1174 "zh_symbol_map"
      {"mao3", 6178},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 97 "zh_symbol_map"
      {"bian6", 5101},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1573 "zh_symbol_map"
      {"qin6", 6577},
#line 1579 "zh_symbol_map"
      {"qing6", 6583},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1457 "zh_symbol_map"
      {"pao4", 6461},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1455 "zh_symbol_map"
      {"pao2", 6459},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1458 "zh_symbol_map"
      {"pao5", 6462},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1456 "zh_symbol_map"
      {"pao3", 6460},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1418 "zh_symbol_map"
      {"o1", 6422},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1543 "zh_symbol_map"
      {"qia6", 6547},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1561 "zh_symbol_map"
      {"qiao6", 6565},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1316 "zh_symbol_map"
      {"ng1", 6320},
      {""},
#line 1549 "zh_symbol_map"
      {"qian6", 6553},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 613 "zh_symbol_map"
      {"gen6", 5617},
#line 619 "zh_symbol_map"
      {"geng6", 5623},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 566 "zh_symbol_map"
      {"ga1", 5570},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1309 "zh_symbol_map"
      {"nen6", 6313},
#line 1315 "zh_symbol_map"
      {"neng6", 6319},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1262 "zh_symbol_map"
      {"na1", 6266},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1585 "zh_symbol_map"
      {"qiong6", 6589},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 79 "zh_symbol_map"
      {"ben6", 5083},
#line 85 "zh_symbol_map"
      {"beng6", 5089},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1182 "zh_symbol_map"
      {"me5", 6186},
#line 1165 "zh_symbol_map"
      {"man6", 6169},
#line 1171 "zh_symbol_map"
      {"mang6", 6175},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 38 "zh_symbol_map"
      {"ba1", 5042},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 595 "zh_symbol_map"
      {"gao6", 5599},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1447 "zh_symbol_map"
      {"pan6", 6451},
#line 1453 "zh_symbol_map"
      {"pang6", 6457},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1322 "zh_symbol_map"
      {"ni1", 6326},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1291 "zh_symbol_map"
      {"nao6", 6295},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1231 "zh_symbol_map"
      {"min6", 6235},
#line 1237 "zh_symbol_map"
      {"ming6", 6241},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 86 "zh_symbol_map"
      {"bi1", 5090},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 67 "zh_symbol_map"
      {"bao6", 5071},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1507 "zh_symbol_map"
      {"pin6", 6511},
#line 1513 "zh_symbol_map"
      {"ping6", 6517},
      {""}, {""},
#line 635 "zh_symbol_map"
      {"gu4", 5639},
      {""}, {""}, {""}, {""},
#line 634 "zh_symbol_map"
      {"gu3", 5638},
#line 671 "zh_symbol_map"
      {"gun4", 5675},
      {""}, {""}, {""},
#line 633 "zh_symbol_map"
      {"gu2", 5637},
      {""},
#line 1219 "zh_symbol_map"
      {"miao6", 6223},
      {""}, {""},
#line 1427 "zh_symbol_map"
      {"ou4", 6431},
#line 669 "zh_symbol_map"
      {"gun2", 5673},
      {""},
#line 1340 "zh_symbol_map"
      {"niang1", 6344},
      {""},
#line 1426 "zh_symbol_map"
      {"ou3", 6430},
      {""}, {""}, {""}, {""},
#line 1425 "zh_symbol_map"
      {"ou2", 6429},
#line 672 "zh_symbol_map"
      {"gun5", 5676},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 670 "zh_symbol_map"
      {"gun3", 5674},
#line 1213 "zh_symbol_map"
      {"mian6", 6217},
      {""}, {""},
#line 1391 "zh_symbol_map"
      {"nu4", 6395},
      {""}, {""}, {""}, {""},
#line 1390 "zh_symbol_map"
      {"nu3", 6394},
      {""}, {""}, {""}, {""},
#line 1389 "zh_symbol_map"
      {"nu2", 6393},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1532 "zh_symbol_map"
      {"qi1", 6536},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 641 "zh_symbol_map"
      {"gua4", 5645},
      {""}, {""}, {""}, {""}, {""},
#line 1495 "zh_symbol_map"
      {"piao6", 6499},
      {""}, {""},
#line 131 "zh_symbol_map"
      {"bu4", 5135},
#line 639 "zh_symbol_map"
      {"gua2", 5643},
      {""}, {""}, {""},
#line 130 "zh_symbol_map"
      {"bu3", 5134},
      {""}, {""}, {""}, {""},
#line 129 "zh_symbol_map"
      {"bu2", 5133},
#line 642 "zh_symbol_map"
      {"gua5", 5646},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 640 "zh_symbol_map"
      {"gua3", 5644},
#line 1489 "zh_symbol_map"
      {"pian6", 6493},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 653 "zh_symbol_map"
      {"guan4", 5657},
#line 659 "zh_symbol_map"
      {"guang4", 5663},
      {""}, {""}, {""}, {""},
#line 657 "zh_symbol_map"
      {"guang2", 5661},
      {""}, {""}, {""},
#line 651 "zh_symbol_map"
      {"guan2", 5655},
#line 660 "zh_symbol_map"
      {"guang5", 5664},
      {""}, {""}, {""}, {""},
#line 658 "zh_symbol_map"
      {"guang3", 5662},
      {""}, {""}, {""},
#line 654 "zh_symbol_map"
      {"guan5", 5658},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 652 "zh_symbol_map"
      {"guan3", 5656},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1397 "zh_symbol_map"
      {"nuan4", 6401},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1395 "zh_symbol_map"
      {"nuan2", 6399},
      {""}, {""}, {""}, {""},
#line 647 "zh_symbol_map"
      {"guai4", 5651},
      {""}, {""}, {""}, {""},
#line 1398 "zh_symbol_map"
      {"nuan5", 6402},
      {""}, {""}, {""}, {""},
#line 645 "zh_symbol_map"
      {"guai2", 5649},
#line 1550 "zh_symbol_map"
      {"qiang1", 6554},
      {""},
#line 596 "zh_symbol_map"
      {"ge1", 5600},
      {""},
#line 1396 "zh_symbol_map"
      {"nuan3", 6400},
      {""}, {""}, {""}, {""},
#line 648 "zh_symbol_map"
      {"guai5", 5652},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 482 "zh_symbol_map"
      {"e1", 5486},
      {""}, {""},
#line 646 "zh_symbol_map"
      {"guai3", 5650},
      {""}, {""},
#line 1595 "zh_symbol_map"
      {"qu4", 6599},
      {""}, {""}, {""}, {""},
#line 1594 "zh_symbol_map"
      {"qu3", 6598},
#line 1613 "zh_symbol_map"
      {"qun4", 6617},
      {""}, {""}, {""},
#line 1593 "zh_symbol_map"
      {"qu2", 6597},
      {""}, {""}, {""}, {""}, {""},
#line 1611 "zh_symbol_map"
      {"qun2", 6615},
      {""}, {""}, {""},
#line 1292 "zh_symbol_map"
      {"ne1", 6296},
      {""}, {""}, {""}, {""}, {""},
#line 1614 "zh_symbol_map"
      {"qun5", 6618},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1406 "zh_symbol_map"
      {"nv1", 6410},
#line 1612 "zh_symbol_map"
      {"qun3", 6616},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1195 "zh_symbol_map"
      {"men6", 6199},
#line 1201 "zh_symbol_map"
      {"meng6", 6205},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 575 "zh_symbol_map"
      {"gai4", 5579},
      {""}, {""}, {""},
#line 1148 "zh_symbol_map"
      {"ma1", 6152},
      {""}, {""}, {""}, {""}, {""},
#line 573 "zh_symbol_map"
      {"gai2", 5577},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 576 "zh_symbol_map"
      {"gai5", 5580},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 574 "zh_symbol_map"
      {"gai3", 5578},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1271 "zh_symbol_map"
      {"nai4", 6275},
#line 1601 "zh_symbol_map"
      {"quan4", 6605},
      {""}, {""}, {""},
#line 1471 "zh_symbol_map"
      {"pen6", 6475},
#line 1477 "zh_symbol_map"
      {"peng6", 6481},
      {""}, {""}, {""},
#line 1269 "zh_symbol_map"
      {"nai2", 6273},
#line 1599 "zh_symbol_map"
      {"quan2", 6603},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1272 "zh_symbol_map"
      {"nai5", 6276},
#line 1602 "zh_symbol_map"
      {"quan5", 6606},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1430 "zh_symbol_map"
      {"pa1", 6434},
#line 1270 "zh_symbol_map"
      {"nai3", 6274},
#line 1600 "zh_symbol_map"
      {"quan3", 6604},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 47 "zh_symbol_map"
      {"bai4", 5051},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 45 "zh_symbol_map"
      {"bai2", 5049},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 48 "zh_symbol_map"
      {"bai5", 5052},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1202 "zh_symbol_map"
      {"mi1", 6206},
#line 46 "zh_symbol_map"
      {"bai3", 5050},
      {""}, {""}, {""},
#line 797 "zh_symbol_map"
      {"ji4", 5801},
      {""}, {""}, {""}, {""},
#line 796 "zh_symbol_map"
      {"ji3", 5800},
#line 833 "zh_symbol_map"
      {"jin4", 5837},
#line 839 "zh_symbol_map"
      {"jing4", 5843},
      {""}, {""},
#line 795 "zh_symbol_map"
      {"ji2", 5799},
#line 1177 "zh_symbol_map"
      {"mao6", 6181},
      {""}, {""}, {""},
#line 488 "zh_symbol_map"
      {"ei1", 5492},
#line 831 "zh_symbol_map"
      {"jin2", 5835},
#line 837 "zh_symbol_map"
      {"jing2", 5841},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 834 "zh_symbol_map"
      {"jin5", 5838},
#line 840 "zh_symbol_map"
      {"jing5", 5844},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 636 "zh_symbol_map"
      {"gu5", 5640},
#line 832 "zh_symbol_map"
      {"jin3", 5836},
#line 838 "zh_symbol_map"
      {"jing3", 5842},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1428 "zh_symbol_map"
      {"ou5", 6432},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1478 "zh_symbol_map"
      {"pi1", 6482},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 803 "zh_symbol_map"
      {"jia4", 5807},
      {""}, {""}, {""},
#line 1392 "zh_symbol_map"
      {"nu5", 6396},
#line 1459 "zh_symbol_map"
      {"pao6", 6463},
      {""}, {""}, {""}, {""},
#line 801 "zh_symbol_map"
      {"jia2", 5805},
      {""}, {""}, {""}, {""}, {""},
#line 821 "zh_symbol_map"
      {"jiao4", 5825},
      {""}, {""}, {""},
#line 804 "zh_symbol_map"
      {"jia5", 5808},
      {""}, {""}, {""}, {""}, {""},
#line 819 "zh_symbol_map"
      {"jiao2", 5823},
      {""}, {""}, {""},
#line 802 "zh_symbol_map"
      {"jia3", 5806},
      {""}, {""}, {""}, {""}, {""},
#line 822 "zh_symbol_map"
      {"jiao5", 5826},
      {""}, {""}, {""}, {""},
#line 809 "zh_symbol_map"
      {"jian4", 5813},
#line 815 "zh_symbol_map"
      {"jiang4", 5819},
      {""},
#line 132 "zh_symbol_map"
      {"bu5", 5136},
      {""},
#line 820 "zh_symbol_map"
      {"jiao3", 5824},
#line 813 "zh_symbol_map"
      {"jiang2", 5817},
      {""}, {""}, {""},
#line 807 "zh_symbol_map"
      {"jian2", 5811},
#line 816 "zh_symbol_map"
      {"jiang5", 5820},
      {""}, {""}, {""}, {""},
#line 814 "zh_symbol_map"
      {"jiang3", 5818},
      {""}, {""}, {""},
#line 810 "zh_symbol_map"
      {"jian5", 5814},
      {""}, {""},
#line 1259 "zh_symbol_map"
      {"mu4", 6263},
      {""}, {""}, {""}, {""},
#line 1258 "zh_symbol_map"
      {"mu3", 6262},
      {""},
#line 808 "zh_symbol_map"
      {"jian3", 5812},
      {""}, {""},
#line 1257 "zh_symbol_map"
      {"mu2", 6261},
      {""}, {""},
#line 661 "zh_symbol_map"
      {"guang6", 5665},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 677 "zh_symbol_map"
      {"guo4", 5681},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1529 "zh_symbol_map"
      {"pu4", 6533},
#line 675 "zh_symbol_map"
      {"guo2", 5679},
      {""}, {""}, {""},
#line 1528 "zh_symbol_map"
      {"pu3", 6532},
      {""}, {""}, {""}, {""},
#line 1527 "zh_symbol_map"
      {"pu2", 6531},
#line 678 "zh_symbol_map"
      {"guo5", 5682},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 676 "zh_symbol_map"
      {"guo3", 5680},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1403 "zh_symbol_map"
      {"nuo4", 6407},
      {""}, {""}, {""},
#line 1596 "zh_symbol_map"
      {"qu5", 6600},
      {""}, {""}, {""}, {""}, {""},
#line 1401 "zh_symbol_map"
      {"nuo2", 6405},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1404 "zh_symbol_map"
      {"nuo5", 6408},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1402 "zh_symbol_map"
      {"nuo3", 6406},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1580 "zh_symbol_map"
      {"qiong1", 6584},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 605 "zh_symbol_map"
      {"gei4", 5609},
      {""}, {""}, {""},
#line 1178 "zh_symbol_map"
      {"me1", 6182},
      {""}, {""}, {""}, {""}, {""},
#line 603 "zh_symbol_map"
      {"gei2", 5607},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 606 "zh_symbol_map"
      {"gei5", 5610},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 604 "zh_symbol_map"
      {"gei3", 5608},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1301 "zh_symbol_map"
      {"nei4", 6305},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1299 "zh_symbol_map"
      {"nei2", 6303},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1302 "zh_symbol_map"
      {"nei5", 6306},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 623 "zh_symbol_map"
      {"gong4", 5627},
      {""},
#line 1300 "zh_symbol_map"
      {"nei3", 6304},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 621 "zh_symbol_map"
      {"gong2", 5625},
      {""},
#line 71 "zh_symbol_map"
      {"bei4", 5075},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 624 "zh_symbol_map"
      {"gong5", 5628},
      {""},
#line 69 "zh_symbol_map"
      {"bei2", 5073},
      {""}, {""}, {""}, {""},
#line 1157 "zh_symbol_map"
      {"mai4", 6161},
      {""}, {""},
#line 622 "zh_symbol_map"
      {"gong3", 5626},
      {""},
#line 72 "zh_symbol_map"
      {"bei5", 5076},
      {""}, {""}, {""}, {""},
#line 1155 "zh_symbol_map"
      {"mai2", 6159},
      {""}, {""},
#line 1379 "zh_symbol_map"
      {"nong4", 6383},
#line 798 "zh_symbol_map"
      {"ji5", 5802},
#line 70 "zh_symbol_map"
      {"bei3", 5074},
      {""}, {""}, {""}, {""},
#line 1158 "zh_symbol_map"
      {"mai5", 6162},
      {""}, {""},
#line 1377 "zh_symbol_map"
      {"nong2", 6381},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1156 "zh_symbol_map"
      {"mai3", 6160},
      {""}, {""},
#line 1380 "zh_symbol_map"
      {"nong5", 6384},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1378 "zh_symbol_map"
      {"nong3", 6382},
      {""}, {""},
#line 125 "zh_symbol_map"
      {"bo4", 5129},
      {""}, {""}, {""}, {""},
#line 124 "zh_symbol_map"
      {"bo3", 5128},
      {""}, {""}, {""},
#line 673 "zh_symbol_map"
      {"gun6", 5677},
#line 123 "zh_symbol_map"
      {"bo2", 5127},
      {""}, {""}, {""}, {""},
#line 2035 "zh_symbol_map"
      {"wa4", 7039},
      {""}, {""}, {""},
#line 1439 "zh_symbol_map"
      {"pai4", 6443},
#line 2034 "zh_symbol_map"
      {"wa3", 7038},
#line 2047 "zh_symbol_map"
      {"wan4", 7051},
#line 2053 "zh_symbol_map"
      {"wang4", 7057},
      {""}, {""},
#line 2033 "zh_symbol_map"
      {"wa2", 7037},
      {""}, {""}, {""},
#line 1437 "zh_symbol_map"
      {"pai2", 6441},
      {""},
#line 2045 "zh_symbol_map"
      {"wan2", 7049},
#line 2051 "zh_symbol_map"
      {"wang2", 7055},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1440 "zh_symbol_map"
      {"pai5", 6444},
      {""},
#line 2048 "zh_symbol_map"
      {"wan5", 7052},
#line 2054 "zh_symbol_map"
      {"wang5", 7058},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1438 "zh_symbol_map"
      {"pai3", 6442},
      {""},
#line 2046 "zh_symbol_map"
      {"wan3", 7050},
#line 2052 "zh_symbol_map"
      {"wang3", 7056},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 643 "zh_symbol_map"
      {"gua6", 5647},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 817 "zh_symbol_map"
      {"jiang6", 5821},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1260 "zh_symbol_map"
      {"mu5", 6264},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1710 "zh_symbol_map"
      {"sa4", 6714},
      {""}, {""}, {""},
#line 655 "zh_symbol_map"
      {"guan6", 5659},
#line 1709 "zh_symbol_map"
      {"sa3", 6713},
#line 1722 "zh_symbol_map"
      {"san4", 6726},
#line 1728 "zh_symbol_map"
      {"sang4", 6732},
      {""}, {""},
#line 1708 "zh_symbol_map"
      {"sa2", 6712},
      {""}, {""}, {""}, {""}, {""},
#line 1720 "zh_symbol_map"
      {"san2", 6724},
#line 1726 "zh_symbol_map"
      {"sang2", 6730},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1723 "zh_symbol_map"
      {"san5", 6727},
#line 1729 "zh_symbol_map"
      {"sang5", 6733},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1721 "zh_symbol_map"
      {"san3", 6725},
#line 1727 "zh_symbol_map"
      {"sang3", 6731},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1399 "zh_symbol_map"
      {"nuan6", 6403},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 845 "zh_symbol_map"
      {"jiong4", 5849},
      {""},
#line 1530 "zh_symbol_map"
      {"pu5", 6534},
      {""},
#line 649 "zh_symbol_map"
      {"guai6", 5653},
#line 843 "zh_symbol_map"
      {"jiong2", 5847},
      {""}, {""}, {""}, {""},
#line 846 "zh_symbol_map"
      {"jiong5", 5850},
      {""}, {""}, {""}, {""},
#line 844 "zh_symbol_map"
      {"jiong3", 5848},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1615 "zh_symbol_map"
      {"qun6", 6619},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1873 "zh_symbol_map"
      {"si4", 6877},
      {""}, {""}, {""}, {""},
#line 1872 "zh_symbol_map"
      {"si3", 6876},
      {""}, {""}, {""}, {""},
#line 1871 "zh_symbol_map"
      {"si2", 6875},
      {""}, {""},
#line 578 "zh_symbol_map"
      {"gan1", 5582},
#line 584 "zh_symbol_map"
      {"gang1", 5588},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1274 "zh_symbol_map"
      {"nan1", 6278},
#line 1280 "zh_symbol_map"
      {"nang1", 6284},
      {""}, {""}, {""},
#line 577 "zh_symbol_map"
      {"gai6", 5581},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 50 "zh_symbol_map"
      {"ban1", 5054},
#line 56 "zh_symbol_map"
      {"bang1", 5060},
      {""}, {""}, {""},
#line 1273 "zh_symbol_map"
      {"nai6", 6277},
#line 1603 "zh_symbol_map"
      {"quan6", 6607},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 49 "zh_symbol_map"
      {"bai6", 5053},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1358 "zh_symbol_map"
      {"nin1", 6362},
#line 1364 "zh_symbol_map"
      {"ning1", 6368},
      {""}, {""}, {""},
#line 1187 "zh_symbol_map"
      {"mei4", 6191},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1185 "zh_symbol_map"
      {"mei2", 6189},
      {""}, {""}, {""}, {""},
#line 835 "zh_symbol_map"
      {"jin6", 5839},
#line 841 "zh_symbol_map"
      {"jing6", 5845},
      {""}, {""}, {""},
#line 1188 "zh_symbol_map"
      {"mei5", 6192},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1186 "zh_symbol_map"
      {"mei3", 6190},
      {""}, {""}, {""}, {""},
#line 110 "zh_symbol_map"
      {"bin1", 5114},
#line 116 "zh_symbol_map"
      {"bing1", 5120},
      {""}, {""}, {""}, {""},
#line 126 "zh_symbol_map"
      {"bo5", 5130},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2036 "zh_symbol_map"
      {"wa5", 7040},
      {""}, {""}, {""},
#line 1328 "zh_symbol_map"
      {"nia1", 6332},
#line 1247 "zh_symbol_map"
      {"mo4", 6251},
      {""}, {""}, {""},
#line 1463 "zh_symbol_map"
      {"pei4", 6467},
#line 1246 "zh_symbol_map"
      {"mo3", 6250},
#line 2065 "zh_symbol_map"
      {"wen4", 7069},
#line 2071 "zh_symbol_map"
      {"weng4", 7075},
      {""}, {""},
#line 1245 "zh_symbol_map"
      {"mo2", 6249},
      {""}, {""}, {""},
#line 1461 "zh_symbol_map"
      {"pei2", 6465},
#line 1346 "zh_symbol_map"
      {"niao1", 6350},
#line 2063 "zh_symbol_map"
      {"wen2", 7067},
#line 2069 "zh_symbol_map"
      {"weng2", 7073},
#line 632 "zh_symbol_map"
      {"gu1", 5636},
#line 805 "zh_symbol_map"
      {"jia6", 5809},
      {""}, {""}, {""}, {""},
#line 1464 "zh_symbol_map"
      {"pei5", 6468},
      {""},
#line 2066 "zh_symbol_map"
      {"wen5", 7070},
#line 2072 "zh_symbol_map"
      {"weng5", 7076},
      {""}, {""}, {""}, {""}, {""},
#line 1424 "zh_symbol_map"
      {"ou1", 6428},
#line 1462 "zh_symbol_map"
      {"pei3", 6466},
#line 823 "zh_symbol_map"
      {"jiao6", 5827},
#line 2064 "zh_symbol_map"
      {"wen3", 7068},
#line 2070 "zh_symbol_map"
      {"weng3", 7074},
      {""}, {""},
#line 1334 "zh_symbol_map"
      {"nian1", 6338},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 98 "zh_symbol_map"
      {"biao1", 5102},
      {""}, {""},
#line 1388 "zh_symbol_map"
      {"nu1", 6392},
      {""},
#line 811 "zh_symbol_map"
      {"jian6", 5815},
      {""}, {""}, {""}, {""},
#line 1517 "zh_symbol_map"
      {"po4", 6521},
      {""}, {""}, {""}, {""},
#line 1516 "zh_symbol_map"
      {"po3", 6520},
      {""}, {""}, {""}, {""},
#line 1515 "zh_symbol_map"
      {"po2", 6519},
      {""}, {""}, {""}, {""},
#line 92 "zh_symbol_map"
      {"bian1", 5096},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1711 "zh_symbol_map"
      {"sa5", 6715},
      {""}, {""},
#line 1568 "zh_symbol_map"
      {"qin1", 6572},
#line 1574 "zh_symbol_map"
      {"qing1", 6578},
#line 1740 "zh_symbol_map"
      {"se4", 6744},
      {""},
#line 128 "zh_symbol_map"
      {"bu1", 5132},
      {""}, {""},
#line 1739 "zh_symbol_map"
      {"se3", 6743},
#line 1746 "zh_symbol_map"
      {"sen4", 6750},
#line 1752 "zh_symbol_map"
      {"seng4", 6756},
      {""}, {""},
#line 1738 "zh_symbol_map"
      {"se2", 6742},
      {""}, {""}, {""}, {""}, {""},
#line 1744 "zh_symbol_map"
      {"sen2", 6748},
#line 1750 "zh_symbol_map"
      {"seng2", 6754},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1747 "zh_symbol_map"
      {"sen5", 6751},
#line 1753 "zh_symbol_map"
      {"seng5", 6757},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1745 "zh_symbol_map"
      {"sen3", 6749},
#line 1751 "zh_symbol_map"
      {"seng3", 6755},
      {""}, {""},
#line 656 "zh_symbol_map"
      {"guang1", 5660},
      {""}, {""},
#line 679 "zh_symbol_map"
      {"guo6", 5683},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 847 "zh_symbol_map"
      {"jiong6", 5851},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1538 "zh_symbol_map"
      {"qia1", 6542},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1556 "zh_symbol_map"
      {"qiao1", 6560},
      {""}, {""}, {""},
#line 1405 "zh_symbol_map"
      {"nuo6", 6409},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1874 "zh_symbol_map"
      {"si5", 6878},
      {""}, {""}, {""},
#line 1544 "zh_symbol_map"
      {"qian1", 6548},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 608 "zh_symbol_map"
      {"gen1", 5612},
#line 614 "zh_symbol_map"
      {"geng1", 5618},
      {""}, {""},
#line 1592 "zh_symbol_map"
      {"qu1", 6596},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1758 "zh_symbol_map"
      {"sha4", 6762},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1756 "zh_symbol_map"
      {"sha2", 6760},
      {""}, {""}, {""}, {""},
#line 1734 "zh_symbol_map"
      {"sao4", 6738},
#line 1782 "zh_symbol_map"
      {"shao4", 6786},
      {""}, {""}, {""},
#line 1759 "zh_symbol_map"
      {"sha5", 6763},
      {""},
#line 1304 "zh_symbol_map"
      {"nen1", 6308},
#line 1310 "zh_symbol_map"
      {"neng1", 6314},
      {""},
#line 1732 "zh_symbol_map"
      {"sao2", 6736},
#line 1780 "zh_symbol_map"
      {"shao2", 6784},
#line 607 "zh_symbol_map"
      {"gei6", 5611},
      {""}, {""},
#line 1757 "zh_symbol_map"
      {"sha3", 6761},
      {""}, {""}, {""}, {""},
#line 1735 "zh_symbol_map"
      {"sao5", 6739},
#line 1783 "zh_symbol_map"
      {"shao5", 6787},
      {""}, {""}, {""}, {""},
#line 1770 "zh_symbol_map"
      {"shan4", 6774},
#line 1776 "zh_symbol_map"
      {"shang4", 6780},
      {""}, {""},
#line 1733 "zh_symbol_map"
      {"sao3", 6737},
#line 1781 "zh_symbol_map"
      {"shao3", 6785},
#line 1774 "zh_symbol_map"
      {"shang2", 6778},
      {""}, {""}, {""},
#line 1768 "zh_symbol_map"
      {"shan2", 6772},
#line 1777 "zh_symbol_map"
      {"shang5", 6781},
      {""}, {""}, {""}, {""},
#line 1775 "zh_symbol_map"
      {"shang3", 6779},
      {""}, {""}, {""},
#line 1771 "zh_symbol_map"
      {"shan5", 6775},
#line 74 "zh_symbol_map"
      {"ben1", 5078},
#line 80 "zh_symbol_map"
      {"beng1", 5084},
      {""}, {""}, {""},
#line 1303 "zh_symbol_map"
      {"nei6", 6307},
      {""}, {""}, {""},
#line 1769 "zh_symbol_map"
      {"shan3", 6773},
      {""}, {""}, {""}, {""}, {""},
#line 1160 "zh_symbol_map"
      {"man1", 6164},
#line 1166 "zh_symbol_map"
      {"mang1", 6170},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 625 "zh_symbol_map"
      {"gong6", 5629},
#line 1764 "zh_symbol_map"
      {"shai4", 6768},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1762 "zh_symbol_map"
      {"shai2", 6766},
#line 73 "zh_symbol_map"
      {"bei6", 5077},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1765 "zh_symbol_map"
      {"shai5", 6769},
      {""}, {""}, {""}, {""}, {""},
#line 1159 "zh_symbol_map"
      {"mai6", 6163},
      {""}, {""}, {""},
#line 1763 "zh_symbol_map"
      {"shai3", 6767},
#line 590 "zh_symbol_map"
      {"gao1", 5594},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1381 "zh_symbol_map"
      {"nong6", 6385},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1442 "zh_symbol_map"
      {"pan1", 6446},
#line 1448 "zh_symbol_map"
      {"pang1", 6452},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1286 "zh_symbol_map"
      {"nao1", 6290},
      {""}, {""}, {""},
#line 794 "zh_symbol_map"
      {"ji1", 5798},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1226 "zh_symbol_map"
      {"min1", 6230},
#line 1232 "zh_symbol_map"
      {"ming1", 6236},
      {""}, {""}, {""},
#line 1441 "zh_symbol_map"
      {"pai6", 6445},
#line 1248 "zh_symbol_map"
      {"mo5", 6252},
#line 2049 "zh_symbol_map"
      {"wan6", 7053},
#line 2055 "zh_symbol_map"
      {"wang6", 7059},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 62 "zh_symbol_map"
      {"bao1", 5066},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1502 "zh_symbol_map"
      {"pin1", 6506},
#line 1508 "zh_symbol_map"
      {"ping1", 6512},
      {""}, {""}, {""}, {""},
#line 1518 "zh_symbol_map"
      {"po5", 6522},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1214 "zh_symbol_map"
      {"miao1", 6218},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1741 "zh_symbol_map"
      {"se5", 6745},
#line 1724 "zh_symbol_map"
      {"san6", 6728},
#line 1730 "zh_symbol_map"
      {"sang6", 6734},
      {""},
#line 1208 "zh_symbol_map"
      {"mian1", 6212},
#line 812 "zh_symbol_map"
      {"jiang1", 5816},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 665 "zh_symbol_map"
      {"gui4", 5669},
      {""}, {""}, {""},
#line 1256 "zh_symbol_map"
      {"mu1", 6260},
      {""}, {""}, {""}, {""},
#line 857 "zh_symbol_map"
      {"ju4", 5861},
#line 663 "zh_symbol_map"
      {"gui2", 5667},
      {""}, {""}, {""},
#line 856 "zh_symbol_map"
      {"ju3", 5860},
#line 875 "zh_symbol_map"
      {"jun4", 5879},
      {""}, {""}, {""},
#line 855 "zh_symbol_map"
      {"ju2", 5859},
#line 666 "zh_symbol_map"
      {"gui5", 5670},
      {""}, {""}, {""}, {""},
#line 873 "zh_symbol_map"
      {"jun2", 5877},
#line 1490 "zh_symbol_map"
      {"piao1", 6494},
      {""}, {""}, {""},
#line 664 "zh_symbol_map"
      {"gui3", 5668},
      {""}, {""}, {""}, {""},
#line 876 "zh_symbol_map"
      {"jun5", 5880},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 874 "zh_symbol_map"
      {"jun3", 5878},
      {""}, {""}, {""},
#line 2263 "zh_symbol_map"
      {"za4", 7267},
      {""},
#line 1484 "zh_symbol_map"
      {"pian1", 6488},
      {""}, {""},
#line 2262 "zh_symbol_map"
      {"za3", 7266},
#line 2275 "zh_symbol_map"
      {"zan4", 7279},
#line 2281 "zh_symbol_map"
      {"zang4", 7285},
      {""}, {""},
#line 2261 "zh_symbol_map"
      {"za2", 7265},
      {""}, {""}, {""}, {""}, {""},
#line 2273 "zh_symbol_map"
      {"zan2", 7277},
#line 2279 "zh_symbol_map"
      {"zang2", 7283},
      {""}, {""},
#line 1526 "zh_symbol_map"
      {"pu1", 6530},
      {""}, {""}, {""}, {""}, {""},
#line 2276 "zh_symbol_map"
      {"zan5", 7280},
#line 2282 "zh_symbol_map"
      {"zang5", 7286},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2274 "zh_symbol_map"
      {"zan3", 7278},
#line 2280 "zh_symbol_map"
      {"zang3", 7284},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 11 "zh_symbol_map"
      {"a4", 5015},
      {""}, {""}, {""}, {""},
#line 9 "zh_symbol_map"
      {"a2", 5013},
      {""}, {""},
#line 863 "zh_symbol_map"
      {"juan4", 5867},
      {""},
#line 12 "zh_symbol_map"
      {"a5", 5016},
      {""}, {""}, {""}, {""},
#line 10 "zh_symbol_map"
      {"a3", 5014},
      {""}, {""},
#line 861 "zh_symbol_map"
      {"juan2", 5865},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 864 "zh_symbol_map"
      {"juan5", 5868},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 862 "zh_symbol_map"
      {"juan3", 5866},
      {""}, {""},
#line 2437 "zh_symbol_map"
      {"zi4", 7441},
      {""}, {""}, {""}, {""},
#line 2436 "zh_symbol_map"
      {"zi3", 7440},
#line 1778 "zh_symbol_map"
      {"shang6", 6782},
      {""}, {""}, {""},
#line 2435 "zh_symbol_map"
      {"zi2", 7439},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1190 "zh_symbol_map"
      {"men1", 6194},
#line 1196 "zh_symbol_map"
      {"meng1", 6200},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1189 "zh_symbol_map"
      {"mei6", 6193},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1466 "zh_symbol_map"
      {"pen1", 6470},
#line 1472 "zh_symbol_map"
      {"peng1", 6476},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 515 "zh_symbol_map"
      {"fa4", 5519},
      {""}, {""}, {""}, {""},
#line 514 "zh_symbol_map"
      {"fa3", 5518},
#line 521 "zh_symbol_map"
      {"fan4", 5525},
#line 527 "zh_symbol_map"
      {"fang4", 5531},
      {""}, {""},
#line 513 "zh_symbol_map"
      {"fa2", 5517},
      {""}, {""}, {""}, {""}, {""},
#line 519 "zh_symbol_map"
      {"fan2", 5523},
#line 525 "zh_symbol_map"
      {"fang2", 5529},
      {""}, {""}, {""},
#line 1465 "zh_symbol_map"
      {"pei6", 6469},
      {""},
#line 2067 "zh_symbol_map"
      {"wen6", 7071},
#line 2073 "zh_symbol_map"
      {"weng6", 7077},
      {""},
#line 522 "zh_symbol_map"
      {"fan5", 5526},
#line 528 "zh_symbol_map"
      {"fang5", 5532},
      {""}, {""}, {""}, {""},
#line 122 "zh_symbol_map"
      {"bo1", 5126},
      {""}, {""},
#line 17 "zh_symbol_map"
      {"ai4", 5021},
#line 520 "zh_symbol_map"
      {"fan3", 5524},
#line 526 "zh_symbol_map"
      {"fang3", 5530},
      {""}, {""},
#line 16 "zh_symbol_map"
      {"ai3", 5020},
      {""}, {""}, {""}, {""},
#line 15 "zh_symbol_map"
      {"ai2", 5019},
      {""},
#line 2032 "zh_symbol_map"
      {"wa1", 7036},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1172 "zh_symbol_map"
      {"mao1", 6176},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1819 "zh_symbol_map"
      {"shou4", 6823},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1817 "zh_symbol_map"
      {"shou2", 6821},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1820 "zh_symbol_map"
      {"shou5", 6824},
      {""}, {""}, {""},
#line 1748 "zh_symbol_map"
      {"sen6", 6752},
#line 1754 "zh_symbol_map"
      {"seng6", 6758},
#line 1454 "zh_symbol_map"
      {"pao1", 6458},
      {""}, {""}, {""},
#line 1818 "zh_symbol_map"
      {"shou3", 6822},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1355 "zh_symbol_map"
      {"nie4", 6359},
      {""}, {""}, {""},
#line 858 "zh_symbol_map"
      {"ju5", 5862},
      {""}, {""},
#line 1707 "zh_symbol_map"
      {"sa1", 6711},
      {""}, {""},
#line 1353 "zh_symbol_map"
      {"nie2", 6357},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1356 "zh_symbol_map"
      {"nie5", 6360},
#line 137 "zh_symbol_map"
      {"ca4", 5141},
      {""}, {""}, {""}, {""},
#line 136 "zh_symbol_map"
      {"ca3", 5140},
#line 149 "zh_symbol_map"
      {"can4", 5153},
#line 155 "zh_symbol_map"
      {"cang4", 5159},
      {""},
#line 1354 "zh_symbol_map"
      {"nie3", 6358},
#line 135 "zh_symbol_map"
      {"ca2", 5139},
      {""}, {""}, {""}, {""}, {""},
#line 147 "zh_symbol_map"
      {"can2", 5151},
#line 153 "zh_symbol_map"
      {"cang2", 5157},
      {""},
#line 107 "zh_symbol_map"
      {"bie4", 5111},
      {""}, {""}, {""},
#line 2264 "zh_symbol_map"
      {"za5", 7268},
      {""}, {""},
#line 150 "zh_symbol_map"
      {"can5", 5154},
#line 156 "zh_symbol_map"
      {"cang5", 5160},
#line 2293 "zh_symbol_map"
      {"ze4", 7297},
#line 105 "zh_symbol_map"
      {"bie2", 5109},
      {""}, {""}, {""},
#line 2292 "zh_symbol_map"
      {"ze3", 7296},
#line 2305 "zh_symbol_map"
      {"zen4", 7309},
#line 2311 "zh_symbol_map"
      {"zeng4", 7315},
#line 148 "zh_symbol_map"
      {"can3", 5152},
#line 154 "zh_symbol_map"
      {"cang3", 5158},
#line 2291 "zh_symbol_map"
      {"ze2", 7295},
#line 108 "zh_symbol_map"
      {"bie5", 5112},
      {""}, {""}, {""}, {""},
#line 2303 "zh_symbol_map"
      {"zen2", 7307},
#line 2309 "zh_symbol_map"
      {"zeng2", 7313},
#line 842 "zh_symbol_map"
      {"jiong1", 5846},
      {""},
#line 509 "zh_symbol_map"
      {"er4", 5513},
#line 106 "zh_symbol_map"
      {"bie3", 5110},
      {""}, {""}, {""},
#line 508 "zh_symbol_map"
      {"er3", 5512},
#line 2306 "zh_symbol_map"
      {"zen5", 7310},
#line 2312 "zh_symbol_map"
      {"zeng5", 7316},
      {""}, {""},
#line 507 "zh_symbol_map"
      {"er2", 5511},
      {""}, {""}, {""}, {""}, {""},
#line 2304 "zh_symbol_map"
      {"zen3", 7308},
#line 2310 "zh_symbol_map"
      {"zeng3", 7314},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 13 "zh_symbol_map"
      {"a6", 5017},
      {""}, {""}, {""},
#line 1870 "zh_symbol_map"
      {"si1", 6874},
#line 1760 "zh_symbol_map"
      {"sha6", 6764},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 299 "zh_symbol_map"
      {"ci4", 5303},
      {""},
#line 1736 "zh_symbol_map"
      {"sao6", 6740},
#line 1784 "zh_symbol_map"
      {"shao6", 6788},
      {""},
#line 298 "zh_symbol_map"
      {"ci3", 5302},
      {""}, {""}, {""}, {""},
#line 297 "zh_symbol_map"
      {"ci2", 5301},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2438 "zh_symbol_map"
      {"zi5", 7442},
      {""}, {""}, {""}, {""},
#line 1772 "zh_symbol_map"
      {"shan6", 6776},
      {""},
#line 2083 "zh_symbol_map"
      {"wu4", 7087},
      {""}, {""}, {""},
#line 1565 "zh_symbol_map"
      {"qie4", 6569},
#line 2082 "zh_symbol_map"
      {"wu3", 7086},
      {""}, {""}, {""}, {""},
#line 2081 "zh_symbol_map"
      {"wu2", 7085},
      {""}, {""}, {""},
#line 1563 "zh_symbol_map"
      {"qie2", 6567},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1566 "zh_symbol_map"
      {"qie5", 6570},
      {""}, {""}, {""}, {""},
#line 2317 "zh_symbol_map"
      {"zha4", 7321},
      {""}, {""}, {""}, {""},
#line 1564 "zh_symbol_map"
      {"qie3", 6568},
      {""}, {""}, {""}, {""},
#line 2315 "zh_symbol_map"
      {"zha2", 7319},
      {""}, {""}, {""}, {""},
#line 2287 "zh_symbol_map"
      {"zao4", 7291},
#line 2341 "zh_symbol_map"
      {"zhao4", 7345},
      {""}, {""}, {""},
#line 2318 "zh_symbol_map"
      {"zha5", 7322},
      {""}, {""}, {""},
#line 1766 "zh_symbol_map"
      {"shai6", 6770},
#line 2285 "zh_symbol_map"
      {"zao2", 7289},
#line 2339 "zh_symbol_map"
      {"zhao2", 7343},
      {""}, {""}, {""},
#line 2316 "zh_symbol_map"
      {"zha3", 7320},
      {""}, {""}, {""}, {""},
#line 2288 "zh_symbol_map"
      {"zao5", 7292},
#line 2342 "zh_symbol_map"
      {"zhao5", 7346},
      {""}, {""}, {""}, {""},
#line 2329 "zh_symbol_map"
      {"zhan4", 7333},
#line 2335 "zh_symbol_map"
      {"zhang4", 7339},
      {""}, {""},
#line 2286 "zh_symbol_map"
      {"zao3", 7290},
#line 2340 "zh_symbol_map"
      {"zhao3", 7344},
#line 2333 "zh_symbol_map"
      {"zhang2", 7337},
      {""}, {""}, {""},
#line 2327 "zh_symbol_map"
      {"zhan2", 7331},
#line 2336 "zh_symbol_map"
      {"zhang5", 7340},
      {""}, {""}, {""}, {""},
#line 2334 "zh_symbol_map"
      {"zhang3", 7338},
      {""}, {""}, {""},
#line 2330 "zh_symbol_map"
      {"zhan5", 7334},
      {""}, {""}, {""}, {""}, {""},
#line 1891 "zh_symbol_map"
      {"su4", 6895},
      {""}, {""}, {""},
#line 2328 "zh_symbol_map"
      {"zhan3", 7332},
#line 1890 "zh_symbol_map"
      {"su3", 6894},
#line 1909 "zh_symbol_map"
      {"sun4", 6913},
      {""}, {""}, {""},
#line 1889 "zh_symbol_map"
      {"su2", 6893},
      {""}, {""}, {""}, {""}, {""},
#line 1907 "zh_symbol_map"
      {"sun2", 6911},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1910 "zh_symbol_map"
      {"sun5", 6914},
      {""}, {""},
#line 2323 "zh_symbol_map"
      {"zhai4", 7327},
      {""}, {""},
#line 516 "zh_symbol_map"
      {"fa5", 5520},
      {""}, {""}, {""},
#line 1908 "zh_symbol_map"
      {"sun3", 6912},
      {""}, {""},
#line 2321 "zh_symbol_map"
      {"zhai2", 7325},
      {""}, {""}, {""},
#line 539 "zh_symbol_map"
      {"fen4", 5543},
#line 545 "zh_symbol_map"
      {"feng4", 5549},
      {""}, {""}, {""}, {""},
#line 2324 "zh_symbol_map"
      {"zhai5", 7328},
      {""}, {""}, {""},
#line 537 "zh_symbol_map"
      {"fen2", 5541},
#line 543 "zh_symbol_map"
      {"feng2", 5547},
      {""}, {""}, {""}, {""},
#line 2322 "zh_symbol_map"
      {"zhai3", 7326},
      {""}, {""}, {""},
#line 540 "zh_symbol_map"
      {"fen5", 5544},
#line 546 "zh_symbol_map"
      {"feng5", 5550},
      {""}, {""},
#line 18 "zh_symbol_map"
      {"ai5", 5022},
      {""}, {""}, {""}, {""}, {""},
#line 538 "zh_symbol_map"
      {"fen3", 5542},
#line 544 "zh_symbol_map"
      {"feng3", 5548},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1415 "zh_symbol_map"
      {"nve4", 6419},
#line 1244 "zh_symbol_map"
      {"mo1", 6248},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1413 "zh_symbol_map"
      {"nve2", 6417},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1416 "zh_symbol_map"
      {"nve5", 6420},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1897 "zh_symbol_map"
      {"suan4", 6901},
#line 1414 "zh_symbol_map"
      {"nve3", 6418},
      {""}, {""}, {""}, {""},
#line 668 "zh_symbol_map"
      {"gun1", 5672},
      {""}, {""}, {""},
#line 1895 "zh_symbol_map"
      {"suan2", 6899},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1898 "zh_symbol_map"
      {"suan5", 6902},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1896 "zh_symbol_map"
      {"suan3", 6900},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1514 "zh_symbol_map"
      {"po1", 6518},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 667 "zh_symbol_map"
      {"gui6", 5671},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2041 "zh_symbol_map"
      {"wai4", 7045},
      {""}, {""},
#line 877 "zh_symbol_map"
      {"jun6", 5881},
      {""},
#line 1737 "zh_symbol_map"
      {"se1", 6741},
      {""}, {""},
#line 638 "zh_symbol_map"
      {"gua1", 5642},
      {""},
#line 2039 "zh_symbol_map"
      {"wai2", 7043},
      {""}, {""}, {""},
#line 138 "zh_symbol_map"
      {"ca5", 5142},
      {""}, {""}, {""}, {""},
#line 167 "zh_symbol_map"
      {"ce4", 5171},
#line 2042 "zh_symbol_map"
      {"wai5", 7046},
      {""}, {""}, {""},
#line 166 "zh_symbol_map"
      {"ce3", 5170},
#line 173 "zh_symbol_map"
      {"cen4", 5177},
#line 179 "zh_symbol_map"
      {"ceng4", 5183},
      {""}, {""},
#line 165 "zh_symbol_map"
      {"ce2", 5169},
#line 2040 "zh_symbol_map"
      {"wai3", 7044},
      {""}, {""}, {""}, {""},
#line 171 "zh_symbol_map"
      {"cen2", 5175},
#line 177 "zh_symbol_map"
      {"ceng2", 5181},
      {""}, {""}, {""}, {""}, {""},
#line 2294 "zh_symbol_map"
      {"ze5", 7298},
#line 2277 "zh_symbol_map"
      {"zan6", 7281},
#line 2283 "zh_symbol_map"
      {"zang6", 7287},
#line 174 "zh_symbol_map"
      {"cen5", 5178},
#line 180 "zh_symbol_map"
      {"ceng5", 5184},
      {""}, {""},
#line 650 "zh_symbol_map"
      {"guan1", 5654},
      {""}, {""}, {""},
#line 1223 "zh_symbol_map"
      {"mie4", 6227},
      {""},
#line 172 "zh_symbol_map"
      {"cen3", 5176},
#line 178 "zh_symbol_map"
      {"ceng3", 5182},
      {""}, {""}, {""}, {""}, {""},
#line 510 "zh_symbol_map"
      {"er5", 5514},
#line 1221 "zh_symbol_map"
      {"mie2", 6225},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1224 "zh_symbol_map"
      {"mie5", 6228},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1222 "zh_symbol_map"
      {"mie3", 6226},
      {""}, {""}, {""}, {""}, {""},
#line 1394 "zh_symbol_map"
      {"nuan1", 6398},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1716 "zh_symbol_map"
      {"sai4", 6720},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 644 "zh_symbol_map"
      {"guai1", 5648},
      {""},
#line 1714 "zh_symbol_map"
      {"sai2", 6718},
      {""}, {""},
#line 865 "zh_symbol_map"
      {"juan6", 5869},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1717 "zh_symbol_map"
      {"sai5", 6721},
      {""},
#line 1499 "zh_symbol_map"
      {"pie4", 6503},
#line 300 "zh_symbol_map"
      {"ci5", 5304},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1715 "zh_symbol_map"
      {"sai3", 6719},
      {""},
#line 1497 "zh_symbol_map"
      {"pie2", 6501},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1500 "zh_symbol_map"
      {"pie5", 6504},
      {""}, {""}, {""}, {""},
#line 1610 "zh_symbol_map"
      {"qun1", 6614},
      {""}, {""}, {""}, {""},
#line 1498 "zh_symbol_map"
      {"pie3", 6502},
#line 2084 "zh_symbol_map"
      {"wu5", 7088},
      {""}, {""}, {""}, {""}, {""},
#line 185 "zh_symbol_map"
      {"cha4", 5189},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 183 "zh_symbol_map"
      {"cha2", 5187},
      {""}, {""}, {""}, {""},
#line 161 "zh_symbol_map"
      {"cao4", 5165},
#line 209 "zh_symbol_map"
      {"chao4", 5213},
      {""}, {""},
#line 1373 "zh_symbol_map"
      {"niu4", 6377},
#line 186 "zh_symbol_map"
      {"cha5", 5190},
      {""}, {""}, {""}, {""},
#line 159 "zh_symbol_map"
      {"cao2", 5163},
#line 207 "zh_symbol_map"
      {"chao2", 5211},
      {""}, {""},
#line 1371 "zh_symbol_map"
      {"niu2", 6375},
#line 184 "zh_symbol_map"
      {"cha3", 5188},
      {""}, {""},
#line 1773 "zh_symbol_map"
      {"shang1", 6777},
      {""},
#line 162 "zh_symbol_map"
      {"cao5", 5166},
#line 210 "zh_symbol_map"
      {"chao5", 5214},
      {""}, {""},
#line 1374 "zh_symbol_map"
      {"niu5", 6378},
      {""},
#line 197 "zh_symbol_map"
      {"chan4", 5201},
#line 203 "zh_symbol_map"
      {"chang4", 5207},
      {""}, {""},
#line 160 "zh_symbol_map"
      {"cao3", 5164},
#line 208 "zh_symbol_map"
      {"chao3", 5212},
#line 201 "zh_symbol_map"
      {"chang2", 5205},
      {""},
#line 1372 "zh_symbol_map"
      {"niu3", 6376},
      {""},
#line 195 "zh_symbol_map"
      {"chan2", 5199},
#line 204 "zh_symbol_map"
      {"chang5", 5208},
#line 572 "zh_symbol_map"
      {"gai1", 5576},
      {""}, {""}, {""},
#line 202 "zh_symbol_map"
      {"chang3", 5206},
      {""}, {""}, {""},
#line 198 "zh_symbol_map"
      {"chan5", 5202},
      {""}, {""}, {""},
#line 2337 "zh_symbol_map"
      {"zhang6", 7341},
      {""}, {""}, {""}, {""}, {""},
#line 196 "zh_symbol_map"
      {"chan3", 5200},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1892 "zh_symbol_map"
      {"su5", 6896},
      {""}, {""},
#line 1268 "zh_symbol_map"
      {"nai1", 6272},
#line 1598 "zh_symbol_map"
      {"quan1", 6602},
      {""},
#line 191 "zh_symbol_map"
      {"chai4", 5195},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 189 "zh_symbol_map"
      {"chai2", 5193},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 192 "zh_symbol_map"
      {"chai5", 5196},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 190 "zh_symbol_map"
      {"chai3", 5194},
      {""},
#line 523 "zh_symbol_map"
      {"fan6", 5527},
#line 529 "zh_symbol_map"
      {"fang6", 5533},
      {""}, {""}, {""},
#line 44 "zh_symbol_map"
      {"bai1", 5048},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2377 "zh_symbol_map"
      {"zhong4", 7381},
      {""}, {""}, {""}, {""},
#line 2375 "zh_symbol_map"
      {"zhong2", 7379},
      {""}, {""}, {""}, {""},
#line 2378 "zh_symbol_map"
      {"zhong5", 7382},
      {""}, {""}, {""}, {""},
#line 2376 "zh_symbol_map"
      {"zhong3", 7380},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 830 "zh_symbol_map"
      {"jin1", 5834},
#line 836 "zh_symbol_map"
      {"jing1", 5840},
      {""}, {""}, {""}, {""},
#line 1589 "zh_symbol_map"
      {"qiu4", 6593},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1587 "zh_symbol_map"
      {"qiu2", 6591},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1590 "zh_symbol_map"
      {"qiu5", 6594},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1588 "zh_symbol_map"
      {"qiu3", 6592},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1821 "zh_symbol_map"
      {"shou6", 6825},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 800 "zh_symbol_map"
      {"jia1", 5804},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1915 "zh_symbol_map"
      {"suo4", 6919},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 818 "zh_symbol_map"
      {"jiao1", 5822},
      {""},
#line 1913 "zh_symbol_map"
      {"suo2", 6917},
      {""},
#line 1357 "zh_symbol_map"
      {"nie6", 6361},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1916 "zh_symbol_map"
      {"suo5", 6920},
      {""}, {""}, {""},
#line 2059 "zh_symbol_map"
      {"wei4", 7063},
      {""}, {""}, {""}, {""}, {""},
#line 1914 "zh_symbol_map"
      {"suo3", 6918},
      {""}, {""},
#line 806 "zh_symbol_map"
      {"jian1", 5810},
#line 2057 "zh_symbol_map"
      {"wei2", 7061},
      {""}, {""}, {""},
#line 168 "zh_symbol_map"
      {"ce5", 5172},
#line 151 "zh_symbol_map"
      {"can6", 5155},
#line 157 "zh_symbol_map"
      {"cang6", 5161},
      {""}, {""},
#line 2383 "zh_symbol_map"
      {"zhou4", 7387},
#line 2060 "zh_symbol_map"
      {"wei5", 7064},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 854 "zh_symbol_map"
      {"ju1", 5858},
#line 109 "zh_symbol_map"
      {"bie6", 5113},
#line 2381 "zh_symbol_map"
      {"zhou2", 7385},
#line 2058 "zh_symbol_map"
      {"wei3", 7062},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2384 "zh_symbol_map"
      {"zhou5", 7388},
      {""}, {""}, {""},
#line 2307 "zh_symbol_map"
      {"zen6", 7311},
#line 2313 "zh_symbol_map"
      {"zeng6", 7317},
      {""}, {""}, {""}, {""},
#line 2382 "zh_symbol_map"
      {"zhou3", 7386},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 497 "zh_symbol_map"
      {"en4", 5501},
      {""}, {""},
#line 2077 "zh_symbol_map"
      {"wo4", 7081},
      {""},
#line 496 "zh_symbol_map"
      {"en3", 5500},
      {""}, {""},
#line 2076 "zh_symbol_map"
      {"wo3", 7080},
#line 2260 "zh_symbol_map"
      {"za1", 7264},
#line 495 "zh_symbol_map"
      {"en2", 5499},
      {""}, {""},
#line 2075 "zh_symbol_map"
      {"wo2", 7079},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 674 "zh_symbol_map"
      {"guo1", 5678},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1400 "zh_symbol_map"
      {"nuo1", 6404},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 8 "zh_symbol_map"
      {"a1", 5012},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1567 "zh_symbol_map"
      {"qie6", 6571},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1813 "zh_symbol_map"
      {"shi4", 6817},
      {""}, {""},
#line 1879 "zh_symbol_map"
      {"song4", 6883},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1811 "zh_symbol_map"
      {"shi2", 6815},
#line 2434 "zh_symbol_map"
      {"zi1", 7438},
#line 2319 "zh_symbol_map"
      {"zha6", 7323},
#line 1877 "zh_symbol_map"
      {"song2", 6881},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1814 "zh_symbol_map"
      {"shi5", 6818},
      {""}, {""},
#line 1880 "zh_symbol_map"
      {"song5", 6884},
      {""}, {""}, {""},
#line 2289 "zh_symbol_map"
      {"zao6", 7293},
#line 2343 "zh_symbol_map"
      {"zhao6", 7347},
      {""},
#line 1812 "zh_symbol_map"
      {"shi3", 6816},
#line 205 "zh_symbol_map"
      {"chang6", 5209},
      {""},
#line 1878 "zh_symbol_map"
      {"song3", 6882},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 602 "zh_symbol_map"
      {"gei1", 5606},
      {""}, {""}, {""}, {""}, {""},
#line 2331 "zh_symbol_map"
      {"zhan6", 7335},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1241 "zh_symbol_map"
      {"miu4", 6245},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1239 "zh_symbol_map"
      {"miu2", 6243},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1911 "zh_symbol_map"
      {"sun6", 6915},
      {""},
#line 1298 "zh_symbol_map"
      {"nei1", 6302},
#line 1242 "zh_symbol_map"
      {"miu5", 6246},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1240 "zh_symbol_map"
      {"miu3", 6244},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2325 "zh_symbol_map"
      {"zhai6", 7329},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 620 "zh_symbol_map"
      {"gong1", 5624},
#line 239 "zh_symbol_map"
      {"chong4", 5243},
      {""}, {""}, {""}, {""},
#line 237 "zh_symbol_map"
      {"chong2", 5241},
#line 541 "zh_symbol_map"
      {"fen6", 5545},
#line 547 "zh_symbol_map"
      {"feng6", 5551},
      {""}, {""},
#line 240 "zh_symbol_map"
      {"chong5", 5244},
#line 68 "zh_symbol_map"
      {"bei1", 5072},
      {""}, {""}, {""},
#line 238 "zh_symbol_map"
      {"chong3", 5242},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2379 "zh_symbol_map"
      {"zhong6", 7383},
      {""}, {""},
#line 1154 "zh_symbol_map"
      {"mai1", 6158},
      {""}, {""}, {""},
#line 512 "zh_symbol_map"
      {"fa1", 5516},
      {""}, {""}, {""}, {""},
#line 2455 "zh_symbol_map"
      {"zu4", 7459},
      {""}, {""}, {""},
#line 1376 "zh_symbol_map"
      {"nong1", 6380},
#line 2454 "zh_symbol_map"
      {"zu3", 7458},
#line 2473 "zh_symbol_map"
      {"zun4", 7477},
      {""}, {""}, {""},
#line 2453 "zh_symbol_map"
      {"zu2", 7457},
#line 1417 "zh_symbol_map"
      {"nve6", 6421},
      {""}, {""}, {""}, {""},
#line 2471 "zh_symbol_map"
      {"zun2", 7475},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2474 "zh_symbol_map"
      {"zun5", 7478},
      {""}, {""}, {""},
#line 14 "zh_symbol_map"
      {"ai1", 5018},
      {""}, {""}, {""}, {""}, {""},
#line 2472 "zh_symbol_map"
      {"zun3", 7476},
      {""}, {""}, {""},
#line 1899 "zh_symbol_map"
      {"suan6", 6903},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1436 "zh_symbol_map"
      {"pai1", 6440},
      {""},
#line 2044 "zh_symbol_map"
      {"wan1", 7048},
#line 2050 "zh_symbol_map"
      {"wang1", 7054},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2043 "zh_symbol_map"
      {"wai6", 7047},
#line 245 "zh_symbol_map"
      {"chou4", 5249},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2461 "zh_symbol_map"
      {"zuan4", 7465},
      {""},
#line 243 "zh_symbol_map"
      {"chou2", 5247},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2459 "zh_symbol_map"
      {"zuan2", 7463},
      {""},
#line 246 "zh_symbol_map"
      {"chou5", 5250},
      {""}, {""}, {""},
#line 175 "zh_symbol_map"
      {"cen6", 5179},
#line 181 "zh_symbol_map"
      {"ceng6", 5185},
      {""}, {""},
#line 2462 "zh_symbol_map"
      {"zuan5", 7466},
      {""},
#line 244 "zh_symbol_map"
      {"chou3", 5248},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2460 "zh_symbol_map"
      {"zuan3", 7464},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 134 "zh_symbol_map"
      {"ca1", 5138},
      {""},
#line 1719 "zh_symbol_map"
      {"san1", 6723},
#line 1725 "zh_symbol_map"
      {"sang1", 6729},
#line 1225 "zh_symbol_map"
      {"mie6", 6229},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 498 "zh_symbol_map"
      {"en5", 5502},
      {""}, {""},
#line 2078 "zh_symbol_map"
      {"wo5", 7082},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2290 "zh_symbol_map"
      {"ze1", 7294},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1718 "zh_symbol_map"
      {"sai6", 6722},
#line 506 "zh_symbol_map"
      {"er1", 5510},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1501 "zh_symbol_map"
      {"pie6", 6505},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 563 "zh_symbol_map"
      {"fu4", 5567},
      {""}, {""}, {""}, {""},
#line 562 "zh_symbol_map"
      {"fu3", 5566},
      {""}, {""}, {""}, {""},
#line 561 "zh_symbol_map"
      {"fu2", 5565},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 296 "zh_symbol_map"
      {"ci1", 5300},
#line 187 "zh_symbol_map"
      {"cha6", 5191},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2269 "zh_symbol_map"
      {"zai4", 7273},
      {""},
#line 163 "zh_symbol_map"
      {"cao6", 5167},
#line 211 "zh_symbol_map"
      {"chao6", 5215},
      {""}, {""},
#line 1375 "zh_symbol_map"
      {"niu6", 6379},
      {""}, {""}, {""},
#line 2267 "zh_symbol_map"
      {"zai2", 7271},
      {""}, {""}, {""}, {""}, {""},
#line 2080 "zh_symbol_map"
      {"wu1", 7084},
      {""}, {""}, {""},
#line 2270 "zh_symbol_map"
      {"zai5", 7274},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 199 "zh_symbol_map"
      {"chan6", 5203},
      {""},
#line 2268 "zh_symbol_map"
      {"zai3", 7272},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 193 "zh_symbol_map"
      {"chai6", 5197},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2332 "zh_symbol_map"
      {"zhang1", 7336},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1888 "zh_symbol_map"
      {"su1", 6892},
      {""},
#line 241 "zh_symbol_map"
      {"chong6", 5245},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 317 "zh_symbol_map"
      {"cu4", 5321},
      {""}, {""}, {""}, {""},
#line 316 "zh_symbol_map"
      {"cu3", 5320},
#line 335 "zh_symbol_map"
      {"cun4", 5339},
      {""}, {""}, {""},
#line 315 "zh_symbol_map"
      {"cu2", 5319},
      {""}, {""}, {""}, {""}, {""},
#line 333 "zh_symbol_map"
      {"cun2", 5337},
      {""}, {""},
#line 1184 "zh_symbol_map"
      {"mei1", 6188},
      {""}, {""}, {""},
#line 2456 "zh_symbol_map"
      {"zu5", 7460},
      {""}, {""},
#line 336 "zh_symbol_map"
      {"cun5", 5340},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1607 "zh_symbol_map"
      {"que4", 6611},
      {""},
#line 334 "zh_symbol_map"
      {"cun3", 5338},
      {""}, {""}, {""},
#line 1591 "zh_symbol_map"
      {"qiu6", 6595},
      {""}, {""}, {""},
#line 1605 "zh_symbol_map"
      {"que2", 6609},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1608 "zh_symbol_map"
      {"que5", 6612},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1606 "zh_symbol_map"
      {"que3", 6610},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1460 "zh_symbol_map"
      {"pei1", 6464},
      {""},
#line 2062 "zh_symbol_map"
      {"wen1", 7066},
#line 2068 "zh_symbol_map"
      {"weng1", 7072},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1917 "zh_symbol_map"
      {"suo6", 6921},
      {""}, {""}, {""}, {""},
#line 323 "zh_symbol_map"
      {"cuan4", 5327},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 321 "zh_symbol_map"
      {"cuan2", 5325},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2061 "zh_symbol_map"
      {"wei6", 7065},
#line 324 "zh_symbol_map"
      {"cuan5", 5328},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 322 "zh_symbol_map"
      {"cuan3", 5326},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2385 "zh_symbol_map"
      {"zhou6", 7389},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2479 "zh_symbol_map"
      {"zuo4", 7483},
#line 164 "zh_symbol_map"
      {"ce1", 5168},
      {""},
#line 1743 "zh_symbol_map"
      {"sen1", 6747},
#line 1749 "zh_symbol_map"
      {"seng1", 6753},
      {""}, {""}, {""}, {""}, {""},
#line 2477 "zh_symbol_map"
      {"zuo2", 7481},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2480 "zh_symbol_map"
      {"zuo5", 7484},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2478 "zh_symbol_map"
      {"zuo3", 7482},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 827 "zh_symbol_map"
      {"jie4", 5831},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 825 "zh_symbol_map"
      {"jie2", 5829},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 828 "zh_symbol_map"
      {"jie5", 5832},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 826 "zh_symbol_map"
      {"jie3", 5830},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 564 "zh_symbol_map"
      {"fu5", 5568},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 143 "zh_symbol_map"
      {"cai4", 5147},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 141 "zh_symbol_map"
      {"cai2", 5145},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 144 "zh_symbol_map"
      {"cai5", 5148},
#line 1815 "zh_symbol_map"
      {"shi6", 6819},
      {""}, {""},
#line 1881 "zh_symbol_map"
      {"song6", 6885},
      {""}, {""}, {""},
#line 2299 "zh_symbol_map"
      {"zei4", 7303},
      {""},
#line 142 "zh_symbol_map"
      {"cai3", 5146},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2297 "zh_symbol_map"
      {"zei2", 7301},
      {""}, {""},
#line 1755 "zh_symbol_map"
      {"sha1", 6759},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2300 "zh_symbol_map"
      {"zei5", 7304},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1731 "zh_symbol_map"
      {"sao1", 6735},
#line 1779 "zh_symbol_map"
      {"shao1", 6783},
#line 2298 "zh_symbol_map"
      {"zei3", 7302},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1767 "zh_symbol_map"
      {"shan1", 6771},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1243 "zh_symbol_map"
      {"miu6", 6247},
      {""}, {""},
#line 200 "zh_symbol_map"
      {"chang1", 5204},
#line 2371 "zh_symbol_map"
      {"zhi4", 7375},
      {""}, {""},
#line 2443 "zh_symbol_map"
      {"zong4", 7447},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2369 "zh_symbol_map"
      {"zhi2", 7373},
      {""}, {""},
#line 2441 "zh_symbol_map"
      {"zong2", 7445},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2372 "zh_symbol_map"
      {"zhi5", 7376},
      {""}, {""},
#line 2444 "zh_symbol_map"
      {"zong5", 7448},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2370 "zh_symbol_map"
      {"zhi3", 7374},
      {""}, {""},
#line 2442 "zh_symbol_map"
      {"zong3", 7446},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1761 "zh_symbol_map"
      {"shai1", 6765},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 318 "zh_symbol_map"
      {"cu5", 5322},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2475 "zh_symbol_map"
      {"zun6", 7479},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2374 "zh_symbol_map"
      {"zhong1", 7378},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1903 "zh_symbol_map"
      {"sui4", 6907},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1901 "zh_symbol_map"
      {"sui2", 6905},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1904 "zh_symbol_map"
      {"sui5", 6908},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 35 "zh_symbol_map"
      {"ao4", 5039},
      {""},
#line 1902 "zh_symbol_map"
      {"sui3", 6906},
      {""}, {""},
#line 34 "zh_symbol_map"
      {"ao3", 5038},
      {""}, {""}, {""},
#line 533 "zh_symbol_map"
      {"fei4", 5537},
#line 33 "zh_symbol_map"
      {"ao2", 5037},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 247 "zh_symbol_map"
      {"chou6", 5251},
      {""},
#line 531 "zh_symbol_map"
      {"fei2", 5535},
      {""}, {""}, {""}, {""}, {""},
#line 2463 "zh_symbol_map"
      {"zuan6", 7467},
      {""}, {""}, {""},
#line 534 "zh_symbol_map"
      {"fei5", 5538},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 532 "zh_symbol_map"
      {"fei3", 5536},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 341 "zh_symbol_map"
      {"cuo4", 5345},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 339 "zh_symbol_map"
      {"cuo2", 5343},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 342 "zh_symbol_map"
      {"cuo5", 5346},
      {""}, {""}, {""},
#line 551 "zh_symbol_map"
      {"fo4", 5555},
      {""}, {""}, {""}, {""},
#line 550 "zh_symbol_map"
      {"fo3", 5554},
#line 340 "zh_symbol_map"
      {"cuo3", 5344},
      {""}, {""}, {""},
#line 549 "zh_symbol_map"
      {"fo2", 5553},
      {""}, {""}, {""},
#line 662 "zh_symbol_map"
      {"gui1", 5666},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 872 "zh_symbol_map"
      {"jun1", 5876},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 494 "zh_symbol_map"
      {"en1", 5498},
      {""}, {""},
#line 2074 "zh_symbol_map"
      {"wo1", 7078},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2272 "zh_symbol_map"
      {"zan1", 7276},
#line 2278 "zh_symbol_map"
      {"zang1", 7282},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2271 "zh_symbol_map"
      {"zai6", 7275},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 860 "zh_symbol_map"
      {"juan1", 5864},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 233 "zh_symbol_map"
      {"chi4", 5237},
      {""}, {""},
#line 305 "zh_symbol_map"
      {"cong4", 5309},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 231 "zh_symbol_map"
      {"chi2", 5235},
      {""}, {""},
#line 303 "zh_symbol_map"
      {"cong2", 5307},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 234 "zh_symbol_map"
      {"chi5", 5238},
      {""}, {""},
#line 306 "zh_symbol_map"
      {"cong5", 5310},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 232 "zh_symbol_map"
      {"chi3", 5236},
      {""}, {""},
#line 304 "zh_symbol_map"
      {"cong3", 5308},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 851 "zh_symbol_map"
      {"jiu4", 5855},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 849 "zh_symbol_map"
      {"jiu2", 5853},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 852 "zh_symbol_map"
      {"jiu5", 5856},
      {""}, {""}, {""}, {""}, {""},
#line 337 "zh_symbol_map"
      {"cun6", 5341},
      {""}, {""}, {""},
#line 850 "zh_symbol_map"
      {"jiu3", 5854},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 236 "zh_symbol_map"
      {"chong1", 5240},
      {""}, {""}, {""}, {""}, {""},
#line 1609 "zh_symbol_map"
      {"que6", 6613},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2452 "zh_symbol_map"
      {"zu1", 7456},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 518 "zh_symbol_map"
      {"fan1", 5522},
#line 524 "zh_symbol_map"
      {"fang1", 5528},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 36 "zh_symbol_map"
      {"ao5", 5040},
      {""},
#line 325 "zh_symbol_map"
      {"cuan6", 5329},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1816 "zh_symbol_map"
      {"shou1", 6820},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2481 "zh_symbol_map"
      {"zuo6", 7485},
#line 552 "zh_symbol_map"
      {"fo5", 5556},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1352 "zh_symbol_map"
      {"nie1", 6356},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 829 "zh_symbol_map"
      {"jie6", 5833},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 146 "zh_symbol_map"
      {"can1", 5150},
#line 152 "zh_symbol_map"
      {"cang1", 5156},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 104 "zh_symbol_map"
      {"bie1", 5108},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2302 "zh_symbol_map"
      {"zen1", 7306},
#line 2308 "zh_symbol_map"
      {"zeng1", 7312},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 145 "zh_symbol_map"
      {"cai6", 5149},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 560 "zh_symbol_map"
      {"fu1", 5564},
#line 2301 "zh_symbol_map"
      {"zei6", 7305},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 629 "zh_symbol_map"
      {"gou4", 5633},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1562 "zh_symbol_map"
      {"qie1", 6566},
      {""}, {""},
#line 627 "zh_symbol_map"
      {"gou2", 5631},
      {""},
#line 2373 "zh_symbol_map"
      {"zhi6", 7377},
      {""}, {""},
#line 2445 "zh_symbol_map"
      {"zong6", 7449},
      {""}, {""}, {""}, {""},
#line 630 "zh_symbol_map"
      {"gou5", 5634},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 628 "zh_symbol_map"
      {"gou3", 5632},
      {""},
#line 2314 "zh_symbol_map"
      {"zha1", 7318},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1385 "zh_symbol_map"
      {"nou4", 6389},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2284 "zh_symbol_map"
      {"zao1", 7288},
#line 2338 "zh_symbol_map"
      {"zhao1", 7342},
      {""},
#line 1383 "zh_symbol_map"
      {"nou2", 6387},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1386 "zh_symbol_map"
      {"nou5", 6390},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1384 "zh_symbol_map"
      {"nou3", 6388},
      {""}, {""},
#line 2326 "zh_symbol_map"
      {"zhan1", 7330},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1788 "zh_symbol_map"
      {"she4", 6792},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1786 "zh_symbol_map"
      {"she2", 6790},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1789 "zh_symbol_map"
      {"she5", 6793},
      {""}, {""},
#line 314 "zh_symbol_map"
      {"cu1", 5318},
      {""},
#line 1906 "zh_symbol_map"
      {"sun1", 6910},
      {""}, {""}, {""}, {""},
#line 1787 "zh_symbol_map"
      {"she3", 6791},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1801 "zh_symbol_map"
      {"shen4", 6805},
#line 1807 "zh_symbol_map"
      {"sheng4", 6811},
      {""}, {""}, {""}, {""},
#line 1805 "zh_symbol_map"
      {"sheng2", 6809},
#line 2320 "zh_symbol_map"
      {"zhai1", 7324},
      {""}, {""},
#line 1799 "zh_symbol_map"
      {"shen2", 6803},
#line 1808 "zh_symbol_map"
      {"sheng5", 6812},
      {""}, {""}, {""}, {""},
#line 1806 "zh_symbol_map"
      {"sheng3", 6810},
      {""},
#line 1798 "zh_symbol_map"
      {"shen2me5", 6802},
      {""},
#line 1802 "zh_symbol_map"
      {"shen5", 6806},
#line 536 "zh_symbol_map"
      {"fen1", 5540},
#line 542 "zh_symbol_map"
      {"feng1", 5546},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1905 "zh_symbol_map"
      {"sui6", 6909},
#line 1800 "zh_symbol_map"
      {"shen3", 6804},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1794 "zh_symbol_map"
      {"shei4", 6798},
      {""}, {""}, {""}, {""}, {""},
#line 1412 "zh_symbol_map"
      {"nve1", 6416},
      {""}, {""}, {""},
#line 1792 "zh_symbol_map"
      {"shei2", 6796},
#line 535 "zh_symbol_map"
      {"fei6", 5539},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1795 "zh_symbol_map"
      {"shei5", 6799},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1793 "zh_symbol_map"
      {"shei3", 6797},
      {""}, {""}, {""}, {""},
#line 1894 "zh_symbol_map"
      {"suan1", 6898},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2467 "zh_symbol_map"
      {"zui4", 7471},
      {""},
#line 343 "zh_symbol_map"
      {"cuo6", 5347},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2465 "zh_symbol_map"
      {"zui2", 7469},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2468 "zh_symbol_map"
      {"zui5", 7472},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2466 "zh_symbol_map"
      {"zui3", 7470},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2038 "zh_symbol_map"
      {"wai1", 7042},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 170 "zh_symbol_map"
      {"cen1", 5174},
#line 176 "zh_symbol_map"
      {"ceng1", 5180},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1220 "zh_symbol_map"
      {"mie1", 6224},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1713 "zh_symbol_map"
      {"sai1", 6717},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1496 "zh_symbol_map"
      {"pie1", 6500},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 235 "zh_symbol_map"
      {"chi6", 5239},
      {""}, {""},
#line 307 "zh_symbol_map"
      {"cong6", 5311},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 182 "zh_symbol_map"
      {"cha1", 5186},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 158 "zh_symbol_map"
      {"cao1", 5162},
#line 206 "zh_symbol_map"
      {"chao1", 5210},
      {""}, {""},
#line 1370 "zh_symbol_map"
      {"niu1", 6374},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 869 "zh_symbol_map"
      {"jue4", 5873},
      {""}, {""}, {""}, {""}, {""},
#line 853 "zh_symbol_map"
      {"jiu6", 5857},
      {""},
#line 194 "zh_symbol_map"
      {"chan1", 5198},
      {""},
#line 867 "zh_symbol_map"
      {"jue2", 5871},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 870 "zh_symbol_map"
      {"jue5", 5874},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 868 "zh_symbol_map"
      {"jue3", 5872},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1253 "zh_symbol_map"
      {"mou4", 6257},
      {""}, {""}, {""}, {""},
#line 188 "zh_symbol_map"
      {"chai1", 5192},
      {""}, {""}, {""}, {""},
#line 1251 "zh_symbol_map"
      {"mou2", 6255},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1254 "zh_symbol_map"
      {"mou5", 6258},
      {""},
#line 1809 "zh_symbol_map"
      {"sheng6", 6813},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1252 "zh_symbol_map"
      {"mou3", 6256},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1523 "zh_symbol_map"
      {"pou4", 6527},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1521 "zh_symbol_map"
      {"pou2", 6525},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1524 "zh_symbol_map"
      {"pou5", 6528},
      {""}, {""},
#line 1586 "zh_symbol_map"
      {"qiu1", 6590},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1522 "zh_symbol_map"
      {"pou3", 6526},
      {""}, {""}, {""},
#line 329 "zh_symbol_map"
      {"cui4", 5333},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 327 "zh_symbol_map"
      {"cui2", 5331},
      {""}, {""}, {""},
#line 32 "zh_symbol_map"
      {"ao1", 5036},
      {""}, {""}, {""}, {""}, {""},
#line 330 "zh_symbol_map"
      {"cui5", 5334},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 328 "zh_symbol_map"
      {"cui3", 5332},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1912 "zh_symbol_map"
      {"suo1", 6916},
      {""}, {""}, {""}, {""}, {""},
#line 1825 "zh_symbol_map"
      {"shu4", 6829},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1823 "zh_symbol_map"
      {"shu2", 6827},
      {""}, {""}, {""}, {""}, {""},
#line 1867 "zh_symbol_map"
      {"shuo4", 6871},
      {""},
#line 2056 "zh_symbol_map"
      {"wei1", 7060},
      {""},
#line 1826 "zh_symbol_map"
      {"shu5", 6830},
      {""},
#line 548 "zh_symbol_map"
      {"fo1", 5552},
      {""}, {""}, {""},
#line 1865 "zh_symbol_map"
      {"shuo2", 6869},
      {""}, {""}, {""},
#line 1824 "zh_symbol_map"
      {"shu3", 6828},
      {""}, {""}, {""}, {""}, {""},
#line 1868 "zh_symbol_map"
      {"shuo5", 6872},
#line 2380 "zh_symbol_map"
      {"zhou1", 7384},
      {""}, {""}, {""},
#line 1861 "zh_symbol_map"
      {"shun4", 6865},
      {""}, {""}, {""}, {""},
#line 1866 "zh_symbol_map"
      {"shuo3", 6870},
      {""}, {""}, {""}, {""},
#line 1859 "zh_symbol_map"
      {"shun2", 6863},
#line 347 "zh_symbol_map"
      {"da4", 5351},
      {""}, {""}, {""}, {""},
#line 346 "zh_symbol_map"
      {"da3", 5350},
#line 359 "zh_symbol_map"
      {"dan4", 5363},
#line 365 "zh_symbol_map"
      {"dang4", 5369},
      {""},
#line 1862 "zh_symbol_map"
      {"shun5", 6866},
#line 345 "zh_symbol_map"
      {"da2", 5349},
      {""}, {""}, {""}, {""}, {""},
#line 357 "zh_symbol_map"
      {"dan2", 5361},
#line 363 "zh_symbol_map"
      {"dang2", 5367},
      {""},
#line 1860 "zh_symbol_map"
      {"shun3", 6864},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 360 "zh_symbol_map"
      {"dan5", 5364},
#line 366 "zh_symbol_map"
      {"dang5", 5370},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 358 "zh_symbol_map"
      {"dan3", 5362},
#line 364 "zh_symbol_map"
      {"dang3", 5368},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1855 "zh_symbol_map"
      {"shui4", 6859},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1853 "zh_symbol_map"
      {"shui2", 6857},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1856 "zh_symbol_map"
      {"shui5", 6860},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1854 "zh_symbol_map"
      {"shui3", 6858},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 401 "zh_symbol_map"
      {"di4", 5405},
      {""},
#line 631 "zh_symbol_map"
      {"gou6", 5635},
      {""}, {""},
#line 400 "zh_symbol_map"
      {"di3", 5404},
      {""},
#line 431 "zh_symbol_map"
      {"ding4", 5435},
      {""}, {""},
#line 399 "zh_symbol_map"
      {"di2", 5403},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 429 "zh_symbol_map"
      {"ding2", 5433},
      {""}, {""}, {""}, {""},
#line 1810 "zh_symbol_map"
      {"shi1", 6814},
      {""}, {""},
#line 1876 "zh_symbol_map"
      {"song1", 6880},
      {""},
#line 432 "zh_symbol_map"
      {"ding5", 5436},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 430 "zh_symbol_map"
      {"ding3", 5434},
      {""}, {""}, {""}, {""},
#line 1387 "zh_symbol_map"
      {"nou6", 6391},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 407 "zh_symbol_map"
      {"dia4", 5411},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 405 "zh_symbol_map"
      {"dia2", 5409},
#line 1790 "zh_symbol_map"
      {"she6", 6794},
      {""}, {""}, {""}, {""},
#line 419 "zh_symbol_map"
      {"diao4", 5423},
      {""}, {""},
#line 1238 "zh_symbol_map"
      {"miu1", 6242},
#line 408 "zh_symbol_map"
      {"dia5", 5412},
      {""}, {""}, {""}, {""}, {""},
#line 417 "zh_symbol_map"
      {"diao2", 5421},
      {""}, {""}, {""},
#line 406 "zh_symbol_map"
      {"dia3", 5410},
      {""}, {""}, {""}, {""}, {""},
#line 420 "zh_symbol_map"
      {"diao5", 5424},
      {""}, {""}, {""}, {""},
#line 413 "zh_symbol_map"
      {"dian4", 5417},
      {""}, {""}, {""}, {""},
#line 418 "zh_symbol_map"
      {"diao3", 5422},
      {""}, {""}, {""}, {""},
#line 411 "zh_symbol_map"
      {"dian2", 5415},
#line 1803 "zh_symbol_map"
      {"shen6", 6807},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 414 "zh_symbol_map"
      {"dian5", 5418},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 412 "zh_symbol_map"
      {"dian3", 5416},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2347 "zh_symbol_map"
      {"zhe4", 7351},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2345 "zh_symbol_map"
      {"zhe2", 7349},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2348 "zh_symbol_map"
      {"zhe5", 7352},
      {""}, {""}, {""},
#line 1796 "zh_symbol_map"
      {"shei6", 6800},
#line 2470 "zh_symbol_map"
      {"zun1", 7474},
      {""}, {""}, {""}, {""},
#line 2346 "zh_symbol_map"
      {"zhe3", 7350},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2359 "zh_symbol_map"
      {"zhen4", 7363},
#line 2365 "zh_symbol_map"
      {"zheng4", 7369},
      {""}, {""}, {""}, {""},
#line 2363 "zh_symbol_map"
      {"zheng2", 7367},
      {""}, {""}, {""},
#line 2357 "zh_symbol_map"
      {"zhen2", 7361},
#line 2366 "zh_symbol_map"
      {"zheng5", 7370},
      {""}, {""}, {""}, {""},
#line 2364 "zh_symbol_map"
      {"zheng3", 7368},
      {""}, {""}, {""},
#line 2360 "zh_symbol_map"
      {"zhen5", 7364},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2469 "zh_symbol_map"
      {"zui6", 7473},
#line 2358 "zh_symbol_map"
      {"zhen3", 7362},
      {""}, {""}, {""},
#line 995 "zh_symbol_map"
      {"la4", 5999},
      {""}, {""}, {""}, {""},
#line 994 "zh_symbol_map"
      {"la3", 5998},
#line 1007 "zh_symbol_map"
      {"lan4", 6011},
#line 1013 "zh_symbol_map"
      {"lang4", 6017},
      {""}, {""},
#line 993 "zh_symbol_map"
      {"la2", 5997},
      {""}, {""}, {""}, {""}, {""},
#line 1005 "zh_symbol_map"
      {"lan2", 6009},
#line 1011 "zh_symbol_map"
      {"lang2", 6015},
      {""}, {""}, {""},
#line 2353 "zh_symbol_map"
      {"zhei4", 7357},
      {""}, {""}, {""}, {""},
#line 1008 "zh_symbol_map"
      {"lan5", 6012},
#line 1014 "zh_symbol_map"
      {"lang5", 6018},
      {""}, {""}, {""},
#line 2351 "zh_symbol_map"
      {"zhei2", 7355},
      {""}, {""}, {""}, {""},
#line 1006 "zh_symbol_map"
      {"lan3", 6010},
#line 1012 "zh_symbol_map"
      {"lang3", 6016},
      {""}, {""}, {""},
#line 2354 "zh_symbol_map"
      {"zhei5", 7358},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 242 "zh_symbol_map"
      {"chou1", 5246},
      {""}, {""},
#line 2352 "zh_symbol_map"
      {"zhei3", 7356},
      {""}, {""}, {""}, {""},
#line 2458 "zh_symbol_map"
      {"zuan1", 7462},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 348 "zh_symbol_map"
      {"da5", 5352},
      {""}, {""}, {""}, {""},
#line 377 "zh_symbol_map"
      {"de4", 5381},
      {""}, {""}, {""}, {""},
#line 376 "zh_symbol_map"
      {"de3", 5380},
#line 389 "zh_symbol_map"
      {"den4", 5393},
#line 395 "zh_symbol_map"
      {"deng4", 5399},
      {""}, {""},
#line 375 "zh_symbol_map"
      {"de2", 5379},
      {""}, {""}, {""},
#line 1043 "zh_symbol_map"
      {"li4", 6047},
      {""},
#line 387 "zh_symbol_map"
      {"den2", 5391},
#line 393 "zh_symbol_map"
      {"deng2", 5397},
      {""},
#line 1042 "zh_symbol_map"
      {"li3", 6046},
#line 1079 "zh_symbol_map"
      {"lin4", 6083},
#line 1085 "zh_symbol_map"
      {"ling4", 6089},
      {""}, {""},
#line 1041 "zh_symbol_map"
      {"li2", 6045},
      {""},
#line 390 "zh_symbol_map"
      {"den5", 5394},
#line 396 "zh_symbol_map"
      {"deng5", 5400},
      {""},
#line 23 "zh_symbol_map"
      {"an4", 5027},
#line 1077 "zh_symbol_map"
      {"lin2", 6081},
#line 1083 "zh_symbol_map"
      {"ling2", 6087},
      {""}, {""},
#line 22 "zh_symbol_map"
      {"an3", 5026},
      {""},
#line 388 "zh_symbol_map"
      {"den3", 5392},
#line 394 "zh_symbol_map"
      {"deng3", 5398},
      {""},
#line 21 "zh_symbol_map"
      {"an2", 5025},
#line 1080 "zh_symbol_map"
      {"lin5", 6084},
#line 1086 "zh_symbol_map"
      {"ling5", 6090},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1078 "zh_symbol_map"
      {"lin3", 6082},
#line 1084 "zh_symbol_map"
      {"ling3", 6088},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1049 "zh_symbol_map"
      {"lia4", 6053},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1047 "zh_symbol_map"
      {"lia2", 6051},
      {""}, {""}, {""}, {""},
#line 402 "zh_symbol_map"
      {"di5", 5406},
#line 1067 "zh_symbol_map"
      {"liao4", 6071},
      {""},
#line 1616 "zh_symbol_map"
      {"r5", 6620},
      {""},
#line 1050 "zh_symbol_map"
      {"lia5", 6054},
      {""}, {""}, {""}, {""}, {""},
#line 1065 "zh_symbol_map"
      {"liao2", 6069},
      {""}, {""}, {""},
#line 1048 "zh_symbol_map"
      {"lia3", 6052},
      {""}, {""}, {""}, {""}, {""},
#line 1068 "zh_symbol_map"
      {"liao5", 6072},
      {""}, {""},
#line 2266 "zh_symbol_map"
      {"zai1", 7270},
      {""},
#line 1055 "zh_symbol_map"
      {"lian4", 6059},
#line 1061 "zh_symbol_map"
      {"liang4", 6065},
      {""}, {""}, {""},
#line 1066 "zh_symbol_map"
      {"liao3", 6070},
#line 1059 "zh_symbol_map"
      {"liang2", 6063},
      {""}, {""}, {""},
#line 1053 "zh_symbol_map"
      {"lian2", 6057},
#line 1062 "zh_symbol_map"
      {"liang5", 6066},
      {""}, {""}, {""}, {""},
#line 1060 "zh_symbol_map"
      {"liang3", 6064},
      {""},
#line 871 "zh_symbol_map"
      {"jue6", 5875},
      {""},
#line 1056 "zh_symbol_map"
      {"lian5", 6060},
      {""}, {""}, {""}, {""},
#line 371 "zh_symbol_map"
      {"dao4", 5375},
      {""}, {""}, {""}, {""},
#line 1054 "zh_symbol_map"
      {"lian3", 6058},
      {""}, {""}, {""}, {""},
#line 369 "zh_symbol_map"
      {"dao2", 5373},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 372 "zh_symbol_map"
      {"dao5", 5376},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 370 "zh_symbol_map"
      {"dao3", 5374},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1255 "zh_symbol_map"
      {"mou6", 6259},
      {""}, {""},
#line 1620 "zh_symbol_map"
      {"ran4", 6624},
#line 1626 "zh_symbol_map"
      {"rang4", 6630},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1618 "zh_symbol_map"
      {"ran2", 6622},
#line 1624 "zh_symbol_map"
      {"rang2", 6628},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1621 "zh_symbol_map"
      {"ran5", 6625},
#line 1627 "zh_symbol_map"
      {"rang5", 6631},
      {""}, {""}, {""}, {""},
#line 215 "zh_symbol_map"
      {"che4", 5219},
      {""}, {""}, {""},
#line 1619 "zh_symbol_map"
      {"ran3", 6623},
#line 1625 "zh_symbol_map"
      {"rang3", 6629},
      {""}, {""}, {""}, {""},
#line 213 "zh_symbol_map"
      {"che2", 5217},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 216 "zh_symbol_map"
      {"che5", 5220},
      {""}, {""}, {""}, {""},
#line 332 "zh_symbol_map"
      {"cun1", 5336},
      {""}, {""}, {""}, {""},
#line 214 "zh_symbol_map"
      {"che3", 5218},
      {""}, {""},
#line 1804 "zh_symbol_map"
      {"sheng1", 6808},
      {""}, {""},
#line 1525 "zh_symbol_map"
      {"pou6", 6529},
      {""}, {""}, {""}, {""},
#line 221 "zh_symbol_map"
      {"chen4", 5225},
#line 227 "zh_symbol_map"
      {"cheng4", 5231},
      {""}, {""}, {""}, {""},
#line 225 "zh_symbol_map"
      {"cheng2", 5229},
      {""}, {""}, {""},
#line 219 "zh_symbol_map"
      {"chen2", 5223},
#line 228 "zh_symbol_map"
      {"cheng5", 5232},
#line 1604 "zh_symbol_map"
      {"que1", 6608},
      {""}, {""}, {""},
#line 226 "zh_symbol_map"
      {"cheng3", 5230},
      {""}, {""}, {""},
#line 222 "zh_symbol_map"
      {"chen5", 5226},
      {""}, {""}, {""},
#line 2367 "zh_symbol_map"
      {"zheng6", 7371},
      {""}, {""}, {""}, {""},
#line 331 "zh_symbol_map"
      {"cui6", 5335},
#line 220 "zh_symbol_map"
      {"chen3", 5224},
      {""},
#line 1656 "zh_symbol_map"
      {"ri4", 6660},
      {""}, {""}, {""}, {""},
#line 1655 "zh_symbol_map"
      {"ri3", 6659},
      {""}, {""}, {""}, {""},
#line 1654 "zh_symbol_map"
      {"ri2", 6658},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 996 "zh_symbol_map"
      {"la5", 6000},
      {""}, {""}, {""}, {""},
#line 1025 "zh_symbol_map"
      {"le4", 6029},
      {""}, {""}, {""}, {""},
#line 1024 "zh_symbol_map"
      {"le3", 6028},
      {""},
#line 1037 "zh_symbol_map"
      {"leng4", 6041},
      {""}, {""},
#line 1023 "zh_symbol_map"
      {"le2", 6027},
      {""}, {""}, {""}, {""},
#line 1139 "zh_symbol_map"
      {"lv4", 6143},
      {""},
#line 1035 "zh_symbol_map"
      {"leng2", 6039},
      {""}, {""},
#line 1138 "zh_symbol_map"
      {"lv3", 6142},
      {""}, {""}, {""}, {""},
#line 1137 "zh_symbol_map"
      {"lv2", 6141},
      {""},
#line 1038 "zh_symbol_map"
      {"leng5", 6042},
#line 320 "zh_symbol_map"
      {"cuan1", 5324},
#line 1827 "zh_symbol_map"
      {"shu6", 6831},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1036 "zh_symbol_map"
      {"leng3", 6040},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1869 "zh_symbol_map"
      {"shuo6", 6873},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1863 "zh_symbol_map"
      {"shun6", 6867},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 378 "zh_symbol_map"
      {"de5", 5382},
#line 361 "zh_symbol_map"
      {"dan6", 5365},
#line 367 "zh_symbol_map"
      {"dang6", 5371},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2476 "zh_symbol_map"
      {"zuo1", 7480},
      {""}, {""}, {""}, {""},
#line 1044 "zh_symbol_map"
      {"li5", 6048},
#line 2389 "zh_symbol_map"
      {"zhu4", 7393},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2387 "zh_symbol_map"
      {"zhu2", 7391},
      {""}, {""}, {""},
#line 24 "zh_symbol_map"
      {"an5", 5028},
      {""},
#line 2431 "zh_symbol_map"
      {"zhuo4", 7435},
      {""}, {""}, {""},
#line 2390 "zh_symbol_map"
      {"zhu5", 7394},
      {""},
#line 2089 "zh_symbol_map"
      {"xi4", 7093},
      {""},
#line 1857 "zh_symbol_map"
      {"shui6", 6861},
      {""},
#line 2429 "zh_symbol_map"
      {"zhuo2", 7433},
#line 2088 "zh_symbol_map"
      {"xi3", 7092},
#line 2125 "zh_symbol_map"
      {"xin4", 7129},
#line 2131 "zh_symbol_map"
      {"xing4", 7135},
#line 2388 "zh_symbol_map"
      {"zhu3", 7392},
      {""},
#line 2087 "zh_symbol_map"
      {"xi2", 7091},
      {""},
#line 824 "zh_symbol_map"
      {"jie1", 5828},
      {""},
#line 2432 "zh_symbol_map"
      {"zhuo5", 7436},
      {""},
#line 2123 "zh_symbol_map"
      {"xin2", 7127},
#line 2129 "zh_symbol_map"
      {"xing2", 7133},
      {""},
#line 2425 "zh_symbol_map"
      {"zhun4", 7429},
      {""}, {""}, {""}, {""},
#line 2430 "zh_symbol_map"
      {"zhuo3", 7434},
      {""},
#line 2126 "zh_symbol_map"
      {"xin5", 7130},
#line 2132 "zh_symbol_map"
      {"xing5", 7136},
#line 1019 "zh_symbol_map"
      {"lao4", 6023},
#line 2423 "zh_symbol_map"
      {"zhun2", 7427},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2124 "zh_symbol_map"
      {"xin3", 7128},
#line 2130 "zh_symbol_map"
      {"xing3", 7134},
#line 1017 "zh_symbol_map"
      {"lao2", 6021},
#line 2426 "zh_symbol_map"
      {"zhun5", 7430},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1020 "zh_symbol_map"
      {"lao5", 6024},
#line 2424 "zh_symbol_map"
      {"zhun3", 7428},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1018 "zh_symbol_map"
      {"lao3", 6022},
      {""}, {""}, {""}, {""}, {""},
#line 140 "zh_symbol_map"
      {"cai1", 5144},
      {""}, {""}, {""}, {""}, {""},
#line 433 "zh_symbol_map"
      {"ding6", 5437},
#line 2095 "zh_symbol_map"
      {"xia4", 7099},
      {""}, {""},
#line 2419 "zh_symbol_map"
      {"zhui4", 7423},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2093 "zh_symbol_map"
      {"xia2", 7097},
      {""}, {""},
#line 2417 "zh_symbol_map"
      {"zhui2", 7421},
      {""}, {""},
#line 2113 "zh_symbol_map"
      {"xiao4", 7117},
      {""}, {""},
#line 1063 "zh_symbol_map"
      {"liang6", 6067},
#line 2096 "zh_symbol_map"
      {"xia5", 7100},
#line 2296 "zh_symbol_map"
      {"zei1", 7300},
      {""},
#line 2420 "zh_symbol_map"
      {"zhui5", 7424},
      {""}, {""},
#line 2111 "zh_symbol_map"
      {"xiao2", 7115},
      {""}, {""}, {""},
#line 2094 "zh_symbol_map"
      {"xia3", 7098},
      {""}, {""},
#line 2418 "zh_symbol_map"
      {"zhui3", 7422},
      {""}, {""},
#line 2114 "zh_symbol_map"
      {"xiao5", 7118},
      {""}, {""}, {""}, {""},
#line 2101 "zh_symbol_map"
      {"xian4", 7105},
#line 2107 "zh_symbol_map"
      {"xiang4", 7111},
      {""}, {""}, {""},
#line 2112 "zh_symbol_map"
      {"xiao3", 7116},
#line 2105 "zh_symbol_map"
      {"xiang2", 7109},
      {""}, {""}, {""},
#line 2099 "zh_symbol_map"
      {"xian2", 7103},
#line 2108 "zh_symbol_map"
      {"xiang5", 7112},
      {""}, {""}, {""}, {""},
#line 2106 "zh_symbol_map"
      {"xiang3", 7110},
      {""}, {""}, {""},
#line 2102 "zh_symbol_map"
      {"xian5", 7106},
      {""},
#line 409 "zh_symbol_map"
      {"dia6", 5413},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2100 "zh_symbol_map"
      {"xian3", 7104},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 421 "zh_symbol_map"
      {"diao6", 5425},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2368 "zh_symbol_map"
      {"zhi1", 7372},
      {""}, {""},
#line 2440 "zh_symbol_map"
      {"zong1", 7444},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1638 "zh_symbol_map"
      {"re4", 6642},
      {""}, {""}, {""}, {""},
#line 1637 "zh_symbol_map"
      {"re3", 6641},
#line 1644 "zh_symbol_map"
      {"ren4", 6648},
#line 1650 "zh_symbol_map"
      {"reng4", 6654},
#line 415 "zh_symbol_map"
      {"dian6", 5419},
      {""},
#line 1636 "zh_symbol_map"
      {"re2", 6640},
      {""}, {""}, {""}, {""}, {""},
#line 1642 "zh_symbol_map"
      {"ren2", 6646},
#line 1648 "zh_symbol_map"
      {"reng2", 6652},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1645 "zh_symbol_map"
      {"ren5", 6649},
#line 1651 "zh_symbol_map"
      {"reng5", 6655},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1643 "zh_symbol_map"
      {"ren3", 6647},
#line 1649 "zh_symbol_map"
      {"reng3", 6653},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2349 "zh_symbol_map"
      {"zhe6", 7353},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 229 "zh_symbol_map"
      {"cheng6", 5233},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2361 "zh_symbol_map"
      {"zhen6", 7365},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1657 "zh_symbol_map"
      {"ri5", 6661},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1900 "zh_symbol_map"
      {"sui1", 6904},
      {""},
#line 1026 "zh_symbol_map"
      {"le5", 6030},
#line 1009 "zh_symbol_map"
      {"lan6", 6013},
#line 1015 "zh_symbol_map"
      {"lang6", 6019},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1140 "zh_symbol_map"
      {"lv5", 6144},
#line 2355 "zh_symbol_map"
      {"zhei6", 7359},
      {""}, {""}, {""}, {""},
#line 1632 "zh_symbol_map"
      {"rao4", 6636},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1630 "zh_symbol_map"
      {"rao2", 6634},
      {""}, {""}, {""},
#line 530 "zh_symbol_map"
      {"fei1", 5534},
      {""}, {""}, {""}, {""}, {""},
#line 1633 "zh_symbol_map"
      {"rao5", 6637},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1631 "zh_symbol_map"
      {"rao3", 6635},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 338 "zh_symbol_map"
      {"cuo1", 5342},
      {""}, {""}, {""}, {""}, {""},
#line 251 "zh_symbol_map"
      {"chu4", 5255},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 249 "zh_symbol_map"
      {"chu2", 5253},
      {""}, {""}, {""},
#line 391 "zh_symbol_map"
      {"den6", 5395},
#line 397 "zh_symbol_map"
      {"deng6", 5401},
#line 293 "zh_symbol_map"
      {"chuo4", 5297},
      {""}, {""}, {""},
#line 252 "zh_symbol_map"
      {"chu5", 5256},
      {""}, {""}, {""}, {""}, {""},
#line 291 "zh_symbol_map"
      {"chuo2", 5295},
      {""},
#line 1081 "zh_symbol_map"
      {"lin6", 6085},
#line 1087 "zh_symbol_map"
      {"ling6", 6091},
#line 250 "zh_symbol_map"
      {"chu3", 5254},
      {""}, {""}, {""}, {""}, {""},
#line 294 "zh_symbol_map"
      {"chuo5", 5298},
      {""},
#line 344 "zh_symbol_map"
      {"da1", 5348},
      {""}, {""},
#line 287 "zh_symbol_map"
      {"chun4", 5291},
      {""}, {""}, {""},
#line 2090 "zh_symbol_map"
      {"xi5", 7094},
#line 292 "zh_symbol_map"
      {"chuo3", 5296},
      {""}, {""}, {""}, {""},
#line 285 "zh_symbol_map"
      {"chun2", 5289},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 288 "zh_symbol_map"
      {"chun5", 5292},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 286 "zh_symbol_map"
      {"chun3", 5290},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1051 "zh_symbol_map"
      {"lia6", 6055},
      {""}, {""},
#line 281 "zh_symbol_map"
      {"chui4", 5285},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 279 "zh_symbol_map"
      {"chui2", 5283},
      {""}, {""},
#line 1069 "zh_symbol_map"
      {"liao6", 6073},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 282 "zh_symbol_map"
      {"chui5", 5286},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 280 "zh_symbol_map"
      {"chui3", 5284},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1057 "zh_symbol_map"
      {"lian6", 6061},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 398 "zh_symbol_map"
      {"di1", 5402},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2109 "zh_symbol_map"
      {"xiang6", 7113},
#line 373 "zh_symbol_map"
      {"dao6", 5377},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 230 "zh_symbol_map"
      {"chi1", 5234},
      {""}, {""},
#line 302 "zh_symbol_map"
      {"cong1", 5306},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1639 "zh_symbol_map"
      {"re5", 6643},
#line 1622 "zh_symbol_map"
      {"ran6", 6626},
#line 1628 "zh_symbol_map"
      {"rang6", 6632},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 848 "zh_symbol_map"
      {"jiu1", 5852},
#line 217 "zh_symbol_map"
      {"che6", 5221},
      {""}, {""}, {""},
#line 2137 "zh_symbol_map"
      {"xiong4", 7141},
      {""}, {""}, {""}, {""},
#line 2135 "zh_symbol_map"
      {"xiong2", 7139},
      {""}, {""}, {""}, {""},
#line 2138 "zh_symbol_map"
      {"xiong5", 7142},
      {""}, {""}, {""}, {""},
#line 2136 "zh_symbol_map"
      {"xiong3", 7140},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 223 "zh_symbol_map"
      {"chen6", 5227},
      {""}, {""},
#line 455 "zh_symbol_map"
      {"du4", 5459},
      {""}, {""}, {""}, {""},
#line 454 "zh_symbol_map"
      {"du3", 5458},
#line 473 "zh_symbol_map"
      {"dun4", 5477},
      {""}, {""}, {""},
#line 453 "zh_symbol_map"
      {"du2", 5457},
      {""}, {""}, {""}, {""}, {""},
#line 471 "zh_symbol_map"
      {"dun2", 5475},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 474 "zh_symbol_map"
      {"dun5", 5478},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 472 "zh_symbol_map"
      {"dun3", 5476},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2362 "zh_symbol_map"
      {"zheng1", 7366},
      {""}, {""}, {""}, {""},
#line 1039 "zh_symbol_map"
      {"leng6", 6043},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 992 "zh_symbol_map"
      {"la1", 5996},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 461 "zh_symbol_map"
      {"duan4", 5465},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 459 "zh_symbol_map"
      {"duan2", 5463},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 462 "zh_symbol_map"
      {"duan5", 5466},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 460 "zh_symbol_map"
      {"duan3", 5464},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2391 "zh_symbol_map"
      {"zhu6", 7395},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2433 "zh_symbol_map"
      {"zhuo6", 7437},
      {""}, {""}, {""},
#line 374 "zh_symbol_map"
      {"de1", 5378},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2127 "zh_symbol_map"
      {"xin6", 7131},
#line 2133 "zh_symbol_map"
      {"xing6", 7137},
      {""}, {""}, {""}, {""},
#line 1040 "zh_symbol_map"
      {"li1", 6044},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2427 "zh_symbol_map"
      {"zhun6", 7431},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 20 "zh_symbol_map"
      {"an1", 5024},
#line 1021 "zh_symbol_map"
      {"lao6", 6025},
      {""}, {""}, {""}, {""},
#line 1885 "zh_symbol_map"
      {"sou4", 6889},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1883 "zh_symbol_map"
      {"sou2", 6887},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1886 "zh_symbol_map"
      {"sou5", 6890},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1884 "zh_symbol_map"
      {"sou3", 6888},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2097 "zh_symbol_map"
      {"xia6", 7101},
      {""}, {""},
#line 2421 "zh_symbol_map"
      {"zhui6", 7425},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2115 "zh_symbol_map"
      {"xiao6", 7119},
      {""},
#line 353 "zh_symbol_map"
      {"dai4", 5357},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 351 "zh_symbol_map"
      {"dai2", 5355},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 354 "zh_symbol_map"
      {"dai5", 5358},
      {""}, {""},
#line 2103 "zh_symbol_map"
      {"xian6", 7107},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 352 "zh_symbol_map"
      {"dai3", 5356},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1058 "zh_symbol_map"
      {"liang1", 6062},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 626 "zh_symbol_map"
      {"gou1", 5630},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1115 "zh_symbol_map"
      {"lu4", 6119},
      {""}, {""}, {""}, {""},
#line 1114 "zh_symbol_map"
      {"lu3", 6118},
#line 1127 "zh_symbol_map"
      {"lun4", 6131},
      {""}, {""}, {""},
#line 1113 "zh_symbol_map"
      {"lu2", 6117},
      {""}, {""}, {""}, {""}, {""},
#line 1125 "zh_symbol_map"
      {"lun2", 6129},
      {""}, {""}, {""}, {""},
#line 1646 "zh_symbol_map"
      {"ren6", 6650},
#line 1652 "zh_symbol_map"
      {"reng6", 6656},
      {""}, {""}, {""},
#line 1128 "zh_symbol_map"
      {"lun5", 6132},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1382 "zh_symbol_map"
      {"nou1", 6386},
      {""}, {""},
#line 1126 "zh_symbol_map"
      {"lun3", 6130},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2139 "zh_symbol_map"
      {"xiong6", 7143},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1785 "zh_symbol_map"
      {"she1", 6789},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 456 "zh_symbol_map"
      {"du5", 5460},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 683 "zh_symbol_map"
      {"ha4", 5687},
      {""}, {""}, {""}, {""},
#line 682 "zh_symbol_map"
      {"ha3", 5686},
#line 695 "zh_symbol_map"
      {"han4", 5699},
#line 701 "zh_symbol_map"
      {"hang4", 5705},
      {""}, {""},
#line 681 "zh_symbol_map"
      {"ha2", 5685},
#line 1121 "zh_symbol_map"
      {"luan4", 6125},
      {""},
#line 1797 "zh_symbol_map"
      {"shen1", 6801},
      {""}, {""},
#line 693 "zh_symbol_map"
      {"han2", 5697},
#line 699 "zh_symbol_map"
      {"hang2", 5703},
      {""}, {""}, {""},
#line 1119 "zh_symbol_map"
      {"luan2", 6123},
      {""},
#line 224 "zh_symbol_map"
      {"cheng1", 5228},
      {""}, {""},
#line 696 "zh_symbol_map"
      {"han5", 5700},
#line 702 "zh_symbol_map"
      {"hang5", 5706},
      {""}, {""}, {""},
#line 1122 "zh_symbol_map"
      {"luan5", 6126},
      {""}, {""}, {""}, {""},
#line 694 "zh_symbol_map"
      {"han3", 5698},
#line 700 "zh_symbol_map"
      {"hang3", 5704},
      {""}, {""}, {""},
#line 1120 "zh_symbol_map"
      {"luan3", 6124},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1653 "zh_symbol_map"
      {"ri1", 6657},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1634 "zh_symbol_map"
      {"rao6", 6638},
      {""}, {""},
#line 1791 "zh_symbol_map"
      {"shei1", 6795},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1022 "zh_symbol_map"
      {"le1", 6026},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1136 "zh_symbol_map"
      {"lv1", 6140},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2464 "zh_symbol_map"
      {"zui1", 7468},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 253 "zh_symbol_map"
      {"chu6", 5257},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 295 "zh_symbol_map"
      {"chuo6", 5299},
      {""}, {""},
#line 479 "zh_symbol_map"
      {"duo4", 5483},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 477 "zh_symbol_map"
      {"duo2", 5481},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 480 "zh_symbol_map"
      {"duo5", 5484},
      {""},
#line 289 "zh_symbol_map"
      {"chun6", 5293},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1001 "zh_symbol_map"
      {"lai4", 6005},
#line 478 "zh_symbol_map"
      {"duo3", 5482},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 999 "zh_symbol_map"
      {"lai2", 6003},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1674 "zh_symbol_map"
      {"ru4", 6678},
#line 1002 "zh_symbol_map"
      {"lai5", 6006},
      {""}, {""}, {""},
#line 1673 "zh_symbol_map"
      {"ru3", 6677},
#line 1698 "zh_symbol_map"
      {"run4", 6702},
      {""}, {""}, {""},
#line 1672 "zh_symbol_map"
      {"ru2", 6676},
#line 1000 "zh_symbol_map"
      {"lai3", 6004},
      {""},
#line 2086 "zh_symbol_map"
      {"xi1", 7090},
      {""}, {""},
#line 1696 "zh_symbol_map"
      {"run2", 6700},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1699 "zh_symbol_map"
      {"run5", 6703},
      {""}, {""},
#line 283 "zh_symbol_map"
      {"chui6", 5287},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1697 "zh_symbol_map"
      {"run3", 6701},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1680 "zh_symbol_map"
      {"rua4", 6684},
#line 383 "zh_symbol_map"
      {"dei4", 5387},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1678 "zh_symbol_map"
      {"rua2", 6682},
#line 381 "zh_symbol_map"
      {"dei2", 5385},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1681 "zh_symbol_map"
      {"rua5", 6685},
#line 384 "zh_symbol_map"
      {"dei5", 5388},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1679 "zh_symbol_map"
      {"rua3", 6683},
#line 382 "zh_symbol_map"
      {"dei3", 5386},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1686 "zh_symbol_map"
      {"ruan4", 6690},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1684 "zh_symbol_map"
      {"ruan2", 6688},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1116 "zh_symbol_map"
      {"lu5", 6120},
#line 2104 "zh_symbol_map"
      {"xiang1", 7108},
#line 1687 "zh_symbol_map"
      {"ruan5", 6691},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 443 "zh_symbol_map"
      {"dong4", 5447},
      {""},
#line 1685 "zh_symbol_map"
      {"ruan3", 6689},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 441 "zh_symbol_map"
      {"dong2", 5445},
      {""}, {""},
#line 2149 "zh_symbol_map"
      {"xu4", 7153},
      {""}, {""}, {""}, {""},
#line 2148 "zh_symbol_map"
      {"xu3", 7152},
#line 2167 "zh_symbol_map"
      {"xun4", 7171},
#line 444 "zh_symbol_map"
      {"dong5", 5448},
      {""}, {""},
#line 2147 "zh_symbol_map"
      {"xu2", 7151},
      {""},
#line 866 "zh_symbol_map"
      {"jue1", 5870},
      {""}, {""}, {""},
#line 2165 "zh_symbol_map"
      {"xun2", 7169},
#line 442 "zh_symbol_map"
      {"dong3", 5446},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2168 "zh_symbol_map"
      {"xun5", 7172},
#line 1635 "zh_symbol_map"
      {"re1", 6639},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2166 "zh_symbol_map"
      {"xun3", 7170},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 475 "zh_symbol_map"
      {"dun6", 5479},
#line 1250 "zh_symbol_map"
      {"mou1", 6254},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 684 "zh_symbol_map"
      {"ha5", 5688},
      {""}, {""}, {""}, {""},
#line 713 "zh_symbol_map"
      {"he4", 5717},
      {""}, {""}, {""}, {""},
#line 712 "zh_symbol_map"
      {"he3", 5716},
#line 725 "zh_symbol_map"
      {"hen4", 5729},
#line 731 "zh_symbol_map"
      {"heng4", 5735},
      {""}, {""},
#line 711 "zh_symbol_map"
      {"he2", 5715},
      {""}, {""}, {""}, {""}, {""},
#line 723 "zh_symbol_map"
      {"hen2", 5727},
#line 729 "zh_symbol_map"
      {"heng2", 5733},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 726 "zh_symbol_map"
      {"hen5", 5730},
#line 732 "zh_symbol_map"
      {"heng5", 5736},
      {""},
#line 2155 "zh_symbol_map"
      {"xuan4", 7159},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 724 "zh_symbol_map"
      {"hen3", 5728},
#line 730 "zh_symbol_map"
      {"heng3", 5734},
      {""},
#line 2153 "zh_symbol_map"
      {"xuan2", 7157},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2156 "zh_symbol_map"
      {"xuan5", 7160},
#line 1133 "zh_symbol_map"
      {"luo4", 6137},
      {""},
#line 1520 "zh_symbol_map"
      {"pou1", 6524},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2154 "zh_symbol_map"
      {"xuan3", 7158},
#line 1131 "zh_symbol_map"
      {"luo2", 6135},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1134 "zh_symbol_map"
      {"luo5", 6138},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1132 "zh_symbol_map"
      {"luo3", 6136},
      {""}, {""}, {""}, {""}, {""},
#line 326 "zh_symbol_map"
      {"cui1", 5330},
      {""}, {""}, {""}, {""}, {""},
#line 463 "zh_symbol_map"
      {"duan6", 5467},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1822 "zh_symbol_map"
      {"shu1", 6826},
      {""}, {""},
#line 707 "zh_symbol_map"
      {"hao4", 5711},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 705 "zh_symbol_map"
      {"hao2", 5709},
      {""}, {""},
#line 1864 "zh_symbol_map"
      {"shuo1", 6868},
      {""}, {""}, {""}, {""}, {""},
#line 1031 "zh_symbol_map"
      {"lei4", 6035},
#line 708 "zh_symbol_map"
      {"hao5", 5712},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1029 "zh_symbol_map"
      {"lei2", 6033},
#line 706 "zh_symbol_map"
      {"hao3", 5710},
      {""}, {""},
#line 1675 "zh_symbol_map"
      {"ru5", 6679},
      {""}, {""}, {""}, {""},
#line 1858 "zh_symbol_map"
      {"shun1", 6862},
#line 1032 "zh_symbol_map"
      {"lei5", 6036},
      {""}, {""}, {""}, {""},
#line 1887 "zh_symbol_map"
      {"sou6", 6891},
      {""}, {""}, {""}, {""},
#line 1030 "zh_symbol_map"
      {"lei3", 6034},
      {""}, {""}, {""}, {""}, {""},
#line 356 "zh_symbol_map"
      {"dan1", 5360},
#line 362 "zh_symbol_map"
      {"dang1", 5366},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1097 "zh_symbol_map"
      {"lo4", 6101},
      {""}, {""}, {""}, {""},
#line 1096 "zh_symbol_map"
      {"lo3", 6100},
      {""},
#line 1103 "zh_symbol_map"
      {"long4", 6107},
      {""}, {""},
#line 1095 "zh_symbol_map"
      {"lo2", 6099},
      {""}, {""},
#line 1852 "zh_symbol_map"
      {"shui1", 6856},
      {""}, {""}, {""},
#line 1101 "zh_symbol_map"
      {"long2", 6105},
      {""}, {""},
#line 355 "zh_symbol_map"
      {"dai6", 5359},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1104 "zh_symbol_map"
      {"long5", 6108},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2449 "zh_symbol_map"
      {"zou4", 7453},
#line 1102 "zh_symbol_map"
      {"long3", 6106},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2447 "zh_symbol_map"
      {"zou2", 7451},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2450 "zh_symbol_map"
      {"zou5", 7454},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2448 "zh_symbol_map"
      {"zou3", 7452},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 428 "zh_symbol_map"
      {"ding1", 5432},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1129 "zh_symbol_map"
      {"lun6", 6133},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2150 "zh_symbol_map"
      {"xu5", 7154},
      {""}, {""},
#line 1704 "zh_symbol_map"
      {"ruo4", 6708},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1702 "zh_symbol_map"
      {"ruo2", 6706},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1705 "zh_symbol_map"
      {"ruo5", 6709},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1703 "zh_symbol_map"
      {"ruo3", 6707},
      {""}, {""}, {""}, {""}, {""},
#line 404 "zh_symbol_map"
      {"dia1", 5408},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 416 "zh_symbol_map"
      {"diao1", 5420},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2134 "zh_symbol_map"
      {"xiong1", 7138},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 410 "zh_symbol_map"
      {"dian1", 5414},
      {""}, {""},
#line 714 "zh_symbol_map"
      {"he5", 5718},
#line 697 "zh_symbol_map"
      {"han6", 5701},
#line 703 "zh_symbol_map"
      {"hang6", 5707},
      {""}, {""}, {""},
#line 1123 "zh_symbol_map"
      {"luan6", 6127},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 452 "zh_symbol_map"
      {"du1", 5456},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2344 "zh_symbol_map"
      {"zhe1", 7348},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2356 "zh_symbol_map"
      {"zhen1", 7360},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 557 "zh_symbol_map"
      {"fou4", 5561},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 555 "zh_symbol_map"
      {"fou2", 5559},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 558 "zh_symbol_map"
      {"fou5", 5562},
#line 1662 "zh_symbol_map"
      {"rong4", 6666},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 556 "zh_symbol_map"
      {"fou3", 5560},
#line 1660 "zh_symbol_map"
      {"rong2", 6664},
      {""},
#line 1004 "zh_symbol_map"
      {"lan1", 6008},
#line 1010 "zh_symbol_map"
      {"lang1", 6014},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1663 "zh_symbol_map"
      {"rong5", 6667},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2350 "zh_symbol_map"
      {"zhei1", 7354},
#line 481 "zh_symbol_map"
      {"duo6", 5485},
      {""},
#line 1661 "zh_symbol_map"
      {"rong3", 6665},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1003 "zh_symbol_map"
      {"lai6", 6007},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1700 "zh_symbol_map"
      {"run6", 6704},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 386 "zh_symbol_map"
      {"den1", 5390},
#line 392 "zh_symbol_map"
      {"deng1", 5396},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1076 "zh_symbol_map"
      {"lin1", 6080},
#line 1082 "zh_symbol_map"
      {"ling1", 6086},
      {""}, {""}, {""}, {""},
#line 1098 "zh_symbol_map"
      {"lo5", 6102},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 311 "zh_symbol_map"
      {"cou4", 5315},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 309 "zh_symbol_map"
      {"cou2", 5313},
#line 1682 "zh_symbol_map"
      {"rua6", 6686},
#line 385 "zh_symbol_map"
      {"dei6", 5389},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 312 "zh_symbol_map"
      {"cou5", 5316},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 310 "zh_symbol_map"
      {"cou3", 5314},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1046 "zh_symbol_map"
      {"lia1", 6050},
      {""}, {""}, {""}, {""}, {""},
#line 1688 "zh_symbol_map"
      {"ruan6", 6692},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1064 "zh_symbol_map"
      {"liao1", 6068},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 445 "zh_symbol_map"
      {"dong6", 5449},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1052 "zh_symbol_map"
      {"lian1", 6056},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2169 "zh_symbol_map"
      {"xun6", 7173},
      {""}, {""}, {""}, {""}, {""},
#line 1112 "zh_symbol_map"
      {"lu1", 6116},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 368 "zh_symbol_map"
      {"dao1", 5372},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1617 "zh_symbol_map"
      {"ran1", 6621},
#line 1623 "zh_symbol_map"
      {"rang1", 6627},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 727 "zh_symbol_map"
      {"hen6", 5731},
#line 733 "zh_symbol_map"
      {"heng6", 5737},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 212 "zh_symbol_map"
      {"che1", 5216},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2157 "zh_symbol_map"
      {"xuan6", 7161},
#line 680 "zh_symbol_map"
      {"ha1", 5684},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1135 "zh_symbol_map"
      {"luo6", 6139},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 218 "zh_symbol_map"
      {"chen1", 5222},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 467 "zh_symbol_map"
      {"dui4", 5471},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 465 "zh_symbol_map"
      {"dui2", 5469},
      {""}, {""}, {""},
#line 1321 "zh_symbol_map"
      {"ng6", 6325},
#line 1034 "zh_symbol_map"
      {"leng1", 6038},
      {""}, {""}, {""}, {""},
#line 468 "zh_symbol_map"
      {"dui5", 5472},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 466 "zh_symbol_map"
      {"dui3", 5470},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 881 "zh_symbol_map"
      {"ka4", 5885},
#line 709 "zh_symbol_map"
      {"hao6", 5713},
      {""}, {""}, {""},
#line 880 "zh_symbol_map"
      {"ka3", 5884},
#line 893 "zh_symbol_map"
      {"kan4", 5897},
#line 899 "zh_symbol_map"
      {"kang4", 5903},
      {""}, {""},
#line 879 "zh_symbol_map"
      {"ka2", 5883},
      {""}, {""}, {""}, {""},
#line 571 "zh_symbol_map"
      {"ga6", 5575},
#line 891 "zh_symbol_map"
      {"kan2", 5895},
#line 897 "zh_symbol_map"
      {"kang2", 5901},
      {""}, {""},
#line 1033 "zh_symbol_map"
      {"lei6", 6037},
      {""}, {""}, {""}, {""}, {""},
#line 894 "zh_symbol_map"
      {"kan5", 5898},
#line 900 "zh_symbol_map"
      {"kang5", 5904},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 892 "zh_symbol_map"
      {"kan3", 5896},
#line 898 "zh_symbol_map"
      {"kang3", 5902},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1267 "zh_symbol_map"
      {"na6", 6271},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1671 "zh_symbol_map"
      {"ru1", 6675},
#line 2386 "zh_symbol_map"
      {"zhu1", 7390},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2428 "zh_symbol_map"
      {"zhuo1", 7432},
      {""},
#line 1105 "zh_symbol_map"
      {"long6", 6109},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 43 "zh_symbol_map"
      {"ba6", 5047},
      {""}, {""},
#line 2122 "zh_symbol_map"
      {"xin1", 7126},
#line 2128 "zh_symbol_map"
      {"xing1", 7132},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2422 "zh_symbol_map"
      {"zhun1", 7426},
      {""}, {""}, {""}, {""}, {""},
#line 2451 "zh_symbol_map"
      {"zou6", 7455},
      {""}, {""},
#line 1016 "zh_symbol_map"
      {"lao1", 6020},
      {""}, {""}, {""}, {""},
#line 749 "zh_symbol_map"
      {"hu4", 5753},
      {""}, {""}, {""},
#line 1831 "zh_symbol_map"
      {"shua4", 6835},
#line 748 "zh_symbol_map"
      {"hu3", 5752},
#line 785 "zh_symbol_map"
      {"hun4", 5789},
      {""}, {""}, {""},
#line 747 "zh_symbol_map"
      {"hu2", 5751},
      {""}, {""}, {""},
#line 1829 "zh_symbol_map"
      {"shua2", 6833},
      {""},
#line 783 "zh_symbol_map"
      {"hun2", 5787},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1832 "zh_symbol_map"
      {"shua5", 6836},
      {""},
#line 786 "zh_symbol_map"
      {"hun5", 5790},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1830 "zh_symbol_map"
      {"shua3", 6834},
#line 1327 "zh_symbol_map"
      {"ni6", 6331},
#line 784 "zh_symbol_map"
      {"hun3", 5788},
      {""},
#line 2092 "zh_symbol_map"
      {"xia1", 7096},
      {""}, {""},
#line 2416 "zh_symbol_map"
      {"zhui1", 7420},
      {""}, {""}, {""},
#line 1843 "zh_symbol_map"
      {"shuan4", 6847},
#line 1849 "zh_symbol_map"
      {"shuang4", 6853},
      {""}, {""}, {""},
#line 1841 "zh_symbol_map"
      {"shuan2", 6845},
#line 1847 "zh_symbol_map"
      {"shuang2", 6851},
      {""}, {""},
#line 2110 "zh_symbol_map"
      {"xiao1", 7114},
#line 1844 "zh_symbol_map"
      {"shuan5", 6848},
#line 1850 "zh_symbol_map"
      {"shuang5", 6854},
      {""}, {""}, {""},
#line 1842 "zh_symbol_map"
      {"shuan3", 6846},
#line 1848 "zh_symbol_map"
      {"shuang3", 6852},
      {""}, {""}, {""},
#line 1706 "zh_symbol_map"
      {"ruo6", 6710},
      {""}, {""}, {""}, {""}, {""},
#line 755 "zh_symbol_map"
      {"hua4", 5759},
      {""}, {""}, {""},
#line 91 "zh_symbol_map"
      {"bi6", 5095},
      {""}, {""}, {""},
#line 2098 "zh_symbol_map"
      {"xian1", 7102},
      {""},
#line 753 "zh_symbol_map"
      {"hua2", 5757},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 756 "zh_symbol_map"
      {"hua5", 5760},
      {""}, {""}, {""}, {""}, {""},
#line 2146 "zh_symbol_map"
      {"xu1", 7150},
      {""}, {""},
#line 1837 "zh_symbol_map"
      {"shuai4", 6841},
#line 754 "zh_symbol_map"
      {"hua3", 5758},
      {""}, {""}, {""},
#line 1835 "zh_symbol_map"
      {"shuai2", 6839},
      {""}, {""}, {""}, {""},
#line 1838 "zh_symbol_map"
      {"shuai5", 6842},
      {""},
#line 767 "zh_symbol_map"
      {"huan4", 5771},
#line 773 "zh_symbol_map"
      {"huang4", 5777},
      {""},
#line 1836 "zh_symbol_map"
      {"shuai3", 6840},
      {""}, {""},
#line 771 "zh_symbol_map"
      {"huang2", 5775},
      {""}, {""}, {""},
#line 765 "zh_symbol_map"
      {"huan2", 5769},
#line 774 "zh_symbol_map"
      {"huang5", 5778},
      {""}, {""}, {""}, {""},
#line 772 "zh_symbol_map"
      {"huang3", 5776},
      {""}, {""}, {""},
#line 768 "zh_symbol_map"
      {"huan5", 5772},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1641 "zh_symbol_map"
      {"ren1", 6645},
#line 1647 "zh_symbol_map"
      {"reng1", 6651},
#line 766 "zh_symbol_map"
      {"huan3", 5770},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 761 "zh_symbol_map"
      {"huai4", 5765},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 759 "zh_symbol_map"
      {"huai2", 5763},
      {""}, {""},
#line 710 "zh_symbol_map"
      {"he1", 5714},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 762 "zh_symbol_map"
      {"huai5", 5766},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1537 "zh_symbol_map"
      {"qi6", 6541},
      {""},
#line 760 "zh_symbol_map"
      {"huai3", 5764},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 425 "zh_symbol_map"
      {"die4", 5429},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 423 "zh_symbol_map"
      {"die2", 5427},
#line 559 "zh_symbol_map"
      {"fou6", 5563},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 426 "zh_symbol_map"
      {"die5", 5430},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 424 "zh_symbol_map"
      {"die3", 5428},
      {""},
#line 1664 "zh_symbol_map"
      {"rong6", 6668},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 689 "zh_symbol_map"
      {"hai4", 5693},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 687 "zh_symbol_map"
      {"hai2", 5691},
      {""}, {""}, {""},
#line 1629 "zh_symbol_map"
      {"rao1", 6633},
      {""}, {""}, {""}, {""}, {""},
#line 690 "zh_symbol_map"
      {"hai5", 5694},
      {""}, {""}, {""},
#line 882 "zh_symbol_map"
      {"ka5", 5886},
      {""}, {""}, {""}, {""},
#line 911 "zh_symbol_map"
      {"ke4", 5915},
#line 688 "zh_symbol_map"
      {"hai3", 5692},
      {""}, {""}, {""},
#line 910 "zh_symbol_map"
      {"ke3", 5914},
#line 923 "zh_symbol_map"
      {"ken4", 5927},
#line 929 "zh_symbol_map"
      {"keng4", 5933},
      {""}, {""},
#line 909 "zh_symbol_map"
      {"ke2", 5913},
      {""}, {""}, {""}, {""},
#line 601 "zh_symbol_map"
      {"ge6", 5605},
#line 921 "zh_symbol_map"
      {"ken2", 5925},
#line 927 "zh_symbol_map"
      {"keng2", 5931},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 924 "zh_symbol_map"
      {"ken5", 5928},
#line 930 "zh_symbol_map"
      {"keng5", 5934},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 922 "zh_symbol_map"
      {"ken3", 5926},
#line 928 "zh_symbol_map"
      {"keng3", 5932},
      {""}, {""}, {""}, {""},
#line 248 "zh_symbol_map"
      {"chu1", 5252},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1297 "zh_symbol_map"
      {"ne6", 6301},
      {""}, {""},
#line 290 "zh_symbol_map"
      {"chuo1", 5294},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1411 "zh_symbol_map"
      {"nv6", 6415},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 284 "zh_symbol_map"
      {"chun1", 5288},
      {""}, {""}, {""}, {""}, {""},
#line 313 "zh_symbol_map"
      {"cou6", 5317},
#line 2173 "zh_symbol_map"
      {"ya4", 7177},
      {""}, {""}, {""}, {""},
#line 2172 "zh_symbol_map"
      {"ya3", 7176},
#line 2179 "zh_symbol_map"
      {"yan4", 7183},
#line 2185 "zh_symbol_map"
      {"yang4", 7189},
      {""}, {""},
#line 2171 "zh_symbol_map"
      {"ya2", 7175},
      {""}, {""}, {""}, {""}, {""},
#line 2177 "zh_symbol_map"
      {"yan2", 7181},
#line 2183 "zh_symbol_map"
      {"yang2", 7187},
      {""}, {""},
#line 1153 "zh_symbol_map"
      {"ma6", 6157},
#line 1094 "zh_symbol_map"
      {"lo1", 6098},
      {""}, {""}, {""}, {""},
#line 2180 "zh_symbol_map"
      {"yan5", 7184},
#line 2186 "zh_symbol_map"
      {"yang5", 7190},
      {""}, {""},
#line 750 "zh_symbol_map"
      {"hu5", 5754},
      {""}, {""}, {""}, {""}, {""},
#line 2178 "zh_symbol_map"
      {"yan3", 7182},
#line 2184 "zh_symbol_map"
      {"yang3", 7188},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 278 "zh_symbol_map"
      {"chui1", 5282},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 905 "zh_symbol_map"
      {"kao4", 5909},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1692 "zh_symbol_map"
      {"rui4", 6696},
#line 903 "zh_symbol_map"
      {"kao2", 5907},
      {""}, {""}, {""},
#line 1845 "zh_symbol_map"
      {"shuan6", 6849},
#line 1851 "zh_symbol_map"
      {"shuang6", 6855},
      {""}, {""}, {""},
#line 1690 "zh_symbol_map"
      {"rui2", 6694},
#line 906 "zh_symbol_map"
      {"kao5", 5910},
      {""}, {""}, {""},
#line 1435 "zh_symbol_map"
      {"pa6", 6439},
      {""}, {""}, {""}, {""},
#line 1693 "zh_symbol_map"
      {"rui5", 6697},
#line 904 "zh_symbol_map"
      {"kao3", 5908},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1691 "zh_symbol_map"
      {"rui3", 6695},
      {""}, {""}, {""}, {""},
#line 2203 "zh_symbol_map"
      {"yi4", 7207},
      {""}, {""}, {""}, {""},
#line 2202 "zh_symbol_map"
      {"yi3", 7206},
#line 2209 "zh_symbol_map"
      {"yin4", 7213},
#line 2215 "zh_symbol_map"
      {"ying4", 7219},
      {""}, {""},
#line 2201 "zh_symbol_map"
      {"yi2", 7205},
      {""}, {""}, {""}, {""}, {""},
#line 2207 "zh_symbol_map"
      {"yin2", 7211},
#line 2213 "zh_symbol_map"
      {"ying2", 7217},
      {""}, {""},
#line 1207 "zh_symbol_map"
      {"mi6", 6211},
      {""}, {""}, {""}, {""},
#line 1839 "zh_symbol_map"
      {"shuai6", 6843},
#line 2210 "zh_symbol_map"
      {"yin5", 7214},
#line 2216 "zh_symbol_map"
      {"ying5", 7220},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2208 "zh_symbol_map"
      {"yin3", 7212},
#line 2214 "zh_symbol_map"
      {"ying3", 7218},
#line 775 "zh_symbol_map"
      {"huang6", 5779},
      {""},
#line 493 "zh_symbol_map"
      {"ei6", 5497},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1073 "zh_symbol_map"
      {"lie4", 6077},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1071 "zh_symbol_map"
      {"lie2", 6075},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1074 "zh_symbol_map"
      {"lie5", 6078},
#line 791 "zh_symbol_map"
      {"huo4", 5795},
      {""}, {""}, {""},
#line 1483 "zh_symbol_map"
      {"pi6", 6487},
      {""}, {""}, {""}, {""},
#line 1072 "zh_symbol_map"
      {"lie3", 6076},
#line 789 "zh_symbol_map"
      {"huo2", 5793},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 792 "zh_symbol_map"
      {"huo5", 5796},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 790 "zh_symbol_map"
      {"huo3", 5794},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 470 "zh_symbol_map"
      {"dun1", 5474},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 469 "zh_symbol_map"
      {"dui6", 5473},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 719 "zh_symbol_map"
      {"hei4", 5723},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 717 "zh_symbol_map"
      {"hei2", 5721},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 720 "zh_symbol_map"
      {"hei5", 5724},
      {""}, {""}, {""},
#line 912 "zh_symbol_map"
      {"ke5", 5916},
#line 895 "zh_symbol_map"
      {"kan6", 5899},
#line 901 "zh_symbol_map"
      {"kang6", 5905},
      {""}, {""}, {""},
#line 718 "zh_symbol_map"
      {"hei3", 5722},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 458 "zh_symbol_map"
      {"duan1", 5462},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 737 "zh_symbol_map"
      {"hong4", 5741},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 735 "zh_symbol_map"
      {"hong2", 5739},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 738 "zh_symbol_map"
      {"hong5", 5742},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 736 "zh_symbol_map"
      {"hong3", 5740},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2174 "zh_symbol_map"
      {"ya5", 7178},
      {""}, {""}, {""}, {""},
#line 2197 "zh_symbol_map"
      {"ye4", 7201},
      {""}, {""}, {""}, {""},
#line 2196 "zh_symbol_map"
      {"ye3", 7200},
      {""},
#line 437 "zh_symbol_map"
      {"diu4", 5441},
      {""}, {""},
#line 2195 "zh_symbol_map"
      {"ye2", 7199},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 435 "zh_symbol_map"
      {"diu2", 5439},
      {""}, {""},
#line 1183 "zh_symbol_map"
      {"me6", 6187},
      {""}, {""}, {""}, {""},
#line 1882 "zh_symbol_map"
      {"sou1", 6886},
      {""},
#line 438 "zh_symbol_map"
      {"diu5", 5442},
      {""},
#line 1833 "zh_symbol_map"
      {"shua6", 6837},
      {""},
#line 787 "zh_symbol_map"
      {"hun6", 5791},
      {""}, {""}, {""}, {""}, {""},
#line 436 "zh_symbol_map"
      {"diu3", 5440},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1145 "zh_symbol_map"
      {"lve4", 6149},
      {""}, {""}, {""}, {""}, {""},
#line 350 "zh_symbol_map"
      {"dai1", 5354},
      {""}, {""}, {""},
#line 1143 "zh_symbol_map"
      {"lve2", 6147},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1146 "zh_symbol_map"
      {"lve5", 6150},
#line 757 "zh_symbol_map"
      {"hua6", 5761},
      {""}, {""}, {""},
#line 2204 "zh_symbol_map"
      {"yi5", 7208},
#line 2395 "zh_symbol_map"
      {"zhua4", 7399},
      {""}, {""}, {""},
#line 1144 "zh_symbol_map"
      {"lve3", 6148},
      {""}, {""}, {""}, {""}, {""},
#line 2393 "zh_symbol_map"
      {"zhua2", 7397},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2396 "zh_symbol_map"
      {"zhua5", 7400},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2394 "zh_symbol_map"
      {"zhua3", 7398},
      {""}, {""}, {""}, {""}, {""},
#line 769 "zh_symbol_map"
      {"huan6", 5773},
      {""}, {""}, {""}, {""},
#line 2407 "zh_symbol_map"
      {"zhuan4", 7411},
#line 2413 "zh_symbol_map"
      {"zhuang4", 7417},
      {""}, {""}, {""},
#line 2405 "zh_symbol_map"
      {"zhuan2", 7409},
#line 2411 "zh_symbol_map"
      {"zhuang2", 7415},
      {""},
#line 1124 "zh_symbol_map"
      {"lun1", 6128},
#line 2191 "zh_symbol_map"
      {"yao4", 7195},
#line 2408 "zh_symbol_map"
      {"zhuan5", 7412},
#line 2414 "zh_symbol_map"
      {"zhuang5", 7418},
      {""}, {""}, {""},
#line 2406 "zh_symbol_map"
      {"zhuan3", 7410},
#line 2412 "zh_symbol_map"
      {"zhuang3", 7416},
      {""}, {""},
#line 2189 "zh_symbol_map"
      {"yao2", 7193},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2192 "zh_symbol_map"
      {"yao5", 7196},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2190 "zh_symbol_map"
      {"yao3", 7194},
      {""},
#line 2119 "zh_symbol_map"
      {"xie4", 7123},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 763 "zh_symbol_map"
      {"huai6", 5767},
#line 2117 "zh_symbol_map"
      {"xie2", 7121},
      {""}, {""}, {""},
#line 2401 "zh_symbol_map"
      {"zhuai4", 7405},
      {""}, {""}, {""}, {""},
#line 2399 "zh_symbol_map"
      {"zhuai2", 7403},
#line 2120 "zh_symbol_map"
      {"xie5", 7124},
      {""}, {""}, {""},
#line 2402 "zh_symbol_map"
      {"zhuai5", 7406},
      {""}, {""}, {""}, {""},
#line 2400 "zh_symbol_map"
      {"zhuai3", 7404},
#line 2118 "zh_symbol_map"
      {"xie3", 7122},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 427 "zh_symbol_map"
      {"die6", 5431},
      {""}, {""}, {""}, {""},
#line 692 "zh_symbol_map"
      {"han1", 5696},
#line 698 "zh_symbol_map"
      {"hang1", 5702},
      {""}, {""}, {""},
#line 1118 "zh_symbol_map"
      {"luan1", 6122},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 691 "zh_symbol_map"
      {"hai6", 5695},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1921 "zh_symbol_map"
      {"ta4", 6925},
      {""}, {""}, {""}, {""},
#line 1920 "zh_symbol_map"
      {"ta3", 6924},
#line 1933 "zh_symbol_map"
      {"tan4", 6937},
#line 1939 "zh_symbol_map"
      {"tang4", 6943},
      {""}, {""},
#line 1919 "zh_symbol_map"
      {"ta2", 6923},
#line 925 "zh_symbol_map"
      {"ken6", 5929},
#line 931 "zh_symbol_map"
      {"keng6", 5935},
      {""}, {""}, {""},
#line 1931 "zh_symbol_map"
      {"tan2", 6935},
#line 1937 "zh_symbol_map"
      {"tang2", 6941},
      {""}, {""}, {""},
#line 1091 "zh_symbol_map"
      {"liu4", 6095},
      {""}, {""}, {""}, {""},
#line 1934 "zh_symbol_map"
      {"tan5", 6938},
#line 1940 "zh_symbol_map"
      {"tang5", 6944},
      {""}, {""}, {""},
#line 1089 "zh_symbol_map"
      {"liu2", 6093},
      {""}, {""}, {""},
#line 878 "zh_symbol_map"
      {"ka1", 5882},
#line 1932 "zh_symbol_map"
      {"tan3", 6936},
#line 1938 "zh_symbol_map"
      {"tang3", 6942},
      {""}, {""}, {""},
#line 1092 "zh_symbol_map"
      {"liu5", 6096},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1090 "zh_symbol_map"
      {"liu3", 6094},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 476 "zh_symbol_map"
      {"duo1", 5480},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 998 "zh_symbol_map"
      {"lai1", 6002},
      {""}, {""}, {""}, {""},
#line 2198 "zh_symbol_map"
      {"ye5", 7202},
#line 2181 "zh_symbol_map"
      {"yan6", 7185},
#line 2187 "zh_symbol_map"
      {"yang6", 7191},
      {""}, {""},
#line 1963 "zh_symbol_map"
      {"ti4", 6967},
      {""}, {""}, {""}, {""},
#line 1962 "zh_symbol_map"
      {"ti3", 6966},
      {""},
#line 1987 "zh_symbol_map"
      {"ting4", 6991},
      {""}, {""},
#line 1961 "zh_symbol_map"
      {"ti2", 6965},
      {""}, {""}, {""}, {""},
#line 1695 "zh_symbol_map"
      {"run1", 6699},
      {""},
#line 1985 "zh_symbol_map"
      {"ting2", 6989},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1988 "zh_symbol_map"
      {"ting5", 6992},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1986 "zh_symbol_map"
      {"ting3", 6990},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 907 "zh_symbol_map"
      {"kao6", 5911},
      {""}, {""}, {""},
#line 746 "zh_symbol_map"
      {"hu1", 5750},
      {""}, {""}, {""}, {""},
#line 1694 "zh_symbol_map"
      {"rui6", 6698},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 257 "zh_symbol_map"
      {"chua4", 5261},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1677 "zh_symbol_map"
      {"rua1", 6681},
#line 380 "zh_symbol_map"
      {"dei1", 5384},
      {""},
#line 255 "zh_symbol_map"
      {"chua2", 5259},
      {""}, {""}, {""},
#line 1975 "zh_symbol_map"
      {"tiao4", 6979},
      {""}, {""}, {""}, {""}, {""},
#line 258 "zh_symbol_map"
      {"chua5", 5262},
      {""}, {""}, {""},
#line 1973 "zh_symbol_map"
      {"tiao2", 6977},
      {""}, {""}, {""},
#line 2211 "zh_symbol_map"
      {"yin6", 7215},
#line 2217 "zh_symbol_map"
      {"ying6", 7221},
#line 256 "zh_symbol_map"
      {"chua3", 5260},
      {""},
#line 1840 "zh_symbol_map"
      {"shuan1", 6844},
#line 1846 "zh_symbol_map"
      {"shuang1", 6850},
#line 1976 "zh_symbol_map"
      {"tiao5", 6980},
      {""}, {""}, {""}, {""},
#line 1969 "zh_symbol_map"
      {"tian4", 6973},
      {""},
#line 269 "zh_symbol_map"
      {"chuan4", 5273},
#line 275 "zh_symbol_map"
      {"chuang4", 5279},
      {""},
#line 1974 "zh_symbol_map"
      {"tiao3", 6978},
      {""},
#line 267 "zh_symbol_map"
      {"chuan2", 5271},
#line 273 "zh_symbol_map"
      {"chuang2", 5277},
#line 1683 "zh_symbol_map"
      {"ruan1", 6687},
#line 1967 "zh_symbol_map"
      {"tian2", 6971},
      {""},
#line 270 "zh_symbol_map"
      {"chuan5", 5274},
#line 276 "zh_symbol_map"
      {"chuang5", 5280},
      {""}, {""}, {""},
#line 268 "zh_symbol_map"
      {"chuan3", 5272},
#line 274 "zh_symbol_map"
      {"chuang3", 5278},
      {""},
#line 1970 "zh_symbol_map"
      {"tian5", 6974},
      {""}, {""}, {""}, {""},
#line 2409 "zh_symbol_map"
      {"zhuan6", 7413},
#line 2415 "zh_symbol_map"
      {"zhuang6", 7419},
      {""}, {""}, {""},
#line 1968 "zh_symbol_map"
      {"tian3", 6972},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 440 "zh_symbol_map"
      {"dong1", 5444},
      {""}, {""}, {""}, {""}, {""},
#line 1075 "zh_symbol_map"
      {"lie6", 6079},
      {""}, {""}, {""}, {""},
#line 1834 "zh_symbol_map"
      {"shuai1", 6838},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2164 "zh_symbol_map"
      {"xun1", 7168},
#line 263 "zh_symbol_map"
      {"chuai4", 5267},
      {""}, {""}, {""},
#line 770 "zh_symbol_map"
      {"huang1", 5774},
#line 261 "zh_symbol_map"
      {"chuai2", 5265},
#line 947 "zh_symbol_map"
      {"ku4", 5951},
#line 793 "zh_symbol_map"
      {"huo6", 5797},
      {""}, {""},
#line 264 "zh_symbol_map"
      {"chuai5", 5268},
#line 946 "zh_symbol_map"
      {"ku3", 5950},
#line 983 "zh_symbol_map"
      {"kun4", 5987},
      {""}, {""},
#line 262 "zh_symbol_map"
      {"chuai3", 5266},
#line 945 "zh_symbol_map"
      {"ku2", 5949},
      {""}, {""}, {""}, {""},
#line 637 "zh_symbol_map"
      {"gu6", 5641},
#line 981 "zh_symbol_map"
      {"kun2", 5985},
#line 2403 "zh_symbol_map"
      {"zhuai6", 7407},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 984 "zh_symbol_map"
      {"kun5", 5988},
      {""}, {""}, {""},
#line 1429 "zh_symbol_map"
      {"ou6", 6433},
      {""}, {""}, {""}, {""}, {""},
#line 982 "zh_symbol_map"
      {"kun3", 5986},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1393 "zh_symbol_map"
      {"nu6", 6397},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 953 "zh_symbol_map"
      {"kua4", 5957},
      {""}, {""}, {""}, {""},
#line 722 "zh_symbol_map"
      {"hen1", 5726},
#line 728 "zh_symbol_map"
      {"heng1", 5732},
      {""}, {""}, {""},
#line 951 "zh_symbol_map"
      {"kua2", 5955},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 954 "zh_symbol_map"
      {"kua5", 5958},
      {""}, {""}, {""},
#line 133 "zh_symbol_map"
      {"bu6", 5137},
      {""}, {""}, {""},
#line 2152 "zh_symbol_map"
      {"xuan1", 7156},
      {""},
#line 952 "zh_symbol_map"
      {"kua3", 5956},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 965 "zh_symbol_map"
      {"kuan4", 5969},
#line 971 "zh_symbol_map"
      {"kuang4", 5975},
      {""}, {""}, {""}, {""},
#line 969 "zh_symbol_map"
      {"kuang2", 5973},
      {""},
#line 1130 "zh_symbol_map"
      {"luo1", 6134},
#line 721 "zh_symbol_map"
      {"hei6", 5725},
#line 963 "zh_symbol_map"
      {"kuan2", 5967},
#line 972 "zh_symbol_map"
      {"kuang5", 5976},
      {""}, {""}, {""}, {""},
#line 970 "zh_symbol_map"
      {"kuang3", 5974},
      {""}, {""}, {""},
#line 966 "zh_symbol_map"
      {"kuan5", 5970},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1922 "zh_symbol_map"
      {"ta5", 6926},
      {""},
#line 964 "zh_symbol_map"
      {"kuan3", 5968},
      {""}, {""},
#line 1951 "zh_symbol_map"
      {"te4", 6955},
      {""}, {""}, {""}, {""},
#line 1950 "zh_symbol_map"
      {"te3", 6954},
      {""},
#line 1957 "zh_symbol_map"
      {"teng4", 6961},
      {""}, {""},
#line 1949 "zh_symbol_map"
      {"te2", 6953},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1955 "zh_symbol_map"
      {"teng2", 6959},
      {""}, {""}, {""}, {""},
#line 959 "zh_symbol_map"
      {"kuai4", 5963},
      {""}, {""}, {""}, {""},
#line 1958 "zh_symbol_map"
      {"teng5", 6962},
      {""}, {""}, {""}, {""},
#line 957 "zh_symbol_map"
      {"kuai2", 5961},
      {""}, {""},
#line 908 "zh_symbol_map"
      {"ke1", 5912},
      {""},
#line 1956 "zh_symbol_map"
      {"teng3", 6960},
      {""},
#line 2143 "zh_symbol_map"
      {"xiu4", 7147},
      {""}, {""},
#line 960 "zh_symbol_map"
      {"kuai5", 5964},
      {""},
#line 739 "zh_symbol_map"
      {"hong6", 5743},
      {""}, {""}, {""}, {""},
#line 2141 "zh_symbol_map"
      {"xiu2", 7145},
      {""}, {""},
#line 958 "zh_symbol_map"
      {"kuai3", 5962},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2144 "zh_symbol_map"
      {"xiu5", 7148},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2142 "zh_symbol_map"
      {"xiu3", 7146},
#line 1597 "zh_symbol_map"
      {"qu6", 6601},
      {""}, {""}, {""}, {""}, {""},
#line 704 "zh_symbol_map"
      {"hao1", 5708},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1028 "zh_symbol_map"
      {"lei1", 6032},
      {""}, {""}, {""}, {""},
#line 1964 "zh_symbol_map"
      {"ti5", 6968},
      {""},
#line 439 "zh_symbol_map"
      {"diu6", 5443},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2170 "zh_symbol_map"
      {"ya1", 7174},
#line 887 "zh_symbol_map"
      {"kai4", 5891},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 885 "zh_symbol_map"
      {"kai2", 5889},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 888 "zh_symbol_map"
      {"kai5", 5892},
      {""}, {""}, {""}, {""},
#line 1945 "zh_symbol_map"
      {"tao4", 6949},
      {""}, {""}, {""}, {""},
#line 886 "zh_symbol_map"
      {"kai3", 5890},
      {""}, {""}, {""}, {""},
#line 1943 "zh_symbol_map"
      {"tao2", 6947},
      {""},
#line 1100 "zh_symbol_map"
      {"long1", 6104},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1147 "zh_symbol_map"
      {"lve6", 6151},
#line 1946 "zh_symbol_map"
      {"tao5", 6950},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1944 "zh_symbol_map"
      {"tao3", 6948},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2446 "zh_symbol_map"
      {"zou1", 7450},
      {""}, {""}, {""},
#line 2397 "zh_symbol_map"
      {"zhua6", 7401},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 271 "zh_symbol_map"
      {"chuan6", 5275},
#line 277 "zh_symbol_map"
      {"chuang6", 5281},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2200 "zh_symbol_map"
      {"yi1", 7204},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2193 "zh_symbol_map"
      {"yao6", 7197},
      {""}, {""}, {""}, {""},
#line 779 "zh_symbol_map"
      {"hui4", 5783},
      {""}, {""}, {""},
#line 799 "zh_symbol_map"
      {"ji6", 5803},
      {""}, {""}, {""}, {""}, {""},
#line 777 "zh_symbol_map"
      {"hui2", 5781},
      {""}, {""}, {""},
#line 1701 "zh_symbol_map"
      {"ruo1", 6705},
      {""}, {""}, {""},
#line 265 "zh_symbol_map"
      {"chuai6", 5269},
      {""},
#line 780 "zh_symbol_map"
      {"hui5", 5784},
      {""}, {""}, {""},
#line 948 "zh_symbol_map"
      {"ku5", 5952},
      {""}, {""},
#line 2121 "zh_symbol_map"
      {"xie6", 7125},
      {""}, {""},
#line 778 "zh_symbol_map"
      {"hui3", 5782},
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
#line 2239 "zh_symbol_map"
      {"yu4", 7243},
      {""}, {""}, {""}, {""},
#line 2238 "zh_symbol_map"
      {"yu3", 7242},
#line 2257 "zh_symbol_map"
      {"yun4", 7261},
      {""}, {""}, {""},
#line 2237 "zh_symbol_map"
      {"yu2", 7241},
      {""}, {""}, {""}, {""}, {""},
#line 2255 "zh_symbol_map"
      {"yun2", 7259},
      {""},
#line 973 "zh_symbol_map"
      {"kuang6", 5977},
      {""},
#line 1261 "zh_symbol_map"
      {"mu6", 6265},
      {""}, {""}, {""}, {""}, {""},
#line 2258 "zh_symbol_map"
      {"yun5", 7262},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2256 "zh_symbol_map"
      {"yun3", 7260},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1952 "zh_symbol_map"
      {"te5", 6956},
#line 1935 "zh_symbol_map"
      {"tan6", 6939},
#line 1941 "zh_symbol_map"
      {"tang6", 6945},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 989 "zh_symbol_map"
      {"kuo4", 5993},
#line 554 "zh_symbol_map"
      {"fou1", 5558},
      {""}, {""}, {""},
#line 1093 "zh_symbol_map"
      {"liu6", 6097},
      {""}, {""}, {""}, {""},
#line 987 "zh_symbol_map"
      {"kuo2", 5991},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 990 "zh_symbol_map"
      {"kuo5", 5994},
      {""},
#line 1659 "zh_symbol_map"
      {"rong1", 6663},
      {""},
#line 1531 "zh_symbol_map"
      {"pu6", 6535},
      {""}, {""}, {""}, {""}, {""},
#line 988 "zh_symbol_map"
      {"kuo3", 5992},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2245 "zh_symbol_map"
      {"yuan4", 7249},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2243 "zh_symbol_map"
      {"yuan2", 7247},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2246 "zh_symbol_map"
      {"yuan5", 7250},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2244 "zh_symbol_map"
      {"yuan3", 7248},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1989 "zh_symbol_map"
      {"ting6", 6993},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2194 "zh_symbol_map"
      {"ye1", 7198},
#line 917 "zh_symbol_map"
      {"kei4", 5921},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 915 "zh_symbol_map"
      {"kei2", 5919},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 918 "zh_symbol_map"
      {"kei5", 5922},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 916 "zh_symbol_map"
      {"kei3", 5920},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 308 "zh_symbol_map"
      {"cou1", 5312},
      {""}, {""}, {""},
#line 259 "zh_symbol_map"
      {"chua6", 5263},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1977 "zh_symbol_map"
      {"tiao6", 6981},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 935 "zh_symbol_map"
      {"kong4", 5939},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 933 "zh_symbol_map"
      {"kong2", 5937},
      {""}, {""},
#line 1971 "zh_symbol_map"
      {"tian6", 6975},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 936 "zh_symbol_map"
      {"kong5", 5940},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 934 "zh_symbol_map"
      {"kong3", 5938},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2404 "zh_symbol_map"
      {"zhuan1", 7408},
#line 2410 "zh_symbol_map"
      {"zhuang1", 7414},
      {""}, {""},
#line 985 "zh_symbol_map"
      {"kun6", 5989},
      {""}, {""}, {""}, {""}, {""},
#line 127 "zh_symbol_map"
      {"bo6", 5131},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2037 "zh_symbol_map"
      {"wa6", 7041},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2398 "zh_symbol_map"
      {"zhuai1", 7402},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 955 "zh_symbol_map"
      {"kua6", 5959},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2240 "zh_symbol_map"
      {"yu5", 7244},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 967 "zh_symbol_map"
      {"kuan6", 5971},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1712 "zh_symbol_map"
      {"sa6", 6716},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1959 "zh_symbol_map"
      {"teng6", 6963},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 961 "zh_symbol_map"
      {"kuai6", 5965},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1918 "zh_symbol_map"
      {"ta1", 6922},
      {""}, {""},
#line 2161 "zh_symbol_map"
      {"xue4", 7165},
      {""}, {""},
#line 464 "zh_symbol_map"
      {"dui1", 5468},
      {""}, {""},
#line 2145 "zh_symbol_map"
      {"xiu6", 7149},
      {""}, {""}, {""},
#line 2159 "zh_symbol_map"
      {"xue2", 7163},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2162 "zh_symbol_map"
      {"xue5", 7166},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2160 "zh_symbol_map"
      {"xue3", 7164},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 890 "zh_symbol_map"
      {"kan1", 5894},
#line 896 "zh_symbol_map"
      {"kang1", 5900},
#line 1875 "zh_symbol_map"
      {"si6", 6879},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 889 "zh_symbol_map"
      {"kai6", 5893},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1960 "zh_symbol_map"
      {"ti1", 6964},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1947 "zh_symbol_map"
      {"tao6", 6951},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1828 "zh_symbol_map"
      {"shua1", 6832},
      {""},
#line 782 "zh_symbol_map"
      {"hun1", 5786},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 781 "zh_symbol_map"
      {"hui6", 5785},
      {""}, {""},
#line 266 "zh_symbol_map"
      {"chuan1", 5270},
#line 272 "zh_symbol_map"
      {"chuang1", 5276},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 752 "zh_symbol_map"
      {"hua1", 5756},
      {""}, {""}, {""},
#line 2005 "zh_symbol_map"
      {"tu4", 7009},
      {""}, {""}, {""}, {""},
#line 2004 "zh_symbol_map"
      {"tu3", 7008},
#line 2023 "zh_symbol_map"
      {"tun4", 7027},
      {""}, {""}, {""},
#line 2003 "zh_symbol_map"
      {"tu2", 7007},
      {""},
#line 2221 "zh_symbol_map"
      {"yo4", 7225},
      {""}, {""}, {""},
#line 2021 "zh_symbol_map"
      {"tun2", 7025},
#line 2220 "zh_symbol_map"
      {"yo3", 7224},
      {""},
#line 2227 "zh_symbol_map"
      {"yong4", 7231},
      {""}, {""},
#line 2219 "zh_symbol_map"
      {"yo2", 7223},
      {""}, {""}, {""},
#line 2024 "zh_symbol_map"
      {"tun5", 7028},
      {""}, {""},
#line 2225 "zh_symbol_map"
      {"yong2", 7229},
      {""}, {""},
#line 1249 "zh_symbol_map"
      {"mo6", 6253},
      {""},
#line 260 "zh_symbol_map"
      {"chuai1", 5264},
#line 944 "zh_symbol_map"
      {"ku1", 5948},
#line 2022 "zh_symbol_map"
      {"tun3", 7026},
#line 764 "zh_symbol_map"
      {"huan1", 5768},
      {""},
#line 2228 "zh_symbol_map"
      {"yong5", 7232},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2226 "zh_symbol_map"
      {"yong3", 7230},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 758 "zh_symbol_map"
      {"huai1", 5762},
      {""}, {""}, {""}, {""},
#line 1519 "zh_symbol_map"
      {"po6", 6523},
      {""}, {""}, {""},
#line 2259 "zh_symbol_map"
      {"yun6", 7263},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2011 "zh_symbol_map"
      {"tuan4", 7015},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2009 "zh_symbol_map"
      {"tuan2", 7013},
      {""}, {""}, {""}, {""}, {""},
#line 1742 "zh_symbol_map"
      {"se6", 6746},
      {""}, {""}, {""},
#line 2012 "zh_symbol_map"
      {"tuan5", 7016},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2010 "zh_symbol_map"
      {"tuan3", 7014},
      {""}, {""}, {""},
#line 422 "zh_symbol_map"
      {"die1", 5426},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 968 "zh_symbol_map"
      {"kuang1", 5972},
      {""}, {""},
#line 991 "zh_symbol_map"
      {"kuo6", 5995},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1948 "zh_symbol_map"
      {"te1", 6952},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 686 "zh_symbol_map"
      {"hai1", 5690},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2247 "zh_symbol_map"
      {"yuan6", 7251},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 920 "zh_symbol_map"
      {"ken1", 5924},
#line 926 "zh_symbol_map"
      {"keng1", 5930},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1927 "zh_symbol_map"
      {"tai4", 6931},
      {""}, {""}, {""}, {""},
#line 919 "zh_symbol_map"
      {"kei6", 5923},
      {""}, {""}, {""}, {""},
#line 1925 "zh_symbol_map"
      {"tai2", 6929},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1928 "zh_symbol_map"
      {"tai5", 6932},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1926 "zh_symbol_map"
      {"tai3", 6930},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2176 "zh_symbol_map"
      {"yan1", 7180},
#line 2182 "zh_symbol_map"
      {"yang1", 7186},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 449 "zh_symbol_map"
      {"dou4", 5453},
      {""}, {""}, {""}, {""},
#line 937 "zh_symbol_map"
      {"kong6", 5941},
      {""}, {""}, {""}, {""},
#line 447 "zh_symbol_map"
      {"dou2", 5451},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 450 "zh_symbol_map"
      {"dou5", 5454},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 448 "zh_symbol_map"
      {"dou3", 5452},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 902 "zh_symbol_map"
      {"kao1", 5906},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1689 "zh_symbol_map"
      {"rui1", 6693},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2006 "zh_symbol_map"
      {"tu5", 7010},
      {""}, {""}, {""}, {""}, {""},
#line 2206 "zh_symbol_map"
      {"yin1", 7210},
#line 2212 "zh_symbol_map"
      {"ying1", 7216},
      {""}, {""}, {""}, {""},
#line 2222 "zh_symbol_map"
      {"yo5", 7226},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 503 "zh_symbol_map"
      {"eng4", 5507},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 501 "zh_symbol_map"
      {"eng2", 5505},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1070 "zh_symbol_map"
      {"lie1", 6074},
      {""},
#line 504 "zh_symbol_map"
      {"eng5", 5508},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 502 "zh_symbol_map"
      {"eng3", 5506},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 788 "zh_symbol_map"
      {"huo1", 5792},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2236 "zh_symbol_map"
      {"yu1", 7240},
#line 977 "zh_symbol_map"
      {"kui4", 5981},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 975 "zh_symbol_map"
      {"kui2", 5979},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 978 "zh_symbol_map"
      {"kui5", 5982},
      {""}, {""}, {""},
#line 859 "zh_symbol_map"
      {"ju6", 5863},
#line 2029 "zh_symbol_map"
      {"tuo4", 7033},
      {""}, {""}, {""}, {""},
#line 976 "zh_symbol_map"
      {"kui3", 5980},
      {""}, {""}, {""}, {""},
#line 2027 "zh_symbol_map"
      {"tuo2", 7031},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2030 "zh_symbol_map"
      {"tuo5", 7034},
      {""},
#line 2163 "zh_symbol_map"
      {"xue6", 7167},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2028 "zh_symbol_map"
      {"tuo3", 7032},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2265 "zh_symbol_map"
      {"za6", 7269},
      {""}, {""}, {""}, {""}, {""},
#line 716 "zh_symbol_map"
      {"hei1", 5720},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1109 "zh_symbol_map"
      {"lou4", 6113},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1107 "zh_symbol_map"
      {"lou2", 6111},
      {""}, {""}, {""}, {""}, {""},
#line 734 "zh_symbol_map"
      {"hong1", 5738},
      {""}, {""}, {""},
#line 1110 "zh_symbol_map"
      {"lou5", 6114},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1108 "zh_symbol_map"
      {"lou3", 6112},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2439 "zh_symbol_map"
      {"zi6", 7443},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 434 "zh_symbol_map"
      {"diu1", 5438},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1993 "zh_symbol_map"
      {"tong4", 6997},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1991 "zh_symbol_map"
      {"tong2", 6995},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1994 "zh_symbol_map"
      {"tong5", 6998},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1992 "zh_symbol_map"
      {"tong3", 6996},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1142 "zh_symbol_map"
      {"lve1", 6146},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2392 "zh_symbol_map"
      {"zhua1", 7396},
      {""}, {""}, {""}, {""},
#line 2025 "zh_symbol_map"
      {"tun6", 7029},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 517 "zh_symbol_map"
      {"fa6", 5521},
      {""}, {""}, {""},
#line 2229 "zh_symbol_map"
      {"yong6", 7233},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 19 "zh_symbol_map"
      {"ai6", 5023},
#line 2188 "zh_symbol_map"
      {"yao1", 7192},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2116 "zh_symbol_map"
      {"xie1", 7120},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2013 "zh_symbol_map"
      {"tuan6", 7017},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 1668 "zh_symbol_map"
      {"rou4", 6672},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1666 "zh_symbol_map"
      {"rou2", 6670},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1669 "zh_symbol_map"
      {"rou5", 6673},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1667 "zh_symbol_map"
      {"rou3", 6671},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 139 "zh_symbol_map"
      {"ca6", 5143},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2295 "zh_symbol_map"
      {"ze6", 7299},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 511 "zh_symbol_map"
      {"er6", 5515},
      {""}, {""}, {""}, {""}, {""},
#line 1930 "zh_symbol_map"
      {"tan1", 6934},
#line 1936 "zh_symbol_map"
      {"tang1", 6940},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1088 "zh_symbol_map"
      {"liu1", 6092},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1929 "zh_symbol_map"
      {"tai6", 6933},
      {""}, {""}, {""}, {""}, {""},
#line 301 "zh_symbol_map"
      {"ci6", 5305},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2085 "zh_symbol_map"
      {"wu6", 7089},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1984 "zh_symbol_map"
      {"ting1", 6988},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 451 "zh_symbol_map"
      {"dou6", 5455},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 254 "zh_symbol_map"
      {"chua1", 5258},
      {""}, {""}, {""}, {""},
#line 1893 "zh_symbol_map"
      {"su6", 6897},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1972 "zh_symbol_map"
      {"tiao1", 6976},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1966 "zh_symbol_map"
      {"tian1", 6970},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2002 "zh_symbol_map"
      {"tu1", 7006},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 505 "zh_symbol_map"
      {"eng6", 5509},
      {""}, {""}, {""}, {""},
#line 2218 "zh_symbol_map"
      {"yo1", 7222},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 980 "zh_symbol_map"
      {"kun1", 5984},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 979 "zh_symbol_map"
      {"kui6", 5983},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 950 "zh_symbol_map"
      {"kua1", 5954},
      {""}, {""}, {""}, {""},
#line 2031 "zh_symbol_map"
      {"tuo6", 7035},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 169 "zh_symbol_map"
      {"ce6", 5173},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 962 "zh_symbol_map"
      {"kuan1", 5966},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1954 "zh_symbol_map"
      {"teng1", 6958},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 956 "zh_symbol_map"
      {"kuai1", 5960},
      {""}, {""}, {""}, {""}, {""},
#line 1111 "zh_symbol_map"
      {"lou6", 6115},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2140 "zh_symbol_map"
      {"xiu1", 7144},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1995 "zh_symbol_map"
      {"tong6", 6999},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 884 "zh_symbol_map"
      {"kai1", 5888},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 1942 "zh_symbol_map"
      {"tao1", 6946},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 776 "zh_symbol_map"
      {"hui1", 5780},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1670 "zh_symbol_map"
      {"rou6", 6674},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2017 "zh_symbol_map"
      {"tui4", 7021},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2015 "zh_symbol_map"
      {"tui2", 7019},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2018 "zh_symbol_map"
      {"tui5", 7022},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2016 "zh_symbol_map"
      {"tui3", 7020},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2254 "zh_symbol_map"
      {"yun1", 7258},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 499 "zh_symbol_map"
      {"en6", 5503},
      {""}, {""},
#line 2079 "zh_symbol_map"
      {"wo6", 7083},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 986 "zh_symbol_map"
      {"kuo1", 5990},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2242 "zh_symbol_map"
      {"yuan1", 7246},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 914 "zh_symbol_map"
      {"kei1", 5918},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 932 "zh_symbol_map"
      {"kong1", 5936},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2457 "zh_symbol_map"
      {"zu6", 7461},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1981 "zh_symbol_map"
      {"tie4", 6985},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1979 "zh_symbol_map"
      {"tie2", 6983},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1982 "zh_symbol_map"
      {"tie5", 6986},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1980 "zh_symbol_map"
      {"tie3", 6984},
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
#line 743 "zh_symbol_map"
      {"hou4", 5747},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 741 "zh_symbol_map"
      {"hou2", 5745},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 744 "zh_symbol_map"
      {"hou5", 5748},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 742 "zh_symbol_map"
      {"hou3", 5746},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2158 "zh_symbol_map"
      {"xue1", 7162},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 565 "zh_symbol_map"
      {"fu6", 5569},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 319 "zh_symbol_map"
      {"cu6", 5323},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2020 "zh_symbol_map"
      {"tun1", 7024},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2224 "zh_symbol_map"
      {"yong1", 7228},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2019 "zh_symbol_map"
      {"tui6", 7023},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2008 "zh_symbol_map"
      {"tuan1", 7012},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1924 "zh_symbol_map"
      {"tai1", 6928},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 2251 "zh_symbol_map"
      {"yue4", 7255},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2249 "zh_symbol_map"
      {"yue2", 7253},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2252 "zh_symbol_map"
      {"yue5", 7256},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2250 "zh_symbol_map"
      {"yue3", 7254},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 446 "zh_symbol_map"
      {"dou1", 5450},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1983 "zh_symbol_map"
      {"tie6", 6987},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 500 "zh_symbol_map"
      {"eng1", 5504},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 37 "zh_symbol_map"
      {"ao6", 5041},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 974 "zh_symbol_map"
      {"kui1", 5978},
#line 553 "zh_symbol_map"
      {"fo6", 5557},
      {""},
#line 745 "zh_symbol_map"
      {"hou6", 5749},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 2026 "zh_symbol_map"
      {"tuo1", 7030},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1106 "zh_symbol_map"
      {"lou1", 6110},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1990 "zh_symbol_map"
      {"tong1", 6994},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 29 "zh_symbol_map"
      {"ang4", 5033},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 27 "zh_symbol_map"
      {"ang2", 5031},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 30 "zh_symbol_map"
      {"ang5", 5034},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 28 "zh_symbol_map"
      {"ang3", 5032},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 1665 "zh_symbol_map"
      {"rou1", 6669},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2253 "zh_symbol_map"
      {"yue6", 7257},
      {""}, {""},
#line 941 "zh_symbol_map"
      {"kou4", 5945},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 939 "zh_symbol_map"
      {"kou2", 5943},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 942 "zh_symbol_map"
      {"kou5", 5946},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 940 "zh_symbol_map"
      {"kou3", 5944},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 2233 "zh_symbol_map"
      {"you4", 7237},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2231 "zh_symbol_map"
      {"you2", 7235},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2234 "zh_symbol_map"
      {"you5", 7238},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2232 "zh_symbol_map"
      {"you3", 7236},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 31 "zh_symbol_map"
      {"ang6", 5035},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 2014 "zh_symbol_map"
      {"tui1", 7018},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 349 "zh_symbol_map"
      {"da6", 5353},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 943 "zh_symbol_map"
      {"kou6", 5947},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 403 "zh_symbol_map"
      {"di6", 5407},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1978 "zh_symbol_map"
      {"tie1", 6982},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 997 "zh_symbol_map"
      {"la6", 6001},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 379 "zh_symbol_map"
      {"de6", 5383},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 740 "zh_symbol_map"
      {"hou1", 5744},
      {""},
#line 1045 "zh_symbol_map"
      {"li6", 6049},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 25 "zh_symbol_map"
      {"an6", 5029},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1999 "zh_symbol_map"
      {"tou4", 7003},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1997 "zh_symbol_map"
      {"tou2", 7001},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2000 "zh_symbol_map"
      {"tou5", 7004},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1998 "zh_symbol_map"
      {"tou3", 7002},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 2235 "zh_symbol_map"
      {"you6", 7239},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1658 "zh_symbol_map"
      {"ri6", 6662},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 1027 "zh_symbol_map"
      {"le6", 6031},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 1141 "zh_symbol_map"
      {"lv6", 6145},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 2091 "zh_symbol_map"
      {"xi6", 7095},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2248 "zh_symbol_map"
      {"yue1", 7252},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1640 "zh_symbol_map"
      {"re6", 6644},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 2001 "zh_symbol_map"
      {"tou6", 7005},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 457 "zh_symbol_map"
      {"du6", 5461},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 26 "zh_symbol_map"
      {"ang1", 5030},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 938 "zh_symbol_map"
      {"kou1", 5942},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 1117 "zh_symbol_map"
      {"lu6", 6121},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 685 "zh_symbol_map"
      {"ha6", 5689},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1676 "zh_symbol_map"
      {"ru6", 6680},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 2151 "zh_symbol_map"
      {"xu6", 7155},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 2230 "zh_symbol_map"
      {"you1", 7234},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 715 "zh_symbol_map"
      {"he6", 5719},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1099 "zh_symbol_map"
      {"lo6", 6103},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1996 "zh_symbol_map"
      {"tou1", 7000},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 883 "zh_symbol_map"
      {"ka6", 5887},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 751 "zh_symbol_map"
      {"hu6", 5755},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 913 "zh_symbol_map"
      {"ke6", 5917},
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
#line 2175 "zh_symbol_map"
      {"ya6", 7179},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 2205 "zh_symbol_map"
      {"yi6", 7209},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 2199 "zh_symbol_map"
      {"ye6", 7203},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1923 "zh_symbol_map"
      {"ta6", 6927},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 1965 "zh_symbol_map"
      {"ti6", 6969},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 949 "zh_symbol_map"
      {"ku6", 5953},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 1953 "zh_symbol_map"
      {"te6", 6957},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 2241 "zh_symbol_map"
      {"yu6", 7245},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
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
#line 2007 "zh_symbol_map"
      {"tu6", 7011},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 2223 "zh_symbol_map"
      {"yo6", 7227}
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
