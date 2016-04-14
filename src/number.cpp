#include "ekho_dict.h"
#include "character.h"
#include <list>
#include "config.h"

using namespace std;

namespace ekho {
// detect whether there are number
bool Dict::hasNumbers(list<Character> &charList) {
  int count = 0;
  list<Character>::iterator i = charList.begin();
  for (; i != charList.end(); i++) {
    if (i->code >= 48 && i->code < 58) {
      return true;
    }
  }

  return false;
}

static int numbersBefore(list<Character> &charlist,
    list<Character>::iterator itor) {
  int count = 0;
  while (itor != charlist.begin()) {
    itor--;
    if (itor->code >= 48 && itor->code < 58)
      count++;
    else
      return count;
  }

  return count;
}

static int numbersAfter(list<Character> &charlist,
    list<Character>::iterator itor) {
  int count = 0;
  itor++;
  while (itor != charlist.end()) {
    if (itor->code >= 48 && itor->code < 58)
      count++;
    else
      return count;
    itor++;
  }
  return count;
}

static int lfind(list<Character> &charlist,
    list<Character>::iterator itor, int code, int max_count) {
  int count = 0;
  while (itor != charlist.begin() && count <= max_count) {
    itor--;
    if (itor->code != code)
      count++;
    else
      return count + 1;
  }

  if (itor->code != code)
    return -1;
  else
    return count + 1;
}

static int rfind(list<Character> &charlist,
    list<Character>::iterator itor, int code, int max_count) {
  int count = 0;
  itor++;
  while (itor != charlist.end() && count <= max_count) {
    if (itor->code != code)
      count++;
    else
      return count + 1;
    itor++;
  }

  if (itor == charlist.end())
    return -1;
  else
    return count + 1;
}

static void readNumberOneByOne(list<Character> &charList,
    list<Character> &convertedCharList,
    list<Character>::iterator &i) {
  while (i != charList.end() && i->code >= '0' && i->code <= '9') {
    int code = 0;
    switch (i->code) {
      case '0': code = 38646; break;
      case '1': code = 19968; break;
      case '2': code = 20108; break;
      case '3': code = 19977; break;
      case '4': code = 22235; break;
      case '5': code = 20116; break;
      case '6': code = 20845; break;
      case '7': code = 19971; break;
      case '8': code = 20843; break;
      case '9': code = 20061; break;
    }
    convertedCharList.push_back(code);
    i++;
  }

  i--;
}

static void readIntegerNumber(list<Character> &charList,
    list<Character> &convertedCharList,
    list<Character>::iterator &i) {
  int count_before = 0;
  int count_after = numbersAfter(charList, i);
  while (i != charList.end() && i->code >= 48 && i->code < 58) {

    // 0
    if (i->code == 48) {
      // skip zeros
      while (i != charList.end() && i->code == 48) {
        switch (count_after) {
          case 4: convertedCharList.push_back(19975); break; // wan
          case 8: convertedCharList.push_back(20159); break; // yi 
        }
        i++;
        count_before++;
        count_after--;
      }

      if (i != charList.end() && i->code >= 48 && i->code < 58)
        convertedCharList.push_back(48);
      else
        break;
    }

    // 1x
    if ((count_after == 1 || count_after == 5 || count_after == 9) && count_before == 0 && i->code == '1') {
      // skip sound 1
    } else {
      convertedCharList.push_back(i->code);
    }

    switch (count_after) {
      case 1: convertedCharList.push_back(21313); break; // ten
      case 2: convertedCharList.push_back(30334); break; // hundred
      case 3: convertedCharList.push_back(21315); break; // thousand 
      case 4: convertedCharList.push_back(19975); break; // wan
      case 5: convertedCharList.push_back(21313); break; // ten
      case 6: convertedCharList.push_back(30334); break; // hundred
      case 7: convertedCharList.push_back(21315); break; // thousand 
      case 8: convertedCharList.push_back(20159); break; // yi 
      case 9: convertedCharList.push_back(21313); break; // ten
      case 10: convertedCharList.push_back(30334); break; // hundred
      case 11: convertedCharList.push_back(21315); break; // thousand 
    }

    i++;
    count_before++;
    count_after--;
  }

  i--;
}

// convert number in charList
// output to convertedCharList
// code of Chinese ten: 21313
// code of Chinese hundred: 30334
// code of Chinese thousand: 21315
// code of wan: 19975
// code of Chinese point: 28857
// zip code
// telephone
void Dict::replaceNumbers(list<Character> &charList,
    list<Character> &convertedCharList) {
  list<Character>::iterator begin = charList.begin();
  list<Character>::iterator end = charList.end();
  list<Character>::iterator i = charList.begin();
  list<Character>::iterator i0 = i;
  list<Character>::iterator i2 = i;

  // insert_pos only work for 2 digit
  list<Character>::iterator insert_pos = charList.end();
  int insert_code = 0;
  int insert_code2 = 0;

  // for %
  list<Character>::iterator percent_pos = charList.end();
  
  while (i != charList.end()) {
    // skip translated percent
    if (i == percent_pos) {
      percent_pos = charList.end();
      i++;
      continue;
    }

    // .[0-9]*
    if (i->code == 46) {
      i2 = i;
      i2++;
      if (i2 != charList.end() && i2->code >= 48 && i2->code < 58) {
        convertedCharList.push_back(28857); // point
        i++;
        readNumberOneByOne(charList, convertedCharList, i);
        if (i == insert_pos) {
          convertedCharList.push_back(insert_code);
          if (insert_code2)
            convertedCharList.push_back(insert_code2);
        }
        i++;
        continue;
      }
    }

    bool one_by_one = true;
    // $, yuan
    if (i->code == '$' || i->code == 65509) {
      int count = numbersAfter(charList, i);
      if (count > 0) {
        insert_pos = i;
        if (i->code == '$') {
          insert_code = 32654; // mei
          insert_code2 = 20803; // yuan
        } else {
          insert_code = 20803;
          insert_code2 = 0;
        }
        for (int j = 0; j < count; j++)
          insert_pos++;

        i++;
        one_by_one = false;
      }
    }

    // yyyy-mm-dd
    if (i->code == '-') { // -
      if (numbersBefore(charList, i) == 4) {
        int count = numbersAfter(charList, i);
        if (count == 1 || count == 2) {
          i2 = i; i2--; i2--; i2--; i2--;
          if (i2->code == '1' || i2->code == '2')
            convertedCharList.push_back(24180); // year
          i++;

          int count2 = rfind(charList, i, '-', 2);
          if (count2 != 1 && count2 != 2) {
            insert_code = 26376; // month
            insert_pos = i;
            if (count == 2)
              insert_pos++;
          }

          continue;
        }
      } else {
        int count = lfind(charList, i, '-', 3);
        if (count == 2 || count == 3) {
          int count_before = numbersBefore(charList, i);
          if (count_before == 1 || count_before == 2) {
            count = numbersAfter(charList, i);
            if (count == 1 || count == 2) {
              i2 = i; i2--; i2--;
              if (count_before == 2)
                i2--;
              if (numbersBefore(charList, i2) == 4) {
                i2--; i2--; i2--; i2--;
                if (i2->code == '1' || i2->code == '2') {
                  convertedCharList.push_back(26376); // month
                  i++;
                  insert_pos = i;
                  if (count == 2)
                    insert_pos++;
                  insert_code = 26085; // day
                  continue;
                }
              }
            }
          }
        }
      }
    }

    // yyyy/mm/dd
    if (i->code == '/') {
      if (numbersBefore(charList, i) == 4) {
        int count = numbersAfter(charList, i);
        if (count == 1 || count == 2) {
          count = rfind(charList, i, '/', 3);
          if (count == 2 || count == 3) {
            i2 = i; i2--; i2--; i2--; i2--;
            if (i2->code == '1' || i2->code == '2')
              convertedCharList.push_back(24180); // year
            i++;
            continue;
          }
        }
      } else {
        int count = lfind(charList, i, '/', 3);
        if (count == 2 || count == 3) {
          int count_before = numbersBefore(charList, i);
          if (count_before == 1 || count_before == 2) {
            count = numbersAfter(charList, i);
            if (count == 1 || count == 2) {
              i2 = i; i2--; i2--;
              if (count_before == 2)
                i2--;
              if (numbersBefore(charList, i2) == 4) {
                i2--; i2--; i2--; i2--;
                if (i2->code == '1' || i2->code == '2') {
                  convertedCharList.push_back(26376); // month
                  i++;
                  insert_pos = i;
                  if (count == 2)
                    insert_pos++;
                  insert_code = 26085; // day
                  continue;
                }
              }
            }
          }
        }
      }
    }
    
    // hh:mm:ss
    if (i->code == 58) { // :
      int count = numbersBefore(charList, i);
      if (count > 0 && count <= 2) {
        if (numbersAfter(charList, i) == 2) {
          count = lfind(charList, i, 58, 3);
          if (count < 0) {
            // hh:mm
            convertedCharList.push_back(28857); // point/o'clock
            i++;

            // hh:00:00
            if (i->code == 48) {
              i2 = i;
              i2++;
              if (i2->code == 48) {
                i2++;
                if (i2->code < 48 || i2->code > 58) {
                  convertedCharList.push_back(27491); // zheng/o'clock
                  i++; i++;
                } else if (i2->code == 58) {
                  i2++;
                  if (i2->code == 48) {
                    i2++;
                    if (i2->code == 48 && numbersAfter(charList, i2) == 0) {
                      convertedCharList.push_back(27491); // zheng/o'clock
                      i++; i++; i++; i++; i++;
                    }
                  }
                }
              } else {
                if (rfind(charList, i, 58, 2) < 0) {
                  insert_code = 20998; // minute
                  insert_pos = i;
                  insert_pos++;
                }
              }
            } else {
              if (rfind(charList, i, 58, 2) < 0) {
                insert_code = 20998; // minute
                insert_pos = i;
                insert_pos++;
              }
            }

            continue;
          } else if (count == 3) {
            // hh:mm:ss
            convertedCharList.push_back(20998); // minute
            i++;
            insert_code = 31186; // second
            insert_pos = i;
            insert_pos++;
            continue;
          }
        } else {
          // 3:4
          convertedCharList.push_back(27604); // compare
          i++;
          continue;
        }
      } else if (count > 0) {
        // 3:4
        convertedCharList.push_back(27604); // compare
        i++;
        continue;
      }
    }

    if (i->code >= 48 && i->code < 58) {
      // get suffix
      i2 = i;
      do {
        i2++;
      } while (i2 != charList.end() &&
          (i2->code == 46 || (i2->code >= 48 && i2->code < 58)));

      // %
      if (percent_pos == charList.end() && i2 != charList.end() && i2->code == '%') {
        percent_pos = i2;
        convertedCharList.push_back(30334); // bai
        convertedCharList.push_back(20998); // fen
        convertedCharList.push_back(20043); // zhi
      }

      // find money unit: yuan, wan, yi, yuan sign, $
      if (i2 != charList.end()) {
        if (i2->code == 20803 || // yuan
            i2->code == 19975 || // wan
            i2->code == 20159 // yi
           ) {
          one_by_one = false;
        }
      }

      if (one_by_one && i->code != 48 && numbersAfter(charList, i) < 3)
        one_by_one = false;

      // 0[0-9]*
      if (i->code == 48) {
        // time
        if (numbersAfter(charList, i) == 1 && rfind(charList, i, 58, 2) == 2) {
          i++;
        }
      }

      if (one_by_one)
        readNumberOneByOne(charList, convertedCharList, i);
      else
        readIntegerNumber(charList, convertedCharList, i);
    } else {
      convertedCharList.push_back(i->code);
    }

    if (i == insert_pos) {
      convertedCharList.push_back(insert_code);
      if (insert_code2)
        convertedCharList.push_back(insert_code2);
    }

    if (i == charList.end())
      break;
    i++;
  }  
}

} // end of namespace ekho
