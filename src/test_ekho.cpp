/***************************************************************************
 * Copyright (C) 2008-2013 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: http://www.eguidedog.net                                       *
 *                                                                         *
 * This program is free software; you can redistribute it and/or           *
 * modify it under the terms of the GNU General Public License             *
 * as published by the Free Software Foundation; either version 2          *
 * of the License, or any later version.                                   *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 *                                                                         *
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software             *
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,              *
 * MA  02110-1301, USA.                                                    *
 **************************************************************************/

// build command: g++ ../src/test_ekho.cpp  -I. -I../include/soundtouch libekho.a -lsndfile ../lib/libSoundTouch.a -lportaudio ../lib/libFestival.a ../lib/libestools.a -lcurses ../lib/libeststring.a ../lib/libestbase.a -lvorbisenc -lvorbis -lm -logg  
//g++ -I../utfcpp/source -DEKHO_DATA_PATH="" test.cpp ekho_dict.cpp -lsndfile

#include <iostream>
#include <stdlib.h>
#include "config.h"
#include "ekho.h"

#ifdef ENABLE_WIN32
#include <windows.h>
#define sleep(seconds) Sleep((seconds)*1000)
#endif

using namespace ekho;
using namespace std;

void callback(void *arg) {
  printf("Finished speaking %s\n", (const char*)arg);
}

int main(int argc, char**argv) {
  // test numbers
  Ekho wong("Cantonese");
  wong.debug();
  wong.blockSpeak("0.123456789");
  wong.blockSpeak("$54321 ￥99999");
  wong.blockSpeak("50万 30.4亿 1234567890元 10086元");
  wong.blockSpeak("50% 49.2%");
  wong.blockSpeak("2014/12/23");
  wong.blockSpeak("2014/1/23");
  wong.blockSpeak("2014/12/2");
  wong.blockSpeak("2014-12");
  wong.blockSpeak("2014-12-23");
  wong.blockSpeak("12/23/2014");
  wong.blockSpeak("2014-1");
  wong.blockSpeak("2014-1-2");
  wong.blockSpeak("现在是2014年1月14日17:21:21");
  wong.blockSpeak("17:21:21");
  wong.blockSpeak("17:00:00");
  wong.blockSpeak("1:00:00");
  wong.blockSpeak("1:08");
  wong.blockSpeak("上午08:00，下午1:00");
  wong.blockSpeak("巴西2:1击败阿根廷");
  wong.blockSpeak("详情请致电81234567查询");
  wong.blockSpeak("a. b . c1 0 020, .314, 0.314, 10, 11, 20, 21, 100, 101, 110, 111, 1234");
  wong.blockSpeak("圆周率是3.14");
  wong.blockSpeak("为什么 hello world 123");

  Ekho tibetan("Tibetan");
  tibetan.blockSpeak("English is easy. 中文很简单。་སྐད་སླ་བོ་རེད།");

  // If you share your heart, there would be nothing to get jealous about
  Ekho *haesung = new Ekho("Korean");
  haesung->blockSpeak("사랑을 나누면 부러울것이 없습니다");
  delete(haesung);
  
  system("./ekho -l \"列出拼音\"");

  system("./ekho -f test.txt");
  printf("speaking '123' with jyutping-wong-44100-v4 voice\n");
  Ekho *wong_v4 = new Ekho("jyutping-wong-44100-v4");
  wong_v4->blockSpeak("123");
  delete(wong_v4);

  // test dictionary
  Dict dict(CANTONESE);
  list<PhoneticSymbol*> phons = dict.lookup("为什么，画面画画");
  for (list<PhoneticSymbol*>::iterator li = phons.begin(); li != phons.end(); li++) {
    cout << (*li)->symbol << " ";
  }
  cout << endl;
  dict.setLanguage(MANDARIN);
  cout << dict.lookup(0x7684) << endl;

/*
  Ekho *wong2 = new Ekho("Cantonese");
  wong2->blockSpeak("123");
  wong2->setVoice("Mandarin");
  PhoneticSymbol *ps = wong2->mDict.lookup(49);
  cout << ps->symbol << endl;
  wong2->speak("123", callback, (void*)"Mandarin 123");
  delete(wong2);

  Ekho *cameron = new Ekho("Cantonese");
  cameron->speak("123456789");
  sleep(1);
  cameron->pause();
  sleep(1);
  cameron->resume();
  sleep(1);
  cameron->stop();
  delete(cameron);

  Ekho *recorder = new Ekho("jyutping"); // same to Cantonese
  recorder->saveWav("123456789", "output.wav");
#ifdef ENABLE_OGG
  recorder->saveOgg("123456789", "output.ogg");
#endif
#ifdef ENABLE_MP3
  recorder->saveMp3("123456789", "output.mp3");
#endif
  delete(recorder);

  cameron = new Ekho("Cantonese");
  cameron->setSpeed(50);
  cameron->setPitch(50);
  cameron->setRate(10);
  cameron->blockSpeak("123456789");
  printf("current speed delta (-50 .. 100): %d\n", cameron->getSpeed());
  printf("current pitch delta (-100 .. 100): %d\n", cameron->getPitch());
  printf("current rate delta (-50 .. 100): %d\n", cameron->getRate());
  delete(cameron);

  Ekho *yali = new Ekho("Mandarin");
  yali->blockSpeak("123456789");
  delete(yali);

  */

  return 0;
}
