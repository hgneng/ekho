/***************************************************************************
 * Copyright (C) 2008-2024 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: https://eguidedog.net                                       *
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
#include <dirent.h>
#include <dirent.h>
#include <getopt.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <iostream>
#include <fstream>
#include "config.h"

#ifdef ENABLE_WINDOWS
#include <windows.h>
#define sleep(seconds) Sleep((seconds)*1000)
#endif

#include "ekho.h"

using namespace ekho;

static Ekho *ekho_g = NULL;
static bool isDebugging = false;

//#include "gtk2.cpp"

static void show_help(void) {
  printf("\
Ekho text-to-speech engine.\n\
Version: %s\n\n\
Syntax: ekho [option] [text]\n\
-v, --voice=VOICE\n\
        Specified language or voice. ('Cantonese', 'Mandarin', 'Toisanese', 'Hakka', 'Tibetan', 'Ngangien' and 'Hangul' are available now. Mandarin is the default language.)\n\
--EmotiVoice\n\
        Use EmotiVoice to sythesize Mandarin.\n\
-l, --symbol\n\
        List phonetic symbol of text. Characters' symbols are splited by space.\n\
-f, --file=FILE\n\
        Speak text file. ('-' for stdin)\n\
-o, --output=FILE\n\
        Output to file.\n\
-t, --type=OUTPUT_TYPE\n\
        Output type: wav(default), ogg or mp3\n\
-p, --pitch=PITCH_DELTA\n\
        Set delta pitch. Value range from -100 to 100 (percent)\n\
-a, --volume=VOLUME_DELTA\n\
        Set delta volume. Value range from -100 to 100 (percent)\n\
-s, --speed=SPEED\n\
        Set delta speed. Value range from -50 to 300 (percent)\n\
--english-speed=SPEED\n\
        Set English delta speed. Value range from -50 to 150 (percent)\n\
--samplerate=SAMPLE_RATE\n\
        Usually SAMPLE_RATE should be at least 8000. 44100 is CD quality.\n\
--channels=CHANNELS\n\
        CHANNELS should be 1 or 2.\n\
--server\n\
        Start Ekho TTS server.\n\
--request=TEXT\n\
        Send request to Ekho TTS server.\n\
--port\n\
        Set server port. Default is 2046.\n\
--version\n\
        Show version number.\n\
-h, --help\n\
        Display this help message.\n\n\
Please report bugs to Cameron Wong (hgneng at gmail.com)\n",
      PACKAGE_VERSION);
}

static int read_textfile(const char *filename, char **text) {
  FILE *fp = fopen(filename, "r");
  if (!fp) {
    fprintf(stderr, "Fail to open file %s\n", filename);
    exit(-1);
  }
  struct stat file_stat;
  if (fstat(fileno(fp), &file_stat)) {
    fprintf(stderr, "stat %s fail\n", filename);
    exit(-1);
  }
  *text = (char *)realloc(*text, file_stat.st_size);
  if (!text) {
    fprintf(stderr, "malloc(%d) to wtext fail.\n", (int)file_stat.st_size);
    exit(-1);
  }
  size_t size = fread(*text, 1, file_stat.st_size, fp);
  if (size != (size_t)file_stat.st_size) {
    fprintf(stderr, "%d read, supposed to read %d", (int)size,
            (int)file_stat.st_size);
  }
  fclose(fp);
  return 0;
}

// Argument: text - for output
static bool read_stdin(char **text) {
  int c;
  int size = 0;
  int buffer_size = 256;
  char *buffer = (char *)malloc(buffer_size);
  buffer[0] = 0;

/*
  while ((c = fgetc(stdin)) != EOF) {
    size++;
    if (size > buffer_size) {
      buffer_size *= 2;
      buffer = (char *)realloc(buffer, buffer_size);
    }
    buffer[size - 1] = c;
  }*/

  for (std::string line; std::getline(std::cin, line);) {
    //cerr << "length=" << line.length() << ", size=" << line.size() << endl;
    size += line.length();
    
    while (size >= buffer_size) {
      buffer_size *= 2;
    }

    //printf("src addr: %p, strlen:%d\n", buffer, strlen(buffer));
    buffer = (char *)realloc(buffer, buffer_size);
    //printf("dest addr: %p, strlen:%d\n", buffer, strlen(buffer));
    if (!buffer) { 
      cerr << "fail to realloc" << endl;
    }

    //cerr << "size=" << size << ", buffer_size=" << buffer_size << ", strlen=" << strlen(line.c_str()) << endl;
    strcat(buffer, line.c_str());
  }
  buffer[size] = 0;

  if (isDebugging) {
    ofstream myfile;
    myfile.open("/tmp/ekho.debug");
    myfile << "text size is " << size << ", buffer size is " << buffer_size << endl;
    myfile << buffer << endl;
    myfile.close();
    cerr << "finish reading" << endl;
  }

  if (*text) free(*text);
  *text = buffer;

  return size > 0;
}

int main(int argc, char *argv[]) {
  struct option opts[] = {{"help", 0, NULL, 'h'},
                          {"gui", 0, NULL, 'g'},
                          {"voice", 1, NULL, 'v'},
                          {"file", 1, NULL, 'f'},
                          {"output", 1, NULL, 'o'},
                          {"type", 1, NULL, 't'},
                          {"pitch", 1, NULL, 'p'},
                          {"volume", 1, NULL, 'a'},
                          {"rate", 1, NULL, 'r'},
                          {"speed", 1, NULL, 's'},
                          {"english-speed", 1, NULL, 'i'},
                          {"english-voice", 1, NULL, 'u'},
                          {"samplerate", 1, NULL, 'j'},
                          {"channels", 1, NULL, 'k'},
                          {"port", 1, NULL, '1'},
                          {"EmotiVoice", 0, NULL, 'm'},
                          {"overlap", 1, NULL, 'c'},
                          {"server", 0, NULL, 'e'},
                          {"request", 1, NULL, 'q'},
                          {"symbol", 0, NULL, 'l'},
                          {"debug", 0, NULL, 'd'},
                          {"sing", 1, NULL, 'b'},
                          {"version", 0, NULL, 'n'},
                          {NULL, 0, NULL, 0}};
  /* set locale to zh_CN.UTF-8 */
  //  setlocale(LC_ALL, "zh_CN.UTF-8");

  /* get arguments */
  int opt;
  int optidx;
  string language = "Mandarin";
#define NORMAL_MODE 0
#define SERVER_MODE 2
#define REQUEST_MODE 3
#define LIST_MODE 4
#define SING_MODE 5

  int mode = NORMAL_MODE;
  int text_buffer_size = 256;
  char* text = (char *)malloc(text_buffer_size);
  text[0] = 0;
  const char* text_filename = NULL;
  const char* save_filename = NULL;
  char* save_type = NULL;
  int pitch_delta = 0;
  int volume_delta = 0;
  int rate_delta = 0;
  int tempo_delta = 0;
  int english_speed_delta = 0;
  const char* english_voice = NULL;
  int sample_rate = 0;
  int channels = 1;
  int overlap = 2048;
  extern char* optarg;
  extern int optind, optopt;
  bool is_listing_symbols = false;
  bool is_listing_word = false;
  int server_port = 2046;
  bool useEmotiVoice = false;

  while ((opt = getopt_long(argc, argv, ":i:b:hgmv:n:f:o:t:p:r:a:s:eq:lwd1:",
                            opts, &optidx)) != -1) {
    switch (opt) {
      case 'c':
        overlap = atoi(optarg);
        break;
      case 'd':
        isDebugging = true;
        break;
      case 'l':
        is_listing_symbols = true;
        break;
      case 'w':
        is_listing_word = true;
        break;
      case 'h':
        show_help();
        return 0;
      case 'v':
        language = optarg;
        break;
      case 'b':
        mode = SING_MODE;
        break;
      case 'f':
        text_filename = optarg;
        if (!text_filename) {
          cerr << "no filename" << endl;
          show_help();
          exit(-1);
        }
        break;
      case 'o':
        save_filename = optarg;
        if (!save_filename) {
          show_help();
          exit(-1);
        }
        break;
      case 't':
        save_type = optarg;
        if (!save_type) {
          show_help();
          exit(-1);
        }
        break;
      case 'p':
        pitch_delta = atof(optarg);
        break;
      case 'r':
        rate_delta = atoi(optarg);
        break;
      case 's':
        tempo_delta = atoi(optarg);
        break;
      case 'j':
        sample_rate = atoi(optarg);
        break;
      case 'k':
        channels = atoi(optarg);
        if (channels < 1 || channels > 2) {
          cerr << "only support 1 or 2 channels." << endl;
          return -1;
        }
        break;
      case 'i':
        english_speed_delta = atoi(optarg);
        break;
      case 'u':
        english_voice = optarg;
        break;
      case 'm':
        useEmotiVoice = true;
        break;
      case 'a':
        volume_delta = atoi(optarg);
        break;
      case 'e':
        mode = SERVER_MODE;
        break;
      case '1':
        server_port = atoi(optarg);
        break;
      case 'q':
        mode = REQUEST_MODE;
        if (text_buffer_size < strlen(optarg) + 1) {
          do {
            text_buffer_size *= 2;
          } while (text_buffer_size < strlen(optarg) + 1);
          text = (char *)realloc(text, text_buffer_size);
        }

        strcpy(text, optarg);
        break;
      case 'g':
        /* xmain seems not thread safe. don't specify other option is -x is
         * specified because argv seems be free */
        //xmain(&argc, &argv);
        return 0;
      case 'n':
        printf("%s\n", PACKAGE_VERSION);
        return 0;
      case '?':
        fprintf(stderr, "Invalid option: -%c\n", optopt);
        return -1;
    }
  }

  if (text_filename) {
    // input from file
    if (strcmp(text_filename, "-") == 0) {
      read_stdin(&text);
    } else {
      read_textfile(text_filename, &text);
    }
  } else {
    // get text to synthesize
    bool is_first_text = true;
    for (; optind < argc; optind++) {
      if (access(argv[optind], R_OK)) {
        if (strlen(text) + strlen(argv[optind]) + 2 > text_buffer_size) {
          do {
            text_buffer_size *= 2;
          } while (strlen(text) + strlen(argv[optind]) + 2 > text_buffer_size);
          text = (char *)realloc(text, text_buffer_size);
        }
        if (is_first_text)
          is_first_text = false;
        else
          strcat(text, " ");
        strcat(text, argv[optind]);
      }
    }
  }

  Ekho::debug(isDebugging);

  if (mode != REQUEST_MODE && is_listing_symbols) {
    // output phonetic symbols
    Language lang = ENGLISH;
    if (language.compare("Cantonese") == 0) {
      lang = CANTONESE;
    } else if (language.compare("Mandarin") == 0) {
      lang = MANDARIN;
    } else if (language.compare("Korean") == 0) {
      lang = KOREAN;
    } else if (language.compare("Toisanese") == 0) {
      lang = TOISANESE;
    } else if (language.compare("Hakka") == 0) {
      lang = HAKKA;
    } else if (language.compare("Tibetan") == 0) {
      lang = TIBETAN;
    } else if (language.compare("Ngangien") == 0) {
      lang = NGANGIEN;
    }
    Dict dict(lang);

    list<Word> wordlist = Word::split(text);
    for (list<Word>::iterator word = wordlist.begin(); word != wordlist.end();
         word++) {
      list<PhoneticSymbol *> phonList = word->symbols;

      list<PhoneticSymbol *>::iterator phonItor = phonList.begin();
      for (; phonItor != phonList.end(); ++phonItor) {
        cout << (*phonItor)->symbol << " ";
      }
    }

    cout << endl;
  } else if (mode != REQUEST_MODE && is_listing_word) {
    // output word list for debug
    Dict dict(MANDARIN);
    list<Word> word_list = Word::split(text);
    list<Word>::iterator word = word_list.begin();
    for (; word != word_list.end(); word++) {
      cout << word->text;
      cout << "/";
    }
  } else if (mode == SERVER_MODE) {
    // start as server
    ekho_g = new Ekho(language);
    ekho_g->setOverlap(overlap);
    ekho_g->setEnglishSpeed(english_speed_delta);
    if (english_voice) {
      ekho_g->setEnglishVoice(english_voice);
    }
    if (useEmotiVoice) {
      ekho_g->enableEmotiVoice();
    }
    ekho_g->startServer(server_port);
  } else if (mode == REQUEST_MODE) {
    // request to a server
    ekho_g = new Ekho();
    ekho_g->setSpeed(tempo_delta);
    //ekho_g->setEnglishSpeed(english_speed_delta);
    ekho_g->setOverlap(overlap);
    ekho_g->setPitch(pitch_delta);
    ekho_g->setVolume(volume_delta);

    Command cmd;
    if (is_listing_symbols) {
      cmd = GETPHONSYMBOLS;
    } else if (save_type && strcmp(save_type, "ogg") == 0) {
      cmd = SAVEOGG;
    } else {
      cmd = SAVEMP3;
    }

    if (!save_filename) {
      if (cmd == GETPHONSYMBOLS) {
        save_filename = "output.sym";
      } else if (cmd == SAVEOGG) {
        save_filename = "output.ogg";
      } else {
        save_filename = "output.mp3";
      }
    }

    ekho_g->request("127.0.0.1", server_port, cmd, text, save_filename);
  } else if (mode == SING_MODE) {
    // sing song (experimental)
    ekho_g = new Ekho(language);
    string saveFilename = "";
    if (save_filename) {
      saveFilename = string(save_filename);
    }
    ekho_g->singMusicXml(text_filename, saveFilename);
    delete ekho_g;
    ekho_g = 0;
  } else {
    // main synthesize
    ekho_g = new Ekho();
    ekho_g->setSampleRate(sample_rate);
    ekho_g->setChannels(channels);
    if (ekho_g->setVoice(language) != 0) {
      return -1;
    }
    ekho_g->setEnglishSpeed(english_speed_delta);
    if (english_voice) {
      ekho_g->setEnglishVoice(english_voice);
    }
    ekho_g->setPitch(pitch_delta);
    ekho_g->setSpeed(tempo_delta);
    ekho_g->setOverlap(overlap);
    ekho_g->setVolume(volume_delta);
    ekho_g->setRate(rate_delta);
    if (useEmotiVoice) {
      ekho_g->enableEmotiVoice();
    }

    if (save_filename) {
      // output to file
      if (save_type && strcmp(save_type, "ogg") == 0) {
        ekho_g->saveOgg(text, save_filename);
      } else if (save_type && strcmp(save_type, "mp3") == 0) {
#ifdef HAVE_MP3LAME
        ekho_g->saveMp3(text, save_filename);
#else
        printf("mp3 type is not supported!\n");
#endif
      } else {
        ekho_g->saveWav(text, save_filename);
      }
    } else if (strlen(text) > 0) {
      // synthesize through speaker
      ekho_g->blockSpeak(text);
    } else {
      show_help();
    }

    delete (ekho_g);
    ekho_g = NULL;
  }

  if (text) free(text);

  return 0;
}
