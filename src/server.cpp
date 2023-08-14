#include <string>
#include <stdlib.h>
#include <sndfile.h>
#include <pthread.h>
#include <unistd.h>
#include <fstream>
#include <iostream>
#include <signal.h>
#include "config.h"

#ifdef ENABLE_WIN32
#include <windows.h>
#include <winsock2.h>
/* We need the following two to set stdin/stdout to binary */
#include <fcntl.h>
#include <io.h>
#define sleep(seconds) Sleep((seconds)*1000)
#else
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#endif

#include "ekho_impl.h"
using namespace std;

namespace ekho {
int EkhoImpl::startServer(int port) {
  int sockfd, clientFd;        // listen on sock_fd, new connection on clientFd
  struct sockaddr_in my_addr;  // my address information
  struct sockaddr_in their_addr;  // connector's address information
  unsigned int sin_size;
  //  struct sigaction sa;
  const char yes = 1;
  int numbytes;
  char buffer[BUFFER_SIZE];
  mPort = port;

#ifdef ENABLE_WIN32
  WSADATA wsaData;  // if this doesn't work
  // WSAData wsaData; // then try this instead

  if (WSAStartup(MAKEWORD(1, 1), &wsaData) != 0) {
    fprintf(stderr, "WSAStartup failed.\n");
    exit(1);
  }
#endif

  if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    perror("socket");
    exit(1);
  }

  if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1) {
    perror("setsockopt");
    exit(1);
  }

  my_addr.sin_family = AF_INET;          // host byte order
  my_addr.sin_port = htons(port);        // short, network byte order
  my_addr.sin_addr.s_addr = INADDR_ANY;  // automatically fill with my IP
  memset(my_addr.sin_zero, '\0', sizeof my_addr.sin_zero);

  if (::bind(sockfd, (struct sockaddr *)&my_addr, sizeof my_addr) == -1) {
    perror("bind");
    exit(1);
  }
  if (listen(sockfd, MAX_CLIENTS) == -1) {
    perror("listen");
    exit(1);
  }

#ifndef ENABLE_WIN32
  // disable SIGPIPE
  struct sigaction act, oact;
  act.sa_handler = SIG_IGN;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  act.sa_flags |= SA_RESTART;
  if (sigaction(SIGPIPE, &act, &oact) < 0) {
    fprintf(stderr, "sigaction fail!\n");
  }
#endif

  while (1) {  // main accept() loop
    sin_size = sizeof their_addr;
#ifdef ENABLE_WIN32
    if ((clientFd = accept(sockfd, (struct sockaddr *)&their_addr,
                           (int *)&sin_size)) == -1) {
#else
    if ((clientFd = accept(sockfd, (struct sockaddr *)&their_addr,
                           (socklen_t *)&sin_size)) == -1) {
#endif
      perror("accept");
      continue;
    }

    if (EkhoImpl::mDebug) {
      cerr << "got connection from " << inet_ntoa(their_addr.sin_addr) << endl;
    }

    // process request
    if ((numbytes = recv(clientFd, buffer, BUFFER_SIZE - 1, 0)) == -1) {
      cerr << "Fail to receive request" << endl;
    }
    buffer[numbytes] = 0;

    string tmpfile;

    if (buffer[0] == SAVEOGG) {
      // get audio data in OGG format
      if (EkhoImpl::mDebug) {
        cerr << "cmd=SAVEOGG, speedDelta=" << buffer[1]
             << ", pitchDelta=" << buffer[2] << ", volumeDelta=" << buffer[3]
             << ", text=" << buffer + 4 << endl;
      }
      this->setSpeed(buffer[1]);
      this->setPitch(buffer[2]);
      this->setVolume(buffer[3]);
      tmpfile = Audio::genTempFilename() + ".ogg";
      this->saveOgg(buffer + 4, tmpfile);
    } else if (buffer[0] == GETPHONSYMBOLS) {
      // get phonetic symbos of text
      if (EkhoImpl::mDebug) {
        cerr << "cmd=GETPHONSYMBOLS, text=" << buffer + 1 << endl;
      }
      tmpfile = Audio::genTempFilename() + ".sym";
      list<PhoneticSymbol *> phons = mDict.lookup(buffer + 1);
      ofstream fs;
      fs.open(tmpfile.c_str());
      for (list<PhoneticSymbol *>::iterator li = phons.begin();
           li != phons.end(); ++li) {
        fs << (*li)->symbol << " ";
      }
      fs.close();
    } else {
      // get audio data in MP3 format (default)
      if (EkhoImpl::mDebug) {
        cerr << "cmd=SAVEMP3, speedDelta=" << buffer[1]
             << ", pitchDelta=" << buffer[2] << ", volumeDelta=" << buffer[3]
             << ", text=" << buffer + 4 << endl;
      }
      this->setSpeed(buffer[1]);
      this->setPitch(buffer[2]);
      this->setVolume(buffer[3]);
      tmpfile = Audio::genTempFilename() + ".mp3";
#ifdef HAVE_MP3LAME
      this->saveMp3(buffer + 4, tmpfile);
#endif
    }

    FILE *tmpf = fopen(tmpfile.c_str(), "rb");

    if (tmpf) {
      int size = 0;
      int total_size = 0;
      do {
        size = fread(buffer, 1, BUFFER_SIZE, tmpf);
        if (size < 0) {
          cerr << "Fail to read " << tmpfile << " at line" << __LINE__ << endl;
          break;
        }
        if (send(clientFd, buffer, size, 0) == -1) {
          cerr << "Fail to send " << tmpfile << " to client at line "
               << __LINE__ << endl;
          break;
        }
        total_size += size;
      } while (size == static_cast<size_t>(BUFFER_SIZE));

      fclose(tmpf);

      if (EkhoImpl::mDebug) {
        cerr << total_size << " bytes sent." << endl;
      }
    } else {
      cerr << "Fail to open " << tmpfile << endl;
    }

    close(clientFd);
    if (EkhoImpl::mDebug) {
      cerr << "close connection from " << inet_ntoa(their_addr.sin_addr)
           << endl;
    }

    remove(tmpfile.c_str());
  }

  close(sockfd);  // This will never be executed
  return 0;
}

// the first byte is tempo(speed) delta
int EkhoImpl::request(string ip, int port, Command cmd, string text,
                      string outfile) {
#ifdef ENABLE_WIN32
  WSADATA wsaData;  // if this doesn't work
  // WSAData wsaData; // then try this instead

  if (WSAStartup(MAKEWORD(1, 1), &wsaData) != 0) {
    fprintf(stderr, "WSAStartup failed.\n");
    exit(1);
  }
#endif

  int sockfd;
  long numbytes;
  char buf[BUFFER_SIZE];
  struct hostent *he;
  struct sockaddr_in their_addr;  // connector's address information

  if ((he = gethostbyname(ip.c_str())) == NULL) {  // get the host info
    fprintf(stderr, "gethostbyname error\n");
    exit(1);
  }

  if ((sockfd = socket(PF_INET, SOCK_STREAM, 0)) == -1) {
    perror("socket");
    exit(1);
  }

  their_addr.sin_family = AF_INET;    // host byte order
  their_addr.sin_port = htons(port);  // short, network byte order
  their_addr.sin_addr = *((struct in_addr *)he->h_addr);
  memset(their_addr.sin_zero, 0, sizeof their_addr.sin_zero);

  // connect socket, retry 3 times
  if (connect(sockfd, (struct sockaddr *)&their_addr, sizeof their_addr) ==
      -1) {
    sleep(1);
    if (connect(sockfd, (struct sockaddr *)&their_addr, sizeof their_addr) ==
        -1) {
      sleep(1);
      if (connect(sockfd, (struct sockaddr *)&their_addr, sizeof their_addr) ==
          -1) {
        perror("connect");
        exit(1);
      }
    }
  }

  // set data
  char *data;
  if (cmd == GETPHONSYMBOLS) {
    data = new char[text.size() + 2];
    data[0] = cmd;
    strcpy(data + 1, text.c_str());
  } else {
    data = new char[text.size() + 5];
    data[0] = cmd;
    data[1] = (char)this->tempoDelta;
    data[2] = (char)this->pitchDelta;
    data[3] = (char)this->volumeDelta;
    strcpy(data + 4, text.c_str());
  }

  // send text
  if (send(sockfd, data, text.size() + 4, 0) == -1) {
    fprintf(stderr, "Fail to send %s\n", text.c_str());
  }

  if (EkhoImpl::mDebug) {
    cerr << "Receiving " << outfile << "..." << endl;
  }

  size_t total_size = 0;
  FILE *mp3 = fopen(outfile.c_str(), "wb");
  if (!mp3) {
    cerr << "Fail to open file " << outfile << endl;
    close(sockfd);
    return -1;
  }

  do {
    if ((numbytes = recv(sockfd, buf, BUFFER_SIZE, 0)) == -1) {
      cerr << "Fail to receive " << outfile << " at line " << __LINE__ << endl;
      break;
    }
    size_t size = fwrite(buf, 1, numbytes, mp3);
    if (size != static_cast<size_t>(numbytes)) {
      cerr << "Fail to write " << outfile << "(" << numbytes << " -> " << size
           << ")" << endl;
      total_size += numbytes;
      break;
    }
    total_size += numbytes;
  } while (numbytes == BUFFER_SIZE);
  fclose(mp3);

  close(sockfd);

  if (EkhoImpl::mDebug) {
    cerr << total_size << " bytes received" << endl;
  }

  delete[] data;

  return 0;
}
}
