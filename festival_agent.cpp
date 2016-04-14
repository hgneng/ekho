/***************************************************************************
 *   Copyright (C) 2008 by Cameron Wong                                    *
 *   email: hgneng at gmail.com                                            *
 *   website: http://www.eguidedog.net                                     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the Creative Commons GNU GPL.                   *
 *                                                                         *
 *   To get Human-Readable description of this licese,                     *
 *   please refer http://creativecommons.org/licenses/GPL/2.0/             *
 *                                                                         *
 *   To get Commons Deed Lawyer-Readable description of this license,      *
 *   please refer http://www.gnu.org/licenses/old-licenses/gpl-2.0.html    *
 *                                                                         *
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "config.h"
#include "festival_agent.h"

#ifdef ENABLE_WINDOWS
#include <winsock2.h>
#include <windows.h>
#define sleep(seconds) Sleep((seconds)*1000)
#else
#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#endif

#define FA_PORT 1314
#define FA_HOST "localhost"
#define FA_MAX_TEXT_LEN 1024
#define FA_DATA_BUF_LEN 1024

static int client_fd_g;

static int fa_start_server(void) {
#ifdef ENABLE_WINDOWS
/** eSpeak in Windows create a process cmd if you use system, exec, ... **/ 
/** You must use COM to process ... **/ 
 
//$WshShell = new COM("WScript.Shell"); 
//$output = $WshShell->Run("$cmd", 0, false); 

  return system("start festival.exe --server --libdir lib");
#else
  return system("festival --server &");
#endif
}

static int fa_stop_server(void) {
#ifdef ENABLE_WINDOWS
  system("taskkill /IM festival.exe");
#endif
  return 0;
}

static int fa_start_client(void) {
#ifdef ENABLE_WINDOWS	
    WSADATA wsaData;   // if this doesn't work
    //WSAData wsaData; // then try this instead
 
    if (WSAStartup(MAKEWORD(1, 1), &wsaData) != 0) {
        fprintf(stderr, "WSAStartup failed.\n");
        exit(1);
    }
#endif  

  struct hostent *he;
  struct sockaddr_in their_addr; // connector's address information

  if ((he = gethostbyname(FA_HOST)) == NULL) {  // get the host info 
    fprintf(stderr, "gethostbyname error\n");
    exit(1);
  }

  if ((client_fd_g = socket(PF_INET, SOCK_STREAM, 0)) == -1) {
    perror("socket");
    exit(1);
  }

  their_addr.sin_family = AF_INET;    // host byte order 
  their_addr.sin_port = htons(FA_PORT);  // short, network byte order 
  their_addr.sin_addr = *((struct in_addr *)he->h_addr);
  memset(their_addr.sin_zero, '\0', sizeof their_addr.sin_zero);

  /* connect socket, retry 3 times */
  if (connect(client_fd_g, (struct sockaddr *)&their_addr, sizeof their_addr) == -1) {
    sleep(1);
    if (connect(client_fd_g, (struct sockaddr *)&their_addr, sizeof their_addr) == -1) {
      sleep(1);
      if (connect(client_fd_g, (struct sockaddr *)&their_addr, sizeof their_addr) == -1) {
        perror("connect");
        exit(1);
      }
    }
  }

  return 0;
}

static int fa_stop_client(void) {
  close(client_fd_g);
  fa_stop_server();
  return 0;
}

int fa_start(void) {
  if (not fa_start_server()) {
    fa_start_client();
    return 0;
  } else {
    return -1;
  }
}

int fa_stop(void) {
  return fa_stop_client();
}

/* deprecated
int fa_set_samplerate(int rate) {
  int numbytes;  
  char buf[FA_DATA_BUF_LEN];
//  char cmd[256 + FA_MAX_TEXT_LEN]; // 256 may not big enough, a potential bug.
  char *cmd = "(Parameter.set 'Audio_Required_Rate 44100)\n";

  printf("send command: %s\n", cmd);
  int n = send(client_fd_g, cmd, strlen(cmd), 0);
  printf("%d bytes sent\n", n);

  if ((numbytes = recv(client_fd_g, buf, FA_DATA_BUF_LEN - 1, 0)) == -1) {
    perror("recv");
    exit(1);
  }

  buf[numbytes] = '\0';
  printf("Received: %s\n",buf);

  if ((numbytes = recv(client_fd_g, buf, FA_DATA_BUF_LEN - 1, 0)) == -1) {
    perror("recv");
    exit(1);
  }

  buf[numbytes] = '\0';
  printf("Received: %s\n",buf);

  return 0;  
}
*/

int fa_speak(const char *text)
{
  int numbytes;  
  char buf[FA_DATA_BUF_LEN];
  char cmd[256 + FA_MAX_TEXT_LEN]; // 256 may not big enough, a potential bug.

  /* generate Festival command */
  if (strlen(text) > FA_MAX_TEXT_LEN) {
    fprintf(stderr, "You speak a text with length more than %d\n", FA_MAX_TEXT_LEN);
    return -1;
  }
  cmd[0] = 0;
  strcat(cmd, "(SayText \"");
  const char *cmd_end = "\")\n";
  char *textendp = cmd + 256 - strlen(cmd_end) + FA_MAX_TEXT_LEN;
  char *cmdp = cmd + strlen(cmd);
  const char *textp = text;
  /* also, there is a bug here if too many \ ". Some data will be droped */
  while (*textp && cmdp < textendp) {
    if (*textp == '\\') {
      *cmdp++ = '\\';
      *cmdp++ = '\\';
    } else if (*textp == '"') {
      *cmdp++ = '\\';
      *cmdp++ = '"';
    } else {
      *cmdp++ = *textp;
    }
    textp++;
  }
  *cmdp = 0;
  strcat(cmd, cmd_end);

//  printf("send command: %s\n", cmd);
  send(client_fd_g, cmd, strlen(cmd), 0);

  if ((numbytes = recv(client_fd_g, buf, FA_DATA_BUF_LEN - 1, 0)) == -1) {
    perror("recv");
    exit(1);
  }

  buf[numbytes] = '\0';
  printf("Received: %s\n",buf);

  if ((numbytes = recv(client_fd_g, buf, FA_DATA_BUF_LEN - 1, 0)) == -1) {
    perror("recv");
    exit(1);
  }

//  buf[numbytes] = '\0';
//  printf("Received: %s\n",buf);

  return 0;
}

/* get wave data
 * Parameters:
 *  text - string to speak
 *  buffer
 *  size - size of buffer
 *
 * Return:
 *  0 in normal case
 *  1 if wav data is not finish read because buffer is not enough. Then we should call fa_continue_get_wav.
 */
int fa_get_wav(const char *text, char *buffer, long *size) {
  char cmd[256 + FA_MAX_TEXT_LEN]; // 256 may not big enough, a potential bug.
  long max_size = *size;

  /* generate Festival command */
  if (strlen(text) > FA_MAX_TEXT_LEN) {
    fprintf(stderr, "You speak a text with length more than %d\n", FA_MAX_TEXT_LEN);
    return -1;
  }
  cmd[0] = 0;
  //(Parameter.set 'Wavefiletype 'nist)\n
  strcat(cmd, "(tts_textall \"");
  const char *cmd_end = "\" \"fundamental\")";
//  char *cmd_end= "\" 'file)";
  char *textendp = cmd + 256 - strlen(cmd_end) + FA_MAX_TEXT_LEN;
  char *cmdp = cmd + strlen(cmd);
  const char *textp = text;
  /* also, there is a bug here if too many \ ". Some data will be droped */
  while (*textp && cmdp < textendp) {
    if (*textp == '\\') {
      *cmdp++ = '\\';
      *cmdp++ = '\\';
    } else if (*textp == '"') {
      *cmdp++ = '\\';
      *cmdp++ = '"';
    } else {
      *cmdp++ = *textp;
    }
    textp++;
  }
  *cmdp = 0;
  strcat(cmd, cmd_end);

#ifdef ENABLE_DEBUG
  fflush(stdout);
  printf("send command: %s\n", cmd);
#endif
  send(client_fd_g, cmd, strlen(cmd), 0);

  // get wave
  do {
    if ((*size = recv(client_fd_g, buffer, 3, 0)) == -1) {
      perror("recv");
      exit(1);
    }
#ifdef ENABLE_DEBUG
    buffer[*size] = '\0';
    printf("Received: %s\n",buffer);
#endif
  } while (buffer[0] != 'W' || buffer[1] != 'V');

  *size = max_size;
  int result = fa_continue_get_wav(buffer, size);

  return result;
}

int fa_continue_get_wav(char *buffer, long *size) {
  static const char *file_stuff_key = "ft_StUfF_key"; /* must == Festival's key */
  long max_size = *size;
  int n = 0;
  int k,i;
  char c;
  *size = 0;
  int result;

  /* 14 = the next even number of 13 (length of file_stuff_key) */
  for (k = 0; file_stuff_key[k] != '\0' && (k > 0 || *size + 14 < max_size);) {
#ifdef ENABLE_WINDOWS
    n = recv(client_fd_g, &c, 1, 0);
#else
    n = read(client_fd_g, &c, 1);
#endif
    if (n == 0) {
      break; /* hit stream eof before end of file */
    } else if (n < 0) {
      //fprintf(stderr, "Fail to read from Festival server at size = %d.\n", *size);
      perror("read");
      exit(1);
    } else if (file_stuff_key[k] == c) {
      k++;
    } else if ((c == 'X') && (file_stuff_key[k+1] == '\0')) {   /* It looked like the key but wasn't */
      for (i = 0; i < k; i++,(*size)++) {
        buffer[*size] = file_stuff_key[i];
      }
      k = 0;
      /* omit the stuffed 'X' */
    } else {
      for (i = 0; i < k; i++,(*size)++) {
        buffer[*size] = file_stuff_key[i];
      }
      k = 0;
      buffer[*size] = c;
      (*size)++;
    }
  }

  if (file_stuff_key[k] == 0) {
    result = 0;
  } else {
    result = n; /* 0 or 1 */
  }

#ifdef ENABLE_DEBUG
  printf("Total received %d, result %d\n", *size, result);
#endif

  return result;
}

