
#ifndef PARSE_H
 #define PARSE_H

char* parse(const char* buf, const int bytes, const int fd);

char* parse_history(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_set(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_stop(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_cancel(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_pause(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_resume(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_snd_icon(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_char(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_key(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_list(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_get(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_help(const char* buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket);
char* parse_block(const char* buf, const int bytes, const int fd, TSpeechDSock* speechd_socket);

char* deescape_dot(const char *orig_text, size_t orig_len);

/* Function for parsing the input from clients */
char* get_param(const char *buf, const int n, const int bytes, const int lower_case);

/* Other internal functions */
char* parse_general_event(const char *buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket,
			  EMessageType type);
int spd_utf8_read_char(char* pointer, char* character);

#endif
