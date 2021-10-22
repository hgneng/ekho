/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*           Copyright (c) 1999 University of Edinburgh, UK              */
/*                        All Rights Reserved.                           */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*             Author :  Alan W Black (awb@cstr.ed.ac.uk)                */
/*             Date   :  March 1999                                      */
/*             Modified: Hynek Hanke (hanke@brailcom.org) (2003-2005)    */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Client end of Festival server API in C designed specifically formerly */
/* for Galaxy Communicator use, but rewritten to suit Speech Dispatcher  */
/* needs. Please look also at the original festival_client.c library     */
/* that can be found in festival/examples/festival_client.c -- it will   */
/* be probably more up-to-date.                                          */
/*                                                                       */
/* This can be also compiled as a stand-alone program for testing        */
/* purposes:                                                             */
/*    gcc -o festival_client -DSTANDALONE festival_client.c              */
/* and run as:                                                           */
/*    ./festival_client                                                  */
/* This creates a file test.snd, containing the synthesized waveform     */
/* You can run for example:                                              */
/*    play test.snd                                                      */
/* to hear the message                                                   */
/*=======================================================================*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>

#include <glib.h>

/* I'm including my local .h, not the Alan's one! */
#include "festival_client.h"

#include "module_utils.h"

/* For testing endianness */
int fapi_endian_loc = 1;

int festival_connection_crashed;

static char *socket_receive_file_to_buff(int fd, int *size);

/* --- MANAGING FT STRUCTURES --- */

void delete_FT_Wave(FT_Wave * wave)
{
	if (wave != 0) {
		if (wave->samples != 0)
			g_free(wave->samples);
		g_free(wave);
	}
}

int save_FT_Wave_snd(FT_Wave * wave, const char *filename)
{
	FILE *fd;
	struct {
		unsigned int magic;	/* magic number */
		unsigned int hdr_size;	/* size of this header */
		int data_size;	/* length of data (optional) */
		unsigned int encoding;	/* data encoding format */
		unsigned int sample_rate;	/* samples per second */
		unsigned int channels;	/* number of interleaved channels */
	} header;
	short sw_short;
	int i;

	if ((filename == 0) ||
	    (strcmp(filename, "stdout") == 0) || (strcmp(filename, "-") == 0))
		fd = stdout;
	else if ((fd = fopen(filename, "wb")) == NULL) {
		fprintf(stderr,
			"save_FT_Wave: can't open file \"%s\" for writing\n",
			filename);
		return -1;
	}

	header.magic = (unsigned int)0x2e736e64;
	header.hdr_size = sizeof(header);
	header.data_size = 2 * wave->num_samples;
	header.encoding = 3;	/* short */
	header.sample_rate = wave->sample_rate;
	header.channels = 1;
	if (FAPI_LITTLE_ENDIAN) {
		/* snd is always sparc/68000 byte order */
		header.magic = SWAPINT(header.magic);
		header.hdr_size = SWAPINT(header.hdr_size);
		header.data_size = SWAPINT(header.data_size);
		header.encoding = SWAPINT(header.encoding);
		header.sample_rate = SWAPINT(header.sample_rate);
		header.channels = SWAPINT(header.channels);
	}

	/* write header */
	if (fwrite(&header, sizeof(header), 1, fd) != 1) {
		if (fd != stdout)
			fclose(fd);
		return -1;
	}
	if (FAPI_BIG_ENDIAN)
		fwrite(wave->samples, sizeof(short), wave->num_samples, fd);
	else {
		/* have to swap */
		for (i = 0; i < wave->num_samples; i++) {
			sw_short = SWAPSHORT(wave->samples[i]);
			fwrite(&sw_short, sizeof(short), 1, fd);
		}
	}

	if (fd != stdout)
		fclose(fd);
	return 0;
}

void delete_FT_Info(FT_Info * info)
{
	if (info != 0)
		g_free(info);
}

/* --- FESTIVAL REPLY PARSING --- */

static int nist_get_param_int(char *hdr, char *field, int def_val)
{
	char *p;
	int val;

	if (((p = strstr(hdr, field)) != NULL) &&
	    (strncmp(" -i ", p + strlen(field), 4) == 0)) {
		sscanf(p + strlen(field) + 4, "%d", &val);
		return val;
	} else
		return def_val;
}

static int nist_require_swap(char *hdr)
{
	char *p;
	char *field = "sample_byte_format";

	if ((p = strstr(hdr, field)) != NULL) {
		if (((strncmp(" -s2 01", p + strlen(field), 7) == 0)
		     && FAPI_BIG_ENDIAN)
		    || ((strncmp(" -s2 10", p + strlen(field), 7) == 0)
			&& FAPI_LITTLE_ENDIAN))
			return 1;
	}
	return 0;		/* if unknown assume native byte order */
}

/* --- FESTIVAL SOCKET MANIPULATION --- */

/* Return an FD to a remote server */
static int festival_socket_open(const char *host, int port)
{
	struct sockaddr_in serv_addr;
	struct hostent *serverhost;
	int fd;

	fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (fd < 0) {
		fprintf(stderr, "festival_client: can't get socket\n");
		return -1;
	}
	memset(&serv_addr, 0, sizeof(serv_addr));
	if ((serv_addr.sin_addr.s_addr = inet_addr(host)) == -1) {
		/* its a name rather than an ipnum */
		serverhost = gethostbyname(host);
		if (serverhost == (struct hostent *)0) {
			fprintf(stderr,
				"festival_client: gethostbyname failed\n");
			close(fd);
			return -1;
		}
		memmove(&serv_addr.sin_addr, serverhost->h_addr,
			serverhost->h_length);
	}
	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(port);

	if (connect(fd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) != 0) {
		fprintf(stderr, "festival_client: connect to server failed\n");
		close(fd);
		return -1;
	}

	return fd;
}

/* Receive file (probably a waveform file) from socket using   */
/* Festival key stuff technique, but long winded I know, sorry */
/* but will receive any file without closing the stream or     */
/* using OOB data                                              */
static char *socket_receive_file_to_buff(int fd, int *size)
{
	static char *file_stuff_key = "ft_StUfF_key";	/* must == Festival's key */
	char *buff;
	int bufflen;
	int n, k, i;
	char c;

	if (fd < 0)
		return NULL;

	bufflen = 1024;
	buff = (char *)g_malloc(bufflen);
	*size = 0;

	for (k = 0; file_stuff_key[k] != '\0';) {
		n = read(fd, &c, 1);
		if (n <= 0) {
			DBG("ERROR: FESTIVAL CLOSED CONNECTION (1)");
			close(fd);
			festival_connection_crashed = 1;
			g_free(buff);
			return NULL;	/* hit stream eof before end of file */
		}

		if ((*size) + k + 1 >= bufflen) {
			/* +1 so you can add a NULL if you want */
			bufflen += bufflen / 4;
			buff = (char *)g_realloc(buff, bufflen);
		}
		if (file_stuff_key[k] == c)
			k++;
		else if ((c == 'X') && (file_stuff_key[k + 1] == '\0')) {
			/* It looked like the key but wasn't */
			for (i = 0; i < k; i++, (*size)++)
				buff[*size] = file_stuff_key[i];
			k = 0;
			/* omit the stuffed 'X' */
		} else {
			for (i = 0; i < k; i++, (*size)++)
				buff[*size] = file_stuff_key[i];
			k = 0;
			buff[*size] = c;
			(*size)++;
		}

	}

	return buff;
}

static char *client_accept_s_expr(int fd)
{
	/* Read s-expression from server, as a char * */
	char *expr;
	int filesize;

	if (fd < 0)
		return NULL;

	expr = socket_receive_file_to_buff(fd, &filesize);
	expr[filesize] = '\0';
	return expr;
}

static FT_Wave *client_accept_waveform(int fd, int *stop_flag,
				       int stop_by_close)
{
	/* Read waveform from server */
	char *wavefile;
	int filesize;
	int num_samples, sample_rate, i;
	FT_Wave *wave;

	if (fd < 0)
		return NULL;

	wavefile = socket_receive_file_to_buff(fd, &filesize);
	if (wavefile == NULL)
		return NULL;

	wave = NULL;

	/* I know this is NIST file and its an error if it isn't */
	if (filesize >= 1024) {
		/* If this doesn't work, probably you forgot to set
		   the output file type to NIST ! by Parameter.set */
		num_samples = nist_get_param_int(wavefile, "sample_count", 1);
		sample_rate =
		    nist_get_param_int(wavefile, "sample_rate", 16000);

		if ((num_samples * sizeof(short)) + 1024 == filesize) {
			wave = (FT_Wave *) g_malloc(sizeof(FT_Wave));
			DBG("Number of samples from festival: %d", num_samples);
			wave->num_samples = num_samples;
			wave->sample_rate = sample_rate;
			if (num_samples != 0) {
				wave->samples =
				    (short *)g_malloc(num_samples *
						      sizeof(short));
				memmove(wave->samples, wavefile + 1024,
					num_samples * sizeof(short));
				if (nist_require_swap(wavefile))
					for (i = 0; i < num_samples; i++)
						wave->samples[i] =
						    SWAPSHORT(wave->samples[i]);
			} else {
				wave->samples = NULL;
			}
		}
	}

	g_free(wavefile);

	return wave;
}

int festival_get_ack(FT_Info ** info, char *ack)
{
	int read_bytes;
	int n;

	if (*info == NULL)
		return -1;
	if ((*info)->server_fd < 0)
		return -1;

	for (n = 0; n < 3;) {
		read_bytes = read((*info)->server_fd, ack + n, 3 - n);
		if (read_bytes <= 0) {
			/* WARNING: This is a very strange situation
			   but it happens often, I don't really know
			   why??? */
			DBG("ERROR: FESTIVAL CLOSED CONNECTION (2)");
			close((*info)->server_fd);
			festival_connection_crashed = 1;
			return -1;
		}
		n += read_bytes;
	}
	ack[3] = '\0';
	return 0;
}

int festival_read_response(FT_Info * info, char **expr)
{
	char buf[4];
	char *r;

	DBG("Com: Reading response");

	if (info == NULL)
		return 1;
	if (info->server_fd < 0)
		return 1;

	if (festival_get_ack(&info, buf))
		return 1;
	buf[3] = 0;

	DBG("<- Festival: |%s|", buf);

	if (!strcmp(buf, "ER\n")) {
		if (expr != NULL)
			*expr = NULL;
		return 1;
	} else {
		if (expr != NULL) {
			*expr = client_accept_s_expr(info->server_fd);
		} else {
			r = client_accept_s_expr(info->server_fd);
			if (r != NULL)
				g_free(r);
		}
	}

	if (festival_get_ack(&info, buf))
		return 1;
	DBG("<- Festival: |%s|", buf);

	return 0;
}

int festival_accept_any_response(FT_Info * info)
{
	char ack[4];
	int r;
	char *expr;

	DBG("Com: Accepting any response");
	do {
		if ((r = festival_get_ack(&info, ack)))
			return r;
		DBG("<- Festival: |%s|", ack);
		if (strcmp(ack, "WV\n") == 0) {	/* receive a waveform */
			client_accept_waveform(info->server_fd, NULL, 0);
		} else if (strcmp(ack, "LP\n") == 0) {	/* receive an s-expr */
			expr = client_accept_s_expr(info->server_fd);
			if (expr != NULL)
				g_free(expr);
		} else if (strcmp(ack, "ER\n") == 0) {	/* server got an error */
			/* This message ER is returned even if it was because there was
			   no sound produced, for this reason, the warning is disabled */
			/* fprintf(stderr,"festival_client: server returned error\n"); */
			break;
		}
	} while (strcmp(ack, "OK\n") != 0);

	return 0;
}

/* --- HELPER FUNCTIONS --- */
int festival_check_info(FT_Info * info, const char *fnname)
{
	assert(fnname != NULL);
	if ((info == NULL) || (info->server_fd == -1)) {
		DBG("%s called with info = NULL or server_fd == -1\n", fnname);
		return -1;
	}
	return 0;
}

/***********************************************************************/
/* Public Functions to this API                                        */
/***********************************************************************/

/* Opens a connection to Festival server (which must be running)
 * and returns it's identification in new FT_Info */
FT_Info *festivalOpen(FT_Info * info)
{
	char *resp;
	int ret;

	DBG("Opening socket fo Festival server");

	festival_connection_crashed = 0;

	if (info == 0)
		info = festivalDefaultInfo();

	info->server_fd =
	    festival_socket_open(info->server_host, info->server_port);

	if (info->server_fd == -1) {
		delete_FT_Info(info);
		festival_connection_crashed = 1;
		return NULL;
	}

	FEST_SEND_CMD("(require 'speech-dispatcher)");
	ret = festival_read_response(info, &resp);
	if (ret || resp == NULL || strcmp(resp, "t\n")) {
		DBG("ERROR: Can't load speech-dispatcher module into Festival."
		    "Reason: %s", resp);
		return NULL;
	}
	g_free(resp);

	FEST_SEND_CMD("(Parameter.set 'Wavefiletype 'nist)\n");
	ret = festival_read_response(info, &resp);
	if (ret || resp == NULL || strcmp(resp, "nist\n")) {
		DBG("ERROR: Can't set Wavefiletype to nist in Festival. Reason: %s", resp);
		return NULL;
	}
	g_free(resp);

	return info;
}

int
festival_speak_command(FT_Info * info, char *command, const char *text,
		       int symbol, int resp)
{
	FILE *fd;
	const char *p;
	char *str;
	int ret;

	if (festival_check_info(info, "festival_speak_command") == -1)
		return -1;
	if (command == NULL)
		return -1;
	if (text == NULL)
		return -1;

	DBG("(festival_speak_command): %s", text);

	/* Opens a stream associated to the socket */
	fd = fdopen(dup(info->server_fd), "wb");

	/* Send the command and data */
	if (symbol == 0)
		str = g_strdup_printf("(%s \"", command);
	else
		str = g_strdup_printf("(%s '", command);
	fputs(str, fd);
	/* Copy text over to server, escaping any quotes */
	for (p = text; p && (*p != '\0'); p++) {
		if ((*p == '"') || (*p == '\\'))
			putc('\\', fd);
		putc(*p, fd);
	}
	if (symbol == 0)
		fprintf(fd, "\")\n");
	else
		fprintf(fd, ")\n");

	DBG("-> Festival: escaped text is %s", text);
	DBG("-> Festival: |%sthe text is displayed above\")|", str);

	g_free(str);
	/* Close the stream (but not the socket) */
	fclose(fd);
	DBG("Resources freed");

	if (resp) {
		ret = festival_read_response(info, NULL);
		if (ret) {
			DBG("ERROR: Festival reported error in speak command);");
			return -1;
		}
	}

	return 0;
}

#define FEST_SPEAK_CMD(name, cmd, symbol, resp) \
	int \
	name(FT_Info *info, const char *text) \
	{ \
		return festival_speak_command(info, cmd, text, symbol, resp); \
	}

/* Sends a TEXT to Festival server for synthesis. Doesn't
 * wait for reply (see festivalStringToWaveGetData()).
 * Please make sure that the socket is empty before calling
 * this command -- otherwise, you would get another message
 * in response.
 * Returns 0 if everything is ok, -1 otherwise.
*/
FEST_SPEAK_CMD(festivalStringToWaveRequest, "speechd-speak-ssml", 0, 1)
    FEST_SPEAK_CMD(festivalSpell, "speechd-spell", 0, 1)
    FEST_SPEAK_CMD(festivalSoundIcon, "speechd-sound-icon", 1, 0)
    FEST_SPEAK_CMD(festivalCharacter, "speechd-character", 0, 0)
    FEST_SPEAK_CMD(festivalKey, "speechd-key", 0, 0)

/* Reads the wavefile sent back after festivalStringToWaveRequest()
 * has been called. This function blocks until all the data is
 * available. Note that for longer messages this can be quite long
 * on some slower machines. */
FT_Wave *festivalStringToWaveGetData(FT_Info * info)
{
	FT_Wave *wave = NULL;
	char ack[5];

	/* Read back info from server */
	/* This assumes only one waveform will come back, also LP is unlikely */
	do {
		if (festival_get_ack(&info, ack))
			return NULL;
		DBG("<- Festival: %s", ack);
		if (strcmp(ack, "WV\n") == 0) {
			wave = client_accept_waveform(info->server_fd, NULL, 0);
		} else if (strcmp(ack, "LP\n") == 0) {
			client_accept_s_expr(info->server_fd);
		} else if (strcmp(ack, "ER\n") == 0) {
			//  fprintf(stderr,"festival_client: server returned error\n");
			break;
		}
	} while (strcmp(ack, "OK\n") != 0);

	return wave;
}

FT_Wave *festivalGetDataMulti(FT_Info * info, char **callback, int *stop_flag,
			      int stop_by_close)
{
	FT_Wave *wave = NULL;
	char ack[5];
	char *resp = NULL;
	FILE *fd;

	if (festival_check_info(info, "festival_speak_command") == -1) {
		return NULL;
	}

	/* Read back info from server */
	/* This assumes only one waveform will come back */
	wave = NULL;

	*callback = NULL;

	DBG("Stop by close mode : %d", stop_by_close);

	DBG("-> Festival: (speechd-next)");
	fd = fdopen(dup(info->server_fd), "wb");
	fprintf(fd, "(speechd-next)\n");
	fflush(fd);
	fclose(fd);

	do {
		if (festival_get_ack(&info, ack)) {
			DBG("Get ack failed");
			return NULL;
		}
		DBG("<- Festival: %s", ack);

		if (strcmp(ack, "WV\n") == 0) {
			wave =
			    client_accept_waveform(info->server_fd, stop_flag,
						   stop_by_close);
		} else if (strcmp(ack, "LP\n") == 0) {
			if (resp != NULL)
				g_free(resp);
			resp = client_accept_s_expr(info->server_fd);
			if (resp == NULL) {
				DBG("ERROR: Something wrong in communication with Festival, s_expr = NULL");
				return NULL;
			}
			if (strlen(resp) != 0)
				resp[strlen(resp) - 1] = 0;
			DBG("<- Festival: |%s|", resp);
			if (!strcmp(resp, "nil")) {
				DBG("festival_client: end of samples\n");
				g_free(resp);
				wave = NULL;
				resp = NULL;
			}
		} else if (strcmp(ack, "ER\n") == 0) {
			DBG("festival_client: server returned error\n");
			return NULL;
		}
	} while (strcmp(ack, "OK\n") != 0);

	if (resp) {
		if ((strlen(resp) > 0) && (resp[0] != '#'))
			*callback = resp;
		else
			g_free(resp);
	}

	return wave;
}

/* Closes the Festival server socket connection */
int festivalClose(FT_Info * info)
{
	if (info == 0)
		return 0;

	if (info->server_fd != -1) {
		FILE *fd;
		fd = fdopen(dup(info->server_fd), "wb");
		if (fd != NULL) {
			fprintf(fd, "(quit)\n");
			fclose(fd);
		}
		close(info->server_fd);
	}

	return 0;
}

/* --- INFORMATION RETRIEVAL COMMANDS --- */

char **lisp_list_get_vect(const char *expr)
{
	size_t len;
	char *helper;
	gchar **vect;
	size_t i, j;

	len = strlen(expr);
	helper = g_malloc(sizeof(char) * (len + 1));

	//Remove parenthesis
	j = 0;
	for (i = 0; i < len; i++) {
		if ((expr[i] != '(') && (expr[i] != ')')) {
			helper[j] = expr[i];
			j++;
		}
	}
	helper[j] = '\0';

	// Split into a vector of atoms
	vect = g_strsplit(helper, " ", 0);

	return vect;
}

char *vect_read_item(char **vect, char *field)
{
	int i;

	for (i = 0;; i++) {
		if (vect[i] == NULL)
			return NULL;
		else {
			if (!strcmp(vect[i], field)) {
				if (vect[i + 1] != NULL)
					return vect[i + 1];
				else
					return NULL;
			}
		}
	}
}

FT_Info *festivalDefaultInfo()
{
	FT_Info *info;
	info = (FT_Info *) g_malloc(sizeof(FT_Info));

	info->server_host = FESTIVAL_DEFAULT_SERVER_HOST;
	info->server_port = FESTIVAL_DEFAULT_SERVER_PORT;
	info->text_mode = FESTIVAL_DEFAULT_TEXT_MODE;

	info->server_fd = -1;

	return info;
}

/* For testing purposes, this library can be compiled as
 * a standalone program, please see the introduction at
 * the beginning  of this file
 */

#ifdef STANDALONE
int main(int argc, char **argv)
{
	char *server = 0;
	int port = -1;
	char *text = 0;
	char *output = 0;
	char *mode = 0;
	int i;
	FT_Info *info;
	FT_Wave *wave;

	printf("Welcome to Festival client library for Speech Dispatcher\n");

	info = festivalDefaultInfo();
	if (server != 0)
		info->server_host = FESTIVAL_DEFAULT_SERVER_HOST;
	if (port != -1)
		info->server_port = FESTIVAL_DEFAULT_SERVER_PORT;
	if (mode != 0)
		info->text_mode = FESTIVAL_DEFAULT_TEXT_MODE;

	info = festivalOpen(info);

	if (info == 0) {
		printf("Can't open connection to festival server!\n");
		exit(1);
	}
	if (info->server_fd <= 0) {
		printf("Can't open connection to festival server!\n");
		exit(1);
	}

	festivalStringToWaveRequest(info,
				    "Hi, how are you? I'm Festival! And who are you?");
	wave = festivalStringToWaveGetData(info);

	if (wave != 0) {
		save_FT_Wave_snd(wave, "test.snd");
	} else {
		printf("Wave null! Festival server doesn't work properly.\n");
		exit(1);
	}
	festivalClose(info);

	printf("The synthesized text should have been written to test.snd\n");

	return 0;
}
#endif
