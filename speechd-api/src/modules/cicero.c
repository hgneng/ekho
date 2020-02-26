/*
 * cicero.c - Speech Dispatcher backend for Cicero French TTS engine
 *
 * Copyright (C) 2006 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * @author: Olivier BERT
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <speechd_types.h>
#include <safe_io.h>
#include <errno.h>
#include <poll.h>
#include <fcntl.h>
#include <langinfo.h>
#include <sys/stat.h>
#include <semaphore.h>

#include "module_utils.h"

#define MODULE_NAME     "cicero"
#define MODULE_VERSION  "0.3"

// #define DEBUG_MODULE 1
DECLARE_DEBUG()

static int initialized = 0;

/* Thread and process control */
static int cicero_speaking = 0;

static pthread_t cicero_speaking_thread;
static sem_t cicero_semaphore;

static char **cicero_message;
static SPDMessageType cicero_message_type;

static int cicero_position = 0;
static int cicero_pause_requested = 0;
signed int cicero_volume = 0;
static unsigned int CiceroMaxChunkLength = 500;

/* Internal functions prototypes */
static void cicero_set_rate(signed int rate);

static void *_cicero_speak(void *);

int cicero_stop = 0;

/*
** Config file options
*/
//MOD_OPTION_1_STR(CiceroWrapper);
MOD_OPTION_1_STR(CiceroExecutable)
    MOD_OPTION_1_STR(CiceroExecutableLog)

/*
** Pipes to cicero
*/
static int fd1[2], fd2[2];

/*
** Some internal functions
*/
static long int
millisecondsBetween(const struct timeval *from, const struct timeval *to)
{
	return ((to->tv_sec - from->tv_sec) * 1000) +
	    ((to->tv_usec - from->tv_usec)
	     / 1000);
}

long int millisecondsSince(const struct timeval *from)
{
	struct timeval now;
	gettimeofday(&now, NULL);
	return millisecondsBetween(from, &now);
}

static int hasTimedOut(int milliseconds)
{
	static struct timeval start = { 0, 0 };

	if (milliseconds)
		return millisecondsSince(&start) >= milliseconds;

	gettimeofday(&start, NULL);
	return 1;
}

static void mywrite(int fd, const void *buf, int len)
{
	char *pos = (char *)buf;
	int w;
	if (fd < 0)
		return;
	hasTimedOut(0);
	do {
		if ((w = write(fd, pos, len)) < 0) {
			if (errno == EINTR || errno == EAGAIN)
				continue;
			else if (errno == EPIPE) {
				DBG("Broken pipe\n");
			} else
				perror("Pipe write");
			return;
		}
		pos += w;
		len -= w;
	} while (len && !hasTimedOut(600));
	if (len)
		fprintf(stderr, "Pipe write timed out");
}

/* Public functions */

int module_load(void)
{
	INIT_SETTINGS_TABLES();
	REGISTER_DEBUG();
	MOD_OPTION_1_STR_REG(CiceroExecutable, "/usr/bin/cicero");
	MOD_OPTION_1_STR_REG(CiceroExecutableLog,
			     "/var/log/speech-dispatcher/cicero-executable.log");
	return 0;
}

int module_init(char **status_info)
{
	int ret;
	int stderr_redirect;

	DBG("Module init\n");

	if (access(CiceroExecutable, X_OK) != 0) {
		DBG("ERROR: can not reach executable %s", CiceroExecutable);
		return -1;
	}

	(void)signal(SIGPIPE, SIG_IGN);

	DBG("call the pipe system call\n");
	if (pipe(fd1) < 0 || pipe(fd2) < 0) {
		DBG("Error pipe()\n");
		return -1;
	}
	DBG("Call fork system call\n");

	stderr_redirect = open(CiceroExecutableLog,
			       O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
	if (stderr_redirect == -1) {
		DBG("ERROR: Opening debug file for Cicero binary failed: (error=%d) %s", stderr_redirect, strerror(errno));
	} else {
		DBG("Cicero synthesizer logging to file %s",
		    CiceroExecutableLog);
	}
	switch (fork()) {
	case -1:{
			DBG("Error fork()\n");
			return -1;
		}
	case 0:{
			if (dup2(fd2[0], 0) < 0	/* stdin */
			    || dup2(fd1[1], 1) < 0) {	/* stdout */
				DBG("Error dup2()\n");
				exit(1);
			}
			if (stderr_redirect >= 0) {
				if (dup2(stderr_redirect, 2) < 0)
					DBG("ERROR: Couldn't redirect stderr, not logging for Cicero synthesizer.");
			}
			/*close(2);
			   close(fd2[1]);
			   close(fd1[0]); */
			int i = 0;
			for (i = 3; i < 256; i++)
				close(i);
			(void)signal(SIGPIPE, SIG_IGN);
			execl(CiceroExecutable, CiceroExecutable, (void *)NULL);
			DBG("Error execl()\n");
			exit(1);
		}
	default:{
			if (stderr_redirect >= 0)
				close(stderr_redirect);
			close(fd1[1]);
			close(fd2[0]);
			if (fcntl(fd2[1], F_SETFL, O_NDELAY) < 0
			    || fcntl(fd1[0], F_SETFL, O_NDELAY) < 0) {
				DBG("Error fcntl()\n");
				return -1;
			}
		}
	}

	cicero_message = g_malloc(sizeof(char *));
	*cicero_message = NULL;

	sem_init(&cicero_semaphore, 0, 0);

	DBG("Cicero: creating new thread for cicero_tracking\n");
	cicero_speaking = 0;
	ret =
	    pthread_create(&cicero_speaking_thread, NULL, _cicero_speak, NULL);
	if (ret != 0) {
		DBG("Cicero: thread failed\n");
		*status_info =
		    g_strdup("The module couldn't initialize threads "
			     "This can be either an internal problem or an "
			     "architecture problem. If you are sure your architecture "
			     "supports threads, please report a bug.");
		return -1;
	}

	*status_info = g_strdup("Cicero initialized successfully.");

	initialized = 1;

	return 0;
}

SPDVoice **module_list_voices(void)
{
	return NULL;
}

int module_speak(gchar * data, size_t bytes, SPDMessageType msgtype)
{
	DBG("Module speak\n");

	/* The following should not happen */
	if (cicero_speaking) {
		DBG("Speaking when requested to write");
		return 0;
	}

	DBG("Requested data: |%s|\n", data);

	if (*cicero_message != NULL) {
		g_free(*cicero_message);
		*cicero_message = NULL;
	}
	*cicero_message = module_strip_ssml(data);
	cicero_message_type = SPD_MSGTYPE_TEXT;

	/* Setting voice */
	/*    UPDATE_PARAMETER(voice, cicero_set_voice); */
	UPDATE_PARAMETER(rate, cicero_set_rate);
	/*    UPDATE_PARAMETER(pitch, cicero_set_pitch); */

	/* Send semaphore signal to the speaking thread */
	cicero_speaking = 1;
	sem_post(&cicero_semaphore);

	DBG("Cicero: leaving module_speak() normally\n\r");
	return bytes;
}

int module_stop(void)
{
	unsigned char c = 1;

	DBG("cicero: stop()\n");
	cicero_stop = 1;
	mywrite(fd2[1], &c, 1);
	return 0;
}

size_t module_pause(void)
{
	DBG("pause requested\n");
	if (cicero_speaking) {
		DBG("Pause not supported by cicero\n");
		cicero_pause_requested = 1;
		module_stop();
		return -1;
	}
	cicero_pause_requested = 0;
	return 0;
}

int module_close(void)
{
	DBG("cicero: close()\n");
	if (cicero_speaking) {
		module_stop();
	}

	if (!initialized)
		return 0;

	if (module_terminate_thread(cicero_speaking_thread) != 0)
		return -1;

	sem_destroy(&cicero_semaphore);

	initialized = 0;
	return 0;
}

/* Internal functions */

void *_cicero_speak(void *nothing)
{
	char stop_code = 1;
	unsigned int pos = 0, inx = 0, len = 0;
	int flag = 0;
	int bytes;
	int ret;
	char buf[CiceroMaxChunkLength], l[5], b[2];
	struct pollfd ufds = { fd1[0], POLLIN | POLLPRI, 0 };

	DBG("cicero: speaking thread starting.......\n");
	set_speaking_thread_parameters();
	while (1) {
		sem_wait(&cicero_semaphore);
		DBG("Semaphore on\n");
		len = strlen(*cicero_message);
		cicero_stop = 0;
		cicero_speaking = 1;
		cicero_position = 0;
		pos = 0;
		module_report_event_begin();
		while (1) {
			flag = 0;
			if (cicero_stop) {
				DBG("Stop in thread, terminating");
				cicero_speaking = 0;
				module_report_event_stop();
				break;
			}
			if (pos >= len) {	/* end of text */
				DBG("End of text in speaking thread\n");
				module_report_event_end();
				cicero_speaking = 0;
				break;
			}
			DBG("Call get_parts: pos=%d, msg=\"%s\" \n", pos,
			    *cicero_message);
			bytes =
			    module_get_message_part(*cicero_message, buf, &pos,
						    CiceroMaxChunkLength,
						    ".;?!");
			DBG("Returned %d bytes from get_part\n", bytes);
			if (bytes < 0) {
				DBG("ERROR: Can't get message part, terminating");
				cicero_speaking = 0;
				module_report_event_stop();
				break;
			}
			buf[bytes] = 0;
			DBG("Text to synthesize is '%s'\n", buf);

			if (bytes > 0) {
				DBG("Speaking ...");
				DBG("Trying to synthesize text");
				l[0] = 4;	/* say code for UTF-8 data */
				l[1] = bytes >> 8;
				l[2] = bytes & 0xFF;
				l[3] = 0, l[4] = 0;
				mywrite(fd2[1], &stop_code, 1);
				mywrite(fd2[1], l, 5);
				mywrite(fd2[1], buf, bytes);
				cicero_position = 0;
				while (1) {
					ret = poll(&ufds, 1, 60);
					if (ret)
						DBG("poll() system call returned %d, events=%d\n", ret, ufds.events);
					if (ret < 0) {
						perror("poll");
						module_report_event_stop();
						flag = 1;
						cicero_speaking = 0;
						break;
					}
					if (ret > 0)
						safe_read(fd1[0], b, 2);
					if (cicero_stop) {
						cicero_speaking = 0;
						module_report_event_stop();
						flag = 1;
						break;
					}
					if (ret == 0)
						continue;
					inx = (b[0] << 8 | b[1]);
					DBG("Tracking: index=%u, bytes=%d\n",
					    inx, bytes);
					if (inx == bytes) {
						cicero_speaking = 0;
						break;
					} else {
						if (inx)
							cicero_position = inx;
					}
				}
			} else {
				cicero_speaking = 0;
				break;
			}
			if (flag)
				break;
		}
		cicero_stop = 0;
	}
	cicero_speaking = 0;
	DBG("cicero: tracking thread ended.......\n");
	pthread_exit(NULL);
}

static void cicero_set_rate(signed int rate)
{
	const float spkRateTable[] = {
		0.3333,
		0.3720,
		0.4152,
		0.4635,
		0.5173,
		0.5774,
		0.6444,
		0.7192,
		0.8027,
		0.8960,
		1.0000,
		1.1161,
		1.2457,
		1.3904,
		1.5518,
		1.7320,
		1.9332,
		2.1577,
		2.4082,
		2.6879,
		3.0000
	};
	float expand;
	rate -= 100;
	rate = abs(rate);
	rate /= 10;
	expand = spkRateTable[rate];
	unsigned char *p = (unsigned char *)&expand;
	unsigned char l[5];
	l[0] = 3;
	l[1] = p[3];
	l[2] = p[2];
	l[3] = p[1];
	l[4] = p[0];
	mywrite(fd2[1], l, 5);
}
