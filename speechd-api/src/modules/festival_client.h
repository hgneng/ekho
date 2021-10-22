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
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Client end of Festival server API (in C) designed specifically for    */
/* Galaxy Communicator use, though might be of use for other things      */
/*                                                                       */
/*=======================================================================*/
#ifndef _FESTIVAL_CLIENT_H_
#define _FESTIVAL_CLIENT_H_

#define FESTIVAL_DEFAULT_SERVER_HOST "localhost"
#define FESTIVAL_DEFAULT_SERVER_PORT 1314
#define FESTIVAL_DEFAULT_TEXT_MODE "fundamental"

extern int festival_connection_crashed;

typedef struct FT_Info {
	int encoding;
	char *server_host;
	int server_port;
	char *text_mode;

	int server_fd;
} FT_Info;

typedef struct FT_Wave {
	int num_samples;
	int sample_rate;
	short *samples;
} FT_Wave;

void delete_FT_Wave(FT_Wave * wave);
void delete_FT_Info(FT_Info * info);

#define SWAPSHORT(x) ((((unsigned)x) & 0xff) << 8 | \
                      (((unsigned)x) & 0xff00) >> 8)
#define SWAPINT(x) ((((unsigned)x) & 0xff) << 24 | \
                    (((unsigned)x) & 0xff00) << 8 | \
                    (((unsigned)x) & 0xff0000) >> 8 | \
                    (((unsigned)x) & 0xff000000) >> 24)

/* Sun, HP, SGI Mips, M68000 */
#define FAPI_BIG_ENDIAN (((char *)&fapi_endian_loc)[0] == 0)
/* Intel, Alpha, DEC Mips, Vax */
#define FAPI_LITTLE_ENDIAN (((char *)&fapi_endian_loc)[0] != 0)

#define FEST_SEND_CMD(format) \
	do { \
		FILE *fd; \
		char *str; \
		fd = fdopen(dup(info->server_fd),"wb"); \
		if (fd != NULL){ \
			str = g_strdup(format"\n"); \
			fputs(str, fd); \
			DBG("-> Festival: |%s|", str); \
			g_free(str); \
			fclose(fd); \
		}else{ \
			DBG("Can't open connection"); \
		} \
	} while (0)

#define FEST_SEND_CMDA(format, args...) \
	do { \
		FILE *fd; \
		char *str; \
		fd = fdopen(dup(info->server_fd),"wb"); \
		if (fd != NULL){ \
			str = g_strdup_printf(format"\n", args); \
			fputs(str, fd); \
			DBG("-> Festival: |%s|", str); \
			g_free(str); \
			fclose(fd); \
		}else{ \
			DBG("Can't open connection"); \
		} \
	} while (0)

/*****************************************************************/
/*  Public functions to interface                                */
/*****************************************************************/

/* If called with NULL will attempt to access using defaults */
FT_Info *festivalOpen(FT_Info * info);
int festivalClose(FT_Info * info);

int festivalStringToWaveRequest(FT_Info * info, const char *text);
int festivalSoundIcon(FT_Info * info, const char *text);
int festivalCharacter(FT_Info * info, const char *text);
int festivalKey(FT_Info * info, const char *text);
int festivalSpell(FT_Info * info, const char *text);

FT_Wave *festivalStringToWaveGetData(FT_Info * info);

FT_Info *festivalDefaultInfo(void);
void festivalEmptySocket(FT_Info * info);
int save_FT_Wave_snd(FT_Wave * wave, const char *filename);
FT_Wave *festivalGetDataMulti(FT_Info * info, char **callback, int *stop_flag,
			      int stop_by_close);

int festival_check_info(FT_Info * info, const char *fnname);
char **lisp_list_get_vect(const char *expr);
int festival_read_response(FT_Info * info, char **expr);
#endif
