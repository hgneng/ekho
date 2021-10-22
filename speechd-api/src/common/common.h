
/*
 * common.h - Common declarations valid both in server and modules
 *
 * Copyright (C) 2001, 2002, 2003, 2006, 2007 Brailcom, o.p.s.
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
 */

#ifndef _COMMON_H
#define _COMMON_H 1

#ifdef __cplusplus
extern "C" {
#endif

/* Debugging */
void MSG(int level, const char *format, ...) __attribute__((format(printf, 2, 3)));
void MSG2(int level, const char *kind, const char *format, ...) __attribute__((format(printf, 3, 4)));
#define DBG(arg...) MSG(4, arg)

int spd_pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                          void *(*start_routine) (void *), void *arg);

void set_speaking_thread_parameters(void);

/* This should be called when reaching a mark */
void module_report_index_mark(const char *mark);
/* This should be called when beginning playing the speech */
void module_report_event_begin(void);
/* This should be called when finishing playing the speech */
void module_report_event_end(void);
/* This should be called when stopping the speech */
void module_report_event_stop(void);
/* This should be called when pausing the speech */
void module_report_event_pause(void);
/* This should be called when reaching a sound icon */
void module_report_icon(const char *icon);
/* This should be called to play a given file */
int module_play_file(const char *filename);

#ifdef __cplusplus
}
#endif

#endif /* _COMMON_H */
