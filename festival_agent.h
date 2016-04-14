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
#ifndef FESTIVAL_AGENT_H
#define FESTIVAL_AGENT_H

/* Start festival server and client
 * This should be the first function to call.
 */
int fa_start(void);

/* Stop festival client
 * This should be the last function to call.
 * I don't know how to shutdown festival server now.
 */
int fa_stop(void);

/* Speak text */
int fa_speak(const char *text);

/* Get wave data of text
 * The data length will no more than *len
 * The exact return length of the data will be return through len
 * Return 0 if finished reading, 1 means need to continus
 */
int fa_get_wav(const char *text, char *buffer, long *size);

int fa_continue_get_wav(char *buffer, long *size);

#endif
