/*
 * alloc.h - Auxiliary functions for allocating and freeing data structures
 *
 * Copyright (C) 2001,2002,2003,2004,2005,2007 Brailcom, o.p.s.
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

#include "speechd.h"

#ifndef ALLOC_H
#define ALLOC_H

/* Copy a message */
TSpeechDMessage *spd_message_copy(TSpeechDMessage * old);

/* Free a message */
void mem_free_message(TSpeechDMessage * msg);

/* Free a settings element */
void mem_free_fdset(TFDSetElement * set);

#endif
