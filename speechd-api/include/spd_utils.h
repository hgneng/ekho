/*
 * spd_utils.h - prototypes for utility functions used
 * in the Speech Dispatcher server and modules.
 *
 * Copyright (C) 2010 Brailcom, o.p.s.
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
#ifndef SPD_UTILS_H
#define SPD_UTILS_H
#include <stdio.h>
#include <stddef.h>
#include <sys/types.h>

ssize_t spd_getline(char **lineptr, size_t * n, FILE * f);
#endif /* SPD_UTILS_H */
