/*
 * kali_KParle_shim.c - Shim for Kali
 * to be able to build the Kali module without the Kali SDK.
 *
 * Copyright (C) 2018 Hypra
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

#include <kali/Kali/kali.h>
#include <assert.h>

extern "C" {

bool initParle(void) {
	assert(0);
}

bool quitteParle(void) {
	assert(0);
}

}
