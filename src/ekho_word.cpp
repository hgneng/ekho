/***************************************************************************
 * Copyright (C) 2008-2022 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: https://eguidedog.net                                          *
 *                                                                         *
 * This program is free software; you can redistribute it and/or           *
 * modify it under the terms of the GNU General Public License             *
 * as published by the Free Software Foundation; either version 2          *
 * of the License, or any later version.                                   *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 *                                                                         *
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software             *
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,              *
 * MA  02110-1301, USA.                                                    *
 **************************************************************************/

#include <dirent.h>
#include "ekho_word.h"

void Word::loadWordVoiceFiles(string dir) {
  // scan files in voice dir
  DIR *dirp;
  struct dirent *dp;
  if ((dirp = opendir(dir.c_str())) == NULL) {
    cerr << "Fail to open dir " << dir << endl;
    return;
  }

  do {
    if ((dp = readdir(dirp)) != NULL) {
      cerr << dp->d_name << endl;
    }
  } while (dp != NULL);

  closedir(dirp);
}