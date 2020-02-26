#!/bin/sh
#
# Copyright (C) 2017 - 2018 Colomban Wendling <cwendling@hypra.fr>
#
# This is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

# error out upon command error
set -e

NVDADIR=$1

# import all symbols.dic
test -d "$NVDADIR"/source/locale
for f in "$NVDADIR"/source/locale/*/symbols.dic; do
  dest=$(echo "$f" | sed 's|^.*/locale/|locale/|')
  cp -v "$f" "$dest"
done
