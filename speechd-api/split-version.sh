#!/bin/sh
#
# Copyright (C) 2015 - 2018 Luke Yelavich <themuso@themuso.com>
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

# Small script to split out major/minor/micro versions for API version checks.
VERSION=$(./git-version-gen .tarball-version)

case "$1" in
	-ma)
		echo $VERSION | cut -f1 -d.
		;;
	-mi)
		echo $VERSION | cut -f2 -d. | cut -f1 -d~
		;;
	-mc)
		micro_version=$(echo $VERSION | cut -f3 -d. | cut -f1 -d-)
		if test -z "$micro_version"; then
			echo 0
		else
			echo $micro_version
		fi
		;;
	*)
		echo "Usage: $0 [-ma|-mi|-mc]"
		exit 1
		;;
esac
