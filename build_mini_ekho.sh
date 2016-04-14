#!/bin/sh

version=$1

chdir build
make clean
../configure --disable-ogg --disable-festival --disable-soundtouch --disable-portaudio --disable-gtk2
make

rm -rf ekho-mini-$version
mkdir ekho-mini-$version
cp ekho ekho-mini-$version/
cp ../README-mini ekho-mini-$version/
cp ../COPYING ekho-mini-$version/
mkdir ekho-mini-$version/ekho-data
cp ../ekho-data/COPYING-pinyin-yali ekho-mini-$version/ekho-data/
cp ../ekho-data/COPYING-jyutping-wong ekho-mini-$version/ekho-data/
cp ../ekho-data/zhy.dict ekho-mini-$version/ekho-data/
cp ../ekho-data/zh.dict ekho-mini-$version/ekho-data/
cp -r /home/hgneng/sound/pinyin-yali-22050-ogg ekho-mini-$version/ekho-data/pinyin
cp -r /home/hgneng/sound/jyutping-wong-22050-v4-ogg ekho-mini-$version/ekho-data/jyutping

rm ekho-mini-$version.tar.bz2
tar cjf ekho-mini-$version.tar.bz2 ekho-mini-$version
