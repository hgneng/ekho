#!/bin/bash

# install piper TTS
sudo apt install -y alsa-utils python3-pip python3-venv
pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
pip3 install pathvalidate unicode_rbnf sentence_stream --break-system-packages
# g2pw
pip3 install g2pw torch --break-system-packages
pip3 install piper-tts[http] --break-system-packages

#sudo cp -r ekho-data/piper /usr/local/share/ekho-data/
# setproxy
# python3 -m piper.download_voices zh_CN-xiao_ya-medium

# install libpiper
# git clone https://github.com/OHF-Voice/piper1-gpl.git
# apt install cmake
# cd piper1-gpl/libpiper
# cmake -Bbuild -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$PWD/install
# cmake --build build
# cmake --install build
# cp include/piper.h /usr/include/
# cp -r install/lib/* /usr/lib/
# cp install/libpiper.so /usr/lib/