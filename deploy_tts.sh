#!/bin/bash

# install conda
if command -v conda &> /dev/null; then
    echo "Conda 已安装："
    conda --version
else
    sudo apt install -y python3-pip
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
    bash Miniconda3-latest-Linux-x86_64.sh -b -p /opt/miniconda3
    /opt/miniconda3/bin/conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main
    /opt/miniconda3/bin/conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r
    /opt/miniconda3/bin/conda init
    pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
fi

# install piper TTS
source /opt/miniconda3/etc/profile.d/conda.sh
conda activate base
pip3 install pathvalidate unicode_rbnf sentence_stream
pip3 install g2pw torch
pip3 install piper-tts[http]

if [ -f "zhtts.tar.gz" ]; then
    tar zxvf zhtts.tar.gz
    cd zhtts
    ./deploy.sh
    cd ..
fi

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