#!/usr/bin/bash

# setup pip repository
pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple

# install conda if needed
# not sure whether it works, it may need to download from https://anaconda.org/
CONDA=$(which conda)
if [ -z "$CONDA" ]
then
    wget https://mirrors.tuna.tsinghua.edu.cn/anaconda/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/Miniconda3-latest-Linux-x86_64.sh
    /tmp/Miniconda3-latest-Linux-x86_64.sh --yes
fi

# install conda EmotiVoice env
conda create -n EmotiVoice python=3.8 -y
conda activate EmotiVoice
pip install torch torchaudio
pip install numpy numba scipy transformers soundfile yacs g2p_en jieba pypinyin

# copy file to /opt/EmotiVoice
sudo mkdir /opt/EmotiVoice
sudo chmod a+w /opt/EmotiVoice
cp -r EmotiVoice-main/* /opt/EmotiVoice/
cp -rL WangZeJun /opt/EmotiVoice/
cp -rL outputs /opt/EmotiVoice/