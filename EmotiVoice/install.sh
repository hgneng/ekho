#!/usr/bin/bash

# set pip repository to tsinghua
pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple

# install from conda if system's python version is not 3.12
HAS_PYTHON_312=$(python3 -V|grep 3.12)
if [ -z "$HAS_PYTHON_312" ]
then
    # install conda if needed
    # not sure whether it works, it may need to download from https://anaconda.org/
    CONDA=$(which conda)
    if [ -z "$CONDA" ]
    then
        echo '===== install conda ====='
        # install miniconda to /opt/miniconda3
        cd /opt
        sudo mkdir miniconda3
        sudo chmod 777 miniconda3
        # wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O miniconda3/miniconda.sh
        wget https://mirrors.tuna.tsinghua.edu.cn/anaconda/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/Miniconda3-latest-Linux-x86_64.sh
        # /tmp/Miniconda3-latest-Linux-x86_64.sh --yes
        bash /tmp/Miniconda3-latest-Linux-x86_64.sh -b -u -p miniconda3
        miniconda3/bin/conda init bash
        source ~/.bashrc
    fi

    echo '===== conda activate EmotiVoice ====='
    conda create -n EmotiVoice python=3.12 -y
    conda activate EmotiVoice

    # for Python 3.8
    # pip install torch torchaudio
    # pip install numpy numba scipy transformers soundfile yacs g2p_en jieba pypinyin
fi

# install EmotiVoice with Python 3.12.2 wihch is the version of Ubuntu 24.04 (default is 3.8)
echo '===== install pip dependencies ====='
pip install six fsspec numpy packaging psutil fsspec psutil cython numba wrapt scikit-learn scipy torch torchaudio transformers soundfile yacs g2p_en jieba pypinyin pypinyin_dict

# copy file to /opt/EmotiVoice
echo '===== install EmotiVoice ====='
sudo mkdir /opt/EmotiVoice
sudo chmod a+w /opt/EmotiVoice
cp -r EmotiVoice-main/* /opt/EmotiVoice/
cp -rL WangZeJun /opt/EmotiVoice/
cp -rL outputs /opt/EmotiVoice/
