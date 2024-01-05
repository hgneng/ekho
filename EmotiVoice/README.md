# Build EmotiVoice

1. get EmotiVoice source

Clone EmotiVoice in the same level of ekho like this:
- hgneng/ekho
- hgneng/EmotiVoice

The script will use relative path to build package.

"""
$ git clone https://github.com/hgneng/EmotiVoice
"""

2. get sentense vector model to folder WangZeJun

refer to README.md of EmotiVoice

3. download pre-trained model to folder outputs

"""
$ git clone https://www.modelscope.cn/syq163/outputs.git
"""

4. Make sure it's in EmotiVoice conda envirenment

"""
$ conda activate EmotiVoice
"""

5. copy build.sh to root of EmotiVoice and run
"""
$ ./build.sh
"""

# Pack EmotiVoice

"""
$./pack.sh
"""

This command can run over half an hour!