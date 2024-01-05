#!/usr/bin/bash
cp server.py ../../EmotiVoice/
cp client.py ../../EmotiVoice/
cd ../../EmotiVoice
pyinstaller -y --copy-metadata tqdm --copy-metadata regex --copy-metadata requests --copy-metadata packaging \
    --copy-metadata filelock --copy-metadata numpy --copy-metadata tokenizers --copy-metadata torch \
    --copy-metadata pypinyin_dict server.py
pyinstaller -y client.py
cp -r config dist/server/
cp -r data dist/server/
cp -r outputs dist/server/
cp -r WangZeJun dist/server/
cd ../ekho/EmotiVoice

