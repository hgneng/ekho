#!/bin/bash

text_file="/tmp/ekho.in.txt"

#   prepare
if [ -e "${text_file}" ]
then
    rm "${text_file}"
fi
echo "你好！世界！" >> "${text_file}"
echo "一二三四五六七八九十" >> "${text_file}"
echo "背上 背着 背包" >> "${text_file}"
echo "睡觉是第四声，觉得是第三声" >> "${text_file}"
echo "难得糊涂，空难事故" >> "${text_file}"

#   test
#./ekho -v "Cantonese" -f "${text_file}"
./ekho -v "Mandarin" -f "${text_file}"

#   clean
if [ -e "${text_file}" ]
then
    rm "${text_file}"
fi

exit 0

