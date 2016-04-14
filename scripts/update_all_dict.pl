# Cantonese
system("cd /home/hgneng/e-guidedog/eSpeak-Chinese && perl gen_zhy_list.pl");
system("cp /home/hgneng/e-guidedog/eSpeak-Chinese/zhy_list /home/hgneng/e-guidedog/eGuideDog-Dict-Cantonese/zhy_list");
system("cd /home/hgneng/e-guidedog/eGuideDog-Dict-Cantonese && perl update_dict.pl && make && sudo make install");

# Mandarin
system("cp /home/hgneng/e-guidedog/eSpeak-Chinese/zh_list /home/hgneng/e-guidedog/eGuideDog-Dict-Mandarin/zh_list");
system("cp /home/hgneng/e-guidedog/eSpeak-Chinese/zh_listx /home/hgneng/e-guidedog/eGuideDog-Dict-Mandarin/zh_listx");
system("cd /home/hgneng/e-guidedog/eGuideDog-Dict-Mandarin && perl update_dict.pl && make && sudo make install");

system("rm /home/hgneng/e-guidedog/eGuideDog_TTS/ekho-data/zhy.dict");
system("rm /home/hgneng/e-guidedog/eGuideDog_TTS/ekho-data/zh.dict");

