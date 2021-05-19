Install instructions:
1. install espeak-ng
2. install MbrolaTools35.exe
3. extract Mbrola voice file mbrola.zip to C:\Program Files (x86)\eSpeak NG\espeak-ng-data\ (can be download from https://github.com/numediart/MBROLA-voices)
4. extract mb.zip to C:\Program Files (x86)\eSpeak NG\espeak-ng-data\voices\
5. make sure the mbrola voice you want to use has a definition file in espeak-ng-data\voices\mb and has a voice file in espeak-ng-data\mbrola.
    For example, there is no C:\Program Files (x86)\eSpeak NG\espeak-ng-data\voices\mb\mb-es3. So the voice is not usable even you have voice file C:\Program Files (x86)\eSpeak NG\espeak-ng-data\mbrola\es3
6. install ekho
7. for spanish, currently we use mb-vz1. If you want to use other voice or language, put files in right palce, then edit C:\Program Files (x86)\ekho\ekho.conf to set the voice your want.
8. run ttsapp.exe or call EkhoMandarin through SAPI5