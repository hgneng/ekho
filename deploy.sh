#!/bin/bash

DEBUG=0
for i in "$@"
do
case $i in
    -d|--debug)
    DEBUG=1
    shift # past argument=value
    ;;
    *)
            # unknown option
    ;;
esac
done

if [ $DEBUG -eq 1 ]
then
sudo rm -rf ~/ekho-logs
mkdir ~/ekho-logs
./install.pl 2>&1 | tee ~/ekho-logs/install.log
orca --replace&
echo 'finish install. collecting logs'
find ~/.speech-dispatcher | xargs ls -l >> ~/ekho-logs/ekho.logs
find ~/.config/speech-dispatcher | xargs ls -l >> ~/ekho-logs/ekho.logs
ls -l /usr/lib/libsdaudio.so.2 >> ~/ekho-logs/ekho.logs
ls -l /usr/lib/speech-dispatcher-modules >> ~/ekho-logs/ekho.logs
ls -l /usr/share/pyshared/speechd_config >> ~/ekho-logs/ekho.logs
ls -l /usr/lib/speech-dispatcher-modules/sd_ekho >> ~/ekho-logs/ekho.logs
cp /etc/speech-dispatcher/speechd.conf ~/ekho-logs/
cp /usr/share/pyshared/speechd_config/config.py ~/ekho-logs/
cp ~/.speech-dispatcher/log/ekho.log ~/ekho-logs/
cp ~/.speech-dispatcher/log/speech-dispatcher.log ~/ekho-logs/
cp ~/.config/speech-dispatcher/speechd.conf ~/ekho-logs/speechd.conf.2
sudo cp /var/log/speech-dispatcher/ekho.log ~/ekho-logs/ekho.log.2
sudo cp /var/log/speech-dispatcher/speech-dispatcher.log ~/ekho-logs/speech-dispatcher.log.2
sudo cp -r /var/crash ~/ekho-logs/
sudo chmod -R a+rw ~/ekho-logs
tar cJvf ~/ekho-logs.tar.xz ~/ekho-logs
sudo apt-get install -y curl
echo 'finish collecting logs. uploading logs'
curl -F"operation=upload" -F"file=@$HOME/ekho-logs.tar.xz" http://www.eguidedog.net/ekho/upload.php
echo 'finish uploading'
else
  ./install.pl  
fi

