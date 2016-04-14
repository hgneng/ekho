#!/usr/bin/perl

use strict;
use warnings;

our ($lang, $skip_build) = @ARGV;

sub kill_speechd() {
  if (`ps -ef | grep orca | grep -v grep`) {
    `ps -ef | grep orca | grep -v grep | awk '{ print \$2 }' | xargs sudo kill -9`;
  }
  if (`ps -ef | grep speechd.sock | grep -v grep`) {
    `ps -ef | grep speechd.sock | grep -v grep | awk '{ print \$2 }' | xargs sudo kill -9`;
  }
  if (`ps -ef | grep speech-dispatcher | grep -v grep`) {
    `ps -ef | grep speech-dispatcher | grep -v grep | awk '{ print \$2 }' | xargs sudo kill -9`;
  }
  if (`ps -ef | grep sd_ekho | grep -v grep`) {
    `ps -ef | grep sd_ekho | grep -v grep | awk '{ print \$2 }' | xargs sudo kill -9`;
  }
}

sub build_common() {
  system('sudo apt-get -y install libsndfile1-dev');
  system('sudo apt-get -y install libpulse-dev');
  system('sudo apt-get -y install libncurses5-dev');
  system('sudo apt-get -y install build-essential');
  system('sudo apt-get -y install autoconf automake');
  system('sudo apt-get -y install libdotconf-dev');
  system('sudo apt-get -y install libmp3lame-dev');
  system('sudo apt-get -y install festival-dev');
  system('sudo apt-get -y install libestools2.1-dev');
  system('./configure --enable-festival --enable-speechd');
  system('make clean && make');
}

sub setup_common() {
  my $t = time();
  system('sudo cp /etc/speech-dispatcher/speechd.conf /etc/speech-dispatcher/speechd.conf.' . $t);
  `grep -v 'sd_ekho' /etc/speech-dispatcher/speechd.conf.$t | sed -e 's/^DefaultModule.*/DefaultModule ekho/' | sed -e 's/^AddModule "espeak"/AddModule "ekho" "sd_ekho" "ekho.conf"\\nAddModule "espeak"/' >/tmp/speechd.conf.ekho`;
  `sudo mv /tmp/speechd.conf.ekho /etc/speech-dispatcher/speechd.conf`;

  my $config = '/usr/lib/python3/dist-packages/speechd_config/config.py';
  if (-e '/usr/share/pyshared/speechd_config/config.py') {
    # older than 14.04
    $config = '/usr/share/pyshared/speechd_config/config.py';
  }

  if (-e $config) {
    `sudo cp $config $config.$t`;
    `cat $config.$t | sed -e 's/"espeak", /"espeak", "ekho", /' | sed -e 's/"ekho", "ekho", /"ekho", /' > /tmp/config.py.ekho`;
    `sudo mv /tmp/config.py.ekho $config`;
  }
}

sub setup_lang() {
  if (not `grep $lang /etc/speech-dispatcher/speechd.conf`) {
    `echo 'DefaultLanguage "$lang"' | sudo tee -a /etc/speech-dispatcher/speechd.conf`;
    `echo 'LanguageDefaultModule "$lang" "ekho"' | sudo tee -a /etc/speech-dispatcher/speechd.conf`;
  }

  if (-f ($ENV{HOME} . "/.speech-dispatcher/conf/speechd.conf") &&
      not `grep $lang ~/.speech-dispatcher/conf/speechd.conf`) {
    `echo 'DefaultLanguage "$lang"' >>~/.speech-dispatcher/conf/speechd.conf`;
    `echo 'LanguageDefaultModule "$lang"  "ekho"' >>~/.speech-dispatcher/conf/speechd.conf`;
  }

  # for 14.04
  if (-f ($ENV{HOME} . "/.config/speech-dispatcher/speechd.conf") &&
      not `grep $lang ~/.config/speech-dispatcher/speechd.conf`) {
    `echo 'DefaultLanguage "$lang"' >>~/.config/speech-dispatcher/speechd.conf`;
    `echo 'LanguageDefaultModule "$lang"  "ekho"' >>~/.config/speech-dispatcher/speechd.conf`;
  } elsif (-d ($ENV{HOME} . "/.config/speech-dispatcher")) {
    `sudo chown -R $ENV{USER} $ENV{HOME}/.config/speech-dispatcher`;
    `cp /etc/speech-dispatcher/speechd.conf $ENV{HOME}/.config/speech-dispatcher/`;
  }
}

##### main #####
if (not $lang) {
  $lang = 'Mandarin';
}
if ($lang ne 'Tibetan' and $lang ne 'Mandarin' and $lang ne 'Cantonese') {
  print "Only Mandarin, Cantonese and Tibetan are supported. Fallback to Mandarin\n";
  $lang = 'Mandarin';
}

if (`grep precise /etc/lsb-release`) {
  # ubuntu 12.04
  build_common() if (not $skip_build);
  setup_common();
  system('sudo ln -f -s /usr/lib/speech-dispatcher/libsdaudio.so.2 /usr/lib/libsdaudio.so.2');
  kill_speechd();
  system('sudo rm -rf /usr/local/share/ekho-data');
  system('sudo make install');
  setup_lang();
} elsif (`grep "12.10" /etc/lsb-release` ||
    `grep "13.04" /etc/lsb-release` ||
    `grep "14.04" /etc/lsb-release` ||
    `grep "15.10" /etc/lsb-release`) {
  # ubuntu 12.10
  build_common() if (not $skip_build);
  `sudo ln -s /usr/lib/i386-linux-gnu/speech-dispatcher-modules /usr/lib/` if (not `grep "14.04" /etc/lsb-release`);
  if (! -e '/usr/lib/libsdaudio.so.2') {
    `sudo cp speechd-api/src/audio/.libs/libsdaudio.so* /usr/lib/`;
  }
  setup_common();
  kill_speechd();
  system('sudo rm -rf /usr/local/share/ekho-data');
  system('sudo make install');
  setup_lang();
  `sudo rm -f /usr/lib/speech-dispatcher-modules/sd_cicero`;
} else {
  print "Sorry. Your OS is not supported. Please refer to INSTALL. You can also send email to Cameron <hgneng at gmail.com> for help.\n";
}

# start/restart Orca
print "restarting Orca\n";
`ps -ef | grep speech-dispatcher | grep -v grep | awk '{print $2}' | xargs kill`;
system("orca --replace &");
