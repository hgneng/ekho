my $sound_dir = "/home/hgneng/sound/jyutping-wong-44100-v7";

open(FILE, '<:utf8', 'v6_amendment.txt');
while (my $line = <FILE>) {
  if ($line =~ /^([^:]*):/) {
    $phon = $1;
#    next if (-e "$sound_dir/$phon.wav");
    system("espeak -vzhy \"[['$phon]]\"");
    while (not -e '/tmp/output.wav') {
      sleep(1);
    }
    while (-s "/tmp/output.wav" <= 44) {
        sleep(1);
    }
    sleep(2);
    `mv /tmp/output.wav $sound_dir/$phon.wav`;
    sleep(1);
    system("play $sound_dir/$phon.wav");
  } else {
    warn $line;
  }
}
close(FILE);
