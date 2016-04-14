($progress) = @ARGV;
$progress = 0 if (not defined $progress);

binmode('stdout', 'utf8');

`wget -c http://81.102.157.19/~ssb22/hae-sung/index.html`;
`cp index.html hae-sung.html`;

open(HTML_FILE, '<:utf8', 'hae-sung.html');
while ($line = <HTML_FILE>) {
	$line =~ /href=([^>]*)>([0-9]*)\)<\/a>	(.*)/;
	$url = $1;
  $num = $2;
  @chars = split(//, $3);
  next if ((not $url) || (not $num) || (not @chars) || ($num < $progress));

  print "line $num: $url: @chars\n";
  `wget -c http://81.102.157.19/~ssb22/hae-sung/$url`;
  `cp line$num.wav hangul-haesung-lines/line$num.wav`;
  chdir('hangul-haesung-lines');
  `perl ~/e-guidedog/eGuideDog_TTS/split_wav.pl line$num.wav`;
  $i = 2;
  $file = "sound.$i.wav";
  while (-f $file) {
    $char = shift(@chars);
    if (not $char) {
      print "read char for $file fail.";
    } else {
      `cp $file ../hangul-haesung/$char.wav`;
      print "$char ";
      do {
        `play ../hangul-haesung/$char.wav`;
        $cmd = <stdin>;
        if ($cmd =~ /s/) {
          $i++;
          $file = "sound.$i.wav";
          `cp $file ../hangul-haesung/$char.wav`;
        } elsif ($cmd =~ /d/) {
          $i--;
          $cmd = 'r';
        }
      } while (($cmd =~ /s/) || ($cmd =~ /r/));
    }
    $i++;
    $file = "sound.$i.wav";
  }
  chdir('..');
  $char = shift(@chars);
  if ($char) {
    print "$char left!!\n";
  }
  print "line$num finished\n";
  $cmd = <stdin>;
}
close(HTML_FILE);
