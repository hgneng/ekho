# compress all wav files under source directory to target directory at $rate

use strict;
use warnings;

my ($source_dir, $target_dir, $rate, $tempo, $volumn) = @ARGV;

# a sample tempo value is 50, which will accelarate speed by 50%
# a sample volumn value is 0.4 for Cantonese,
# which will decress volumn by 60%. 0.5 for Mandarin
if (not $source_dir || not $target_dir || not $rate) {
  print "compress.pl <source_dir> <target_dir> <rate> [soundtouch_tempo] [volumn]";
  exit 0;   
}

$volumn = 1 if (not $volumn);

my $present_dir = `pwd`;
chomp($present_dir);
mkdir $target_dir;
chdir $source_dir;

my $cmd;
while (my $file = <*.wav>) {
    my $lcfile = lc($file);
    print "procesing $file... ";
    if ($tempo) {
      #`/usr/bin/soundstretch $file /tmp/compress.wav -tempo=$tempo`;
      my $t = (100 + $tempo) / 100;
      $cmd = "sox -v $volumn $file -r $rate -c 1 $present_dir/$target_dir/$lcfile tempo $t";
    } else {
      my $size = (-s $file) - 44;
      my $fade_len = int($size / 20) . 's';
      my $stop = int($size / 2) . 's';
#      `sox -v $volumn $file -r $rate -c 1 /tmp/compress.wav fade $fade_len $stop $fade_len`;
#      my $cmd = "sox -v $volumn /tmp/compress.wav -r $rate -c 1 $present_dir/$target_dir/$lcfile fade $fade_len $stop $fade_len";
#      my $cmd = "sox -v $volumn $file -r $rate -c 1 $present_dir/$target_dir/$lcfile fade $fade_len $stop $fade_len";
      $cmd = "sox -v $volumn $file -r $rate -c 1 $present_dir/$target_dir/$lcfile";
    }
    #print $cmd, "\n";
    `$cmd`;
    my $old = -s $file;
    my $new = -s "$present_dir/$target_dir/$lcfile";
    printf("%d\%%\n", $new * 100 / $old);
#    `lame --quiet --vbr-new -V 9 $file -o - | lame --quiet --mp3input --decode - -o ../$target_dir/$file`;
}
#`cd $source_dir; for N in *.wav ; do lame --vbr-new -V 9 -h $N -o - | lame --mp3input --decode - -o ../$target_dir/$N; done`
