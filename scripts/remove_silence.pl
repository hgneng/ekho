BEGIN {
  unshift(@INC, '/home/hgneng/e-guidedog/eGuideDog-Wav/lib');
}

use strict;
use warnings;
use eGuideDog::Wav;

my ($lang, $src, $tgt) = @ARGV;

my $wav = eGuideDog::Wav->new();

if (-d $src) {
  mkdir $tgt;
  chdir $src;
  while (my $file = <*.wav>) {
    $file =~ /^([^.]*)./;
    my $symbol = $1;
    my $size0 = -s $file;
    $wav->load($file);
    if ($lang eq 'jyutping') {
      $wav->remove_silence(100);
      $wav->remove_silence(100);
    } elsif ($lang eq 'pinyin') {
      $wav->remove_silence(100);
      $wav->remove_silence(100);
    } else {
      die;
    }
    $wav->save("../$tgt/$file");
    my $size = -s "../$tgt/$file";
    print "$file: " . int(100 - 100 * $size / $size0) . "\% removed\n";
  }
} else {
  $wav->load($src);
  if ($lang eq 'jyutping') {
    $wav->remove_silence(100);
    $wav->remove_silence(100);
  } elsif ($lang eq 'pinyin') {
    $wav->remove_silence(100);
    $wav->remove_silence(100);
  } else {
    die;
  }
  $wav->save($tgt);
}
