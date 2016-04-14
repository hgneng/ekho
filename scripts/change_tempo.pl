use strict;
use warnings;
use eGuideDog::Dict::Cantonese;
use eGuideDog::Dict::Mandarin;
use eGuideDog::Wav;

my ($lang, $source_dir, $target_dir, $major_factor, $minor_factor) = @ARGV;

if (not $lang || not $source_dir || not $target_dir) {
  die "change_tempo.pl <lang> <source_dir> <target_dir> [major_factor] [minor_factor].";
}

$major_factor = 100 if (not defined $major_factor); # how many percent faster. 130 is good
$minor_factor = 0.3 if (not defined $minor_factor); # how important espeak statistic

mkdir $target_dir;
chdir $source_dir;

my $wav = eGuideDog::Wav->new();

my $dict;
if ($lang eq 'jyutping') {
  $dict = eGuideDog::Dict::Cantonese->new();
} elsif ($lang eq 'pinyin') {
  $dict = eGuideDog::Dict::Mandarin->new();
} else {
  die "lang should be jyutping or pinyin";
}

while (my $file = <*.wav>) {
  $file =~ /^([^.]*)/;
  my $symbol = $1;
  my $size0 = -s $file;
  $wav->load($file);
  $wav->remove_silence(500);
  $wav->remove_silence(500);
  $wav->save("/tmp/change_tempo.wav");
  my $size = -s "/tmp/change_tempo.wav";
  print int(100 - 100 * $size / $size0), "% silence removed\n";
  #my $percent = int(100 - 100 * ($dict->get_symbol_size($symbol) / $size));
  my $percent = int(100 * ($size / (($dict->get_symbol_size($symbol) || $size * 22025 / 44100) * 44100 / 22025) - 1) * $minor_factor);
  $percent = (100 + $percent) * $major_factor / 100 - 100;
  print "soundstretch /tmp/change_tempo.wav ../$target_dir/$file -tempo=$percent\n";
  `soundstretch /tmp/change_tempo.wav ../$target_dir/$file -tempo=$percent`;
}

