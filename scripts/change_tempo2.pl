use strict;
use warnings;
use eGuideDog::Dict::Cantonese;
use eGuideDog::Dict::Mandarin;

my ($lang, $source_dir, $target_dir, $speed) = @ARGV;

if (not $lang || not $source_dir || not $target_dir) {
  die "change_tempo.pl <lang> <source_dir> <target_dir>";
}

$speed = 2 if (not $speed);

my $dict;
our %timing;
@ARGV = ($source_dir);
my ($min, $avg, $max);
if ($lang eq 'jyutping') {
#  require '/home/hgneng/e-guidedog/eGuideDog_TTS/count_jyutping_timing.pl';
  $dict = eGuideDog::Dict::Cantonese->new();
# from result of perl -e 'use eGuideDog::Dict::Cantonese; $d = eGuideDog::Dict::Cantonese->new(); $d->print_symbol_size_list'
  $min = 6006; # too small, make it no less than 4000
  $max = 19856;
  $avg = 14018;
} elsif ($lang eq 'pinyin') {
  @ARGV = ('pinyin-yali-44100');
  require '/home/hgneng/e-guidedog/eGuideDog_TTS/count_pinyin_timing.pl';
  $dict = eGuideDog::Dict::Mandarin->new();
}

mkdir $target_dir;
chdir $source_dir;

while (my $file = <*.wav>) {
  $file =~ /^([^.]*)/;
  my $symbol = $1;
  my $len = (-s $file) - 44;
#  die $symbol if (not $timing{$symbol});
#  my $percent = int(100 * $len / $timing{$symbol} / $speed - 100);
  my $percent;
  if ($lang eq 'pinyin') {
    $percent = int(100 * $len / ((-s "../pinyin-yali-44100/$file") - 44) / $speed - 100);
  } else {
    system("espeak -s 120 -vzhy \"[['$symbol]]\" -w /tmp/espeak_size.wav");
    my $espeak_size = (-s '/tmp/espeak_size.wav') - 44;
    my $v5_size = (-s "../jyutping-wong-44100-v5/$symbol.wav") - 44;
    $percent = int(100 * $len * 11 / (3 * $len / 2 + 3 * $espeak_size + 5 * $v5_size) - 100);
#    my $real_size = $dict->get_symbol_size($symbol);
#    if ($real_size < $avg) {
#      $real_size = ($avg + $real_size - $min - $min) / ($avg - $min) * $avg / 2
#    }
#    $percent = int(100 * $len / $real_size / $speed - 100);
  }
#  print "$symbol\t$percent\%\t" . int($timing{$symbol}) . "\t" . $dict->get_symbol_size($symbol) . "\tdiff: " . ($timing{$symbol} / $dict->get_symbol_size($symbol)) . "\n";
  `soundstretch $file /tmp/tempo.wav -tempo=$percent`;
  system("sync");
  `perl /home/hgneng/e-guidedog/eGuideDog_TTS/remove_silence.pl jyutping /tmp/tempo.wav ../$target_dir/$file`;
  print "$symbol\ttempo changed: $percent\%\tslience removed: ", int(100 - 100 * (-s "../$target_dir/$file") / (-s "/tmp/tempo.wav")), "\%\n";
}
