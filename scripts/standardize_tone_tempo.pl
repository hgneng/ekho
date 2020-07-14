use strict;
use warnings;

my ($lang, $source_dir, $target_dir) = @ARGV;

if (not $lang || not $source_dir || not $target_dir) {
  die "standardize_tone_tempo.pl <lang> <source_dir> <target_dir>";
}


my @tones;

if ($lang eq 'jyutping') {
} elsif ($lang eq 'pinyin') {
  @tones = (67457, 65369, 60231, 66495, 40983); # pinyin-huang-44100-v2
}

mkdir $target_dir;
chdir $source_dir;

while (my $file = <*.wav>) {
  if ($file =~ /^(.+)([1-7])\.wav/) {
    my $symbol = $1;
    my $tone = $2;
    my $len = (-s $file);
    my $tempo = 1 + ($len - $tones[$tone - 1]) / $len;
    print "process $file $tempo\n";

    `sox $file ../$target_dir/$file tempo $tempo`;
  }
}
