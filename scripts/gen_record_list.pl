# Generate the list syllables the need to be been recorded according to jyutping_db/ or pinyin_db/
# some status message will output through stderr

use strict;
use eGuideDog::Dict::Cantonese;
use eGuideDog::Dict::Mandarin;
use utf8;

binmode(stdout, 'utf8');
binmode(stderr, 'utf8');

my ($lang) = @ARGV;
my $dict;

if ($lang && $lang eq 'jyutping') {
    $dict = eGuideDog::Dict::Cantonese->new()->{jyutping};
} elsif ($lang && $lang eq 'pinyin') {
    $dict = eGuideDog::Dict::Mandarin->new()->{pinyin};
} else {
    print "Please specify the phonetic system: jyutping or pinyin\n";
    exit 0;
}


my $found = 0;
my $not_found = 0;
my %sym;

foreach my $char (keys %{$dict}) {
  my $symbol = $dict->{$char};
  # all syllable less than 3 samples should be recorded
  if (`find jyutping_db/ -name "$symbol.*.wav" | wc -l` >= 3) {
    $found++;
  } else {
    if ($sym{$symbol}) {
      push(@{$sym{$symbol}}, $char);
    } else {
      my @c;
      push(@c, $char);
      $sym{$symbol} = \@c;
    }
    $not_found++;
    printf stderr "$char: $symbol\t";
  }
}

#print "found: $found\nnot found: $not_found\n";

my $sym_len = 0;
foreach my $symbol (sort (keys %sym)) {
  $sym_len++;
  print "$symbol: @{$sym{$symbol}}\n";
}

print stderr "total symbols: $sym_len\n";
