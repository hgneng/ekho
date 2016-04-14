# analyse zhy_list, zh_list and zh_listx from espeak
# generate ekho dictionary file zhy_char, zhy_word, zh_char, zh_word

use strict;
use utf8;
use eGuideDog::Dict::Mandarin;

binmode(stdout, 'utf8');

sub is_legal;
sub import_dict;

# import zhy_list
my $total = 0;
my $index_char = 0;
my @words;
open(CHAR_DICT, '>:utf8', '../build/zhy_char');
open(WORD_DICT, '>:utf8', '../build/zhy_word');
open(LIST, '<:utf8', '../../eSpeak-Chinese/zhy_list');
import_dict();
# add numbers
print CHAR_DICT ord(0), " ling4\n";
print CHAR_DICT ord(1), " jat1\n";
print CHAR_DICT ord(2), " ji6\n";
print CHAR_DICT ord(3), " saam1\n";
print CHAR_DICT ord(4), " sei3\n";
print CHAR_DICT ord(5), " ng5\n";
print CHAR_DICT ord(6), " luk6\n";
print CHAR_DICT ord(7), " cat1\n";
print CHAR_DICT ord(8), " baat3\n";
print CHAR_DICT ord(9), " gau2\n";
close(LIST);
close(WORD_DICT);
close(CHAR_DICT);
print "$total zhy characters\n";

# import zh_list
$total = 0;
$index_char = 0;
@words = ();

my $dict = eGuideDog::Dict::Mandarin->new();

open(CHAR_DICT, '>:utf8', '../build/zh_char');
open(WORD_DICT, '>:utf8', '../build/zh_word');
foreach my $char (keys %{$dict->{chars}}) {
#  print "processing $char...\n";
  if ($dict->{pinyin}->{$char}) {
    print CHAR_DICT ord($char), ' ', $dict->{pinyin}->{$char}, "\n";
  }
  if ($char ne "不") {
    my @words = $dict->get_words($char);
    if (@words) {
      print WORD_DICT $#words + 1, ' ', ord($char), "\n";
      foreach my $word (@words) {
        print WORD_DICT length($word);
        my @pinyin = $dict->get_pinyin($word);
        my @ws = split(//, $word);
        die if ($#ws != $#pinyin);
        for my $i (0 .. $#ws) {
          print WORD_DICT ' ', ord($ws[$i]), ' ', $pinyin[$i];
        }
        print WORD_DICT "\n";
      }
    }
  }
}
close(WORD_DICT);
close(CHAR_DICT);


# import dict
sub import_dict {
    while (my $line = <LIST>) {
        if ($line =~ /^(.)\s+(.*)\s*$/) {
            if ($1 && $2) {
                printf CHAR_DICT ord($1);
                printf CHAR_DICT " $2\n";
                $total++;
            }
        } elsif ($line =~ /^[(](.*)[)]\s+(.*)\s*$/) {
            # format:
            # number_of_words char
            # number_of_chars char phon char phon ...
            my $word = $1;
            my $phon = $2;
            my @chars = split(/ /, $word);
            my @phons;
            if ($phon =~ /[|]/) {
                @phons = split(/[|]/, $phon);
            } else {
                while($phon && $phon =~ /^([a-z]*[0-9])(.*)/) {
                    push(@phons, $1);
                    $phon = $2;
                }
            }
            if (is_legal(\@chars, \@phons)) {
                if ($index_char == ord($chars[0])) {
                    my $ln = ($#chars + 1) . ' ';
                    for (my $i = 0; $i <= $#chars; $i++) {
                        $ln .= ord($chars[$i]) . ' ' . $phons[$i] . ' ';
                    }
                    push(@words, $ln);
                } elsif ($index_char) {
                    print WORD_DICT ($#words + 1), " $index_char\n";
                    print WORD_DICT join("\n", @words), "\n";
                    $index_char = ord($chars[0]);
                    @words = ();
                    my $ln = ($#chars + 1) . ' ';
                    for (my $i = 0; $i <= $#chars; $i++) {
                        $ln .= ord($chars[$i]) . ' ' . $phons[$i] . ' ';
                    }
                    push(@words, $ln);
                } else {
                    $index_char = ord($chars[0]);
                    my $ln = ($#chars + 1) . ' ';
                    for (my $i = 0; $i <= $#chars; $i++) {
                        $ln .= ord($chars[$i]) . ' ' . $phons[$i] . ' ';
                    }
                    push(@words, $ln);
                }
            } else {
                print "dictionary error: @chars - @phons\n";
            }
        }
    }
}

# is_legal
sub is_legal {
  my ($chars, $phons) = @_;

  if ($#{$chars} != $#{$phons}) {
    return 0;
  }

  foreach (@{$phons}) {
    if ($_ !~ /^[a-z]+[1-6]$/) {
      return 0;
    }
  }

  return 1; # success
}
