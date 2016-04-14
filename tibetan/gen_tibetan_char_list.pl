#!/user/bin/perl

use strict;
use warnings;

open(INFILE, 'tibetan_index.txt');

my %char_list;
while (my $line = <INFILE>) {
  my @columns = split(/\s+/, $line);
  my @chars = split(/་/, $columns[3]);
  foreach my $char (@chars) {
    if (exists $char_list{$char}) {
      $char_list{$char}++;
    } else {
      $char_list{$char} = 1;
    }
  }
#  print "found @chars from $line";
}

close(INFILE);

my %symbol_list;
my $total_symbol = 0;
my $total = 0;
my $total_record = 0;
open(OUTFILE, '>', 'bo_freq_list3');
#foreach my $char (sort { $char_list{$b} <=> $char_list{$a} } keys %char_list) {
foreach my $char (sort keys %char_list) {
  if ($char !~ /[a-zA-Z]/) {
    my $symbol = tibetan_char_to_symbol($char);
    if (exists $symbol_list{$symbol}) {
      print OUTFILE "\t", $char, "\t", $char_list{$char}, "\t", $symbol,
          "\t", $symbol_list{$symbol}, "\n";
      $symbol_list{$symbol}++;
    } else {
      if ($symbol =~ /^[a-z]+$/) {
        print OUTFILE $char, "\t", $char_list{$char}, "\t", $symbol, "\n";
        $total_record++;
      } else {
        print OUTFILE "\t", $char, "\t", $char_list{$char}, "\t", $symbol,
        "\n";
      }
      $symbol_list{$symbol} = $char;
      $total_symbol++;
    }

#    print $char, "\t", $char_list{$char}, "\t", $symbol, "\n";
    $total++;
  }
}
close(OUTFILE);
print "Total: $total\n";
print "Total symbol: $total_symbol\n";
print "Total record: $total_record\n";

# Reference: http://www.eki.ee/wgrs/rom1_bo.htm
sub tibetan_char_to_symbol {
  my $char = shift;
  
  # beginning consonants
  $char =~ s/^ཀྱ/gya/;
  $char =~ s/^ཁྱ/kya/;
  $char =~ s/^གྱ/kya/; # gyaA
  $char =~ s/^ཀྲ/zha/;
  $char =~ s/^ཁྲ/cha/;
  $char =~ s/^གྲ/cha/; # zhaA
  $char =~ s/^ཧྲ/sha/;
  $char =~ s/^ལྷ/lha/;
  # TODO: not complete ...
  
  # syllable endings
  $char =~ s/ཨ$/a/;
  $char =~ s/ཨའུ$/au/;
  $char =~ s/ཨག$/ag/;
  $char =~ s/ཨགས$/ag/;
  $char =~ s/ཨང$/ang/;
  $char =~ s/ཨངས$/ang/;
  $char =~ s/ཨབ$/ab/;
  $char =~ s/ཨབས$/ab/;
  $char =~ s/ཨམ$/am/;
  $char =~ s/ཨམས$/am/;
  $char =~ s/ཨར$/ar/;
  $char =~ s/ཨལ$/ai/;
  $char =~ s/ཨའི$/ai/;
  $char =~ s/ཨད$/ai/;
  $char =~ s/ཨས$/ai/;
  $char =~ s/ཨན$/ain/;
  $char =~ s/ཨི$/i/;
  $char =~ s/ཨིལ$/i/;
  $char =~ s/ཨིའི$/i/;
  $char =~ s/ཨིའུ$/iu/;
  $char =~ s/ཨེའུ$/iu/;
  $char =~ s/ཨིག$/ig/;
  $char =~ s/ཨིགས$/ig/;
  $char =~ s/ཨིད$/i/;
  $char =~ s/ཨིས$/i/;
  $char =~ s/ཨིང$/ing/;
  $char =~ s/ཨིངས$/ing/;
  $char =~ s/ཨིབ$/ib/;
  $char =~ s/ཨིབས$/ib/;
  $char =~ s/ཨིམ$/im/;
  $char =~ s/ཨིམས$/im/;
  $char =~ s/ཨིར$/ir/;
  $char =~ s/ཨིན$/in/;
  $char =~ s/ཨུ$/u/;
  $char =~ s/ཨུག$/ug/;
  $char =~ s/ཨུགས$/ug/;
  $char =~ s/ཨུང$/ung/;
  $char =~ s/ཨུངས$/ung/;
  $char =~ s/ཨུབ$/ub/;
  $char =~ s/ཨུབས$/ub/;
  $char =~ s/ཨུམ$/um/;
  $char =~ s/ཨུམས$/um/;
  $char =~ s/ཨུར$/ur/;
  $char =~ s/ཨུལ$/ü/;
  $char =~ s/ཨུའི$/ü/;
  $char =~ s/ཨུད$/ü/;
  $char =~ s/ཨུས$/ü/;
  $char =~ s/ཨུན$/ün/;
  $char =~ s/ཨེ$/ê/;
  $char =~ s/ཨེལ$/ê/;
  $char =~ s/ཨེའི$/ê/;
  $char =~ s/ཨེག$/êg/;
  $char =~ s/ཨེགས$/êg/;
  $char =~ s/ཨེད$/ê/;
  $char =~ s/ཨེས$/ê/;
  $char =~ s/ཨེང$/êng/;
  $char =~ s/ཨེངས$/êng/;
  $char =~ s/ཨེབ$/êb/;
  $char =~ s/ཨེབས$/êb/;
  $char =~ s/ཨེམ$/êm/;
  $char =~ s/ཨེམས$/êm/;
  $char =~ s/ཨེར$/êr/;
  $char =~ s/ཨེན$/ên/;
  $char =~ s/ཨོ$/o/;
  $char =~ s/ཨོག$/og/;
  $char =~ s/ཨོགས$/og/;
  $char =~ s/ཨོང$/ong/;
  $char =~ s/ཨོངས$/ong/;
  $char =~ s/ཨོབ$/ob/;
  $char =~ s/ཨོབས$/ob/;
  $char =~ s/ཨོམ$/om/;
  $char =~ s/ཨོམས$/om/;
  $char =~ s/ཨོར$/or/;
  $char =~ s/ཨོལ$/oi/;
  $char =~ s/ཨོའི$/oi/;
  $char =~ s/ཨོད$/oi/;
  $char =~ s/ཨོས$/oi/;
  $char =~ s/ཨོན$/oin/;
  
  # consonants
  $char =~ s/ཀ/ga/g;
  $char =~ s/ཁ/ka/g;
  $char =~ s/ག/ka/g; # gaA
  $char =~ s/ང/nga/g;
  $char =~ s/ཅ/ja/g;
  $char =~ s/ཆ/qa/g;
  $char =~ s/ཇ/qa/g; # jaA
  $char =~ s/ཉ/nya/g;
  $char =~ s/ཏ/da/g;
  $char =~ s/ཐ/ta/g;
  $char =~ s/ད/ta/g; # daA
  $char =~ s/ན/na/g;
  $char =~ s/པ/ba/g;
  $char =~ s/ཕ/pa/g;
  $char =~ s/བ/pa/g; # baA
  $char =~ s/མ/ma/g;
  $char =~ s/ཙ/za/g;
  $char =~ s/ཚ/ca/g;
  $char =~ s/ཛ/ca/g; # zaA
  $char =~ s/ཝ/wa/g;
  $char =~ s/ཞ/xa/g;
  $char =~ s/ཟ/sa/g;
  $char =~ s/འ/a/g;
  $char =~ s/ཡ/ya/g;
  $char =~ s/ར/ra/g;
  $char =~ s/ལ/la/g;
  $char =~ s/ཤ/xa/g;
  $char =~ s/ས/sa/g;
  $char =~ s/ཧ/ha/g;
  $char =~ s/ཨ/a/g;

  # vowels
  #$char =~ s/(. ི)/i/g;
  #$char =~ s/(. ུ)/u/g;
  #$char =~ s/(. ེ)/e/g;
  #$char =~ s/(. ོ)/o/g;
  $char =~ s/aི/i/g;
  $char =~ s/aུ/u/g;
  $char =~ s/aེ/e/g;
  $char =~ s/aོ/o/g;
  
  return $char;
}
