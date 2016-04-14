#!/usr/bin/perl

use Encode::CNMap;
use utf8;
binmode(STDIN, ':encoding(utf8)');
binmode(STDOUT, ':encoding(utf8)');
binmode(STDERR, ':encoding(utf8)');

$j = 1;
open(INFILE, '<:utf8', 'wordlist');
open(OUTFILE, '>:utf8', "wordlist.phon.$j");

%phons = {};

$i = 0;
while ($line = <INFILE>) {
  chomp($line);
  $word = `./ekho -w $line`;
  chop($word);
  utf8::decode($word);
  if ($word eq $line) {
    my $line_trad = utf8_to_tradutf8($line);
    $word_trad = `./ekho -w $line_trad`;
    chop($word_trad);
    utf8::decode($word_trad);
    if ($word_trad ne $line_trad) {
      print $line_trad, "\n";
      next;
    }

    $phon = `./ekho -vCantonese -l $line`;
    chomp($phon);
    if (!$phons{$phon}) {
      $i++;

      if ($i > 300) {
        close(OUTFILE);
        $j++;
        open(OUTFILE, '>', "wordlist.phon.$j");
        $i = 1;
      }

      $phons{$phon} = 1;
      print OUTFILE "$i\t$line\t$phon\n";
    }
  }
}

close(OUTFILE);
