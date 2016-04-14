#!/usr/bin/perl

use strict;
use warnings;
use utf8;
use Encode::CNMap;

my $infile = 'yayan.csv';
my $outfile1 = 'yayan_list';
my $outfile2 = 'yayan_phon_list';

my %phons;
my %chars;

# read input file
open(INFILE, '<:utf8', $infile);
# skip head line
<INFILE>;
while (my $line = <INFILE>) {
  my @items = split(/,/, $line);
  my $char = $items[0];
  my $phon = $items[1];
  
  next if ($phon !~ /[a-z0-9]+/);
  
  my @phon2 = split(/ /, $phon);
  $phon = $phon2[0];
  
  $chars{$char} = $phon;
  $chars{utf8_to_simputf8($char)} = $phon;
  $chars{utf8_to_tradutf8($char)} = $phon;
  
  foreach my $phon (@phon2) {
    if (!exists($phons{$phon})) {
      my @cs;
      $phons{$phon} = \@cs;
    }
  
    push(@{$phons{$phon}}, $char);
  }
}
close(INFILE);

# output yayan_list
open(OUTFILE1, '>:utf8', $outfile1);
foreach my $char (sort(keys(%chars))) {
  print OUTFILE1 $char, "\t", $chars{$char}, "\n";
}
close(OUTFILE1);

# output yayan_phon_list
open(OUTFILE2, '>:utf8', $outfile2);
foreach my $phon (sort(keys(%phons))) {
  my @cs = @{$phons{$phon}};
  print OUTFILE2 $phon, "\t", "@cs", "\n";
}
close(OUTFILE2);