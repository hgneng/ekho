#!/usr/bin/perl

use strict;
use warnings;
use utf8;
use Encode::CNMap;

my $infile = 'ngangien_list.ori';
my $outfile = 'ngangien_list';

# read input file
open(INFILE, '<:utf8', $infile);
open(OUTFILE, '>:utf8', $outfile);
<INFILE>;
print OUTFILE "// Copyright (C) 2011 by Zhao Qianshan\n";
while (my $line = <INFILE>) {
  print OUTFILE $line;
  my $simp_line = utf8_to_simputf8($line);
  my $trad_line = utf8_to_tradutf8($line);
  if ($simp_line ne $line && $simp_line !~ /\?/) {
    print OUTFILE $simp_line;
  }
  if ($trad_line ne $line && $trad_line !~ /\?/) {
    print OUTFILE $trad_line;
  }
}
close(OUTFILE);
close(INFILE);
