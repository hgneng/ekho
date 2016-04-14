#!/usr/bin/perl

use strict;
use warnings;
use utf8;

my $infile = 'bo_char_list.ori';
my $outfile = 'bo_list';

open(INFILE, '<:utf8', $infile);
open(OUTFILE, '>:utf8', $outfile);

while (my $line = <INFILE>) {
  chomp($line);
  $line =~ s/\s+$//; # remove trailing space
  if ($line =~ /^#/) {
    $line =~ s/^#/\/\//;
  }

  my @items = split(/\t/, $line);
  if (scalar(@items) >= 2) {
    if (length($items[0]) > 1) {
      # multi-char
      my @chars = split(//, $items[0]);
      $line = '(' . join(' ', @chars) . ")\t" . $items[1];
      for (my $i = 0; $i < $#chars; $i++) {
        $line .= '|0';
      }
    }
  }

  print OUTFILE $line, "\n";
}

close(OUTFILE);
close(INFILE);
