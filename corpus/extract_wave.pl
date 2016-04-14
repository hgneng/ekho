#!/usr/bin/perl

use strict;
use warnings;

my ($wave_file, $label_file, $phon_file, $target_dir) = @ARGV;

open(LABEL_FILE, '<', $label_file);
open(PHON_FILE, '<', $phon_file);

while (my $label_line = <LABEL_FILE>) {
  chomp($label_line);
  my @label_items = split("\t", $label_line);
  my $phon_line = <PHON_FILE>;
  chomp($phon_line);
  my @phon_items = split("\t", $phon_line);
  
  if ($label_items[2] eq $phon_items[0]) {
    my $start = $label_items[0];
    my $end = $label_items[1];
    my $filename = join('-', split(' ', $phon_items[2])) . '.wav';
    `sox $wave_file $target_dir/$filename trim $start =$end`;
  } else {
    print 'not align', $label_line, $phon_line, "\n";
  }
}

close(PHON_FILE);
close(LABEL_FILE);
