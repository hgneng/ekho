#!/usr/bin/perl

use strict;
use warnings;

# please run this script in e-guidedog
my ($start_file) = @ARGV;
$start_file = 'jyutping-wong/aa1.wav' if (not $start_file);

my $jyutping_phon_list = '/home/hgneng/e-guidedog/website/htdocs/files/jyutping_phon_list_min.txt';

my %phons;
open(FILE, $jyutping_phon_list);
while (my $line = <FILE>) {
  if ($line =~ /([a-z0-7]+):/) {
    $phons{$1} = 1;
  }
}
close(FILE);

my $started = 0;
while (my $file = <jyutping-wong/*>) {
  if (not $started) {
    if ($file eq $start_file) {
      $started = 1;
    } else {
      next;
    }
  }

  if ($file =~ /\/(.+)[.]wav$/) {
    my $phon = $1;
    if (not($phons{$phon} or $phon =~ /7$/)) {
      next;
    }
  }

  print "$file (re-listen[default]/edit[e]/next[n])";
  system("play $file $file $file 2>/dev/null &");
  while (my $cmd = <STDIN>) {
    if ($cmd =~ /^e/) {
      `audacity $file`;
    } elsif ($cmd =~ /^n/) {
      last;
    } else {
      `play $file $file $file 2>/dev/null`;
    }
  }
}
