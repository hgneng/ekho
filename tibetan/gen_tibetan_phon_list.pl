#!/usr/bin/perl
use strict;
use warnings;

open(INFILE, '<', 'bo_char_list');
open(OUTFILE, '>', 'bo_phon_list');
my %phons;

while (my $line = <INFILE>) {
  chomp($line);
  next if ($line =~ /^#/);
  my ($char, $phon) = split(/\t/, $line);
  
  if ($char && $phon) {
    #    print "processing $char\t$phon\n";
  } else {
    #    print "Fail to parse $line\n";
    next;
  }
  
  $char =~ s/^\s+//g;
  $char =~ s/\s+$//g;
  $phon =~ s/^\s+//g;
  $phon =~ s/\s+$//g;
  if (!exists($phons{$phon})) {
    my @chars;
    $phons{$phon} = \@chars;
  }
  push(@{$phons{$phon}}, $char);
}
close(INFILE);

print "Total:" . scalar(keys(%phons)) . "\n";

foreach my $phon (sort(keys(%phons))) {
  my @value = @{$phons{$phon}};
  print OUTFILE $phon, "\t", "@value\n";
}
close(OUTFILE);
