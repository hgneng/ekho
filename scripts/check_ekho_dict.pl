#!/usr/bin/perl

open(ZHY_LIST, '<', '../../eSpeak-Chinese/zhy_list');
while ($line = <ZHY_LIST>) {
  print $line;
  chomp($line);
  next if ($line !~ /^\(/);
  ($word, $phon) = split("\t", $line);
  $word =~ s/\(//;
  $word =~ s/\)//;
  $word =~ s/\s//g;
  $phon =~ s/\|/ /g;
  $ekho_phon = `../build/ekho -vCantonese -l $word`;
  chomp($ekho_phon);
  $ekho_phon =~ s/\s$//g;

  if ($phon ne $ekho_phon) {
    $seg = `../build/ekho -vCantonese -w $word`;
    print "$seg: $phon / $ekho_phon\n";
  }
}
close(ZHY_LIST);
