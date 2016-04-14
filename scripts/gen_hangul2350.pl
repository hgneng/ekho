binmode(stdout, 'utf8');
open(FILE, '<:utf8', 'hae-sung.txt');
open(OUT, '>:utf8', 'hangul2350.txt');
while($line = <FILE>) {
  $line =~ / (.*)$/;
  @chars = split(//, $1);
  foreach (@chars) {
    $i = sprintf("0x%x", ord($_));
    $hangul_name = `grep -i $i KSC5601.TXT`;
    $hangul_name =~ /HANGUL SYLLABLE (.*)/;
    print OUT "$_: $1\n";
    print "$_: $1\n";
  }
}
close(OUT);
close(FILE);
