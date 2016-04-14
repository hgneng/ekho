use strict;
use warnings;

my ($src_dir, $tgt_dir) = @ARGV;

mkdir($tgt_dir);

open(PHON_LIST, "/home/hgneng/e-guidedog/eGuideDog-Dict-Cantonese/jyutping_min_phon_list.txt");
while (my $line = <PHON_LIST>) {
  if ($line =~ /^([^:]*):/) {
    my $file = $1 . '.wav';
    print "copying $file...\n";
    system("cp $src_dir/$file $tgt_dir/");
  }
}
close(PHON_LIST);
