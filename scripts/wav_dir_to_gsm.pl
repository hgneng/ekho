my ($src_dir, $tgt_dir) = @ARGV;

mkdir($tgt_dir);

opendir(SRC_DIR, $src_dir);
while (my $file = readdir(SRC_DIR)) {
  if (my ($symbol) = ($file =~ /([^.]*)[.]wav/)) {
    print "processing $file\n";
    # convert to lower case for Korean
    $symbol = lc($symbol);
    system("sox $src_dir/$file -e gsm-full-rate -t wav $tgt_dir/$symbol.gsm");
  }
}
closedir(SRC_DIR);
