my ($src_dir, $tgt_dir) = @ARGV;

mkdir($tgt_dir);

opendir(SRC_DIR, $src_dir);
while (my $file = readdir(SRC_DIR)) {
  if (my ($symbol) = ($file =~ /([^.]*)[.]wav/)) {
    system("lame $src_dir/$file $tgt_dir/$symbol.mp3");
  }
}
closedir(SRC_DIR);
