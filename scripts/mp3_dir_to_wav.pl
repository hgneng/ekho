my ($src_dir, $tgt_dir) = @ARGV;

mkdir($tgt_dir);

opendir(SRC_DIR, $src_dir);
while (my $file = readdir(SRC_DIR)) {
  if (my ($symbol) = ($file =~ /([^.]*)[.]mp3/)) {
    system("sox $src_dir/$file -c 1 $tgt_dir/$symbol.wav");
  }
}
closedir(SRC_DIR);
