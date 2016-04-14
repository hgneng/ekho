my ($src_dir, $tgt_dir) = @ARGV;

mkdir($tgt_dir);

opendir(SRC_DIR, $src_dir);
while (my $file = readdir(SRC_DIR)) {
  if (my ($symbol) = ($file =~ /([^.]*)[.]wav/)) {
    system("sox $src_dir/$file -t ogg -r 22050 $tgt_dir/$symbol.ogg");
  }
}
closedir(SRC_DIR);
