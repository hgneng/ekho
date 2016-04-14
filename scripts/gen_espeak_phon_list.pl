open(FILE, '<:utf8', '/var/www/e-guidedog/htdocs/files/jyutping_phon_list.txt');
my $no = 0;
my $end = 0;
my $part = 0;
system('ekho -o /tmp/pause.wav ". . "');
system("sox /tmp/pause.wav -r 44100 /tmp/pause4.wav");
while (not $end) {
    $no++;
    my $join_cmd = "";
    $part++;

    for my $i (1 .. 32) {
        my $text = "";
        system("rm /tmp/new_phon.wav");
        for (1 .. 7) {
            my $phon = <FILE>;
            if (not $phon) {
                $end = 1;
                last;
            } else {
                $phon =~ /^([^:]*):/;
                if (-e "/tmp/new_phon.wav") {
                    system("sox /tmp/new_phon.wav /home/hgneng/sound/jyutping-wong-44100-v5/$1.wav /tmp/newer_phon.wav");
                    system("mv /tmp/newer_phon.wav /tmp/new_phon.wav");
                } else {
                    system("sox /home/hgneng/sound/jyutping-wong-44100-v5/$1.wav /tmp/new_phon.wav");
                }
                $text .= "[['$1]] ";
            }
        }
        system("espeak -s 120 -vzhy -w /tmp/espeak_phon.wav \"$text\"");
        system("sox /tmp/espeak_phon.wav -r 44100 /tmp/espeak_phon4.wav");
        system("sox /tmp/new_phon.wav /tmp/pause4.wav /tmp/espeak_phon4.wav /tmp/pause4.wav phon$i.wav");
        $join_cmd .= " phon$i.wav";
    }

    system("sox $join_cmd part$part.wav");
    print "part$part done.\n";
}
close(FILE);

