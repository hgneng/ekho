# This script is help to delete wong recording files

my $start_num = 0;

$start_num = $ARGV[0] if ($ARGV[0]);

my $num = 0;
while (my $file = <jyutping_db/*>) {
    $num++;
    next if ($num < $start_num);
    next if ($file !~ /wav$/);
    print $num, ': ', $file;
    `play $file`;
    $file =~ /jyutping_db\/([^.]*)/;
    `espeak -vzhy "[['$1]]"`;
  re:
    my $cmd = <stdin>;
    if ($cmd =~ /d/) {
        `mv $file $file.del`
    } elsif ($cmd =~ /r/) {
        `play $file`;
        $file =~ /jyutping_db\/([^.]*)/;
        `espeak -vzhy "[['$1]]"`;
        goto re;
    }
}
