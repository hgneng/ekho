#!/usr/bin/perl

my ($dir) = @ARGV;

while (my $file = <$dir/*.wav>) {
	$file =~ /([^\/]+)\.wav/;
	$symbol = $1;
	open(TMP_SCRIPT, '>count_pitch.praat');
	print TMP_SCRIPT "Read from file... $file\n";
	print TMP_SCRIPT "To Pitch... 0.0 75.0 600\n";
	print TMP_SCRIPT "Get mean... 0.0 0.0 Hertz\n";
	close(TMP_SCRIPT);
	$pitch = `praat count_pitch.praat`;
	$pitch =~ /(.+) Hz/;
	$value = $1;
	print "$symbol=$value\n";
}