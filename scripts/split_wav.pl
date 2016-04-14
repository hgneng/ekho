use strict;
use eGuideDog::Wav;

my ($filename) = @ARGV;

if (not $filename) {
    print "Please provide parameter $filename - the wav file to split";
    exit 0;
}

my $wav = eGuideDog::Wav->new($filename);
$wav->split_by_silence();
