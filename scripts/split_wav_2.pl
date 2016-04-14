#!/usr/bin/perl
# Split wav file to files according to syllables.

use strict;
use Audio::DSP;
use Math::BigInt;
use Audio::SndFile;
use integer;

my ($filename) = @ARGV;

my $source_wav = Audio::SndFile->open('<', $filename);
my %options = {
    type => $source_wav->type,
    subtype => $source_wav->subtype,
    channels => $source_wav->channels,
    endianness => $source_wav->endianness,
    samplerate => $source_wav->samplerate,
};
my $target_wav;

# a voice is defined as signal above $min_sound last for $min_sound_time and doesn't include silence last for $max_silent_time
my $min_sound = 100;
my $min_sound_sec = 10; # per second
my $max_silent_sec = 50; # per second
my $min_sound_unit = $source_wav->samplerate / $min_sound_sec; # second
my $max_silent_unit = $source_wav->samplerate / $max_silent_sec; # second
my $min_pitch = 80;
my $buf_size = 4096;

my $format;
if ($source_wav->subtype eq 'pcm_16') {
    $format = 16;
} else {
    die "unknown subtype: " . $source_wav->subtype;
}
my $size = $source_wav->frames * $format / 8;

my $dsp = new Audio::DSP(buffer   => $buf_size,
		      channels => $source_wav->channels,
		      format   => $format,
		      rate     => $source_wav->samplerate);
$dsp->init() || die $dsp->errstr();

my $total = Math::BigInt->new(0);
my $avg_total = Math::BigInt->new(0);
my $silent_total = Math::BigInt->new(0);
my $silent_avg_total = Math::BigInt->new(0);
my $silent_begin = 0;
my $silent_end = 0;
my $is_sound = 0;
my $begin = 0;
my $end = 0;
my $sound_data = '';
my $file_num = 1;
$filename =~ /(.*)[.]wav$/;
#my $file_prefix = $1;
my $file_prefix = 'sound';

# read the first buffer
my $buf_end = -1;
my @dt;
my $pitch = 0;

sub expend_buffer {
  if ($source_wav->subtype eq 'pcm_16') {
      @dt = $source_wav->unpack_short($buf_size / 2);
  } else {
      die "unknown subtype";
  }
  $buf_end += $buf_size / 2;
}

# count 0 point throuh times, this can be a sign of pitch
sub count_pitch {
  my $time_span = 20;
  return 0 if ($end < $time_span);
  my $a = $dt[$end - $buf_end - 2];
  my $b = $dt[$end - $buf_end - 1];
  if ($b < 0 && $a > 0) {
    for ($end - $buf_end - $time_span .. $end - $buf_end - 3) {
      return 0 if ($dt[$_] < 0);
    }
  } elsif ($b > 0 && $a < 0) {
    for ($end - $buf_end - $time_span .. $end - $buf_end - 3) {
      return 0 if ($dt[$_] > 0);
    }
  } else {
    return 0;
  }
  $pitch++;
}

expend_buffer();

##### Main loop #####
while ($begin < $size) {
    print "\rProgress: begin - $begin, end - $end";
    while ($total >= $avg_total && $end - $begin < $min_sound_unit && $end < $size) {
        count_pitch();
        $total += abs($dt[$end - $buf_end - 1]);
        $avg_total += $min_sound;
        $end++;
        if ($end > $buf_end) {
            expend_buffer();
        }
    }

#    print "\n";
#    print $total, " >= ", $avg_total, "\n";
#    print $end - $begin, " >= ", $min_sound_unit, "\n";
    if ($end - $begin >= $min_sound_unit) {
        $is_sound = 1;
    } elsif ($is_sound) {
        print "is sound\n";
        $silent_begin = ($end + $begin) / 2;
        $silent_end = $silent_begin;
        $silent_total = 0;
        $silent_avg_total = 0;
        while ($silent_total <= $silent_avg_total && $silent_end - $silent_begin < $max_silent_unit && $silent_end < $size) {
            $silent_total += abs($dt[$silent_end - $buf_end - 1]);
            $silent_avg_total += $min_sound;
            $silent_end++;
            expend_buffer() if ($silent_end > $buf_end);
        }
        if ($silent_end - $silent_begin >= $max_silent_unit) {
            if ($pitch >= $min_pitch) {
                $sound_data .= pack('s' x (($end - $begin) / 2), @dt[0 .. ($end - $begin) / 2 - 1]);
                $target_wav = Audio::SndFile->open('>', "$file_prefix.$file_num.wav", %options);
                if ($options{subtype} eq 'pcm_16') {
                    $target_wav->write_short($sound_data);
                }
                print "\tpitch: $pitch\n";
                system("ls -l $file_prefix.$file_num.wav");
                $file_num++;
                $dsp->dwrite($sound_data);
            }

            $sound_data = '';
            $is_sound = 0;
            shift @dt for (1 .. ($end - $begin));
            $begin = $end;
            $pitch = 0;
            $total = 0;
            $avg_total = 0;
        } else {
            $total += abs($dt[$end - $buf_end - 1]);
            $avg_total += $min_sound;
            $end++;
            expend_buffer() if ($end > $buf_end);
        }
    } else {
        $total = 0;
        $avg_total = 0;
        shift @dt for (1 .. ($end - $begin));
        $begin = $end;
        $pitch = 0;
    }

    if ($is_sound && $end - $begin >= $min_sound_unit) {
        for (1 .. ($end - $begin) / 2) {
            $sound_data .= pack('s', shift @dt);
        }
        $begin = ($begin + $end) / 2;
        $end = $begin;
        $total = 0;
        $avg_total = 0;
    }
}

# process the last sound part
if ($is_sound) {
    $target_wav = Audio::SndFile->open('>', "$file_prefix.$file_num.wav", %options);
    $sound_data .= shift @dt for (1 .. ($end - $begin) / 2);
    if ($options{subtype} eq 'pcm_16') {
        $target_wav->write_short($sound_data);
    }
    $dsp->dwrite($sound_data);
}

$dsp->close();
