use strict;
use Math::BigInt; # pay attention division expression with BigInt
use Math::BigFloat;
use eGuideDog::Wav;

sub smooth;
sub compress;

my ($lang, $compress_rate, $tempo_rate) = @ARGV;
my $db_dir;
if ($lang && $lang eq 'jyutping') {
    $db_dir = 'jyutping_db';
} elsif ($lang && $lang eq 'pinyin') {
    $db_dir = 'pinyin_db';
} else {
    print "Please specify language: jyutping or pinyin.\n";
    exit(0);
}

if (not $compress_rate) {
    $compress_rate = 1; # best quality, 5 is for best compression
}
print "compress_rate: $compress_rate\n";

if (not $tempo_rate) {
    $tempo_rate = 1; # the more, the faster
}
print "tempo_rate: $tempo_rate\n";

my $samples = 0;
my $sizes = Math::BigInt->new(0);
my $total_volume = 0; #Math::BigInt->new(0);

my %st_ssa; # symboltone to sample,size array in order
my %s_sa; # symbol to size array
my %s_va; # symbol to volume array

# collect data
my $wav = eGuideDog::Wav->new();
while (my $wav_file = <$db_dir/*.wav>) {
  if ($wav_file =~ /^$db_dir\/([^.]*)([1-6])[.]([^.]*)[.]wav$/) {
    my $symbol = $1;
    my $tone = $2;
    my $sample = $3;
    my $size = (-s $wav_file) - 44;
    $samples++;
    $sizes += $size;
    my @new;
    if ($st_ssa{"$symbol$tone"}) {
      my $aref = $st_ssa{"$symbol$tone"};
      my $st = shift @{$aref};
      my $sz = shift @{$aref};
      while ($sz < $size && @{$aref}) {
	push(@new, $st, $sz);
	$st = shift @{$aref};
	$sz = shift @{$aref};
      }
      push(@new, $sample, $size, @{$aref});
    } else {
      @new = ($sample, $size);
    }
    $st_ssa{"$symbol$tone"} = \@new;

    if ($s_sa{$symbol}) {
#      push(@{$s_sa{symbol}}, $sample, $size);
      push(@{$s_sa{symbol}}, $size);
    } else {
      my @new = ($size);
      $s_sa{$symbol} = \@new;
    }

    # count volume
    my $volume;
    if (-f "$wav_file.volume") {
      $volume = `cat $wav_file.volume`;
      chomp($volume);
    } else {
      $wav->set_filename($wav_file);
      $volume = $wav->get_volume();
      `echo $volume > $wav_file.volume`;
    }
    $total_volume += $volume;
    if ($s_va{symbol}) {
      push(@{$s_va{symbol}}, $volume);
    } else {
      $s_va{$symbol} = [$volume];
    }
    print "$wav_file volume: $volume\n";
  }
}

print "samples: $samples\n";
#my $avg_size = int($sizes / $samples);
my $avg_vol = int($total_volume / $samples);
# make it speak faster
#$avg_size = int($avg_size / 4);
#print "avg size: $avg_size\n";
print "avg vol: $avg_vol\n";

# count average size for each symbol and push it to the end
#foreach my $s (keys %s_sa) {
#  my $total = 0;
#  my $len = 0;
#  foreach (@{$s_sa{$s}}) {
#    $total += $_;
#    $len++;
#  }
#  push(@{$s_sa{$s}}, $total / $len);
#}

# count average volume for each symbol and push it to the end
foreach my $s (keys %s_va) {
  my $total = 0;
  my $len = 0;
  foreach (@{$s_va{$s}}) {
    $total += $_;
    $len++;
  }

  push(@{$s_va{$s}}, $total / $len);
}

# begin to gen normalized sound data
foreach my $st (keys %st_ssa) {
    my $aref = $st_ssa{$st};
    my $i = 1;
    $st =~ /(.*).$/;

    # get length from espeak
    my $espeak_size = 0;
    if (-r "$db_dir/$st.wav.size") {
        $espeak_size = `cat "$db_dir/$st.wav.size"`;
        chomp($espeak_size);
    } else {
        if ($lang eq 'jyutping') {
            system("espeak -vzhy \"[['$st]]\" -w /tmp/espeak_size.1");
            system("espeak -vzhy \"[['$st'$st]]\" -w /tmp/espeak_size.2");
        } else {
            system("espeak -vzh \"[['$st]]\" -w /tmp/espeak_size.1");
            system("espeak -vzh \"[['$st'$st]]\" -w /tmp/espeak_size.2");
        }
        $espeak_size = (-s '/tmp/espeak_size.2') - (-s '/tmp/espeak_size.1');
        $espeak_size = $espeak_size * $tempo_rate;
        system("echo $espeak_size > $db_dir/$st.wav.size");
    }

#  my $avg = $s_sa{$1}->[-1];
  my $avg_volume = $s_va{$1}->[-1];
  while ($aref->[$i + 2] && $espeak_size > $aref->[$i]) {
    $i += 2;
  }
  my $sample = $aref->[$i - 1];
  my $size = $aref->[$i];
#  if ($i - 2 > 0) {
#    if ($avg - $aref->[$i - 2] < $aref->[$i] - $avg) {
#      $sample = $aref->[$i - 3];
#      $size = $aref->[$i - 2];
#    }
#  }

  # adjust tempo(speed)
#  my $percent = int(100 - 100 * ($avg +  $avg_size + $avg_size) / $size / 3);
    my $percent = int(100 - 100 * ($espeak_size / $size));
  print "soundstretch $db_dir/$st.$sample.wav $lang/$st.wav -tempo=$percent\n";
  `soundstretch $db_dir/$st.$sample.wav /tmp/$lang.wav -tempo=$percent`;

  # smooth
#  smooth("/tmp/$lang.wav", "/tmp/jyutping_s.wav", 44100 / 20);
#  `mv /tmp/$lang.wav /tmp/jyutping_s.wav`;

  # change volume
  my $w = eGuideDog::Wav->new("/tmp/$lang.wav");
  $w->load();
  my $vol;
  if (-f "$db_dir/$st.$sample.wav.volume") {
    $vol = `cat $db_dir/$st.$sample.wav.volume`;
    chomp($vol);
  } else {
    print "recount volume of $st.$sample.wav\n";
    $vol = $w->get_volume();
  }
  my $factor = (($avg_vol + $avg_vol + $avg_volume) / ($vol + $vol + $vol));
  $w->change_volume($factor);
  print "change volume: $factor\n";
#  $w->save('/tmp/jyutping_s.wav');

  # compress to 44100 / x bps
#  `mv /tmp/$lang.wav $lang/$st.wav`;
#  compress("/tmp/jyutping_s.wav", "$lang/$st.wav", 1);
  $w->compress_save("$lang/$st.wav", $compress_rate);
}

# smooth
# parameter len is how many samples to be made smooth.
# use y = a x^2 + b to smooth the beginning and ending of sound
# pcm = pcm * y
# heading function: y = 32767 - 32767 / len^2 * (x - len)^2
# ending function: y = 32767 - 32767 / len^2 * (x + len - pcm_len)^2
sub smooth {
  my ($source, $target, $len) = @_;

  open(SOURCE, '<:bytes', $source);

  # read header (44 bytes), the last 4 bytes is for data size
  my $header;
  my $tmp;
  read(SOURCE, $header, 22);

  # channel
  my $chan;
  read(SOURCE, $chan, 2);
  $header .= $chan;
  $chan = unpack('S', $chan);

  # rate
  my $rate;
  read(SOURCE, $rate, 2);
  $rate = unpack('S', $rate);
  $header .= pack('S', $rate);

  read(SOURCE, $tmp, 6);
  $header .= $tmp;

  # bytes per sample
  my $fmt;
  read(SOURCE, $fmt, 2);
  $header .= $fmt;
  $fmt = 8 * unpack('S', $fmt);

  read(SOURCE, $tmp, 6);
  $header .= $tmp;

  # size
  my $size;
  read(SOURCE, $size, 4);
  $size = unpack('l', $size);
  $header .= pack('l', $size);

  open(TARGET, '>:bytes', $target);
  print TARGET $header;
  use integer;
  for (my $i = 0; $i < $size / 2; $i++) {
    read(SOURCE, $tmp, 2);
    if ($i < $len) {
      $tmp = unpack('s', $tmp);
      $tmp = $tmp * (32767 - 32767 * ($i - $len) * ($i - $len) / ($len * $len)) / 32767;
      $tmp = pack('s', $tmp);
    } elsif ($i >= $size / 2 - $len) {
      $tmp = unpack('s', $tmp);
      $tmp = $tmp * (32767 - 32767 * ($i + $len - $size / 2) * ($i + $len - $size / 2) / ($len * $len)) / 32767;
      $tmp = pack('s', $tmp);
    }
    print TARGET $tmp;
  }
  no integer;

  close(TARGET);
  close(SOURCE);
}

# compress to 1/factor
# factor should be even (1, 3, 5 ...)
sub compress {
  my ($source, $target, $factor) = @_;

  open(SOURCE, '<:bytes', $source);

  # read header (44 bytes), the last 4 bytes is for data size
  my $header;
  my $tmp;
  read(SOURCE, $header, 22);

  # channel
  my $chan;
  read(SOURCE, $chan, 2);
  $header .= $chan;
  $chan = unpack('S', $chan);

  # rate
  my $rate;
  read(SOURCE, $rate, 2);
  $rate = unpack('S', $rate) / $factor;
  $header .= pack('S', $rate);

  read(SOURCE, $tmp, 6);
  $header .= $tmp;

  # bytes per sample
  my $fmt;
  read(SOURCE, $fmt, 2);
  $header .= $fmt;
  $fmt = 8 * unpack('S', $fmt);

  read(SOURCE, $tmp, 6);
  $header .= $tmp;

  # size
  my $size;
  read(SOURCE, $size, 4);
  $size = unpack('l', $size) / $factor;
  $header .= pack('l', $size);

  open(TARGET, '>:bytes', $target);
  print TARGET $header;
  for (my $i = 0; $i < $size / 2; $i++) {
    read(SOURCE, $tmp, $factor - 1);
    read(SOURCE, $tmp, 2);
    print TARGET $tmp;
    read(SOURCE, $tmp, $factor - 1);
  }

  close(TARGET);
  close(SOURCE);
}
