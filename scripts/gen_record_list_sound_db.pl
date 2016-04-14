#!/usr/bin/perl

use strict;
use utf8;
use eGuideDog::Dict::Cantonese;
use eGuideDog::Dict::Mandarin;
use Speech::eSpeak;
binmode(STDOUT, 'utf8');

my ($lang, $source_dir, $target_dir, $target_dir2, $text_file, $file_num, $char_num) = @ARGV;

if (not $lang || not $source_dir || not $target_dir || not $target_dir2 || not $text_file) {
    print "parameter: <language> <source_dir> <target_db_dir> <target_dulplicate_dir><text_file> [file_num] [$char_num]\n";
    exit 0;
}

my $file_prefix = 'sound';
# start from 1 if not specified
$file_num = 1 if (not defined $file_num);
$char_num = 1 if (not defined $char_num);
# make sure directory name end with '/'
$source_dir .= '/' if ($source_dir !~ /\/$/);
$target_dir .= '/' if ($target_dir !~ /\/$/);
$target_dir2 .= '/' if ($target_dir2 !~ /\/$/);

# init speaker
my $speaker = Speech::eSpeak->new();

# init dictionary
my $dict;
if ($lang eq 'jyutping') {
  $dict = eGuideDog::Dict::Cantonese->new();
  $speaker->language('zhy');
} elsif ($lang eq 'pinyin') {
  $dict = eGuideDog::Dict::Mandarin->new();
  $speaker->language('zh');
} else {
  die "$lang should be jyutping or pinyin";
}

my $char;
open(TEXT, '<:utf8', $text_file);
# skip some lines
for (2 .. $char_num) {
  <TEXT>;
}
while (my $line = <TEXT>) {
  chomp($line);
  $line =~ /(.*):/;
  my $symbol = $1;
  next if (-e ($target_dir2 . $symbol . '.wav'));
 replay:
  print "[$file_num $char_num] $line";
  my $path = $source_dir . "$file_prefix.$file_num.wav";
  `play $path`;
  if ($lang eq 'jyutping') {
    $speaker->speak("[['$symbol]]");
  } else {
    $speaker->speak($symbol);
  }

  # wait for command
  my $command = <STDIN>;
  if ($command eq "\n") { # record
    my $target_prefix = $target_dir . $symbol;
    my $target_prefix2 = $target_dir2 . $symbol;
    my $num = 1;
    $num++ while (-f "$target_prefix.$num.wav");
    `cp $path $target_prefix.$num.wav`;
    `cp $path $target_prefix2.wav`;
    $file_num++;
  } elsif ($command =~ /^r/) { # replay
    goto replay;
  } elsif ($command =~ /^s/) { # skip one sound
    $file_num++;
    goto replay;
  } elsif ($command =~ /^d/) { # delete one character
    # go on
#  } elsif ($command =~ /^c/) {
#    my $symbol = <STDIN>;
#    chomp($symbol);
#    my $target_prefix = $target_dir . $symbol;
#    my $num = 1;
#    $num++ while (-f "$target_prefix.$num.wav");
#    `cp $path $target_prefix.$num.wav`;
#    $file_num++;
  }
  $char_num++;
}
close(TEXT);
