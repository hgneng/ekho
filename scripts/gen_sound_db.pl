#!/usr/bin/perl

use strict;
use utf8;
use eGuideDog::Dict::Cantonese;
use eGuideDog::Dict::Mandarin;
use Speech::eSpeak;
binmode(STDOUT, 'utf8');

my ($lang, $source_dir, $target_dir, $text_file, $file_num) = @ARGV;

if ($lang ne 'jyutping' && $lang ne 'pinyin') {
    print "language should be jyutping or pinyin\n";
    exit 0;
}

if (not $source_dir || not $target_dir || not $text_file) {
    print "syntax: <lang> <source_dir> <target_dir> <text_file> [file_num]\n";
    exit 0;
}

my $file_prefix = 'sound';
# start from 1 if not specified
$file_num = 1 if (not defined $file_num);
# make sure directory name end with '/'
$source_dir .= '/' if ($source_dir !~ /\/$/);
$target_dir .= '/' if ($target_dir !~ /\/$/);

# init dictionary
my $dict;
if ($lang eq 'jyutping') {
    $dict = eGuideDog::Dict::Cantonese->new();
} else {
    $dict = eGuideDog::Dict::Mandarin->new();
}
#$dict->import_zhy_list('/home/hgneng/e-guidedog/eSpeak-Chinese/zhy_list');

# init speaker
my $speaker = Speech::eSpeak->new();
if ($lang eq 'jyutping') {
    $speaker->language('zhy');
} else {
    $speaker->language('zh');
}

my $char;
open(TEXT, '<:utf8', $text_file);
# skip some characters
for (2 .. $file_num) {
    if ($lang eq 'jyutping') {
        while(read(TEXT, $char, 1) && not defined $dict->get_jyutping($char)) {};
    } else {
        while(read(TEXT, $char, 1) && not defined $dict->get_pinyin($char)) {};
    }
}

while (my $str = <TEXT>) {
    my @symbols;
    if ($lang eq 'jyutping') {
        @symbols = $dict->get_jyutping($str);
    } else {
        @symbols = $dict->get_pinyin($str);
    }
    my $i = 0;
    foreach my $symbol (@symbols) {
        if (defined $symbol) {
            replay:
            print "$file_num: ", substr($str, $i, 1), " - ", $symbol;
            my $path = $source_dir . "$file_prefix.$file_num.wav";
            `play $path`;
            if ($lang eq 'jyutping') {
                $speaker->speak("[['$symbol]]");
            } else {
                $speaker->speak("$symbol");
            }

            # wait for command
            my $command = <STDIN>;
            if ($command eq "\n") { # record
                my $target_prefix = $target_dir . $symbol;
                my $num = 1;
                $num++ while (-f "$target_prefix.$num.wav");
                `cp $path $target_prefix.$num.wav`;
                $file_num++;
            } elsif ($command =~ /^r/) { # replay
                goto replay;
            } elsif ($command =~ /^s/) { # skip one sound
                $file_num++;
                goto replay;
            } elsif ($command =~ /^d/) { # delete one character
                # go on
            } elsif ($command =~ /^c/) {
                my $symbol = <STDIN>;
                chomp($symbol);
                my $target_prefix = $target_dir . $symbol;
                my $num = 1;
                $num++ while (-f "$target_prefix.$num.wav");
                `cp $path $target_prefix.$num.wav`;
                $file_num++;
            }
        }
        $i++;
    }
}
close(TEXT);
