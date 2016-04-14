# translate utf8 hangul character filename to Romanization
use strict;
use warnings;
use Switch;
use utf8;
use integer;

sub get_initial_roman;
sub get_medial_roman;
sub get_final_roman;
sub get_initial_jamo;
sub get_medial_jamo;
sub get_final_jamo;

binmode('stdout', 'utf8');

my ($source, $target) = @ARGV;
die "parameter: source_dir, target_dir" if (not $source || not $target);

#chdir $source;
#mkdir "../$target";
open(CHAR_LIST, '<:utf8', 'hangul2350.txt'); 
while (my $line = <CHAR_LIST>) {
  $line =~ /(.): (.*)/;
  my $src_char = $1;
  my $src_name = $2;
  die "error char: $src_char" if (not $src_char);
  my $code = ord($src_char);
  if ($code >= 0xAc00 && $code <= 0xD7A3) {
    my $initial = (($code - 0xAC00) / 28) / 21;
    my $medial = (($code - 0xAC00) / 28) % 21;
    my $final = ($code - 0xAC00) % 28;
    my ($tgt_full_name, $tgt_name);
    if (get_final_jamo($final)) {
      $tgt_full_name = get_initial_jamo($initial) . '-' . get_medial_jamo($medial) . '-' . get_final_jamo($final);
    } else {
      $tgt_full_name = get_initial_jamo($initial) . '-' . get_medial_jamo($medial);
    }
    $tgt_name = get_initial_roman($initial) . get_medial_roman($medial) . (get_final_roman($final) || '');
    print "$src_char: $tgt_full_name, $tgt_name\n";
  } else {
    die "$src_char: $code";
  }
}
close(CHAR_LIST);

sub get_initial_jamo {
  my $initial = shift;
  switch ($initial) {
    case 0 {return 'KIYEOK'}
    case 1 {return 'SSANGKIYEOK'}
    case 2 {return 'NIEUN'}
    case 3 {return 'TIKEUT'}
    case 4 {return 'SSANGTIKEUT'}
    case 5 {return 'RIEUL'}
    case 6 {return 'MIEUM'}
    case 7 {return 'PIEUP'}
    case 8 {return 'SSANGPIEUP'}
    case 9 {return 'SIOS'}
    case 10 {return 'SSANGSIOS'}
    case 11 {return 'IEUNG'}
    case 12 {return 'CIEUC'}
    case 13 {return 'SSANGCIEUC'}
    case 14 {return 'CHIEUCH'}
    case 15 {return 'KHIEUKH'}
    case 16 {return 'THIEUTH'}
    case 17 {return 'PHIEUPH'}
    case 18 {return 'HIEUH'}
    else {die "initial: $initial"}
  }
}

sub get_medial_jamo {
  my $medial = shift;
  switch ($medial) {
    case 0 {return 'A'}
    case 1 {return 'AE'}
    case 2 {return 'YA'}
    case 3 {return 'YAE'}
    case 4 {return 'EO'}
    case 5 {return 'E'}
    case 6 {return 'YEO'}
    case 7 {return 'YE'}
    case 8 {return 'O'}
    case 9 {return 'WA'}
    case 10 {return 'WAE'}
    case 11 {return 'OE'}
    case 12 {return 'YO'}
    case 13 {return 'U'}
    case 14 {return 'WEO'}
    case 15 {return 'WE'}
    case 16 {return 'WI'}
    case 17 {return 'YU'}
    case 18 {return 'EU'}
    case 19 {return 'YI'}
    case 20 {return 'I'}
    else {die "medial: $medial"}
  }
}

sub get_final_jamo {
  my $final = shift;
  switch ($final) {
    case 0 {return ""}
    case 1 {return 'KIYEOK'}
    case 2 {return 'SSANGKIYEOK'}
    case 3 {return 'KIYEOKSIOS'}
    case 4 {return 'NIEUN'}
    case 5 {return 'NIEUNCIEUC'}
    case 6 {return 'NIEUNHIEUH'}
    case 7 {return 'TIKEUT'}
    case 8 {return 'RIEUL'}
    case 9 {return 'RIEULKIYEOK'}
    case 10 {return 'RIEULMIEUM'}
    case 11 {return 'RIEULPIEUP'}
    case 12 {return 'RIEULSIOS'}
    case 13 {return 'RIEULTHIEUTH'}
    case 14 {return 'RIEULPHIEUPH'}
    case 15 {return 'RIEULHIEUH'}
    case 16 {return 'MIEUM'}
    case 17 {return 'PIEUP'}
    case 18 {return 'PIEUPSIOS'}
    case 19 {return 'SIOS'}
    case 20 {return 'SSANGSIOS'}
    case 21 {return 'IEUNG'}
    case 22 {return 'CIEUC'}
    case 23 {return 'CHIEUCH'}
    case 24 {return 'KHIEUKH'}
    case 25 {return 'THIEUTH'}
    case 26 {return 'PHIEUPH'}
    case 27 {return 'HIEUH'}
    else {die "final: $final"}
  }
}

sub get_initial_roman {
  my $initial = shift;
  switch ($initial) {
    case 0 {return 'G'}
    case 1 {return 'GG'}
    case 2 {return 'N'}
    case 3 {return 'D'}
    case 4 {return 'DD'}
    case 5 {return 'R'}
    case 6 {return 'M'}
    case 7 {return 'B'}
    case 8 {return 'BB'}
    case 9 {return 'S'}
    case 10 {return 'SS'}
    case 11 {return ''}
    case 12 {return 'J'}
    case 13 {return 'JJ'}
    case 14 {return 'C'}
    case 15 {return 'K'}
    case 16 {return 'T'}
    case 17 {return 'P'}
    case 18 {return 'H'}
    else {die "initial: $initial"}
  }

}

sub get_medial_roman {
  my $medial = shift;
  switch ($medial) {
    case 0 {return 'A'}
    case 1 {return 'AE'}
    case 2 {return 'YA'}
    case 3 {return 'YAE'}
    case 4 {return 'EO'}
    case 5 {return 'E'}
    case 6 {return 'YEO'}
    case 7 {return 'YE'}
    case 8 {return 'O'}
    case 9 {return 'WA'}
    case 10 {return 'WAE'}
    case 11 {return 'OE'}
    case 12 {return 'YO'}
    case 13 {return 'U'}
    case 14 {return 'WEO'}
    case 15 {return 'WE'}
    case 16 {return 'WI'}
    case 17 {return 'YU'}
    case 18 {return 'EU'}
    case 19 {return 'YI'}
    case 20 {return 'I'}
    else {die "medial: $medial"}
  }

}

sub get_final_roman {
  my $final = shift;
  switch ($final) {
    case 0 {return ""}
    case 1 {return 'G'}
    case 2 {return 'GG'}
    case 3 {return 'GS'}
    case 4 {return 'N'}
    case 5 {return 'NJ'}
    case 6 {return 'NH'}
    case 7 {return 'D'}
    case 8 {return 'L'}
    case 9 {return 'LG'}
    case 10 {return 'LM'}
    case 11 {return 'LB'}
    case 12 {return 'LS'}
    case 13 {return 'LT'}
    case 14 {return 'LP'}
    case 15 {return 'LH'}
    case 16 {return 'M'}
    case 17 {return 'B'}
    case 18 {return 'BS'}
    case 19 {return 'S'}
    case 20 {return 'SS'}
    case 21 {return 'NG'}
    case 22 {return 'J'}
    case 23 {return 'C'}
    case 24 {return 'K'}
    case 25 {return 'T'}
    case 26 {return 'P'}
    case 27 {return 'H'}
    else {die "final: $final"}
  }
}
