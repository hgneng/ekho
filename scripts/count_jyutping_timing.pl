use strict;
use warnings;

sub count_all_timing;
sub count_tone_timing;
sub count_final_timing;
sub count_initial_timing;
sub count_final_timing_again;
sub count_timing;

my ($voice_dir) = @ARGV;

my @initials = ('b', 'p', 'm', 'f',
   'd', 't', 'n', 'l',
   'g', 'k', 'ng', 'h',
   'gw', 'kw', 'w',
   'z', 'c', 's', 'j');
my @finals = ('aa', 'aai', 'aau', 'aam', 'aan', 'aang', 'aap', 'aat', 'aak',
    'ai', 'au', 'am', 'an', 'ang', 'ap', 'at', 'ak',
    'e', 'ei', 'eu', 'em', 'eng', 'ep', 'ek',
    'i', 'iu', 'im', 'in', 'ing', 'ip', 'it', 'ik',
    'o', 'oi', 'ou', 'on', 'ong', 'ot', 'ok',
    'u', 'ui', 'un', 'ung', 'ut', 'uk',
    'oe', 'oeng', 'oek',
    'eoi', 'eon', 'eot',
    'yu', 'yun', 'yut',
    'm', 'ng');
our %final_timing;
our %initial_timing;
our %timing;
our @tone_factor;
my $different = 75;
my $total_difference = -1;
my $difference;
my $factor = 0.5;

count_all_timing();
count_tone_timing();
print "\n";
count_final_timing();
my $total = 1;
$| = 1;
for (1 .. 200) {
  my %final_timing_bak = %final_timing;
  my %initial_timing_bak = %initial_timing;
  my $different_bak = $different;
  my $total_difference_bak = $total_difference;
  $different = 0;
  $total_difference = 0;
  count_initial_timing();
  count_final_timing_again();

# restore if difference is bigger
  if ($total_difference_bak > 0 && $total_difference > $total_difference_bak) {
    %final_timing = %final_timing_bak;
    %initial_timing = %initial_timing_bak;
    $different = $different_bak;
    $total_difference = $total_difference_bak;
    $factor /= 2;
  } else {
    $factor = 0.5;
  }

  $total++;
  if ($total_difference < $total_difference_bak) {
    print "\n$total, $different, $total_difference";
  } else {
    print "\r$total, $different, $total_difference";
  }
  if ($different == 1) {
    print "diff: $difference\n";

  }
}


print "\ninitial timing:\n";
foreach my $initial (@initials) {
  print "$initial:\t" . int($initial_timing{$initial}) . "\n";
}
print "\nfinal timing:\n";
foreach my $final (@finals) {
  print "$final:\t" . int($final_timing{$final}) . "\n";
}

count_timing();

print "\ninitial timing:\n";
foreach my $initial (@initials) {
  print "$initial:\t" . int($initial_timing{$initial}) . "\n";
}
print "\nfinal timing:\n";
foreach my $final (@finals) {
  print "$final:\t" . int($final_timing{$final}) . "\n";
}
#for my $initial (@initials) {
#  for my $final ('m', 'ng') {
#    for my $tone (1 .. 6) {
#      if ($timing{$initial . $final . $tone}) {
#        print "$initial . $final . $tone: $timing{$initial . $final . $tone}\n";
#      }
#    }
#  }
#}

##### end of main #####

sub count_all_timing {
# read all timing first
  opendir(VOICE_DIR, $voice_dir);
  while (my $file = readdir(VOICE_DIR)) {
    if ($file =~ /([^.]*)[.]wav$/) {
      $timing{$1} = (-s "$voice_dir/$file") - 44;
    }
  }
  closedir(VOICE_DIR);
}

sub count_tone_timing {
# count tone factor
# take tone 1 as 1
  my @tone_total_percent = (1, 0, 0, 0, 0, 0);
  my @tone_total_count = (1, 0, 0, 0, 0, 0);
  foreach my $initial (@initials) {
    foreach my $final (@finals) {
      if ($timing{$initial . $final . 1}) {
        foreach my $tone (2 .. 6) {
          if ($timing{$initial . $final . $tone}) {
            $tone_total_count[$tone - 1]++;
            $tone_total_percent[$tone - 1] += $timing{$initial . $final . $tone} / $timing{$initial . $final . 1};
          }
        }
      }
    }
  }
  print "tone timing: ";
  foreach my $tone (0 .. 5) {
    $tone_factor[$tone] = $tone_total_percent[$tone] / $tone_total_count[$tone];
    printf("%.2f(%d)\t", $tone_total_percent[$tone] / $tone_total_count[$tone], $tone_total_count[$tone]);
  }
  print "\n";
}

sub count_final_timing {
#  print "final timing:\n";
  foreach my $final (@finals) {
    my $final_total_timing = 0;
    my $final_total_count = 0;
    for my $tone (1 .. 6) {
      if ($timing{$final . $tone}) {
        $final_total_timing += $timing{$final . $tone} / $tone_factor[$tone - 1];
        $final_total_count++;
      }
    }
    if ($final_total_timing) {
      $final_timing{$final} = $final_total_timing / $final_total_count;
    } else {
      $final_timing{$final} = 0;
    }
#    print "$final: $final_timing{$final} ($final_total_count)\n";
  }
}

sub count_final_timing_again {
#  print "final timing again:\n";
  foreach my $final (@finals) {
    my $final_total_timing = 0;
    my $final_total_count = 0;
    foreach my $initial (@initials) {
      for my $tone (1 .. 6) {
        if ($timing{$initial . $final . $tone}) {
          my $time = $timing{$initial . $final . $tone} / $tone_factor[$tone - 1] - $initial_timing{$initial};
          if ($time > 0) {
            $final_total_timing += $time;
            $final_total_count++;
          } else {
#            print "[warning] $initial . $final . $tone < 0\n";
          }
        }
      }
    }
    $final_timing{$final} = 0 if (not defined $final_timing{$final});
    if ((int($final_timing{$final}) != int($final_total_timing / $final_total_count))) {
      $different++;
      $total_difference += abs($final_total_timing / $final_total_count - $final_timing{$final});
      $difference = $final;
    }
    if ($final_timing{$final}) {
      $final_timing{$final} = $final_timing{$final} * (1 - $factor) + $final_total_timing * $factor / $final_total_count;
    } else {
      $final_timing{$final} = $final_total_timing / $final_total_count;
    }
#    print "$final: $final_timing{$final} ($final_total_count)\n";
  }
}

sub count_initial_timing {
#  print "initial timing:\n";
  foreach my $initial (@initials) {
    my $initial_total_timing = 0;
    my $initial_total_count = 0;
    foreach my $final (@finals) {
      if ($final_timing{$final}) {
        for my $tone (1 .. 6) {
          if ($timing{$initial . $final . $tone}) {
            my $time += $timing{$initial . $final . $tone} / $tone_factor[$tone - 1] - $final_timing{$final};
            if ($time > 0) {
              $initial_total_timing += $time;
              $initial_total_count++;
            } else {
#              print "[warning] $initial . $final . $tone < 0\n";
            }
          }
        }
      }
    }
    $initial_timing{$initial} = 0 if (not defined $initial_timing{$initial});
    if ((int($initial_timing{$initial}) != int(($initial_total_timing / $initial_total_count)))) {
      $different++;
      $total_difference += abs($initial_total_timing / $initial_total_count - $initial_timing{$initial});
      $difference = $initial;
    }
    if (not $initial_timing{$initial}) {
      $initial_timing{$initial} = $initial_total_timing / $initial_total_count;
    } else {
      $initial_timing{$initial} = $initial_timing{$initial}  * (1 - $factor) + $initial_total_timing * $factor / $initial_total_count;
    }
#    print "$initial: $initial_timing{$initial} ($initial_total_count)\n";
  }
}

sub count_timing {
  # get the shortest and longest and average
  my $initial_min = 99999;
  my $initial_max = 0;
  my $initial_total = 0;
  foreach my $initial (@initials) {
    $initial_min = $initial_timing{$initial} if ($initial_timing{$initial} < $initial_min);
    $initial_max = $initial_timing{$initial} if ($initial_timing{$initial} > $initial_max);
    $initial_total += $initial_timing{$initial};
  }
  my $initial_avg = $initial_total / ($#initials + 1);
  print "initial min: $initial_min, avg: $initial_avg, max: $initial_max\n";
  foreach my $initial (@initials) {
    if ($initial_timing{$initial} > $initial_avg) {
      $initial_timing{$initial} *= $initial_avg * 2 / $initial_max;
    } else {
      $initial_timing{$initial} *= $initial_avg / 2 / $initial_min;
    }
  }

  my $final_min = 99999;
  my $final_max = 0;
  my $final_total = 0;
  foreach my $final (@finals) {
    $final_min = $final_timing{$final} if ($final_timing{$final} < $final_min);
    $final_max = $final_timing{$final} if ($final_timing{$final} > $final_max);
    $final_total += $final_timing{$final};
  }
  my $final_avg = $final_total / ($#finals + 1);
  print "final min: $final_min, avg: $final_avg, max: $final_max\n";
  foreach my $final (@finals) {
    if ($final_timing{$final} > $final_avg) {
      $final_timing{$final} *= $final_avg * 2 / $final_max;
    } else {
      $final_timing{$final} *= $final_avg / 2 / $final_min;
    }
  }

  foreach my $final (@finals) {
    foreach my $tone (1 .. 6) {
      $timing{$final . $tone} = $final_timing{$final} * $tone_factor[$tone - 1];
      foreach my $initial (@initials) {
        $timing{$initial . $final . $tone} = ($initial_timing{$initial} + $final_timing{$final}) * $tone_factor[$tone - 1];
      }
    }
  }
}

1;
