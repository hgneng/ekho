BEGIN {
  unshift(@INC, '/home/hgneng/e-guidedog/eGuideDog-Dict-Korean/lib/eGuideDog/Dict');
}

use strict;
use warnings;
use eGuideDog::Dict::Korean;

sub count_all_timing;
sub count_leads_timing;
sub count_vowel_timing;
sub count_tail_timing;
sub count_vowel_timing_again;
sub count_timing;

my ($voice_dir) = @ARGV;

my $dict = eGuideDog::Dict::Korean->new();

my @leads = @{$dict->{lead_table}};
my @vowels = @{$dict->{vowel_table}};
my @tails = @{$dict->{tail_table}};

our %lead_timing;
our %vowel_timing;
our %tail_timing;
our %timing;
my $total_difference = -1;
my $factor = 0.5;

count_all_timing();
count_vowel_timing();
my $total = 1;
$| = 1;

for (1 .. 200) {
  my %lead_timing_bak = %lead_timing;
  my %vowel_timing_bak = %vowel_timing;
  my %tail_timing_bak = %tail_timing;
  my $total_difference_bak = $total_difference;
  $total_difference = 0;
  count_tail_timing();
  count_lead_timing();
  count_vowel_timing_again();

# restore if difference is bigger
  if ($total_difference_bak > 0 && $total_difference > $total_difference_bak) {
    %lead_timing = %lead_timing_bak;
    %vowel_timing = %vowel_timing_bak;
    %tail_timing = %tail_timing_bak;
    $total_difference = $total_difference_bak;
    $factor /= 2;
  } else {
    $factor = 0.5;
  }

  $total++;
  if ($total_difference < $total_difference_bak) {
    print "\n$total, $total_difference";
  } else {
    print "\r$total, $total_difference";
  }
}

count_timing();

print "\nlead timing:\n";
foreach my $lead (@leads) {
  print "$lead:\t" . int($lead_timing{$lead}) . "\n";
}
print "\nvowel timing:\n";
foreach my $vowel (@vowels) {
  print "$vowel:\t" . int($vowel_timing{$vowel}) . "\n";
}
print "\ntail timing:\n";
foreach my $tail (@tails) {
  print "$tail:\t" . int($tail_timing{$tail}) . "\n";
}

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

sub count_vowel_timing {
  foreach my $vowel (@vowels) {
    my $vowel_total_timing = 0;
    my $vowel_total_count = 0;

    $vowel_timing{$vowel} = $timing{$vowel};
    print "$vowel: $vowel_timing{$vowel}\n";
  }
}

sub count_vowel_timing_again {
  foreach my $vowel (@vowels) {
    my $vowel_total_timing = 0;
    my $vowel_total_count = 0;
    foreach my $lead (@leads) {
      foreach my $tail (@tails) {
        if ($timing{$lead . $vowel . $tail}) {
          my $time = $timing{$lead . $vowel . $tail} - $lead_timing{$lead} - $tail_timing{$tail};
          if ($time > 0) {
            $vowel_total_timing += $time;
            $vowel_total_count++;
          } else {
#            print "[warning] $initial . $final . $tone < 0\n";
          }
        }
      }
    }
    $vowel_timing{$vowel} = 0 if (not defined $vowel_timing{$vowel});
    if ($vowel_total_count) {
      if ((int($vowel_timing{$vowel}) != int($vowel_total_timing / $vowel_total_count))) {
        $total_difference += abs($vowel_total_timing / $vowel_total_count - $vowel_timing{$vowel});
      }
      if ($vowel_timing{$vowel}) {
        $vowel_timing{$vowel} = $vowel_timing{$vowel} * (1 - $factor) + $vowel_total_timing * $factor / $vowel_total_count;
      } else {
        $vowel_timing{$vowel} = $vowel_total_timing / $vowel_total_count;
      }
    }
#    print "$final: $final_timing{$final} ($final_total_count)\n";
  }
}

sub count_lead_timing {
  foreach my $lead (@leads) {
    my $lead_total_timing = 0;
    my $lead_total_count = 0;
    foreach my $vowel (@vowels) {
      if ($vowel_timing{$vowel}) {
        for my $tail (@tails) {
          if ($timing{$lead . $vowel . $tail}) {
            my $time += $timing{$lead . $vowel . $tail} - $vowel_timing{$vowel};
            if ($time > 0) {
              $lead_total_timing += $time;
              $lead_total_count++;
            } else {
#              print "[warning] $initial . $final . $tone < 0\n";
            }
          }
        }
      }
    }
    $lead_timing{$lead} = 0 if (not defined $lead_timing{$lead});
    if ($lead_total_count) {
      if ((int($lead_timing{$lead}) != int(($lead_total_timing / $lead_total_count)))) {
        $total_difference += abs($lead_total_timing / $lead_total_count - $lead_timing{$lead});
      }
      if (not $lead_timing{$lead}) {
        $lead_timing{$lead} = $lead_total_timing / $lead_total_count;
      } else {
        $lead_timing{$lead} = $lead_timing{$lead}  * (1 - $factor) + $lead_total_timing * $factor / $lead_total_count;
      }
    }
#    print "$initial: $initial_timing{$initial} ($initial_total_count)\n";
  }
}

sub count_tail_timing {
  foreach my $tail (@tails) {
    my $tail_total_timing = 0;
    my $tail_total_count = 0;
    foreach my $vowel (@vowels) {
      if ($vowel_timing{$vowel}) {
        for my $lead (@leads) {
          if ($timing{$lead . $vowel . $tail}) {
            my $time += $timing{$lead . $vowel . $tail} - $vowel_timing{$vowel};
            if ($time > 0) {
              $tail_total_timing += $time;
              $tail_total_count++;
            } else {
#              print "[warning] $initial . $final . $tone < 0\n";
            }
          }
        }
      }
    }
    $tail_timing{$tail} = 0 if (not defined $tail_timing{$tail});
    if ($tail_total_count) {
      if ((int($tail_timing{$tail}) != int(($tail_total_timing / $tail_total_count)))) {
        $total_difference += abs($tail_total_timing / $tail_total_count - $tail_timing{$tail});
      }
      if (not $tail_timing{$tail}) {
        $tail_timing{$tail} = $tail_total_timing / $tail_total_count;
      } else {
        $tail_timing{$tail} = $tail_timing{$tail}  * (1 - $factor) + $tail_total_timing * $factor / $tail_total_count;
      }
    }
#    print "$initial: $initial_timing{$initial} ($initial_total_count)\n";
  }
}

sub count_timing {
  # get the shortest and longest and average
  my $lead_min = 99999;
  my $lead_max = 0;
  my $lead_total = 0;
  foreach my $lead (@leads) {
    $lead_min = $lead_timing{$lead} if ($lead_timing{$lead} < $lead_min);
    $lead_max = $lead_timing{$lead} if ($lead_timing{$lead} > $lead_max);
    $lead_total += $lead_timing{$lead};
  }
  my $lead_avg = $lead_total / ($#leads + 1);
  print "lead min: $lead_min, avg: $lead_avg, max: $lead_max\n";
  foreach my $lead (@leads) {
    if ($lead_timing{$lead} > $lead_avg) {
      $lead_timing{$lead} *= $lead_avg * 2 / $lead_max;
    } else {
      $lead_timing{$lead} *= $lead_avg / 2 / $lead_min;
    }
  }

  my $vowel_min = 99999;
  my $vowel_max = 0;
  my $vowel_total = 0;
  my $vowel_count = 0;
  foreach my $vowel (@vowels) {
    $vowel_min = $vowel_timing{$vowel} if ($vowel_timing{$vowel} < $vowel_min && $vowel_timing{$vowel});
    $vowel_max = $vowel_timing{$vowel} if ($vowel_timing{$vowel} > $vowel_max);
    $vowel_total += $vowel_timing{$vowel};
    $vowel_count++ if ($vowel_timing{$vowel});
  }
  my $vowel_avg = $vowel_total / $vowel_count;
  print "vowel min: $vowel_min, avg: $vowel_avg, max: $vowel_max\n";
  foreach my $vowel (@vowels) {
    if ($vowel_timing{$vowel} > $vowel_avg) {
      $vowel_timing{$vowel} *= $vowel_avg * 2 / $vowel_max;
    } else {
      $vowel_timing{$vowel} *= $vowel_avg / 2 / $vowel_min;
    }
  }

  my $tail_min = 99999;
  my $tail_max = 0;
  my $tail_total = 0;
  my $tail_count = 0;
  foreach my $tail (@tails) {
    $tail_min = $tail_timing{$tail} if ($tail_timing{$tail} < $tail_min && $tail_timing{$tail});
    $tail_max = $tail_timing{$tail} if ($tail_timing{$tail} > $tail_max);
    $tail_total += $tail_timing{$tail};
    $tail_count++ if ($tail_timing{$tail});
  }
  my $tail_avg = $tail_total / $tail_count;
  print "tail min: $tail_min, avg: $tail_avg, max: $tail_max\n";
  foreach my $tail (@tails) {
    if ($tail_timing{$tail} > $tail_avg) {
      $tail_timing{$tail} *= $tail_avg * 2 / $tail_max;
    } else {
      $tail_timing{$tail} *= $tail_avg / 2 / $tail_min;
    }
  }

  foreach my $vowel (@vowels) {
    $timing{$vowel} = $vowel_timing{$vowel};
    foreach my $lead (@leads) {
      $timing{$lead . $vowel} = $lead_timing{$lead} + $vowel_timing{$vowel};
    }
    foreach my $tail (@tails) {
      $timing{$vowel . $tail} = $vowel_timing{$vowel} + $tail_timing{$tail};
      foreach my $lead (@leads) {
        $timing{$lead . $vowel . $tail} = $lead_timing{$lead} + $vowel_timing{$vowel} + $lead_timing{$lead};
      }
    }
  }
}

1;
