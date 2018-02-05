#!/usr/bin/perl

@lines = `grep -v 5 pinyin_phon_list.txt`;
foreach $line (@lines) {
	chomp($line);
	@items = split(/:/, $line);
	if ($#items > 0) {
		$pinyin = $items[0];
		$items[1] =~ s/^\s*//g;
		@chars = split(/\s+/, $items[1]);
		$found = 0;

		foreach $char (@chars) {
			@phons = `grep -v 5  pinyin_phon_list.txt | grep $char`;
			if ($#phons > 0) {
				next;
			} else {
				$found = 1;
				#print "download $char\n";
				if (!(-s "voice/$pinyin.mp3" > 0)) {
					`wget -O voice/$pinyin.mp3 "http://120.24.87.124/cgi-bin/ekho2.pl?cmd=SPEAK&voice=BaiduMandarinFemale&speedDelta=0&pitchDelta=0&volumeDelta=0&text=$char"`;
				}
			}
		}

		if (!$found) {
			print "$line\n";
		}
	}
}
