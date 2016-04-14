###########################################################################
##                                                                       ##
##                Centre for Speech Technology Research                  ##
##                     University of Edinburgh, UK                       ##
##                         Copyright (c) 1998                            ##
##                        All Rights Reserved.                           ##
##                                                                       ##
##  Permission is hereby granted, free of charge, to use and distribute  ##
##  this software and its documentation without restriction, including   ##
##  without limitation the rights to use, copy, modify, merge, publish,  ##
##  distribute, sublicense, and/or sell copies of this work, and to      ##
##  permit persons to whom this work is furnished to do so, subject to   ##
##  the following conditions:                                            ##
##   1. The code must retain the above copyright notice, this list of    ##
##      conditions and the following disclaimer.                         ##
##   2. Any modifications must be clearly marked as such.                ##
##   3. Original authors' names are not deleted.                         ##
##   4. The authors' names are not used to endorse or promote products   ##
##      derived from this software without specific prior written        ##
##      permission.                                                      ##
##                                                                       ##
##  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ##
##  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ##
##  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ##
##  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ##
##  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ##
##  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ##
##  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ##
##  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ##
##  THIS SOFTWARE.                                                       ##
##                                                                       ##
###########################################################################
##                                                                       ##
##  Alan W Black (awb@cstr.ed.ac.uk) June 1998                           ##
##                                                                       ##
##  Programs to covert the CMU LEXICON to a form suitable for use in     ##
##  festival.  Note this is for version 0.4                              ##
##                                                                       ##
##  Note this includes a patch to add part of speech to the lexicon for  ##
##  homographs which can be distinguished by part of speech              ##
##                                                                       ##
##  This requires the CMULEX which is available from                     ##
##       http://www.speech.cs.cmu.edu/cgi-bin/cmudict                    ##
##  The Copyright on the CMU lexicon is                                  ##
##    The Carnegie Mellon Pronouncing Dictionary [cmudict.0.4] is        ##
##    Copyright 1995 by Carnegie Mellon University. Use of this          ##
##    dictionary, for any research or commercial purpose, is             ##
##    completely unrestricted.  If you make use of or redistribute       ##
##    this material, we would appreciate acknowlegement of its origin.   ##
##                                                                       ##
## CSTR gratefully acknowledges CMU for the use of this lexicon          ##
##                                                                       ##
## July 2008                                                             ##
##    awb@cs.cmu.edu change syllabification to maximal onset             ##
##                                                                       ##
###########################################################################

SRCS = cmu2ft
RUNTIME = cmulex.scm cmu_lts_rules.scm cmudict-0.4.out
FILES = Makefile COPYING $(SRCS) cmudict-0.4.scm cmudict-0.4.diff \
        allowables.scm cmudict_compile.scm findonsets

cmudict-0.4.out: cmudict-0.4.scm cmudict_extensions.scm
	cat cmudict-0.4.scm cmudict_extensions.scm >all.scm
	${ESTDIR}/../festival/bin/festival -b cmudict_compile.scm
	rm -f all.scm

cmudict-0.4.scm: 
	./cmu2ft cmudict.0.4 cmudict-0.4.scm
	patch <cmudict-0.4.diff

# Can optionally reduce the lexicon size by removing those entries that
# the LTS system correctly predict.
reduce:
	( echo "(load (string-append libdir \"/\" \"siod.scm\"))"; \
	  echo "(require 'radio_phones)"; \
	  echo "(require 'lts_build)"; \
          echo "(reduce_lexicon \"cmudict-0.4.out\" \"cmudict-0.4.exception.out\" cmu_lts_function)" ) | \
	festival -q

dist:
	mkdir -p festival/lib/dicts
	(cd festival/lib/dicts && ln -s ../../../. cmu)
	tar zcvf festlex_CMU.tar.gz \
           festival/lib/dicts/cmu/allowables.scm \
           festival/lib/dicts/cmu/cmu*.scm \
           festival/lib/dicts/cmu/cmudict-0.4.out \
           festival/lib/dicts/cmu/Makefile \
           festival/lib/dicts/cmu/cmudict-0.4.diff \
           festival/lib/dicts/cmu/cmu2ft \
           festival/lib/dicts/cmu/COPYING
	rm -rf festival    

lts_rules:
	if [ ! -d lts_scratch ] ; then mkdir lts_scratch ; fi
	@ cp -p $(FESTVOXDIR)/src/lts/build_lts .
	@ cp -p $(FESTVOXDIR)/src/lts/build_lts_rules .
	festival -b allowables.scm '(dump-flat-entries "cmudict-0.4.out" "lts_scratch/lex_entries.out" "ascii")'
	./build_lts

backup:
	tar zcvf cmudict04.tar.gz $(FILES)