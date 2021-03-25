###########################################################################
##                                                                       ##
##                Centre for Speech Technology Research                  ##
##                     University of Edinburgh, UK                       ##
##                       Copyright (c) 1996,1997                         ##
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
#
#  This directory contains data derived from the Oxford Advanced Learner's
#  Dictionary of Current English.  Although the basic code files in
#  this directory as free the data itself is Copyright Oxford University
#  Press and is made available for non-commercial use only
#
RUNTIME= cuvoald710-0.3.out oald_lts_rules.scm oaldlex.scm
BUILD = cuvoald710-0.2.except.scm oald2ft.tar.gz

FILES = README.oald Makefile COPYING $(RUNTIME) $(BUILD)

oald-0.4.out: oald_extensions.scm cuvoald710-0.2.scm
	cat oald_extensions.scm cuvoald710-0.2.scm >all.scm
	festival -b "(voice_rab_diphone)" "(lex.compile \"all.scm\" \"oald-0.4.out\")"
	rm -f all.scm

# infrastructure
backup: 
	@tar zcvhf festlex_OALD.tar.gz $(FILES)
	@ls -l festlex_OALD.tar.gz

