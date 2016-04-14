;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 1996,1997                         ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;;  this software and its documentation without restriction, including   ;;
;;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;;  the following conditions:                                            ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;   4. The authors' names are not used to endorse or promote products   ;;
;;;      derived from this software without specific prior written        ;;
;;;      permission.                                                      ;;
;;;                                                                       ;;
;;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
;;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
;;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
;;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  mrpa average phoneme durations from gsw 450
;;;
(set! phoneme_durations 
'(
(u 0.067)
(i@ 0.146)
(h 0.067)
(uu 0.105)
(uh 0.090)
(v 0.053)
(oo 0.145)
(i 0.060)
(jh 0.097)
(ii 0.095)
(w 0.066)
(k 0.088)
(+ 0.036)
(y 0.051)
(l 0.067)
(zh 0.080)
(ng 0.072)
(m 0.070)
(z 0.079)
(## 0.256)
(au 0.162)
(a 0.118)
(n 0.065)
(o 0.102)
(ai 0.156)
(b 0.071)
(ou 0.129)
(ch 0.119)
(p 0.094)
(oi 0.165)
(# 0.040)
(e@ 0.131)
(d 0.052)
(dh 0.032)
(e 0.091)
(r 0.062)
(sh 0.101)
(@@ 0.149)
(ei 0.131)
(f 0.091)
(s 0.093)
(g 0.066)
(u@ 0.120)
(aa 0.173)
(t 0.073)
(th 0.080)
(@ 0.054)
))

(set! gsw_durs
'(
(#    0.200 0.100)
(h    0.061 0.028)
(i@   0.141 0.061)
(u    0.067 0.024)
(uu   0.107 0.044)
(uh   0.087 0.025)
(v    0.051 0.019)
(oo   0.138 0.046)
(i    0.058 0.023)
(ii   0.092 0.035)
(w    0.054 0.023)
(jh   0.094 0.024)
(k    0.089 0.034)
(y    0.048 0.025)
(l    0.056 0.026)
(zh   0.077 0.030)
(ng   0.064 0.024)
(m    0.063 0.021)
(z    0.072 0.029)
(a    0.120 0.036)
(au   0.171 0.046)
(n    0.059 0.025)
(ou   0.134 0.039)
(b    0.073 0.021)
(o    0.094 0.037)
(ai   0.137 0.047)
(ch   0.128 0.039)
(oi   0.183 0.050)
(p    0.101 0.032)
(e@   0.144 0.061)
(d    0.048 0.021)
(dh   0.031 0.016)
(e    0.092 0.035)
(r    0.053 0.025)
(sh   0.108 0.031)
(f    0.095 0.033)
(@@   0.147 0.035)
(ei   0.130 0.042)
(s    0.102 0.037)
(u@   0.140 0.057)
(th   0.093 0.050)
(g    0.064 0.021)
(aa   0.155 0.045)
(t    0.070 0.034)
(@    0.046 0.020)
))

(provide 'mrpa_durs)
