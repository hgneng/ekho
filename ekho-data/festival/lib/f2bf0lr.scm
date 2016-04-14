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
;;;   First attempt at a linear regression model to predict F0 values.
;;;   This is an attempt to reimplement the work in Black and
;;;   Hunt ICSLP96, though this model probably isn't as good.
;;;

;;;start
;;; R2 = 0.251, F(74, 12711) = 57.5, Prob>F = 0.000
;;; RMSE = 27.877
;;;mid
;;; R2 = 0.332, F(74, 12711) = 85.6, Prob>F = 0.000
;;; RMSE = 28.293
;;;end
;;; R2 = 0.292, F(74, 12711) = 70.8, Prob>F = 0.000
;;; RMSE = 27.139

(define (emph_syl syl)
  (if (string-equal (item.feat syl "tobi_accent") "NONE")
      0.0
      (if (string-equal (item.feat 
			 syl "R:SylStructure.parent.R:Token.parent.EMPH") "1")
	  2.0
	  0.0)))

(set! f2b_f0_lr_start
'(
( Intercept 160.584956 )
( R:SylStructure.parent.R:Token.parent.EMPH 10.0 )
( pp.tobi_accent 10.081770 (H*) )
( pp.tobi_accent 3.358613 (!H*) )
( pp.tobi_accent 4.144342 (*? X*? H*!H* * L+H* L+!H*) )
( pp.tobi_accent -1.111794 (L*) )
( pp.tobi_accent 19.646313 (L*+H L*+!H) )
( p.tobi_accent 32.081029 (H*) )
( p.tobi_accent 18.090033 (!H*) )
( p.tobi_accent 23.255280 (*? X*? H*!H* * L+H* L+!H*) )
( p.tobi_accent -9.623577 (L*) )
( p.tobi_accent 26.517095 (L*+H L*+!H) )
( tobi_accent 5.221081 (H*) )
( tobi_accent 10.159194 (!H*) )
( tobi_accent 3.645511 (*? X*? H*!H* * L+H* L+!H*) )
( tobi_accent -5.720030 (L*) )
( tobi_accent -6.355773 (L*+H L*+!H) )
( n.tobi_accent -5.691933 (H*) )
( n.tobi_accent 8.265606 (!H*) )
( n.tobi_accent 0.861427 (*? X*? H*!H* * L+H* L+!H*) )
( n.tobi_accent 1.270504 (L*) )
( n.tobi_accent 3.499418 (L*+H L*+!H) )
( nn.tobi_accent -3.785701 (H*) )
( nn.tobi_accent 7.013446 (!H*) )
( nn.tobi_accent 2.637494 (*? X*? H*!H* * L+H* L+!H*) )
( nn.tobi_accent -0.392176 (L*) )
( nn.tobi_accent -2.957502 (L*+H L*+!H) )
( pp.tobi_endtone -3.531153 (L-L%) )
( pp.tobi_endtone 0.131156 (L-) )
( pp.tobi_endtone 2.729199 (H-L% !H-L% -X?) )
( pp.tobi_endtone 8.258756 (L-H%) )
( pp.tobi_endtone 5.836487 (H-) )
( pp.tobi_endtone 11.213440 (!H- H-H%) )
(  R:Syllable.p.tobi_endtone -28.081359 (L-L%) )
(  R:Syllable.p.tobi_endtone -20.553145 (L-) )
(  R:Syllable.p.tobi_endtone -5.442577 (H-L% !H-L% -X?) )
(  R:Syllable.p.tobi_endtone -6.585836 (L-H%) )
(  R:Syllable.p.tobi_endtone 8.537044 (H-) )
(  R:Syllable.p.tobi_endtone 4.243342 (!H- H-H%) )
( tobi_endtone -9.333926 (L-L%) )
( tobi_endtone -0.346711 (L-) )
( tobi_endtone -0.507352 (H-L% !H-L% -X?) )
( tobi_endtone -0.937483 (L-H%) )
( tobi_endtone 9.472265 (H-) )
( tobi_endtone 14.256898 (!H- H-H%) )
( n.tobi_endtone -13.084253 (L-L%) )
( n.tobi_endtone -1.060688 (L-) )
( n.tobi_endtone -7.947205 (H-L% !H-L% -X?) )
( n.tobi_endtone -5.471592 (L-H%) )
( n.tobi_endtone -0.095669 (H-) )
( n.tobi_endtone 4.933708 (!H- H-H%) )
( nn.tobi_endtone -14.993470 (L-L%) )
( nn.tobi_endtone -3.784284 (L-) )
( nn.tobi_endtone -15.505132 (H-L% !H-L% -X?) )
( nn.tobi_endtone -11.352400 (L-H%) )
( nn.tobi_endtone -5.551627 (H-) )
( nn.tobi_endtone -0.661581 (!H- H-H%) )
( pp.old_syl_break -3.367677 )
( p.old_syl_break 0.641755 )
( old_syl_break -0.659002 )
( n.old_syl_break 1.217358 )
( nn.old_syl_break 2.974502 )
( pp.stress 1.588098 )
( p.stress 3.693430 )
( stress 2.009843 )
( n.stress 1.645560 )
( nn.stress 1.926870 )
( syl_in 1.048362 )
( syl_out 0.315553 )
( ssyl_in -2.096079 )
( ssyl_out 0.303531 )
( asyl_in -4.257915 )
( asyl_out -2.422424 )
( last_accent -0.397647 )
( next_accent -0.418613 )
( sub_phrases -5.472055 )
))

(set! f2b_f0_lr_mid
'(
( Intercept 169.183377 )
( R:SylStructure.parent.R:Token.parent.EMPH 10.0 )
( pp.tobi_accent 4.923247 (H*) )
( pp.tobi_accent 0.955474 (!H*) )
( pp.tobi_accent 1.193597 (*? X*? H*!H* * L+H* L+!H*) )
( pp.tobi_accent 1.501383 (L*) )
( pp.tobi_accent 7.992120 (L*+H L*+!H) )
( p.tobi_accent 16.603350 (H*) )
( p.tobi_accent 11.665814 (!H*) )
( p.tobi_accent 13.063298 (*? X*? H*!H* * L+H* L+!H*) )
( p.tobi_accent -2.288798 (L*) )
( p.tobi_accent 29.168430 (L*+H L*+!H) )
( tobi_accent 34.517868 (H*) )
( tobi_accent 22.349656 (!H*) )
( tobi_accent 23.551548 (*? X*? H*!H* * L+H* L+!H*) )
( tobi_accent -14.117284 (L*) )
( tobi_accent -5.978760 (L*+H L*+!H) )
( n.tobi_accent -1.914945 (H*) )
( n.tobi_accent 5.249441 (!H*) )
( n.tobi_accent -1.929947 (*? X*? H*!H* * L+H* L+!H*) )
( n.tobi_accent -3.287877 (L*) )
( n.tobi_accent -4.980375 (L*+H L*+!H) )
( nn.tobi_accent -6.147251 (H*) )
( nn.tobi_accent 8.408949 (!H*) )
( nn.tobi_accent 3.193500 (*? X*? H*!H* * L+H* L+!H*) )
( nn.tobi_accent 1.323099 (L*) )
( nn.tobi_accent 9.148058 (L*+H L*+!H) )
( pp.tobi_endtone 4.255273 (L-L%) )
( pp.tobi_endtone -1.033377 (L-) )
( pp.tobi_endtone 11.992045 (H-L% !H-L% -X?) )
( pp.tobi_endtone 6.989573 (L-H%) )
( pp.tobi_endtone 2.598854 (H-) )
( pp.tobi_endtone 12.178307 (!H- H-H%) )
(  R:Syllable.p.tobi_endtone -4.397973 (L-L%) )
(  R:Syllable.p.tobi_endtone -6.157077 (L-) )
(  R:Syllable.p.tobi_endtone 5.530608 (H-L% !H-L% -X?) )
(  R:Syllable.p.tobi_endtone 6.938086 (L-H%) )
(  R:Syllable.p.tobi_endtone 6.162763 (H-) )
(  R:Syllable.p.tobi_endtone 8.035727 (!H- H-H%) )
( tobi_endtone -19.357902 (L-L%) )
( tobi_endtone -13.877759 (L-) )
( tobi_endtone -6.176061 (H-L% !H-L% -X?) )
( tobi_endtone -7.328882 (L-H%) )
( tobi_endtone 12.694193 (H-) )
( tobi_endtone 30.923398 (!H- H-H%) )
( n.tobi_endtone -17.727785 (L-L%) )
( n.tobi_endtone -2.539592 (L-) )
( n.tobi_endtone -8.126830 (H-L% !H-L% -X?) )
( n.tobi_endtone -8.701685 (L-H%) )
( n.tobi_endtone -1.006439 (H-) )
( n.tobi_endtone 6.834498 (!H- H-H%) )
( nn.tobi_endtone -15.407530 (L-L%) )
( nn.tobi_endtone -2.974196 (L-) )
( nn.tobi_endtone -12.287673 (H-L% !H-L% -X?) )
( nn.tobi_endtone -7.621437 (L-H%) )
( nn.tobi_endtone -0.458837 (H-) )
( nn.tobi_endtone 3.170632 (!H- H-H%) )
( pp.old_syl_break -4.196950 )
( p.old_syl_break -5.176929 )
( old_syl_break 0.047922 )
( n.old_syl_break 2.153968 )
( nn.old_syl_break 2.577074 )
( pp.stress -2.368192 )
( p.stress 1.080493 )
( stress 1.135556 )
( n.stress 2.447219 )
( nn.stress 1.318122 )
( syl_in 0.291663 )
( syl_out -0.411814 )
( ssyl_in -1.643456 )
( ssyl_out 0.580589 )
( asyl_in -5.649243 )
( asyl_out 0.489823 )
( last_accent 0.216634 )
( next_accent 0.244134 )
( sub_phrases -5.758156 )
))


(set! f2b_f0_lr_end
'(
( Intercept 169.570381 )
( R:SylStructure.parent.R:Token.parent.EMPH 10.0 )
( pp.tobi_accent 3.594771 (H*) )
( pp.tobi_accent 0.432519 (!H*) )
( pp.tobi_accent 0.235664 (*? X*? H*!H* * L+H* L+!H*) )
( pp.tobi_accent 1.513892 (L*) )
( pp.tobi_accent 2.474823 (L*+H L*+!H) )
( p.tobi_accent 11.214208 (H*) )
( p.tobi_accent 9.619350 (!H*) )
( p.tobi_accent 9.084690 (*? X*? H*!H* * L+H* L+!H*) )
( p.tobi_accent 0.519202 (L*) )
( p.tobi_accent 26.593112 (L*+H L*+!H) )
( tobi_accent 25.217589 (H*) )
( tobi_accent 13.759851 (!H*) )
( tobi_accent 17.635192 (*? X*? H*!H* * L+H* L+!H*) )
( tobi_accent -12.149974 (L*) )
( tobi_accent 13.345913 (L*+H L*+!H) )
( n.tobi_accent 4.944848 (H*) )
( n.tobi_accent 7.398383 (!H*) )
( n.tobi_accent 1.683011 (*? X*? H*!H* * L+H* L+!H*) )
( n.tobi_accent -6.516900 (L*) )
( n.tobi_accent -6.768201 (L*+H L*+!H) )
( nn.tobi_accent -4.335797 (H*) )
( nn.tobi_accent 5.656462 (!H*) )
( nn.tobi_accent 0.263288 (*? X*? H*!H* * L+H* L+!H*) )
( nn.tobi_accent 1.022002 (L*) )
( nn.tobi_accent 6.702368 (L*+H L*+!H) )
( pp.tobi_endtone 10.274958 (L-L%) )
( pp.tobi_endtone 3.129947 (L-) )
( pp.tobi_endtone 15.476240 (H-L% !H-L% -X?) )
( pp.tobi_endtone 10.446935 (L-H%) )
( pp.tobi_endtone 6.104384 (H-) )
( pp.tobi_endtone 14.182688 (!H- H-H%) )
(  R:Syllable.p.tobi_endtone 1.767454 (L-L%) )
(  R:Syllable.p.tobi_endtone -1.040077 (L-) )
(  R:Syllable.p.tobi_endtone 18.438093 (H-L% !H-L% -X?) )
(  R:Syllable.p.tobi_endtone 8.750018 (L-H%) )
(  R:Syllable.p.tobi_endtone 5.000340 (H-) )
(  R:Syllable.p.tobi_endtone 10.913437 (!H- H-H%) )
( tobi_endtone -12.637935 (L-L%) )
( tobi_endtone -13.597961 (L-) )
( tobi_endtone -6.501965 (H-L% !H-L% -X?) )
( tobi_endtone 8.747483 (L-H%) )
( tobi_endtone 15.165833 (H-) )
( tobi_endtone 50.190326 (!H- H-H%) )
( n.tobi_endtone -16.965781 (L-L%) )
( n.tobi_endtone -5.222475 (L-) )
( n.tobi_endtone -7.358555 (H-L% !H-L% -X?) )
( n.tobi_endtone -7.833168 (L-H%) )
( n.tobi_endtone 4.701087 (H-) )
( n.tobi_endtone 10.349902 (!H- H-H%) )
( nn.tobi_endtone -15.369483 (L-L%) )
( nn.tobi_endtone -2.207161 (L-) )
( nn.tobi_endtone -9.363835 (H-L% !H-L% -X?) )
( nn.tobi_endtone -7.052374 (L-H%) )
( nn.tobi_endtone 2.207854 (H-) )
( nn.tobi_endtone 5.271546 (!H- H-H%) )
( pp.old_syl_break -4.745862 )
( p.old_syl_break -5.685178 )
( old_syl_break -2.633291 )
( n.old_syl_break 1.678340 )
( nn.old_syl_break 2.274729 )
( pp.stress -2.747198 )
( p.stress 0.306724 )
( stress -0.565613 )
( n.stress 2.838327 )
( nn.stress 1.285244 )
( syl_in 0.169955 )
( syl_out -1.045661 )
( ssyl_in -1.487774 )
( ssyl_out 0.752405 )
( asyl_in -5.081677 )
( asyl_out 3.016218 )
( last_accent 0.312900 )
( next_accent 0.837992 )
( sub_phrases -5.397805 )

))

;; groups
;; tobi_accent_1 25.217589 (H*) )
;; tobi_accent_2 13.759851 (!H*) )
;; tobi_accent_3 17.635192 (*? X*? H*!H* * L+H* L+!H*) )
;; tobi_accent_4 -12.149974 (L*) )
;; tobi_accent_5 13.345913 (L*+H L*+!H) )

;; tobi_endtone_1 10.274958 (L-L%) )
;; tobi_endtone_2 3.129947 (L-) )
;; tobi_endtone_3 15.476240 (H-L% !H-L% -X?) )
;; tobi_endtone_4 10.446935 (L-H%) )
;; tobi_endtone_5 6.104384 (H-) )
;; tobi_endtone_6 14.182688 (!H- H-H%) )

(provide 'f2bf0lr)

