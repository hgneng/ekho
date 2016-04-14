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
;;;   A CART tree for predicting ToBI accents (learned from f2b)
;;;   punctuation and minimal pos
;;;

;         NON    !H*  L+H    L*+  
;     NONE10265  583   66   40    0    0 10954      [10265/10954]      93.710
;        H* 650 1805   61   57    0    0 2573      [1805/2573]      70.152
;       !H* 317  241  125   42    0    0  725      [125/725]      17.241
;      L+H* 457  486   76   80    0    0 1099      [80/1099]       7.279
;        L*  45  113   14    4    0    0  176      [0/176]       0.000
;      L*+H   6    6    0    1    0    0   13      [0/13]       0.000
;          11740 3234  342  224    0    0 
;total 15540 correct 12275.000 78.990%

(set! f2b_int_accent_cart_tree
'
;; these first few lines are hand written to deal with emphasis (from ssml)
((R:SylStructure.parent.R:Token.parent.EMPH is 1)
 (((NONE 0.0) (H* 1) (!H* 0.0) (L+H* 0) (L* 0) (L*+H 0) H*))
 ((n.R:SylStructure.parent.R:Token.parent.EMPH is 1)
  (((NONE 1.0) (H* 0) (!H* 0.0) (L+H* 0) (L* 0) (L*+H 0) NONE))
  ((p.R:SylStructure.parent.R:Token.parent.EMPH is 1)
   (((NONE 1.0) (H* 0) (!H* 0.0) (L+H* 0) (L* 0) (L*+H 0) NONE))

((ssyl_in is 10)
 (((NONE 0.99726) (H* 0) (!H* 0.00273973) (L+H* 0) (L* 0) (L*+H 0) NONE))
 ((R:SylStructure.parent.gpos is to)
  (((NONE 0.995984) (H* 0.00401606) (!H* 0) (L+H* 0) (L* 0) (L*+H 0) NONE))
  ((R:SylStructure.parent.gpos is cc)
   (((NONE 0.987768) (H* 0.00611621) (!H* 0) (L+H* 0.00611621) (L* 0) (L*+H 0) NONE))
   ((ssyl_out is 10)
    (((NONE 0.927273) (H* 0.0545455) (!H* 0) (L+H* 0.0181818) (L* 0) (L*+H 0) NONE))
    ((R:SylStructure.parent.gpos is in)
     (((NONE 0.938322) (H* 0.0353618) (!H* 0.00493421) (L+H* 0.0197368) (L* 0.00164474) (L*+H 0) NONE))
     ((R:SylStructure.parent.gpos is wp)
      (((NONE 0.895238) (H* 0.0857143) (!H* 0) (L+H* 0.0190476) (L* 0) (L*+H 0) NONE))
      ((R:SylStructure.parent.gpos is aux)
       (((NONE 0.912281) (H* 0.0380117) (!H* 0.00584795) (L+H* 0.0350877) (L* 0.00584795) (L*+H 0.00292398) NONE))
       ((R:SylStructure.parent.gpos is det)
        (((NONE 0.898004) (H* 0.0643016) (!H* 0.00332594) (L+H* 0.0332594) (L* 0) (L*+H 0.00110865) NONE))
        ((stress is 0)
         (((NONE 0.978415) (H* 0.0144999) (!H* 0.00164772) (L+H* 0.00510793) (L* 0.000329544) (L*+H 0) NONE))
         ((R:SylStructure.parent.R:Word.p.gpos is 0)
          (((NONE 0.209877) (H* 0.716049) (!H* 0) (L+H* 0.0617284) (L* 0.0123457) (L*+H 0) H*))
          ((R:SylStructure.parent.gpos is md)
           (((NONE 0.693548) (H* 0.177419) (!H* 0.0322581) (L+H* 0.0967742) (L* 0) (L*+H 0) NONE))
           ((p.syl_break is 3)
            ((syl_break is 1)
             (((NONE 0.4375) (H* 0.416667) (!H* 0) (L+H* 0.135417) (L* 0.0104167) (L*+H 0) NONE))
             (((NONE 0.171171) (H* 0.666667) (!H* 0) (L+H* 0.144144) (L* 0.018018) (L*+H 0) H*)))
            ((pp.syl_break is 4)
             ((R:SylStructure.parent.R:Word.pp.gpos is in)
              (((NONE 0.0980392) (H* 0.803922) (!H* 0) (L+H* 0.0784314) (L* 0.0196078) (L*+H 0) H*))
              ((syl_out is 0)
               (((NONE 0.0185185) (H* 0.796296) (!H* 0.037037) (L+H* 0.0925926) (L* 0.0555556) (L*+H 0) H*))
               ((R:SylStructure.parent.R:Word.n.gpos is in)
                (((NONE 0.132353) (H* 0.676471) (!H* 0) (L+H* 0.161765) (L* 0.0294118) (L*+H 0) H*))
                ((syl_break is 0)
                 (((NONE 0.125) (H* 0.633929) (!H* 0.0133929) (L+H* 0.183036) (L* 0.0401786) (L*+H 0.00446429) H*))
                 ((n.stress is 0)
                  (((NONE 0.364865) (H* 0.567568) (!H* 0) (L+H* 0.0540541) (L* 0.0135135) (L*+H 0) H*))
                  ((p.syl_break is 0)
                   (((NONE 0.612903) (H* 0.290323) (!H* 0) (L+H* 0.0967742) (L* 0) (L*+H 0) NONE))
                   (((NONE 0.32) (H* 0.44) (!H* 0.02) (L+H* 0.22) (L* 0) (L*+H 0) H*))))))))
             ((ssyl_in is 0)
              (((NONE 0.167769) (H* 0.628926) (!H* 0.0214876) (L+H* 0.142975) (L* 0.0363636) (L*+H 0.00247934) H*))
              ((ssyl_out is 4)
               (((NONE 0.490385) (H* 0.240385) (!H* 0.0961538) (L+H* 0.163462) (L* 0.00961538) (L*+H 0) NONE))
               ((pp.syl_break is 3)
                ((R:SylStructure.parent.R:Word.p.gpos is content)
                 (((NONE 0.346154) (H* 0.346154) (!H* 0.0769231) (L+H* 0.192308) (L* 0.0384615) (L*+H 0) NONE))
                 (((NONE 0.160714) (H* 0.571429) (!H* 0.0178571) (L+H* 0.178571) (L* 0.0714286) (L*+H 0) H*)))
                ((syl_in is 2)
                 ((n.stress is 0)
                  ((R:SylStructure.parent.R:Word.p.gpos is in)
                   (((NONE 0.218182) (H* 0.618182) (!H* 0.0363636) (L+H* 0.0909091) (L* 0.0181818) (L*+H 0.0181818) H*))
                   ((syl_out is 2)
                    (((NONE 0.0961538) (H* 0.634615) (!H* 0.0961538) (L+H* 0.134615) (L* 0.0384615) (L*+H 0) H*))
                    ((R:SylStructure.parent.R:Word.p.gpos is content)
                     ((syl_out is 4)
                      (((NONE 0.56) (H* 0.12) (!H* 0.08) (L+H* 0.24) (L* 0) (L*+H 0) NONE))
                      (((NONE 0.262821) (H* 0.378205) (!H* 0.121795) (L+H* 0.192308) (L* 0.0448718) (L*+H 0) H*)))
                     (((NONE 0.161905) (H* 0.590476) (!H* 0.0285714) (L+H* 0.171429) (L* 0.047619) (L*+H 0) H*)))))
                  ((n.syl_break is 0)
                   (((NONE 0.551724) (H* 0.293103) (!H* 0) (L+H* 0.155172) (L* 0) (L*+H 0) NONE))
                   (((NONE 0.408451) (H* 0.422535) (!H* 0.056338) (L+H* 0.112676) (L* 0) (L*+H 0) H*))))
                 ((R:SylStructure.parent.R:Word.n.gpos is 0)
                  ((syl_break is 0)
                   (((NONE 0.105263) (H* 0.315789) (!H* 0.157895) (L+H* 0.421053) (L* 0) (L*+H 0) L+H*))
                   (((NONE 0.641509) (H* 0.132075) (!H* 0.132075) (L+H* 0.0943396) (L* 0) (L*+H 0) NONE)))
                  ((syl_break is 1)
                   ((ssyl_in is 3)
                    (((NONE 0.638889) (H* 0.152778) (!H* 0.125) (L+H* 0.0833333) (L* 0) (L*+H 0) NONE))
                    ((p.syl_break is 0)
                     (((NONE 0.551402) (H* 0.186916) (!H* 0.158879) (L+H* 0.0841122) (L* 0.0186916) (L*+H 0) NONE))
                     ((n.stress is 0)
                      ((pp.syl_break is 0)
                       (((NONE 0.413043) (H* 0.184783) (!H* 0.152174) (L+H* 0.23913) (L* 0.0108696) (L*+H 0) NONE))
                       (((NONE 0.2125) (H* 0.3375) (!H* 0.1875) (L+H* 0.2125) (L* 0.05) (L*+H 0) H*)))
                      (((NONE 0.449153) (H* 0.245763) (!H* 0.101695) (L+H* 0.20339) (L* 0) (L*+H 0) NONE)))))
                   ((syl_out is 4)
                    ((nn.syl_break is 0)
                     ((pp.syl_break is 0)
                      (((NONE 0.45614) (H* 0.210526) (!H* 0.192982) (L+H* 0.140351) (L* 0) (L*+H 0) NONE))
                      (((NONE 0.288462) (H* 0.25) (!H* 0.0961538) (L+H* 0.346154) (L* 0) (L*+H 0.0192308) L+H*)))
                     (((NONE 0.163934) (H* 0.459016) (!H* 0.131148) (L+H* 0.245902) (L* 0) (L*+H 0) H*)))
                    ((syl_out is 5)
                     ((R:SylStructure.parent.R:Word.p.gpos is content)
                      (((NONE 0.372881) (H* 0.20339) (!H* 0.169492) (L+H* 0.220339) (L* 0.0338983) (L*+H 0) NONE))
                      (((NONE 0.0961538) (H* 0.673077) (!H* 0.115385) (L+H* 0.0961538) (L* 0.0192308) (L*+H 0) H*)))
                     ((R:SylStructure.parent.R:Word.pp.gpos is in)
                      ((syl_in is 4)
                       (((NONE 0.352113) (H* 0.422535) (!H* 0.15493) (L+H* 0.0704225) (L* 0) (L*+H 0) H*))
                       ((syl_in is 3)
                        (((NONE 0.290323) (H* 0.467742) (!H* 0.0806452) (L+H* 0.145161) (L* 0.016129) (L*+H 0) H*))
                        ((pp.syl_break is 0)
                         (((NONE 0.465517) (H* 0.293103) (!H* 0.172414) (L+H* 0.0689655) (L* 0) (L*+H 0) NONE))
                         ((R:SylStructure.parent.R:Word.p.gpos is content)
                          (((NONE 0.18) (H* 0.36) (!H* 0.28) (L+H* 0.14) (L* 0.04) (L*+H 0) H*))
                          (((NONE 0.0877193) (H* 0.22807) (!H* 0.368421) (L+H* 0.298246) (L* 0.0175439) (L*+H 0) !H*))))))
                      ((ssyl_out is 2)
                       ((p.syl_break is 0)
                        (((NONE 0.634921) (H* 0.174603) (!H* 0.0793651) (L+H* 0.111111) (L* 0) (L*+H 0) NONE))
                        ((pp.syl_break is 0)
                         (((NONE 0.388889) (H* 0.148148) (!H* 0.148148) (L+H* 0.259259) (L* 0.0185185) (L*+H 0.037037) NONE))
                         (((NONE 0.294118) (H* 0.137255) (!H* 0.215686) (L+H* 0.333333) (L* 0.0196078) (L*+H 0) L+H*))))
                       ((R:SylStructure.parent.R:Word.pp.gpos is to)
                        (((NONE 0.0877193) (H* 0.350877) (!H* 0.210526) (L+H* 0.315789) (L* 0.0350877) (L*+H 0) H*))
                        ((syl_break is 3)
                         ((pp.syl_break is 0)
                          (((NONE 0.478261) (H* 0.141304) (!H* 0.195652) (L+H* 0.184783) (L* 0) (L*+H 0) NONE))
                          (((NONE 0.217822) (H* 0.366337) (!H* 0.257426) (L+H* 0.128713) (L* 0.029703) (L*+H 0) H*)))
                         ((syl_in is 7)
                          ((n.stress is 0)
                           ((R:SylStructure.parent.R:Word.n.gpos is content)
                            (((NONE 0.117647) (H* 0.220588) (!H* 0.441176) (L+H* 0.176471) (L* 0.0441176) (L*+H 0) !H*))
                            (((NONE 0.415385) (H* 0.0461538) (!H* 0.2) (L+H* 0.246154) (L* 0.0923077) (L*+H 0) NONE)))
                           (((NONE 0.716981) (H* 0.113208) (!H* 0.0943396) (L+H* 0.0754717) (L* 0) (L*+H 0) NONE)))
                          ((R:SylStructure.parent.R:Word.n.gpos is cc)
                           (((NONE 0.292308) (H* 0.184615) (!H* 0.276923) (L+H* 0.246154) (L* 0) (L*+H 0) NONE))
                           ((nn.syl_break is 3)
                            (((NONE 0.2) (H* 0.333333) (!H* 0.283333) (L+H* 0.15) (L* 0.0333333) (L*+H 0) H*))
                            ((ssyl_in is 4)
                             (((NONE 0.383838) (H* 0.151515) (!H* 0.212121) (L+H* 0.20202) (L* 0.050505) (L*+H 0) NONE))
                             ((p.syl_break is 0)
                              ((n.syl_break is 1)
                               (((NONE 0.526316) (H* 0.210526) (!H* 0.0921053) (L+H* 0.171053) (L* 0) (L*+H 0) NONE))
                               ((ssyl_in is 3)
                                (((NONE 0.509804) (H* 0.0980392) (!H* 0.215686) (L+H* 0.156863) (L* 0.0196078) (L*+H 0) NONE))
                                ((pp.syl_break is 0)
                                 (((NONE 0.506667) (H* 0.173333) (!H* 0.106667) (L+H* 0.2) (L* 0.0133333) (L*+H 0) NONE))
                                 ((ssyl_in is 1)
                                  (((NONE 0.1) (H* 0.4) (!H* 0.266667) (L+H* 0.188889) (L* 0.0444444) (L*+H 0) H*))
                                  (((NONE 0.326316) (H* 0.210526) (!H* 0.221053) (L+H* 0.189474) (L* 0.0526316) (L*+H 0) NONE))))))
                              ((R:SylStructure.parent.R:Word.p.gpos is in)
                               (((NONE 0.0625) (H* 0.296875) (!H* 0.265625) (L+H* 0.328125) (L* 0.046875) (L*+H 0) L+H*))
                               ((syl_in is 6)
                                (((NONE 0.271739) (H* 0.152174) (!H* 0.358696) (L+H* 0.184783) (L* 0.0326087) (L*+H 0) !H*))
                                ((syl_out is 2)
                                 (((NONE 0.111111) (H* 0.361111) (!H* 0.319444) (L+H* 0.138889) (L* 0.0555556) (L*+H 0.0138889) H*))
                                 ((syl_in is 4)
                                  (((NONE 0.224) (H* 0.152) (!H* 0.328) (L+H* 0.24) (L* 0.056) (L*+H 0) !H*))
                                  ((n.stress is 0)
                                   ((syl_in is 3)
                                    (((NONE 0.0833333) (H* 0.333333) (!H* 0.233333) (L+H* 0.216667) (L* 0.133333) (L*+H 0) H*))
                                    (((NONE 0.283465) (H* 0.188976) (!H* 0.23622) (L+H* 0.204724) (L* 0.0708661) (L*+H 0.015748) NONE)))
                                   (((NONE 0.305263) (H* 0.284211) (!H* 0.210526) (L+H* 0.178947) (L* 0.0210526) (L*+H 0) NONE))))))))))))))))))))))))))))))))))))))))
)

;          NON             L-L  L-H  H-L  
;    NONE13017    0    0    0    0    0 13017      [13017/13017]     100.000
;       H- 339   81    0    1    1    0  422      [81/422]      19.194
;       L- 223   52    0    5    0    0  280      [0/280]       0.000
;     L-L%  17    0    0 1057   96    0 1170      [1057/1170]      90.342
;     L-H%  16    0    0  457  139    0  612      [139/612]      22.712
;     H-L%   5    0    0   30    4    0   39      [0/39]       0.000
;        13617  133    0 1550  240    0 
;total 15540 correct 14294.000 91.982%
(set! f2b_int_tone_cart_tree
'((lisp_syl_yn_question is 1)
  (((H-H% 1.0) H-H%))
((R:SylStructure.parent.gpos is cc)
 (((NONE 0.996942) (H- 0.0030581) (L- 0) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
 ((ssyl_in is 10)
  (((NONE 0.989041) (H- 0.00273973) (L- 0) (L-L% 0.00273973) (L-H% 0.00547945) (H-L% 0) NONE))
  ((R:SylStructure.parent.gpos is md)
   (((NONE 0.986014) (H- 0) (L- 0) (L-L% 0.00699301) (L-H% 0.00699301) (H-L% 0) NONE))
   ((p.old_syl_break is 4)
    (((NONE 0.99462) (H- 0.00239091) (L- 0.00119546) (L-L% 0) (L-H% 0.00119546) (H-L% 0.000597729) NONE))
    ((R:SylStructure.parent.gpos is det)
     (((NONE 0.984635) (H- 0.00512164) (L- 0.00384123) (L-L% 0.00384123) (L-H% 0.00256082) (H-L% 0) NONE))
     ((n.old_syl_break is 3)
      (((NONE 0.981848) (H- 0.00495049) (L- 0.00330033) (L-L% 0.00660066) (L-H% 0.00330033) (H-L% 0) NONE))
      ((n.old_syl_break is 4)
       (((NONE 0.986982) (H- 0.000591716) (L- 0.0100592) (L-L% 0.00118343) (L-H% 0.00118343) (H-L% 0) NONE))
       ((R:SylStructure.parent.gpos is in)
        (((NONE 0.977865) (H- 0.00390625) (L- 0.00390625) (L-L% 0.0078125) (L-H% 0.00651042) (H-L% 0) NONE))
        ((old_syl_break is 4)
         ((R:SylStructure.parent.R:Word.n.gpos is 0)
          (((NONE 0) (H- 0.00892857) (L- 0) (L-L% 0.982143) (L-H% 0.00892857) (H-L% 0) L-L%))
          ((R:SylStructure.parent.R:Word.p.gpos is aux)
           (((NONE 0) (H- 0) (L- 0) (L-L% 0.761905) (L-H% 0.238095) (H-L% 0) L-L%))
           ((R:SylStructure.parent.R:Word.n.gpos is det)
            (((NONE 0) (H- 0) (L- 0) (L-L% 0.652542) (L-H% 0.347458) (H-L% 0) L-L%))
            ((ssyl_in is 4)
             (((NONE 0) (H- 0) (L- 0) (L-L% 0.682243) (L-H% 0.313084) (H-L% 0.0046729) L-L%))
             ((syl_in is 6)
              (((NONE 0) (H- 0) (L- 0.00649351) (L-L% 0.688312) (L-H% 0.298701) (H-L% 0.00649351) L-L%))
              ((R:SylStructure.parent.R:Word.n.gpos is aux)
               (((NONE 0) (H- 0) (L- 0) (L-L% 0.464286) (L-H% 0.535714) (H-L% 0) L-H%))
               ((syl_in is 5)
                (((NONE 0) (H- 0) (L- 0) (L-L% 0.666667) (L-H% 0.322034) (H-L% 0.0112994) L-L%))
                ((sub_phrases is 2)
                 (((NONE 0) (H- 0) (L- 0) (L-L% 0.696429) (L-H% 0.267857) (H-L% 0.0357143) L-L%))
                 ((R:SylStructure.parent.R:Word.p.gpos is det)
                  (((NONE 0) (H- 0) (L- 0) (L-L% 0.628866) (L-H% 0.350515) (H-L% 0.0206186) L-L%))
                  ((sub_phrases is 0)
                   ((R:SylStructure.parent.R:Word.n.gpos is in)
                    ((n.old_syl_break is 0)
                     (((NONE 0) (H- 0) (L- 0) (L-L% 0.68254) (L-H% 0.31746) (H-L% 0) L-L%))
                     (((NONE 0) (H- 0.0147059) (L- 0) (L-L% 0.338235) (L-H% 0.632353) (H-L% 0.0147059) L-H%)))
                    ((n.stress is 0)
                     (((NONE 0) (H- 0) (L- 0.0108303) (L-L% 0.599278) (L-H% 0.32491) (H-L% 0.064982) L-L%))
                     (((NONE 0) (H- 0) (L- 0) (L-L% 0.386364) (L-H% 0.579545) (H-L% 0.0340909) L-H%))))
                   (((NONE 0) (H- 0) (L- 0.00456621) (L-L% 0.652968) (L-H% 0.324201) (H-L% 0.0182648) L-L%))))))))))))
         ((R:SylStructure.parent.gpos is pps)
          (((NONE 0.988764) (H- 0.011236) (L- 0) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
          ((syl_in is 0)
           (((NONE 0.984848) (H- 0.0126263) (L- 0.00252525) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
           ((R:SylStructure.parent.gpos is content)
            ((R:SylStructure.parent.R:Word.nn.gpos is 0)
             (((NONE 0.967914) (H- 0.0106952) (L- 0.0213904) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
             ((pp.old_syl_break is 4)
              (((NONE 0.972315) (H- 0.0232558) (L- 0.00442968) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
              ((syl_in is 1)
               (((NONE 0.951163) (H- 0.0372093) (L- 0.0116279) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
               ((nn.old_syl_break is 4)
                (((NONE 0.956244) (H- 0.0127621) (L- 0.0291705) (L-L% 0) (L-H% 0) (H-L% 0.00182315) NONE))
                ((R:SylStructure.parent.R:Word.nn.gpos is in)
                 (((NONE 0.941919) (H- 0.0378788) (L- 0.020202) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                 ((R:SylStructure.parent.R:Word.p.gpos is cc)
                  (((NONE 0.919643) (H- 0.0714286) (L- 0.00892857) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                  ((nn.old_syl_break is 3)
                   (((NONE 0.927273) (H- 0.0472727) (L- 0.0254545) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                   ((R:SylStructure.parent.R:Word.nn.gpos is cc)
                    (((NONE 0.921569) (H- 0.0588235) (L- 0.0196078) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                    ((ssyl_in is 0)
                     (((NONE 0.911591) (H- 0.0825147) (L- 0.00589391) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                     ((R:SylStructure.parent.R:Word.nn.gpos is to)
                      (((NONE 0.912281) (H- 0.0350877) (L- 0.0526316) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                      ((R:SylStructure.parent.R:Word.pp.gpos is to)
                       (((NONE 0.894737) (H- 0.0526316) (L- 0.0526316) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                       ((R:SylStructure.parent.R:Word.p.gpos is in)
                        (((NONE 0.888554) (H- 0.0662651) (L- 0.0451807) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                        ((R:SylStructure.parent.R:Word.pp.gpos is in)
                         (((NONE 0.875817) (H- 0.0718954) (L- 0.0522876) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                         ((syl_in is 2)
                          (((NONE 0.869942) (H- 0.0867052) (L- 0.0433526) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                          ((R:SylStructure.parent.R:Word.nn.gpos is aux)
                           (((NONE 0.854839) (H- 0.0967742) (L- 0.0483871) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                           ((sub_phrases is 1)
                            (((NONE 0.836538) (H- 0.0721154) (L- 0.0913462) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                            ((R:SylStructure.parent.R:Word.pp.gpos is det)
                             (((NONE 0.832402) (H- 0.0949721) (L- 0.0726257) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                             ((ssyl_in is 4)
                              (((NONE 0.793103) (H- 0.103448) (L- 0.103448) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                              ((n.old_syl_break is 0)
                               (((NONE 0.850816) (H- 0.0839161) (L- 0.0652681) (L-L% 0) (L-H% 0) (H-L% 0) NONE))
                               ((R:SylStructure.parent.R:Word.n.gpos is content)
                                (((NONE 0.889447) (H- 0.0753769) (L- 0.0251256) (L-L% 0) (L-H% 0) (H-L% 0.0100503) NONE))
                                ((old_syl_break is 3)
                                 (((NONE 0) (H- 0.609023) (L- 0.390977) (L-L% 0) (L-H% 0) (H-L% 0) H-))
                                 (((NONE 1) (H- 0) (L- 0) (L-L% 0) (L-H% 0) (H-L% 0) NONE)))))))))))))))))))))))
            (((NONE 0.978947) (H- 0.0131579) (L- 0.00789474) (L-L% 0) (L-H% 0) (H-L% 0) NONE)))))))))))))))

)

(defvar tobi_support_yn_questions t
  "tobi_support_yn_questions
If set a crude final rise will be added at utterance that are judged
to be yesy/no questions.  Namely ending in a ? and not starting with
a wh-for word.")

(define (first_word syl)
  (let ((w (item.relation.parent syl 'SylStructure)))
    (item.relation.first w 'Word)))

(define (syl_yn_question syl)
"(syl_yn_question utt syl)
Return 1 if this is the last syllable in a yes-no question.  Basically
if it ends in question mark and doesn't start with a wh-woerd.  This
isn't right but it depends on how much you want rising intonation."
  (if (and 
       tobi_support_yn_questions
       (member_string (item.feat syl "syl_break") '("4" "3"))
       (not (member_string 
	     (downcase (item.name (first_word syl)))
	     '("how" "why" "which" "who" "what" "where" "when")))
       (string-matches 
	(item.feat syl "R:SylStructure.parent.R:Token.parent.punc")
		       ".*\\?.*"))
      "1"
      "0"))

(provide 'tobi)
