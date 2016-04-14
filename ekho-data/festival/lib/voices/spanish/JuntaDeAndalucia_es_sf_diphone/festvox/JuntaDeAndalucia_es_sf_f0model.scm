;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                      JUNTA DE ANDALUCÍA                             ;;;
;;;                      Copyright (c) 2007                             ;;;
;;;                      All Rights Reserved.                           ;;;
;;;                                                                     ;;;
;;;  Distribution policy.                                               ;;;
;;;                                                                     ;;;
;;;  Free for any use.                                                  ;;;
;;;                                                                     ;;;
;;;  All the work is based on the Festvox Toolkit, provided by:         ;;;
;;;    - Carnegie Mellon University (http://www.festvox.org)            ;;;
;;;                                                                     ;;;
;;;  The copyright below belongs to the original Festvox project; it    ;;;
;;;  therefore applies to the present work.                             ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;  Standard Spanish female voice                                      ;;;
;;;                                                                     ;;;
;;;  Contractor: Consejería de Innovación, Ciencia y Empresa            ;;;
;;;              de la Junta de Andalucía                               ;;;
;;;                                                                     ;;;
;;;  Developed by: MP Sistemas and                                      ;;;
;;;                Intelligent Dialogue Systems S.L. (INDISYS)          ;;;
;;;                                                                     ;;;
;;;  Authors:   Del Solar, Carmen <c.delsolar@indisys.es>               ;;;
;;;             González, Jesús   <j.gonzalez@indisys.es>               ;;;
;;;             Manchón, Pilar    <p.manchon@indisys.es>                ;;;
;;;             Martín, Antonio   <amam@mpsistemas.es>                  ;;;
;;;             Martínez, Diego   <d.martinez@indisys.es>               ;;;
;;;             Pérez, Guillermo  <g.perez@indisys.es>                  ;;;
;;;             Varela, Víctor    <vmvr@mpsistemas.es>                  ;;;
;;;                                                                     ;;;
;;;  Voice Talent:  Silvia Fernández                                    ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; F0 model for JuntaDeAndalucia_es_sf
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load any necessary files here

;;This loads the predicted CART_tree F0 model (using wagon, and not ols, which allows lineal regression analysis with LR)
(require 'JuntaDeAndalucia_es_sf_contourmodel_f0)
;;This loads the module that we need for using (Parameter.set ’Int_Target_Method Int_Targets_Tree)
;;This file is stored in festival/src/intonation/tree_f0.scm
(require 'tree_f0)

;;This sets the values needed when using the TOBI LR F0 model
;(set! JuntaDeAndalucia_es_sf_int_lr_params
;      '(
	;; These numbers may be modified according to the speaker's range.
;	(target_f0_mean 105)   ;; speaker's mean F0
;	(target_f0_std 14)     ;; speaker's range
	;; These number should remain as they are
;	(model_f0_mean 170)
;	(model_f0_std 34)))

(define (JuntaDeAndalucia_es_sf_targ_func1 utt syl)
 "(JuntaDeAndalucia_es_sf_targ_func1 utt syl)
Simple hat accents."
  (let ((start (item.feat syl 'syllable_start))
        (end (item.feat syl 'syllable_end))
        (ulen (item.feat (utt.relation.last utt 'Segment ) 'segment_end))
        nstart nend fustart fuend fuend fstart fend)
        (set! nstart (/ start ulen))
        (set! nend (/ end ulen))
        (set! fustart '155)
        (set! fuend   '135)
        (set! fstart  (+ (* (- fuend fustart) nstart) fustart))
        (set! fend    (+ (* (- fuend fustart) nend) fustart))
      
    (cond
      ((equal? (item.feat syl "R:Intonation.daughter1.name") "Accented")
       (list
        (list start fstart)
        (list (+ start 0.010) (+ fstart 10 ))
        (list (- end   0.010) (+ fstart 8 ))   
        (list end   fend) 
	))
      ((not (item.next syl))
       (list
	(list end fuend)))
      ((not (item.prev syl))
       (list
	(list start fustart)))
      (t
       nil))))

(define (JuntaDeAndalucia_es_sf::select_f0model)
  "(JuntaDeAndalucia_es_sf::select_f0model)
Set up the F0 model for JuntaDeAndalucia_es."
  ;;Uncomment this for using the rule-based F0 contour model
  (set! int_general_params (list (list 'targ_func JuntaDeAndalucia_es_sf_targ_func1)))
  (Parameter.set 'Int_Method 'General)
  ;;Uncomment this for using the linear regression F0 contour model
  ;;This requires JuntaDeAndalucia_es_sf_f0model_f0.scm and tree_f0.scm
;  (set! F0start_tree Andalucia_Indisys_MP_es_sf_contourmodel::start_f0)
;  (set! F0mid_tree Andalucia_Indisys_MP_es_sf_contourmodel::mid_f0)
;  (set! F0end_tree Andalucia_Indisys_MP_es_sf_contourmodel::end_f0)
;  (set! int_params  
;      '((target_f0_mean 180) (target_f0_std 20)
;	(model_f0_mean 150) (model_f0_std 20)))
;  (Parameter.set 'Int_Target_Method Int_Targets_Tree)
)

(define (JuntaDeAndalucia_es_sf::reset_f0model)
  "(JuntaDeAndalucia_es_sf::reset_f0model)
Reset F0 model information."
  t
)

(provide 'JuntaDeAndalucia_es_sf_f0model)
